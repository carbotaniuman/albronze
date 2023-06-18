mod conditional;
mod define;
mod expand;

#[cfg(test)]
mod tests;

use crate::error::{ErrorHandler, Warning};
use crate::location::{Locatable, Location, SourceKind};
use crate::InternedStr;

use super::error::*;
use super::lexer::Lexer;
use super::manager::{FileManager, IncludeKind};
use super::token::*;

use arcstr::ArcStr;
use derive_more::From;
use indexmap::IndexSet;

use crate::get_str;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq)]
struct Macro {
    builtin: bool,
    kind: MacroKind,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum BaseReplacement {
    // TODO: vararg, param?
    Token(Locatable<TokenKind>),
    Stringify(Locatable<usize>),
    StringifyVarargs(Location),
}

#[derive(From, Clone, Debug, PartialEq)]
enum ReplacementKind<Base = BaseReplacement> {
    #[from]
    Base(Base),
    Concatenate(Vec<Base>),
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum VariadicType {
    Standard,
    Gnu(InternedStr),
    None,
}

#[derive(Clone, Debug, PartialEq)]
enum MacroKind {
    Object {
        replacement: Vec<Locatable<TokenKind>>,
    },
    Function {
        replacement: Vec<ReplacementKind>,
        params: IndexSet<InternedStr>,
        variadic: VariadicType,
    },
}

type Definitions = HashMap<InternedStr, (Macro, Location)>;

/// Keeps track of (part of) the state of a
/// conditional inclusion directive.
///
/// `If` means we are currently processing an `#if`,
/// `Elif` means an `#elif`, and `Else` means an `#else`.
///
///
/// ```dot
/// strict digraph if_state {
///    IF [label="self.nested_ifs.push(If)"];
///    ELIF [label="self.nested_ifs.push(Elif)"];
///    ELSE [label="self.nested_ifs.push(Else)"];
///    END [label="self.nested_ifs.pop()"];
///
///    start -> IF [label="#if 1"]
///    IF -> END [label="#endif"]
///    IF -> consume_all [label="#elif ... / #else"]
///    consume_all -> END [label="#endif"]
///
///    start -> consume_if [label="#if 0"]
///    consume_if -> consume_if [label="#elif 0"]
///    consume_if -> ELIF [label="#elif 1"]
///    consume_if -> ELSE [label="#else"]
///    consume_if -> END  [label="#endif"]
///
///    ELIF -> consume_all [label="#elif ... / #else"]
///    ELIF -> END         [label="#endif"]
///
///    ELSE -> END [label="#endif"]
///  }
/// ```
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IfKind {
    If,
    Elif,
    Else,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IfMatched {
    // No #if or #elif has been matched
    None,
    // The current #if or #elif is being matched
    Current,
    // A previous #if or #elif has been matched
    Previous,
    // The entire #if we are in is being skipped over
    Ignored,
}

impl IfMatched {
    fn initial_if(val: bool) -> Self {
        if val {
            IfMatched::Current
        } else {
            IfMatched::None
        }
    }

    fn middle_elif(self, val: bool) -> Self {
        if self == IfMatched::Ignored {
            IfMatched::Ignored
        } else if self.has_matched() {
            IfMatched::Previous
        } else if val {
            IfMatched::Current
        } else {
            IfMatched:: None
        }
    }

    fn last_else(self) -> Self {
        if self == IfMatched::Ignored {
            IfMatched::Ignored
        } else if self.has_matched() {
            IfMatched::Previous
        } else {
            IfMatched::Current
        }
    }

    fn inert(self) -> Self {
        match self {
            IfMatched::Current => IfMatched::Previous,
            a => a,
        }
    }

    fn has_matched(self) -> bool {
        matches!(self, IfMatched::Current | IfMatched::Previous)
    }
}

/// The `bool` parameter states whether we have
/// seen a true conditional and taken the code inside,
/// so #if 1 would translte to `If(true)` and #if 0
/// would translate to `If(false)`.
///
/// After processing the #if and upon encountering an #elif,
/// the new state would then be `Elif(false)`.
type IfState = (IfKind, IfMatched);

pub struct Preprocessor {
    pub pending_tokens: Vec<Locatable<TokenKind>>,

    definitions: Definitions,
    poisoned: HashSet<InternedStr>,
    counter: u32,

    nested_ifs: Vec<IfState>,

    pub file_manager: FileManager,
    external_mode: bool,

    pub error_handler: RefCell<ErrorHandler<CppError>>,
}

impl Preprocessor {
    pub fn new(file_manager: FileManager, external_mode: bool) -> Self {
        let mut ret = Self {
            pending_tokens: Vec::new(),

            definitions: Definitions::new(),
            poisoned: HashSet::new(),
            counter: 0,

            nested_ifs: Vec::new(),

            file_manager,
            external_mode,

            error_handler: RefCell::new(ErrorHandler::new()),
        };

        ret.preprocess_file(SourceKind::Generated, include_str!("../headers/predefined.h").into());

        ret
    }

    pub fn preprocess_file(&mut self, source: SourceKind, data: ArcStr) {
        let lexer = &mut Lexer::new(source, data, false, self.external_mode);
        let mut ifs = Vec::<IfState>::with_capacity(64);

        // This would be a simple boolean were it not for the complexities of trying to
        // warn about missing newline at EOF, and omitting newlines in the standalone preprocessor
        // mode.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
        enum LineType {
            // no token since newline
            Start,
            // some whitespace non-comments since newline
            PlainWhitespace,
            // we dont allow directives anymore
            DisallowDirective,
        }

        let mut start_of_line = LineType::Start;

        loop {
            let next = lexer.next();
            let token = match next {
                Some(token) => token,
                None => {
                    break;
                }
            };

            let token = match token {
                Ok(token) => token,
                Err(e) => {
                    self.error_handler.get_mut().push_error(e.map(|x| x.into()));
                    continue;
                }
            };

            use TokenKind::*;
            use WhitespaceKind::*;

            let skip_to_newline = |lexer: &mut Lexer| {
                lexer.skip_line();
            };

            // We are skipping the tokens between an #if and and #elif, for example
            let currently_skipping_tokens = match self.nested_ifs.last() {
                Some((_, IfMatched::Current)) | None => false,
                _ => true,
            };

            // We are currently skipping directives because we encountered an #if
            // while we were skipping tokens.
            let currently_skipping_directives = match self.nested_ifs.last() {
                Some((_, IfMatched::Ignored)) => true,
                _ => false,
            };
            debug_assert!(!currently_skipping_tokens || start_of_line != LineType::DisallowDirective);
            
            match token.data {
                Hash(_) => {
                    if start_of_line != LineType::DisallowDirective {
                        // Delete all previous whitespace on this line
                        while let Some(TokenKind::Whitespace(t)) =
                            self.pending_tokens.last().map(|x| x.data)
                        {
                            if t != WhitespaceKind::Newline {
                                self.pending_tokens.pop();
                            } else {
                                break;
                            }
                        }

                        let directive = match self.expect_directive(lexer) {
                            Ok(x) => x,
                            Err(e) => {
                                self.error_handler.get_mut().push_error(e);
                                skip_to_newline(lexer);
                                continue;
                            }
                        };

                        use DirectiveKind::*;
                        let res = match directive.data {
                            // Match directives dealing with #if's first
                            If => (|| {
                                let to_push = if currently_skipping_tokens {
                                    (IfKind::If, IfMatched::Ignored)
                                } else {
                                    let value = self.if_boolean_expr(lexer)?;
                                    (IfKind::If, IfMatched::initial_if(value))
                                };
                                
                                self.nested_ifs.push(to_push);
                                
                                Ok(())
                            })(),

                            IfDef => (|| {
                                let to_push = if currently_skipping_tokens {
                                    (IfKind::If, IfMatched::Ignored)
                                } else {
                                    let id = self.expect_id(lexer, false)?;
                                    let exists = self.definitions.contains_key(&id.data);
                                    (IfKind::If, IfMatched::initial_if(exists))
                                };

                                
                                self.nested_ifs.push(to_push);
                                Ok(())
                            })(),

                            IfNDef => (|| {
                                let to_push = if currently_skipping_tokens {
                                    (IfKind::If, IfMatched::Ignored)
                                } else {
                                    let id = self.expect_id(lexer, false)?;
                                    let exists = self.definitions.contains_key(&id.data);
                                    (IfKind::If, IfMatched::initial_if(!exists))
                                };

                                self.nested_ifs.push(to_push);
                                Ok(())
                            })(),

                            Elif => (|| {
                                let value = if !currently_skipping_directives {
                                    self.if_boolean_expr(lexer)?
                                } else {
                                    false
                                };

                                if let Some((a @ (IfKind::If | IfKind::Elif), v)) = self.nested_ifs.last_mut() {
                                    *a = IfKind::Elif;
                                    *v = v.middle_elif(value);
                                    
                                    Ok(())
                                } else {
                                    Err(directive
                                        .location
                                        .with(CppError::UnexpectedElse))
                                }
                            })(),

                            Else => {
                                if let Some((a @ (IfKind::If | IfKind::Elif), v)) = self.nested_ifs.last_mut() {
                                    *a = IfKind::Else;
                                    *v = v.last_else();

                                    Ok(())
                                } else {
                                    Err(directive
                                        .location
                                        .with(CppError::UnexpectedElse))
                                }
                            }

                            EndIf => {
                                if self.nested_ifs.pop().is_none() {
                                    Err(directive.location.with(CppError::UnexpectedEndIf))
                                } else {
                                    Ok(())
                                }
                            }

                            // If we should ignore tokens,
                            // ignore all other directives
                            _ if currently_skipping_tokens => Ok(()),

                            Define => self.define(lexer),
                            Undef => (|| {
                                let id = self.expect_id(lexer, false)?;
                                self.definitions.remove(&id.data);
                                self.expect_no_tokens(lexer)
                            })(),
                            Include => {
                                lexer.set_include_mode(true);
                                let header = lexer.next_non_whitespace();
                                lexer.set_include_mode(false);
                                if let Some(Ok(Locatable {
                                    data: TokenKind::HeaderName { global, name },
                                    ..
                                })) = header
                                {
                                    let data = self.file_manager.include_path(
                                        std::path::Path::new(get_str!(name)),
                                        std::path::Path::new(""),
                                        if global {
                                            IncludeKind::System
                                        } else {
                                            IncludeKind::Local
                                        },
                                    ).unwrap();
                                    self.preprocess_file(
                                        data.1,
                                        data.0,
                                    );

                                    Ok(())
                                } else {
                                    Err(directive
                                        .location
                                        .with(CppError::InclusionError(IncludeError::BadInclude)))
                                }
                            }

                            Warning => {
                                // ignored
                                Ok(())
                            }

                            Error => {
                                for i in Self::iter_tokens_one_line(lexer) {
                                    println!("{:?}", i);
                                }

                                todo!()
                            }

                            t => todo!("directive {t:?}"),
                        };

                        if let Err(e) = res {
                            self.error_handler.get_mut().push_error(e);
                            skip_to_newline(lexer);
                        }
                    } else {
                        self.pending_tokens.push(token);
                    }
                }
                Whitespace(kind) => {
                    if kind == Newline {
                        start_of_line = LineType::Start;

                        self.pending_tokens.push(token);
                    } else {
                        if start_of_line < LineType::PlainWhitespace {
                            start_of_line = LineType::PlainWhitespace;
                        }

                        // Only add comments when we're emitting for
                        // external users.
                        if !kind.is_comment() || self.external_mode {
                            self.pending_tokens.push(token);
                        }
                    }
                }
                c => {
                    if currently_skipping_tokens {
                        skip_to_newline(lexer);
                        continue;
                    }

                    start_of_line = LineType::DisallowDirective;
                    if let Identifier(ident) = c {
                        if self.poisoned.contains(&ident) {
                            todo!("poisoned")
                        }
                    }
                    self.process_next_token(lexer, token);
                }
            }
        }
    }

    fn iter_tokens_one_line<'a>(
        lexer: &'a mut Lexer,
    ) -> impl Iterator<Item = Result<Locatable<TokenKind>, Locatable<CppError>>> + 'a {
        std::iter::from_fn(|| {
            let token = lexer.next_non_whitespace();

            if let Some(Ok(Locatable {
                data: TokenKind::Whitespace(WhitespaceKind::Newline), ..
            })) = token {
                return None;
            }

            token.map(|token| {
                token.map_err(|locatable| locatable.map(|e| e.into()))
            })
        })
    }

    fn expect_id(
        &mut self,
        lexer: &mut Lexer,
        skip_newline: bool,
    ) -> Result<Locatable<InternedStr>, Locatable<CppError>> {
        self.expect_id_raw(lexer, skip_newline, "identifier")
    }

    fn expect_directive(
        &mut self,
        lexer: &mut Lexer,
    ) -> Result<Locatable<DirectiveKind>, Locatable<CppError>> {
        match self.expect_id_raw(lexer, false, "directive") {
            Ok(Locatable {
                data: ident,
                location,
            }) => {
                if let Ok(directive) = DirectiveKind::try_from(get_str!(ident)) {
                    Ok(Locatable::new(directive, location))
                } else {
                    Err(Locatable::new(CppError::InvalidDirective.into(), location))
                }
            }
            // TODO: better handle this error
            Err(err) => Err(err),
        }
    }

    fn expect_id_raw(
        &mut self,
        lexer: &mut Lexer,
        skip_newline: bool,
        expected: &'static str,
    ) -> Result<Locatable<InternedStr>, Locatable<CppError>> {
        let next = if skip_newline {
            lexer.next_non_whitespace_any()
        } else {
            lexer.next_non_whitespace()
        };

        match next {
            Some(Ok(Locatable {
                data: TokenKind::Identifier(name),
                location,
            })) => Ok(Locatable::new(name, location)),
            Some(Ok(other)) => {
                Err(other.map(|tok| CppError::UnexpectedToken(expected, tok).into()))
            }
            Some(Err(other)) => Err(other.map(|err| err.into())),
            None => Err(Locatable {
                data: CppError::EndOfFile(expected).into(),
                location: lexer.span(lexer.offset()),
            }),
        }
    }

    fn expect_no_tokens(&mut self, lexer: &mut Lexer) -> Result<(), Locatable<CppError>> {
        while let Some(s) = lexer.next() {
            if let Ok(Locatable { data, location }) = s {
                match data {
                    TokenKind::Whitespace(s) => {
                        if s == WhitespaceKind::Newline {
                            break;
                        }
                        continue;
                    }
                    _ => {
                        return Err(location.with(CppError::UnexpectedToken(
                            "whitespace or newline",
                            TokenKind::Whitespace(WhitespaceKind::Newline),
                        )));
                    }
                }
            }
        }

        return Ok(());
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum DirectiveKind {
    If,
    IfDef,
    IfNDef,
    Elif,
    Else,
    EndIf,
    Include,
    Define,
    Undef,
    Line,
    Warning,
    Error,
    Pragma,
}

impl TryFrom<&str> for DirectiveKind {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        use DirectiveKind::*;
        Ok(match s {
            "if" => If,
            "elif" => Elif,
            "endif" => EndIf,
            "else" => Else,
            "ifdef" => IfDef,
            "ifndef" => IfNDef,
            "include" => Include,
            "define" => Define,
            "undef" => Undef,
            "line" => Line,
            "warning" => Warning,
            "error" => Error,
            "pragma" => Pragma,
            _ => return Err(()),
        })
    }
}
