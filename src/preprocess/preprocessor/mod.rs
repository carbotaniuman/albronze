mod define;
mod expand;

#[cfg(test)]
mod tests;

use crate::error::{ErrorHandler, Warning};
use crate::location::{Locatable, Location, SourceKind};
use crate::preprocess::LexResult;
use crate::InternedStr;

use super::error::*;
use super::lexer::Lexer;
use super::manager::{FileManager, IncludeKind};
use super::token::*;

use arcstr::ArcStr;
use codespan::FileId;
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

/// Keeps track of the state of a conditional inclusion directive.
///
/// `If` means we are currently processing an `#if`,
/// `Elif` means an `#elif`, and `Else` means an `#else`.
///
/// There are more states, but they are tracked internally to `consume_directive()`.
/// The state diagram looks like this (pipe to `xdot -` for visualization):
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
#[derive(Copy, Clone, Debug)]
enum IfState {
    If,
    Elif,
    Else,
}

pub struct Preprocessor {
    pub pending_tokens: Vec<Locatable<TokenKind>>,

    definitions: Definitions,
    poisoned: HashSet<InternedStr>,
    counter: u32,

    file_manager: FileManager,
    external_mode: bool,

    pub error_handler: RefCell<ErrorHandler<CppError>>,
}

impl Preprocessor {
    pub fn new(file_manager: FileManager, external_mode: bool) -> Self {
        Self {
            pending_tokens: Vec::new(),

            definitions: Definitions::new(),
            poisoned: HashSet::new(),
            counter: 0,

            file_manager,
            external_mode,

            error_handler: RefCell::new(ErrorHandler::new()),
        }
    }

    pub fn preprocess_file(&mut self, source: FileId, data: ArcStr) {
        let lexer = &mut Lexer::new(SourceKind::File(source), data, false, self.external_mode);
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
                    if start_of_line == LineType::Start {
                        self.error_handler
                            .get_mut()
                            .warn(Warning::NoNewlineAtEOF, lexer.span(lexer.offset()));
                    }

                    break;
                }
            };

            let token = match token {
                Ok(token) => token,
                Err(e) => todo!("{:?}", e),
            };

            use TokenKind::*;
            use WhitespaceKind::*;
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
                                self.skip_to_newline(lexer);
                                continue;
                            }
                        };

                        use DirectiveKind::*;
                        let res = match directive.data {
                            Define => self.define(lexer),
                            Undef => Ok(()),
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
                                    );
                                    self.preprocess_file(
                                        unsafe { std::mem::transmute(1) },
                                        data.unwrap().0,
                                    );

                                    Ok(())
                                } else {
                                    Err(directive
                                        .location
                                        .with(CppError::InclusionError(IncludeError::BadInclude)))
                                }
                            }

                            t => todo!("directive {t:?} not implemetned"),
                        };

                        if let Err(e) = res {
                            self.error_handler.get_mut().push_error(e);
                            self.skip_to_newline(lexer);
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

    fn skip_to_newline(&mut self, lexer: &mut Lexer) {
        lexer.skip_line();
    }

    fn expect_newline(&mut self, lexer: &mut Lexer) -> LexResult<()> {
        let mut errored = false;
        while let Some(s) = lexer.next() {
            if let Ok(Locatable { data, location }) = s {
                match data {
                    TokenKind::Whitespace(s) => {
                        if s == WhitespaceKind::Newline {
                            return Ok(());
                        }
                        continue;
                    }
                    t => {
                        if !errored {
                            self.error_handler.get_mut().error(
                                CppError::UnexpectedToken(
                                    "whitespace or newline",
                                    TokenKind::Whitespace(WhitespaceKind::Newline),
                                ),
                                location,
                            );

                            errored = true;
                        }
                    }
                }
            }
        }

        self.error_handler
            .get_mut()
            .warn(Warning::NoNewlineAtEOF, lexer.span(lexer.offset()));

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
