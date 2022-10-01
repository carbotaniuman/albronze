mod ast;
mod decl;
mod error;
mod expr;
mod stmt;

use crate::error::{ErrorHandler, Warning};
use crate::location::{Locatable, Location};
use crate::parse::error::SyntaxError;
use crate::preprocess::{EncodingKind, Keyword, LexResult, LiteralKind, TokenKind};
use crate::scope::Scope;
use crate::InternedStr;
use ast::ExternalDeclaration;

use std::collections::VecDeque;
use std::mem;

type Lexeme = LexResult<Locatable<TokenKind>>;
type SyntaxResult<T> = Result<T, Locatable<SyntaxError>>;

pub struct Parser<I: Iterator<Item = Lexeme>> {
    /// Parser hack so that we know that `typedef int i; i j;`
    /// is legal, and also to resolve ambiguities
    typedefs: Scope<InternedStr, ()>,
    /// VecDeque supports pop_front with reasonable efficiency
    /// this is useful because there could be multiple declarators
    /// in a single declaration; e.g. `int a, b, c;`
    pending: VecDeque<Locatable<ExternalDeclaration>>,

    /// Whether to debug each declaration
    debug: bool,
    /// in case we get to the end of the file and want to show an error
    last_location: Location,
    /// Internal API which makes it easier to return errors lazily
    error_handler: ErrorHandler<SyntaxError>,

    /// We iterate lazily over the tokens, so if we have a program that's mostly valid but
    /// breaks at the end, we don't only show lex errors
    tokens: std::iter::Peekable<I>,
    /// the last token we saw from the Lexer. None if we haven't looked ahead.
    /// Should only be used in this module.
    current: Option<Locatable<TokenKind>>,
    /// TODO: are we sure we need 2 tokens of lookahead?
    /// this was put here for declarations, so we know the difference between
    /// int (*x) and int (int), but there's probably a workaround
    next: Option<Locatable<TokenKind>>,
}

impl<I: Iterator<Item = Lexeme>> Parser<I> {
    pub fn new(tokens: I, debug: bool) -> Self {
        Parser {
            typedefs: Default::default(),
            tokens: tokens.peekable(),
            pending: Default::default(),
            // The only time this is used is when an error occurs,
            // which only happens after at least one token has been seen.
            // So this default location will never be used.
            last_location: Location::default(),
            current: None,
            next: None,
            debug,
            error_handler: ErrorHandler::new(),
            // recursion_guard: Default::default(),
        }
    }

    /// Return whether this parser has fully finished parsing.
    ///
    /// This can be used if, for example, you call `parser.expr()`
    /// and want to see if there are any left-over tokens.
    pub fn is_empty(&mut self) -> bool {
        self.peek_token().is_none()
    }
}

impl<I: Iterator<Item = Lexeme>> Iterator for Parser<I> {
    type Item = SyntaxResult<Locatable<ExternalDeclaration>>;
    /// ```yacc
    /// translation_unit
    /// : external_declaration
    /// | translation_unit external_declaration
    /// ;
    ///
    /// external_declaration
    /// : function_definition
    /// | declaration
    /// ;
    ///
    /// function_definition
    /// : declarator compound_statement
    /// | declaration_specifiers declarator compound_statement
    /// ;
    /// ```
    /// <http://www.quut.com/c/ANSI-C-grammar-y.html#translation_unit>
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // check for pending changes from the last declaration
            if let Some(err) = self.error_handler.pop_error() {
                return Some(Err(err));
            } else if let Some(decl) = self.pending.pop_front() {
                if self.debug {
                    println!("ast: {:?}", decl.data);
                }
                return Some(Ok(decl));
            }

            // Check for end of file
            if self.peek_token().is_none() {
                // peek_token can encounter lex errors that it doesn't return
                return self.error_handler.pop_error().map(Err);
            }

            // Remove extra semicolons
            while let Some(locatable) = self.match_next(&TokenKind::Semicolon) {
                self.error_handler.warn(
                    Warning::ExtraneousSemicolon("top level"),
                    locatable.location,
                );
            }

            // Parse more of our file
            match self.external_declaration() {
                Ok(decls) => {
                    self.pending.push_back(decls);
                }
                Err(err) => {
                    // there could be semantic errors that were reported in the meantime,
                    // so we can't just return this error (it might be in the wrong order)
                    self.error_handler.push_error(err);
                }
            }
        }
    }
}

impl<I: Iterator<Item = Lexeme>> Parser<I> {
    // fn recursion_check(&mut self) -> RecursionGuard {
    //     self.recursion_guard
    //         .recursion_check(&mut self.error_handler)
    // }
    // don't use this, use next_token instead
    // WARNING: this _cannot_ read or modify `self.current` or `self.next`
    fn __impl_next_token(&mut self) -> Option<Locatable<TokenKind>> {
        loop {
            match self.tokens.next() {
                Some(Ok(Locatable {
                    data: TokenKind::Whitespace(_),
                    ..
                })) => continue,
                // Some(Ok(Locatable {
                //     data: TokenKind::Literal(LiteralToken::Str(mut concat_strs)),
                //     mut location,
                // })) => {
                //     // 5.1.1.2p1: Translation phase 6: Adjacent string literal tokens are concatenated.
                //     loop {
                //         match self.tokens.peek() {
                //             Some(Ok(Locatable {
                //                 data: TokenKind::Literal(LiteralToken::Str(merge_strs)),
                //                 location: next_location,
                //             })) => {
                //                 location = location.merge(next_location);
                //                 concat_strs.append(&mut merge_strs.clone());
                //                 self.tokens.next(); // Actually remove next
                //             }
                //             Some(Ok(Locatable {
                //                 data: TokenKind::Whitespace(_),
                //                 ..
                //             })) => {
                //                 self.tokens.next();
                //             }
                //             Some(Ok(_)) => break,
                //             Some(Err(_)) => {
                //                 let err = self.tokens.next().unwrap().unwrap_err();
                //                 self.error_handler.push_back(err);
                //             }
                //             None => break,
                //         }
                //     }
                //     break Some(Locatable::new(
                //         TokenKind::Literal(LiteralToken::Str(concat_strs)),
                //         location,
                //     ));
                // }
                Some(Ok(token)) => {
                    self.last_location = token.location;
                    // This is _such_ a hack
                    // I'd much rather use `Token::is_decl_specifier()` at the various places it's necessary,
                    // but that runs into limits of the lifetime system since `peek_token()` takes `&mut self`:
                    // https://doc.rust-lang.org/nomicon/lifetime-mismatch.html#limits-of-lifetimes
                    if let TokenKind::Identifier(id) = token.data {
                        if self.typedefs.get(&id).is_some() {
                            break Some(
                                token
                                    .location
                                    .with(TokenKind::Keyword(Keyword::UserTypedef(id))),
                            );
                        }
                    }
                    break Some(token);
                }
                // Some(Err(err)) => {
                //     self.last_location = err.location();
                //     self.lex_error(err);
                // }
                None => break None,
                _ => todo!(),
            }
        }
    }
    fn next_token(&mut self) -> Option<Locatable<TokenKind>> {
        mem::replace(&mut self.current, self.next.take()).or_else(|| self.__impl_next_token())
    }
    fn peek_token(&mut self) -> Option<&TokenKind> {
        if self.current.is_none() {
            self.current = self.next.take().or_else(|| self.__impl_next_token());
        }
        self.current.as_ref().map(|x| &x.data)
    }
    // TODO: this is mostly copied from peek_token
    fn peek_next_token(&mut self) -> Option<&TokenKind> {
        if self.next.is_none() {
            if self.current.is_none() {
                self.current = self.__impl_next_token();
            }
            self.next = self.__impl_next_token();
        }
        self.next.as_ref().map(|x| &x.data)
    }
    fn next_location(&self) -> Location {
        if let Some(token) = &self.current {
            token.location
        } else {
            self.last_location
        }
    }
    fn match_id(&mut self) -> Option<Locatable<InternedStr>> {
        match self.peek_token() {
            Some(&TokenKind::Identifier(name))
            | Some(&TokenKind::Keyword(Keyword::UserTypedef(name))) => {
                let location = self.next_token().unwrap().location;
                Some(Locatable::new(name, location))
            }
            _ => None,
        }
    }
    fn match_keywords(&mut self, keywords: &[Keyword]) -> Option<Locatable<Keyword>> {
        if let Some(&TokenKind::Keyword(keyword)) = self.peek_token() {
            for expected in keywords {
                if keyword == *expected {
                    let location = self.next_token().unwrap().location;
                    return Some(Locatable::new(keyword, location));
                }
            }
            None
        } else {
            None
        }
    }
    fn match_literal(&mut self) -> Option<Locatable<(LiteralKind, InternedStr)>> {
        let next = self.next_token();
        if let Some(Locatable {
            data: TokenKind::Literal(lit, interned),
            location,
        }) = next
        {
            Some(location.with((lit, interned)))
        } else {
            self.unput(next);
            None
        }
    }
    fn match_string_literal(&mut self) -> Option<Locatable<(EncodingKind, InternedStr)>> {
        let next = self.next_token();
        if let Some(Locatable {
            data: TokenKind::Literal(LiteralKind::String(e), interned),
            location,
        }) = next
        {
            Some(location.with((e, interned)))
        } else {
            self.unput(next);
            None
        }
    }
    fn match_next(&mut self, next: &TokenKind) -> Option<Locatable<TokenKind>> {
        self.match_any(&[next])
    }
    fn match_any(&mut self, choices: &[&TokenKind]) -> Option<Locatable<TokenKind>> {
        if let Some(data) = self.peek_token() {
            for token in choices {
                if token.same_kind(data) {
                    return self.next_token();
                }
            }
        }
        None
    }
    /*
     * If we're in an invalid state, try to recover.
     * Consume tokens until the end of a statement - either ';' or '}'
     */
    fn panic(&mut self) {
        while let Some(token) = self.next_token() {
            match token.data {
                TokenKind::Semicolon => break,
                TokenKind::RightBrace(_) => {
                    break;
                }
                _ => continue,
            };
        }
    }
    fn expect_id(&mut self) -> SyntaxResult<Locatable<InternedStr>> {
        if let Some(id) = self.match_id() {
            Ok(id)
        } else {
            let err = Locatable {
                data: SyntaxError::ExpectedId(self.peek_token().cloned()),
                location: self.next_location(),
            };
            self.panic();
            Err(err)
        }
    }
    fn expect(&mut self, next: TokenKind) -> SyntaxResult<Locatable<TokenKind>> {
        let token = match self.peek_token() {
            Some(t) => t,
            None => {
                let err = Locatable {
                    data: SyntaxError::Generic(format!("expected '{}', got '<end-of-file>'", next)),
                    // TODO: we don't actually want this, we want the end of the file
                    location: self.last_location,
                };
                self.panic();
                return Err(err);
            }
        };
        if token.same_kind(&next) {
            Ok(self.next_token().unwrap())
        } else {
            let err = Locatable {
                data: SyntaxError::Generic(format!("expected '{}', got '{}'", next, token)),
                location: self.next_location(),
            };
            self.panic();
            Err(err)
        }
    }
    /// - replace `self.current` with `item`
    /// - replace `self.next` with `self.current`
    /// - the previous value of `self.next` is lost
    fn unput(&mut self, item: Option<Locatable<TokenKind>>) {
        assert!(self.next.is_none());
        self.next = mem::replace(&mut self.current, item);
    }
    fn lex_error(&mut self, err: Locatable<SyntaxError>) {
        self.error_handler.push_error(err);
    }
    pub fn collect_results(
        &mut self,
    ) -> (
        Vec<Locatable<ExternalDeclaration>>,
        Vec<Locatable<SyntaxError>>,
    ) {
        let mut decls = Vec::new();
        let mut errs = Vec::new();
        for result in self {
            match result {
                Ok(decl) => decls.push(decl),
                Err(err) => errs.push(err),
            }
        }
        (decls, errs)
    }
    /// Return all warnings seen so far.
    ///
    /// These warnings are consumed and will not be returned if you call
    /// `warnings()` again.
    pub fn warnings(&mut self) -> VecDeque<Locatable<Warning>> {
        self.error_handler.take_warnings()
    }
}
