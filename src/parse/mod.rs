mod data;
mod error;

use crate::data::ast::ExternalDeclaration;
use crate::error::ErrorHandler;
use crate::location::Locatable;
use crate::parse::error::SyntaxError;
use crate::preprocess::{LexResult, TokenKind};
use crate::scope::Scope;
use crate::InternedStr;

use std::collections::VecDeque;

type Lexeme = LexResult<Locatable<TokenKind>>;
type SyntaxResult<T> = Result<T, Locatable<SyntaxError>>;

pub struct Parser<I: Iterator<Item = Lexeme>> {
    /// Parser hack so that we know that `typedef int i; i j;`
    /// is legal, and also to resolve ambiguities
    typedefs: Scope<InternedStr, ()>,

    /// We iterate lazily over the tokens, so if we have a program that's mostly valid but
    /// breaks at the end, we don't only show lex errors
    tokens: std::iter::Peekable<I>,

    /// VecDeque supports pop_front with reasonable efficiency
    /// this is useful because there could be multiple declarators
    /// in a single declaration; e.g. `int a, b, c;`
    pending: VecDeque<Locatable<ExternalDeclaration>>,

    /// Whether to debug each declaration
    debug: bool,

    /// Internal API which makes it easier to return errors lazily
    error_handler: ErrorHandler<SyntaxError>,
}

// impl<I: Iterator<Item = Lexeme>> Parser<I> {
//     /// Return whether this parser has fully finished parsing.
//     ///
//     /// This can be used if, for example, you call `parser.expr()`
//     /// and want to see if there are any left-over tokens.
//     pub fn is_empty(&mut self) -> bool {
//         self.peek_token().is_none()
//     }
// }

impl<I: Iterator<Item = Lexeme>> Iterator for Parser<I> {
    type Item = SyntaxResult<Locatable<ExternalDeclaration>>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
