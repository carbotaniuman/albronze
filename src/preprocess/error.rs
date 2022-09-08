use crate::InternedStr;

use super::token::{TokenKind, WhitespaceKind};

use arcstr::ArcStr;
use thiserror::Error;

/// Lex errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[non_exhaustive]
pub enum LexError {
    #[error("unterminated /* comment")]
    UnterminatedComment,

    #[error("missing terminating {} character in {} literal",
        if *(.string) { "\"" } else { "'" },
        if *(.string) { "string" } else { "character" })]
    MissingEndQuote { string: bool },

    #[error("missing terminating {} character in header name",
        if *(.global) { ">" } else { "\"" })]
    MissingHeaderEnd { global: bool },

    #[error("illegal newline while parsing string literal")]
    NewlineInString,

    #[error("illegal newline while parsing char literal")]
    NewlineInChar,

    #[error("empty character constant")]
    EmptyChar,
}

/// Inclusion errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum IncludeError {
    /// An `#include<>` or `#include""` was present.
    #[error("empty filename")]
    EmptyInclude,

    #[error("invalid include syntax")]
    BadInclude,

    #[error("file '{0}' not found")]
    FileNotFound(String),

    #[error("IO error: {0}")]
    // TODO: find a way to put io::Error in here (doesn't derive Clone or PartialEq)
    IO(String),
}

/// Preprocessing errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum CppError {
    #[error("failed to include file")]
    InclusionError(#[from] IncludeError),

    #[error("failed to lex input")]
    LexingError(#[from] LexError),
    /// A user-defined error (`#error`) was present.
    /// The `Vec<Token>` contains the tokens which followed the error.

    // TODO: this allocates a string for each token,
    // might be worth separating out into a function at some point
    #[error("error: {0}")]
    User(String),

    /// An invalid directive was present, such as `#invalid`
    #[error("invalid preprocessing directive")]
    InvalidDirective,

    /// A valid token was present in an invalid position, such as `#if *`
    ///
    /// The `&str` describes the expected token;
    /// the `Token` is the actual token found.
    #[error("expected {0}, got {1}")]
    UnexpectedToken(&'static str, TokenKind),

    /// The file ended unexpectedly.
    ///
    /// This error is separate from an unterminated `#if`:
    /// it occurs if the file ends in the middle of a directive,
    /// such as `#define`.
    ///
    /// The `&str` describes what token was expected.
    #[error("expected {0}, got <end-of-file>")]
    EndOfFile(&'static str),

    #[error("wrong number of arguments: expected {0}, got {1}")]
    TooFewArguments(usize, usize),

    /// The file ended before an `#if`, `#ifdef`, or `#ifndef` was closed.
    #[error("#if is never terminated")]
    UnterminatedIf,

    /// An `#if` occurred without an expression following.
    #[error("expected expression for #if")]
    EmptyExpression,

    #[error("macro name missing")]
    ExpectedMacroId,

    #[error("missing {0} in {1}")]
    Expected(&'static str, &'static str),

    /// A `#define` occured without an identifier following.
    #[error("macro name missing")]
    EmptyDefine,

    #[error("duplicate parameter name '{0}'")]
    DuplicateParameter(String),

    /// A `#endif` was present, but no `#if` was currently open
    #[error("#endif without #if")]
    UnexpectedEndIf,

    /// An `#else` was present, but either
    /// a) no `#if` was currently open, or
    /// b) an `#else` has already been seen.
    #[error("#else after #else or #else without #if")]
    UnexpectedElse,

    /// An `#elif` was present, but either
    /// a) no `#if` was currently open, or
    /// b) an `#else` has already been seen.
    #[error("{}", if *early { "#elif without #if" } else { "#elif after #else " })]
    UnexpectedElif { early: bool },

    /// After parsing an `#if` expression, there were tokens left over.
    #[error("trailing tokens in `#if` expression")]
    TooManyTokens,

    /// If a macro is redefined, the new definition must be identical to the
    /// original.
    #[error("redefinition of '{0}' does not match original definition")]
    IncompatibleRedefinition(InternedStr),

    /// '#' in a function macro not followed by function parameter
    #[error("'#' is not followed by a macro parameter")]
    HashMissingParameter,

    /// '##' missing arguments
    #[error("'##' cannot appear at {} of macro expansion", if *(.start) { "start" } else { "end"})]
    HashHashMissingParameter { start: bool },

    // /// The result of '##' is not a valid token
    #[error("token pasting formed '{0}', an invalid preprocessing token")]
    HashHashInvalid(ArcStr),
}

impl CppError {
    pub fn is_unexpected_newline(&self) -> bool {
        matches!(
            self,
            CppError::UnexpectedToken(_, TokenKind::Whitespace(WhitespaceKind::Newline))
        )
    }
}
