use crate::preprocess::TokenKind;

// use super::data::Keyword;

use thiserror::Error;

/// Syntax errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum SyntaxError {
    #[error("expected {0}, got <end-of-file>")]
    EndOfFile(&'static str),

    #[error("expected statement, got {0}")]
    NotAStatement(super::Keyword),

    // expected a primary expression, but got EOF or an invalid token
    #[error("expected variable, literal, or '('")]
    MissingPrimary,

    #[error("expected identifier, got '{}'",
        .0.as_ref().map_or("<end-of-file>".into(),
                           |t| std::borrow::Cow::Owned(t.to_string())))]
    ExpectedId(Option<TokenKind>),

    // #[error("expected declaration specifier, got keyword '{0}'")]
    // ExpectedDeclSpecifier(Keyword),
    #[error("expected declarator in declaration")]
    ExpectedDeclarator,

    #[error("empty type name")]
    ExpectedType,

    #[error("expected '(', '*', or variable, got '{0}'")]
    ExpectedDeclaratorStart(TokenKind),

    #[error("only functions can have a function body (got {0})")]
    NotAFunction(super::ast::InitDeclarator),

    #[error("functions cannot be initialized (got {0})")]
    FunctionInitializer(super::ast::Initializer),

    // #[error("function not allowed in this context (got {})", .0.as_type())]
    // FunctionNotAllowed(ast::FunctionDefinition),
    #[error("function definitions must have a name")]
    MissingFunctionName,

    #[error("`static` for array sizes is only allowed in function declarations")]
    StaticInConcreteArray,

    #[error("overflow while parsing {}integer literal",
        if let Some(signed) = .is_signed {
            if *signed { "signed "} else { "unsigned "}
        } else { "" })]
    IntegerOverflow { is_signed: Option<bool> },

    // #[error("invalid digit {digit} in {radix} constant")]
    // InvalidDigit { digit: u32, radix: Radix },
    #[error("underflow parsing floating literal")]
    FloatUnderflow,

    #[error("{0}")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("{0}")]
    ParseFloat(#[from] std::num::ParseFloatError),

    #[error("{0}")]
    InvalidHexFloat(#[from] hexponent::ParseError),
}
