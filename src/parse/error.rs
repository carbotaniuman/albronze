use crate::preprocess::TokenKind;

// use super::data::Keyword;

use thiserror::Error;

/// Syntax errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum SyntaxError {
    // TODO: replace this
    #[error("{0}")]
    Generic(String),

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

    #[error("expected declaration specifier, got keyword '{0}'")]
    ExpectedDeclSpecifier(super::Keyword),

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

    #[error("function not allowed in this context (got {})", .0.as_type())]
    FunctionNotAllowed(super::ast::FunctionDefinition),

    #[error("function definitions must have a name")]
    MissingFunctionName,

    #[error("`static` for array sizes is only allowed in function declarations")]
    StaticInConcreteArray,
}
