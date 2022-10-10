use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
/// errors are non-exhaustive and may have new variants added at any time
pub enum Warning {
    /// A #warning directive was present, followed by the tokens in this variant.
    // TODO: this allocates a string for each token,
    // might be worth separating out into a function at some point
    // TODO: should this be a Vec<Token>
    // #[error("#warning {0}")]
    // User(String),

    #[error("no space after macro identifier")]
    NoSpaceAfterMacroIdentifier,

    #[error("backslash newline at end of file")]
    BackslashNewlineAtEOF,

    #[error("no newline at end of file")]
    NoNewlineAtEOF,

    #[error("extraneous semicolon in {0}")]
    ExtraneousSemicolon(&'static str),

    #[error("'{0}' qualifier on return type has no effect")]
    FunctionQualifiersIgnored(crate::analyze::hir::Qualifiers),

    #[error("duplicate '{0}' declaration specifier{}",
            if *.1 > 1 { format!(" occurs {} times", .1) } else { String::new() })]
    DuplicateSpecifier(crate::parse::ast::UnitSpecifier, usize),

    #[error("qualifiers in type casts are ignored")]
    IgnoredQualifier(crate::analyze::hir::Qualifiers),

    #[error("declaration does not declare anything")]
    EmptyDeclaration,

    #[error("{} does not support #pragma", env!("CARGO_PKG_NAME"))]
    IgnoredPragma,

    #[error("implicit int is deprecated and may be removed in a future release")]
    ImplicitInt,

    #[error("this is a definition, not a declaration, the 'extern' keyword has no effect")]
    ExtraneousExtern,
}
