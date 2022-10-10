use thiserror::Error;

use crate::InternedStr;

use crate::data::{ComparisonToken, StorageClass};
use crate::hir::*;
use crate::parse::ast::*;

impl<S: Into<String>> From<S> for SemanticError {
    fn from(err: S) -> Self {
        SemanticError::Generic(err.into())
    }
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("invalid program: {0}")]
    Semantic(#[from] SemanticError),

    #[error("invalid syntax: {0}")]
    Syntax(#[from] crate::parse::error::SyntaxError),

    #[error("invalid macro: {0}")]
    PreProcessor(#[from] crate::preprocess::error::CppError),
}

/// Semantic errors are non-exhaustive and may have new variants added at any time
#[derive(Clone, Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum SemanticError {
    #[error("{0}")]
    Generic(String),

    // Declaration specifier errors
    #[error("cannot combine '{new}' specifier with previous '{existing}' type specifier")]
    InvalidSpecifier {
        existing: DeclarationSpecifier,
        new: DeclarationSpecifier,
    },
    #[error("'{0}' is not a qualifier and cannot be used for pointers")]
    NotAQualifier(DeclarationSpecifier),

    #[error("'{}' is too long for {}", vec!["long"; *.0].join(" "), env!("CARGO_PKG_NAME"))]
    TooLong(usize),

    #[error("conflicting storage classes '{0}' and '{1}'")]
    ConflictingStorageClass(StorageClass, StorageClass),

    #[error("conflicting types '{0}' and '{1}'")]
    ConflictingType(TypeKind, TypeKind),

    #[error("'{0}' cannot be signed or unsigned")]
    CannotBeSigned(TypeKind),

    #[error("types cannot be both signed and unsigned")]
    ConflictingSigned,

    #[error("only function-scoped variables can have an `auto` storage class")]
    AutoAtGlobalScope,

    #[error("cannot have empty program")]
    EmptyProgram,

    // Declarator errors
    #[error("expected an integer")]
    NonIntegralLength,

    #[error("arrays must have a positive length")]
    NegativeLength,

    #[error("function parameters always have a storage class of `auto`")]
    ParameterStorageClass(StorageClass),

    #[error("duplicate parameter name '{0}' in function declaration")]
    DuplicateParameter(InternedStr),

    #[error("functions cannot return '{0}'")]
    IllegalReturnType(TypeKind),

    // TODO: print params in the error message
    #[error("arrays cannot contain functions (got '{0}'). help: try storing array of pointer to function: (*{{}}[])(...)")]
    ArrayStoringFunction(TypeKind),

    #[error("void must be the first and only parameter if specified")]
    InvalidVoidParameter,

    #[error("functions taking `void` must not have variadic arguments")]
    VoidVarargs,

    #[error("functions taking variadic arguments must have at least one parameter first")]
    VarargsWithoutParam,

    #[error("overflow in enumeration constant")]
    EnumOverflow,

    #[error("variable has incomplete type 'void'")]
    VoidType,

    // expression errors
    #[error("use of undeclared identifier '{0}'")]
    UndeclaredVar(InternedStr),

    #[error("expected expression, got typedef")]
    TypedefInExpressionContext,

    #[error("type casts cannot have a storage class")]
    IllegalStorageClass(StorageClass),

    #[error("type casts cannot have a variable name")]
    IdInTypeName(InternedStr),

    #[error("expected integer, got '{0}'")]
    NonIntegralExpr(TypeKind),

    #[error("cannot implicitly convert '{0}' to '{1}'{}",
        if .1.is_pointer() {
            format!(". help: use an explicit cast: ({})", .1)
        } else {
            String::new()
        })
    ]
    InvalidCast(TypeKind, TypeKind),

    // String is the reason it couldn't be assigned
    #[error("cannot assign to {0}")]
    NotAssignable(String),

    #[error("invalid operators for '{0}' (expected either arithmetic types or pointer operation, got '{1} {0} {2}'")]
    InvalidAdd(BinaryOp, TypeKind, TypeKind),

    #[error("cannot perform pointer arithmetic when size of pointed type '{0}' is unknown")]
    PointerAddUnknownSize(TypeKind),

    #[error("called object of type '{0}' is not a function")]
    NotAFunction(TypeKind),

    #[error("too {} arguments to function call: expected {0}, have {1}", if .1 > .0 { "many" } else { "few" })]
    /// (actual, expected)
    WrongArgumentNumber(usize, usize),

    #[error("{0} has not yet been defined")]
    IncompleteDefinitionUsed(TypeKind),

    #[error("no member named '{0}' in '{1}'")]
    NotAMember(InternedStr, TypeKind),

    #[error("expected struct or union, got type '{0}'")]
    NotAStruct(TypeKind),

    #[error("cannot use '->' operator on type that is not a pointer")]
    NotAStructPointer(TypeKind),

    #[error("cannot dereference expression of non-pointer type '{0}'")]
    NotAPointer(TypeKind),

    #[error("cannot take address of {0}")]
    InvalidAddressOf(&'static str),

    #[error("cannot increment or decrement value of type '{0}'")]
    InvalidIncrement(TypeKind),

    #[error("cannot use unary plus on expression of non-arithmetic type '{0}'")]
    NotArithmetic(TypeKind),

    #[error("incompatible types in ternary expression: '{0}' cannot be converted to '{1}'")]
    IncompatibleTypes(TypeKind, TypeKind),

    // const fold errors
    #[error("{} overflow in expression", if *(.is_positive) { "positive" } else { "negative" })]
    ConstOverflow { is_positive: bool },

    #[error("cannot divide by zero")]
    DivideByZero,

    #[error("cannot shift {} by a negative amount", if *(.is_left) { "left" } else { "right" })]
    NegativeShift { is_left: bool },

    #[error("cannot shift {} by {maximum} or more bits for type '{ctype}' (got {current})",
        if *(.is_left) { "left" } else { "right" })]
    TooManyShiftBits {
        is_left: bool,
        maximum: u64,
        ctype: TypeKind,
        current: u64,
    },

    #[error("not a constant expression: {0}")]
    NotConstant(crate::hir::Expr),

    #[error("cannot dereference NULL pointer")]
    NullPointerDereference,

    #[error("invalid types for '{0}' (expected arithmetic types or compatible pointers, got {1} {0} {2}")]
    InvalidRelationalType(ComparisonToken, TypeKind, TypeKind),

    #[error("cannot cast pointer to float or vice versa")]
    FloatPointerCast(TypeKind),

    // TODO: this shouldn't be an error
    #[error("cannot cast to non-scalar type '{0}'")]
    NonScalarCast(TypeKind),

    #[error("cannot cast void to any type")]
    VoidCast,

    #[error("cannot cast structs to any type")]
    StructCast,

    // Control flow errors
    #[error("unreachable statement")]
    UnreachableStatement,

    // TODO: this error should happen way before codegen
    // #[cfg(feature = "codegen")]
    // #[error("redeclaration of label {0}")]
    // LabelRedeclaration(cranelift_codegen::ir::entities::Block),
    #[error("use of undeclared label {0}")]
    UndeclaredLabel(InternedStr),

    #[error("{}case outside of switch statement", if *(.is_default) { "default " } else { "" })]
    CaseOutsideSwitch { is_default: bool },

    #[error("cannot have multiple {}cases in a switch statement",
            if *(.is_default) { "default " } else { "" } )]
    DuplicateCase { is_default: bool },

    // Initializer errors
    #[error("initializers cannot be empty")]
    EmptyInitializer,

    #[error("scalar initializers for '{0}' may only have one element (initialized with {1})")]
    AggregateInitializingScalar(TypeKind, usize),

    #[error("too many initializers (declared with {0} elements, found {1})")]
    TooManyMembers(usize, usize),

    // Function definition errors
    #[error("illegal storage class {0} for function (only `static` and `extern` are allowed)")]
    InvalidFuncStorageClass(StorageClass),

    #[error("missing parameter name in function definition (parameter {0} of type '{1}')")]
    MissingParamName(usize, TypeKind),

    #[error("forward declaration of {0} is never completed (used in {1})")]
    ForwardDeclarationIncomplete(InternedStr, InternedStr),

    #[error("illegal signature for main function (expected 'int main(void)' or 'int main(int, char **)'")]
    IllegalMainSignature,

    // declaration errors
    #[error("redefinition of '{0}'")]
    Redefinition(InternedStr),

    #[error("redeclaration of '{0}' with different type or qualifiers (originally {}, now {})", .1.get(), .2.get())]
    IncompatibleRedeclaration(InternedStr, Symbol, Symbol),

    #[error("'{0}' can only appear on functions")]
    FuncQualifiersNotAllowed(FunctionQualifiers),

    #[error("switch expressions must have an integer type (got {0})")]
    NonIntegralSwitch(TypeKind),

    #[error("function '{0}' does not return a value")]
    MissingReturnValue(InternedStr),

    #[error("void function '{0}' should not return a value")]
    ReturnFromVoid(InternedStr),
}
