// use crate::InternedStr;

// use thiserror::Error;

// #[derive(Clone, Debug, Error, PartialEq)]
// pub enum Error {
//     #[error("invalid program: {0}")]
//     Semantic(#[from] SemanticError),

//     #[error("invalid syntax: {0}")]
//     Syntax(#[from] SyntaxError),

//     #[error("invalid macro: {0}")]
//     PreProcessor(#[from] CppError),

//     #[error("invalid token: {0}")]
//     Lex(#[from] LexError),
// }

// /// Semantic errors are non-exhaustive and may have new variants added at any time
// #[derive(Clone, Debug, Error, PartialEq)]
// #[non_exhaustive]
// pub enum SemanticError {
//     #[error("{0}")]
//     Generic(String),

//     // Declaration specifier errors
//     #[error("cannot combine '{new}' specifier with previous '{existing}' type specifier")]
//     InvalidSpecifier {
//         existing: ast::DeclarationSpecifier,
//         new: ast::DeclarationSpecifier,
//     },

//     #[error("'{0}' is not a qualifier and cannot be used for pointers")]
//     NotAQualifier(ast::DeclarationSpecifier),

//     #[error("'{}' is too long for {}", vec!["long"; *.0].join(" "), env!("CARGO_PKG_NAME"))]
//     TooLong(usize),

//     #[error("conflicting storage classes '{0}' and '{1}'")]
//     ConflictingStorageClass(StorageClass, StorageClass),

//     #[error("conflicting types '{0}' and '{1}'")]
//     ConflictingType(Type, Type),

//     #[error("'{0}' cannot be signed or unsigned")]
//     CannotBeSigned(Type),

//     #[error("types cannot be both signed and unsigned")]
//     ConflictingSigned,

//     #[error("only function-scoped variables can have an `auto` storage class")]
//     AutoAtGlobalScope,

//     #[error("cannot have empty program")]
//     EmptyProgram,

//     // Declarator errors
//     #[error("expected an integer")]
//     NonIntegralLength,

//     #[error("arrays must have a positive length")]
//     NegativeLength,

//     #[error("function parameters always have a storage class of `auto`")]
//     ParameterStorageClass(StorageClass),

//     #[error("duplicate parameter name '{0}' in function declaration")]
//     DuplicateParameter(InternedStr),

//     #[error("functions cannot return '{0}'")]
//     IllegalReturnType(Type),

//     // TODO: print params in the error message
//     // #[error("arrays cannot contain functions (got '{0}'). help: try storing array of pointer to function: (*{}[])(...)")]
//     #[error(" y no compile ^^^ ")]
//     ArrayStoringFunction(Type),

//     #[error("void must be the first and only parameter if specified")]
//     InvalidVoidParameter,

//     #[error("functions taking `void` must not have variadic arguments")]
//     VoidVarargs,

//     #[error("functions taking variadic arguments must have at least one parameter first")]
//     VarargsWithoutParam,

//     #[error("overflow in enumeration constant")]
//     EnumOverflow,

//     #[error("variable has incomplete type 'void'")]
//     VoidType,

//     // expression errors
//     #[error("use of undeclared identifier '{0}'")]
//     UndeclaredVar(InternedStr),

//     #[error("expected expression, got typedef")]
//     TypedefInExpressionContext,

//     #[error("type casts cannot have a storage class")]
//     IllegalStorageClass(StorageClass),

//     #[error("type casts cannot have a variable name")]
//     IdInTypeName(InternedStr),

//     #[error("expected integer, got '{0}'")]
//     NonIntegralExpr(Type),

//     #[error("cannot implicitly convert '{0}' to '{1}'{}",
//         if .1.is_pointer() {
//             format!(". help: use an explicit cast: ({})", .1)
//         } else {
//             String::new()
//         })
//     ]
//     InvalidCast(Type, Type),

//     // String is the reason it couldn't be assigned
//     #[error("cannot assign to {0}")]
//     NotAssignable(String),

//     #[error("invalid operators for '{0}' (expected either arithmetic types or pointer operation, got '{1} {0} {2}'")]
//     InvalidAdd(hir::BinaryOp, Type, Type),

//     #[error("cannot perform pointer arithmetic when size of pointed type '{0}' is unknown")]
//     PointerAddUnknownSize(Type),

//     #[error("called object of type '{0}' is not a function")]
//     NotAFunction(Type),

//     #[error("too {} arguments to function call: expected {0}, have {1}", if .1 > .0 { "many" } else { "few" })]
//     /// (actual, expected)
//     WrongArgumentNumber(usize, usize),

//     #[error("{0} has not yet been defined")]
//     IncompleteDefinitionUsed(Type),

//     #[error("no member named '{0}' in '{1}'")]
//     NotAMember(InternedStr, Type),

//     #[error("expected struct or union, got type '{0}'")]
//     NotAStruct(Type),

//     #[error("cannot use '->' operator on type that is not a pointer")]
//     NotAStructPointer(Type),

//     #[error("cannot dereference expression of non-pointer type '{0}'")]
//     NotAPointer(Type),

//     #[error("cannot take address of {0}")]
//     InvalidAddressOf(&'static str),

//     #[error("cannot increment or decrement value of type '{0}'")]
//     InvalidIncrement(Type),

//     #[error("cannot use unary plus on expression of non-arithmetic type '{0}'")]
//     NotArithmetic(Type),

//     #[error("incompatible types in ternary expression: '{0}' cannot be converted to '{1}'")]
//     IncompatibleTypes(Type, Type),

//     // const fold errors
//     #[error("{} overflow in expresson", if *(.is_positive) { "positive" } else { "negative" })]
//     ConstOverflow { is_positive: bool },

//     #[error("cannot divide by zero")]
//     DivideByZero,

//     #[error("cannot shift {} by a negative amount", if *(.is_left) { "left" } else { "right" })]
//     NegativeShift { is_left: bool },

//     #[error("cannot shift {} by {maximum} or more bits for type '{ctype}' (got {current})",
//         if *(.is_left) { "left" } else { "right" })]
//     TooManyShiftBits {
//         is_left: bool,
//         maximum: u64,
//         ctype: Type,
//         current: u64,
//     },

//     #[error("not a constant expression: {0}")]
//     NotConstant(Expr),

//     #[error("cannot dereference NULL pointer")]
//     NullPointerDereference,

//     #[error("invalid types for '{0}' (expected arithmetic types or compatible pointers, got {1} {0} {2}")]
//     InvalidRelationalType(lex::ComparisonToken, Type, Type),

//     #[error("cannot cast pointer to float or vice versa")]
//     FloatPointerCast(Type),

//     // TODO: this shouldn't be an error
//     #[error("cannot cast to non-scalar type '{0}'")]
//     NonScalarCast(Type),

//     #[error("cannot cast void to any type")]
//     VoidCast,

//     #[error("cannot cast structs to any type")]
//     StructCast,

//     // Control flow errors
//     #[error("unreachable statement")]
//     UnreachableStatement,

//     // TODO: this error should happen way before codegen
//     #[cfg(feature = "codegen")]
//     #[error("redeclaration of label {0}")]
//     LabelRedeclaration(cranelift_codegen::ir::entities::Block),

//     #[error("use of undeclared label {0}")]
//     UndeclaredLabel(InternedStr),

//     #[error("{}case outside of switch statement", if *(.is_default) { "default " } else { "" })]
//     CaseOutsideSwitch { is_default: bool },

//     #[error("cannot have multiple {}cases in a switch statement",
//             if *(.is_default) { "default " } else { "" } )]
//     DuplicateCase { is_default: bool },

//     // Initializer errors
//     #[error("initializers cannot be empty")]
//     EmptyInitializer,

//     #[error("scalar initializers for '{0}' may only have one element (initialized with {1})")]
//     AggregateInitializingScalar(Type, usize),

//     #[error("too many initializers (declared with {0} elements, found {1})")]
//     TooManyMembers(usize, usize),

//     // Function definition errors
//     #[error("illegal storage class {0} for function (only `static` and `extern` are allowed)")]
//     InvalidFuncStorageClass(StorageClass),

//     #[error("missing parameter name in function definition (parameter {0} of type '{1}')")]
//     MissingParamName(usize, Type),

//     #[error("forward declaration of {0} is never completed (used in {1})")]
//     ForwardDeclarationIncomplete(InternedStr, InternedStr),

//     #[error("illegal signature for main function (expected 'int main(void)' or 'int main(int, char **)'")]
//     IllegalMainSignature,

//     // declaration errors
//     #[error("redefinition of '{0}'")]
//     Redefinition(InternedStr),

//     #[error("redeclaration of '{0}' with different type or qualifiers (originally {}, now {})", .1.get(), .2.get())]
//     IncompatibleRedeclaration(InternedStr, hir::Symbol, hir::Symbol),

//     #[error("'{0}' can only appear on functions")]
//     FuncQualifiersNotAllowed(hir::FunctionQualifiers),

//     // stmt errors
//     // new with the new parser
//     #[error("switch expressions must have an integer type (got {0})")]
//     NonIntegralSwitch(Type),

//     #[error("function '{0}' does not return a value")]
//     MissingReturnValue(InternedStr),

//     #[error("void function '{0}' should not return a value")]
//     ReturnFromVoid(InternedStr),
// }

// /// Preprocessing errors are non-exhaustive and may have new variants added at any time
// #[derive(Clone, Debug, Error, PartialEq)]
// #[non_exhaustive]
// pub enum CppError {
//     /// A user-defined error (`#error`) was present.
//     /// The `Vec<Token>` contains the tokens which followed the error.

//     // TODO: this allocates a string for each token,
//     // might be worth separating out into a function at some point
//     // #[error("#error {}", (.0).iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))]
//     // User(Vec<Token>),

//     /// An invalid directive was present, such as `#invalid`
//     #[error("invalid preprocessing directive")]
//     InvalidDirective,

//     /// A valid token was present in an invalid position, such as `#if *`
//     ///
//     /// The `&str` describes the expected token;
//     /// the `Token` is the actual token found.
//     #[error("expected {0}, got {1}")]
//     UnexpectedToken(&'static str, Token),

//     /// The file ended unexpectedly.
//     ///
//     /// This error is separate from an unterminated `#if`:
//     /// it occurs if the file ends in the middle of a directive,
//     /// such as `#define`.
//     ///
//     /// The `&str` describes what token was expected.
//     #[error("expected {0}, got <end-of-file>")]
//     EndOfFile(&'static str),

//     #[error("file '{0}' not found")]
//     FileNotFound(String),

//     #[error("wrong number of arguments: expected {0}, got {1}")]
//     TooFewArguments(usize, usize),

//     #[error("IO error: {0}")]
//     // TODO: find a way to put io::Error in here (doesn't derive Clone or PartialEq)
//     IO(String),

//     /// The file ended before an `#if`, `#ifdef`, or `#ifndef` was closed.
//     #[error("#if is never terminated")]
//     UnterminatedIf,

//     /// An `#if` occurred without an expression following.
//     #[error("expected expression for #if")]
//     EmptyExpression,

//     #[error("macro name missing")]
//     ExpectedMacroId,

//     #[error("missing {0} in {1}")]
//     Expected(&'static str, &'static str),

//     /// A `#define` occured without an identifier following.
//     #[error("macro name missing")]
//     EmptyDefine,

//     #[error("duplicate parameter name '{0}'")]
//     DuplicateParameter(String),

//     /// An `#include<>` or `#include""` was present.
//     #[error("empty filename")]
//     EmptyInclude,

//     /// A `#endif` was present, but no `#if` was currently open
//     #[error("#endif without #if")]
//     UnexpectedEndIf,

//     /// An `#else` was present, but either
//     /// a) no `#if` was currently open, or
//     /// b) an `#else` has already been seen.
//     #[error("#else after #else or #else without #if")]
//     UnexpectedElse,

//     /// An `#elif` was present, but either
//     /// a) no `#if` was currently open, or
//     /// b) an `#else` has already been seen.
//     #[error("{}", if *early { "#elif without #if" } else { "#elif after #else " })]
//     UnexpectedElif { early: bool },

//     /// After parsing an `#if` expression, there were tokens left over.
//     #[error("trailing tokens in `#if` expression")]
//     TooManyTokens,

//     /// If a macro is redefined, the new definition must be identical to the
//     /// original.
//     #[error("redefinition of '{0}' does not match original definition")]
//     IncompatibleRedefinition(InternedStr),

//     /// '#' in a function macro not followed by function parameter
//     #[error("'#' is not followed by a macro parameter")]
//     HashMissingParameter,

//     /// '##' missing arguments
//     #[error("'##' cannot appear at {} of macro expansion", if *(.0) { "start" } else { "end"})]
//     HashHashMissingParameter(bool),

//     /// The result of '##' is not a valid token
//     #[error("token pasting formed '{0}{1}', an invalid preprocessing token")]
//     HashHashInvalid(Token, Token),
// }

// /// Lex errors are non-exhaustive and may have new variants added at any time
// #[derive(Clone, Debug, Error, PartialEq, Eq)]
// #[non_exhaustive]
// pub enum LexError {
//     #[error("unterminated /* comment")]
//     UnterminatedComment,

//     #[error("backslash newline at end of file")]
//     BackslashNewlineAtEOF,

//     #[error("no newline at end of file")]
//     NoNewlineAtEOF,

//     #[error("unknown token: '{0}'")]
//     UnknownToken(char),

//     #[error("missing terminating {} character in {} literal",
//         if *(.string) { "\"" } else { "'" },
//         if *(.string) { "string" } else { "character" })]
//     MissingEndQuote { string: bool },

//     #[error("missing terminating {} character in header name",
//         if *(.global) { ">" } else { "\"" })]
//     MissingHeaderEnd { global: bool },

//     #[error("illegal newline while parsing string literal")]
//     NewlineInString,

//     #[error("{0} character escape out of range")]
//     CharEscapeOutOfRange(Radix),

//     #[error("exponent for floating literal has no digits")]
//     ExponentMissingDigits,

//     #[error("missing digits to {0} integer constant")]
//     MissingDigits(Radix),

//     #[error("invalid digit {digit} in {radix} constant")]
//     InvalidDigit { digit: u32, radix: Radix },

//     #[error("multi-byte character literal")]
//     MultiByteCharLiteral,

//     #[error("illegal newline while parsing char literal")]
//     NewlineInChar,

//     #[error("empty character constant")]
//     EmptyChar,
// }
