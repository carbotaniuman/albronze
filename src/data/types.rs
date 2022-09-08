use crate::data::Sign;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArrayType {
    Fixed(u64),
    Incomplete,
}

static_assertions::assert_eq_size!(TypeKind, [u8; 2]);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Void,
    Bool,

    Char(Option<Sign>),
    Short(Sign),
    Int(Sign),
    Long(Sign),
    LongLong(Sign),

    Float,
    Double,
    LongDouble,

    // TODO: separate Qualifiers into LvalQualifiers and FunctionQualifiers
    // Pointer(Box<Type>, super::hir::Qualifiers),
    // Array(Box<Type>, ArrayType),
    // Function(FunctionType),
    // Union(StructType),
    // Struct(StructType),
    // /// Enums should always have members, since tentative definitions are not allowed
    // Enum(Option<InternedStr>, Vec<(InternedStr, i64)>),
    /// This is the type used for variadic arguments.
    VaList,
    /// A semantic error occured while parsing this type.
    Error,
}
