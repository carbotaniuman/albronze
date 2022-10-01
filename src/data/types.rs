use crate::data::Sign;
use crate::data::hir::Symbol;
use crate::InternedStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArrayType {
    Fixed(u64),
    Incomplete,
}

// static_assertions::assert_eq_size!(TypeKind, [u8; 2]);

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
    Pointer(Box<Type>, super::hir::Qualifiers),
    Array(Box<TypeKind>, ArrayType),
    Function(FunctionType),
    // Union(StructType),
    // Struct(StructType),
    /// Enums should always have members, since tentative definitions are not allowed
    Enum(Option<InternedStr>, Vec<(InternedStr, i64)>),
    /// This is the type used for variadic arguments.
    VaList,
    /// A semantic error occured while parsing this type.
    Error,
}

// NOTE: K&R declarations are not supported at this time
#[derive(Clone, Debug)]
pub struct FunctionType {
    pub return_type: Box<TypeKind>,
    // why Metadata instead of Type?
    // 1. we need to know qualifiers for the params. if we made that part of Type,
    //    we'd need qualifiers for every step along the way
    //    (consider that int a[][][] parses as 4 nested types).
    // 2. when we do scoping, we need to know the names of formal parameters
    //    (as opposed to concrete arguments).
    //    this is as good a place to store them as any.
    pub params: Vec<Symbol>,
    pub varargs: bool,
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        // no prototype: any parameters are allowed
        // TODO: issue a warning if a function has empty parameters, it's a holdover
        // from C89
        self.params.is_empty()
            || other.params.is_empty()
            || self.varargs == other.varargs
            && self.return_type == other.return_type
            // don't require parameter names and storage_class to match
            && self.params
                .iter()
                .zip(other.params.iter())
                .all(|(a, b)| {
                    let (this_param, other_param) = (a.get(), b.get());
                    this_param.ctype == other_param.ctype
                        && this_param.qualifiers == other_param.qualifiers
                })
    }
}