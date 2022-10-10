//! https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models
#![allow(missing_docs)]

use crate::hir::{StructType, TypeKind};
use crate::InternedStr;

const CHAR_SIZE: u16 = 1;

#[allow(non_camel_case_types)]
pub type SIZE_T = u64;
#[allow(dead_code)]
pub const SIZE_MAX: SIZE_T = SIZE_T::MAX;

pub const FLOAT_SIZE: u16 = 4;
pub const DOUBLE_SIZE: u16 = 8;
pub const LONG_DOUBLE_SIZE: u16 = 8;

pub const LONG_LONG_SIZE: u16 = 8;
pub const LONG_SIZE: u16 = 8;
pub const INT_SIZE: u16 = 4;
pub const SHORT_SIZE: u16 = 2;
pub const BOOL_SIZE: u16 = 1;

pub const PTR_SIZE: u16 = 8;

pub const CHAR_BIT: u16 = 8; // number of bits in a byte

impl StructType {
    /// Get the offset of the given struct member.
    #[cfg_attr(not(feature = "codegen"), allow(dead_code))]
    pub(crate) fn offset(&self, member: InternedStr) -> u64 {
        let members = self.members();
        let mut current_offset = 0;
        for formal in members.iter() {
            if formal.id == member {
                return current_offset;
            }
            current_offset = Self::next_offset(current_offset, &formal.ctype)
                .expect("structs should have valid size and alignment");
        }
        unreachable!("cannot call struct_offset for member not in struct");
    }
    /// Get the offset of the next struct member given the current offset.
    fn next_offset(mut current_offset: u64, ctype: &TypeKind) -> Result<u64, &'static str> {
        let align = ctype.alignof()?;
        // round up to the nearest multiple of align
        let rem = current_offset % align;
        if rem != 0 {
            // for example: 7%4 == 3; 7 + ((4 - 3) = 1) == 8; 8 % 4 == 0
            current_offset += align - rem;
        }
        Ok(current_offset + ctype.sizeof()?)
    }
    /// Calculate the size of a struct: the sum of all member sizes
    pub(crate) fn struct_size(&self) -> Result<SIZE_T, &'static str> {
        let symbols = &self.members();

        symbols
            .iter()
            .try_fold(0, |offset, symbol| {
                Ok(StructType::next_offset(offset, &symbol.ctype)?)
            })
            .and_then(|size_t| {
                let align_minus_one = self.align()? - 1;

                // Rounds up to the next multiple of `align`
                Ok((size_t + align_minus_one) & !align_minus_one)
            })
    }
    /// Calculate the size of a union: the max of all member sizes
    pub(crate) fn union_size(&self) -> Result<SIZE_T, &'static str> {
        let symbols = &self.members();
        symbols
            .iter()
            .map(|symbol| symbol.ctype.sizeof())
            // max of member sizes
            .try_fold(1, |n, size| Ok(std::cmp::max(n, size?)))
    }
    /// Calculate the alignment of a struct: the max of all member alignments
    pub(crate) fn align(&self) -> Result<SIZE_T, &'static str> {
        let members = &self.members();
        members.iter().try_fold(0, |max, member| {
            Ok(std::cmp::max(member.ctype.alignof()?, max))
        })
    }
}

impl TypeKind {
    /// Returns true if `other` can be converted to `self` without losing infomation.
    pub fn can_represent(&self, other: &TypeKind) -> bool {
        self == other
            || *self == TypeKind::Double && *other == TypeKind::Float
            || (self.is_integral() && other.is_integral())
                && (self.sizeof() > other.sizeof()
                    || self.sizeof() == other.sizeof() && self.is_signed() == other.is_signed())
    }

    /// Get the size of a type in bytes.
    ///
    /// This is the `sizeof` operator in C.
    pub fn sizeof(&self) -> Result<SIZE_T, &'static str> {
        use crate::hir::ArrayType;
        use TypeKind::*;

        match self {
            Bool => Ok(BOOL_SIZE.into()),
            Char(_) => Ok(CHAR_SIZE.into()),
            Short(_) => Ok(SHORT_SIZE.into()),
            Int(_) => Ok(INT_SIZE.into()),
            Long(_) => Ok(LONG_SIZE.into()),
            LongLong(_) => Ok(LONG_LONG_SIZE.into()),
            Float => Ok(FLOAT_SIZE.into()),
            Double => Ok(DOUBLE_SIZE.into()),
            LongDouble => Ok(LONG_DOUBLE_SIZE.into()),
            Pointer(_, _) => Ok(PTR_SIZE.into()),
            // now for the hard ones
            Array(t, ArrayType::Fixed(l)) => t
                .sizeof()
                .and_then(|n| n.checked_mul(*l).ok_or("overflow in array size")),
            Array(_, ArrayType::Unbounded) => Err("cannot take sizeof variable length array"),
            Enum(_, symbols) => {
                let uchar = CHAR_BIT as usize;
                // integer division, but taking the ceiling instead of the floor
                // https://stackoverflow.com/a/17974/7669110
                Ok(match (symbols.len() + uchar - 1) / uchar {
                    0..=8 => 8,
                    9..=16 => 16,
                    17..=32 => 32,
                    33..=64 => 64,
                    _ => return Err("enum cannot be represented in SIZE_T bits"),
                })
            }
            Union(struct_type) => struct_type.union_size(),
            Struct(struct_type) => struct_type.struct_size(),
            // illegal operations
            Function(_) => Err("cannot take `sizeof` a function"),
            Void => Err("cannot take `sizeof` void"),
            VaList => Err("cannot take `sizeof` va_list"),
            Error => Err("cannot take `sizeof` <type error>"),
        }
    }
    /// Get the alignment of a type in bytes.
    pub fn alignof(&self) -> Result<SIZE_T, &'static str> {
        use TypeKind::*;
        match self {
            Bool
            | Char(_)
            | Short(_)
            | Int(_)
            | Long(_)
            | LongLong(_)
            | Float
            | Double
            | LongDouble
            | Pointer(_, _)
            | Enum(_, _) => self.sizeof(),
            Array(t, _) => t.alignof(),
            // Clang uses the largest alignment of any element as the alignment of the whole
            // Not sure why, but who am I to argue
            // Anyway, Faerie panics if the alignment isn't a power of two so it's probably for the best
            Union(struct_type) | Struct(struct_type) => struct_type.align(),
            Function(_) => Err("cannot take `alignof` function"),
            Void => Err("cannot take `alignof` void"),
            VaList => Err("cannot take `alignof` va_list"),
            Error => Err("cannot take `alignof` <type error>"),
        }
    }
}
