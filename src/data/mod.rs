mod hir;
mod types;

use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Radix {
    pub fn as_u8(self) -> u8 {
        match self {
            Radix::Binary => 2,
            Radix::Octal => 8,
            Radix::Decimal => 10,
            Radix::Hexadecimal => 16,
        }
    }
}

impl fmt::Display for Radix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let word = match self {
            Radix::Binary => "binary",
            Radix::Octal => "octal",
            Radix::Decimal => "decimal",
            Radix::Hexadecimal => "hexadecimal",
        };
        write!(f, "{}", word)
    }
}

impl TryFrom<u32> for Radix {
    type Error = ();
    fn try_from(int: u32) -> Result<Radix, ()> {
        match int {
            2 => Ok(Radix::Binary),
            8 => Ok(Radix::Octal),
            10 => Ok(Radix::Decimal),
            16 => Ok(Radix::Hexadecimal),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
    Register,
    Typedef,
}

impl Default for StorageClass {
    fn default() -> StorageClass {
        StorageClass::Auto
    }
}

impl fmt::Display for StorageClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    // literals
    Int(i64),
    UnsignedInt(u64),
    Float(f64),
    String(String),
    Char(char),
}

impl LiteralValue {
    pub fn is_zero(&self) -> bool {
        match *self {
            LiteralValue::Int(i) => i == 0,
            LiteralValue::UnsignedInt(u) => u == 0,
            LiteralValue::Char(c) => c == '\0',
            _ => false,
        }
    }
}
impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // match self {
        //     Int(i) => write!(f, "{}", i),
        //     UnsignedInt(u) => write!(f, "{}", u),
        //     Float(n) => write!(f, "{}", n),
        //     Str(s) => {
        //         let mut escaped = s
        //             .iter()
        //             .flat_map(|c| match c {
        //                 b'\n' => "\\n".bytes().collect(),
        //                 b'\r' => "\\r".bytes().collect(),
        //                 b'\t' => "\\t".bytes().collect(),
        //                 _ => vec![*c],
        //             })
        //             .collect::<Vec<_>>();

        //         // Remove the null byte at the end,
        //         // because this will break tests and
        //         // it's not needed in debug output.
        //         assert_eq!(escaped.pop(), Some(b'\0'));

        //         write!(f, "\"{}\"", String::from_utf8_lossy(&escaped))
        //     }
        //     Char(c) => write!(f, "'{}'", char::from(*c).escape_default()),
        // }
        todo!()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AssignmentToken {
    Equal,
    AddEqual,
    SubEqual,
    MulEqual,
    DivEqual,
    ModEqual,
    ShlEqual, // <<=
    ShrEqual, // >>=
    AndEqual,
    OrEqual,
    XorEqual, // ^=
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ComparisonToken {
    Less,
    Greater,
    EqualEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,
}

impl fmt::Display for ComparisonToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ComparisonToken::*;
        let s = match self {
            Less => "<",
            Greater => ">",
            EqualEqual => "==",
            NotEqual => "!=",
            LessEqual => "<=",
            GreaterEqual => ">=",
        };
        write!(f, "{}", s)
    }
}