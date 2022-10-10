use crate::location::Locatable;
use std::fmt;

pub(crate) const INDENT: &str = "    ";

pub(crate) fn joined<I: IntoIterator<Item = T>, T: ToString>(it: I, delim: &str) -> String {
    it.into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(delim)
}

pub(crate) fn joined_locatable<'a, I: IntoIterator<Item = &'a Locatable<T>>, T: ToString + 'a>(
    it: I,
    delim: &str,
) -> String {
    joined(it.into_iter().map(|s| s.data.to_string()), delim)
}

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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
    // todo: this is wrong
    Char(u8),
}

impl LiteralValue {
    pub fn is_zero(&self) -> bool {
        match *self {
            LiteralValue::Int(i) => i == 0,
            LiteralValue::UnsignedInt(u) => u == 0,
            LiteralValue::Char(c) => c == 0,
            _ => false,
        }
    }
}
impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use LiteralValue::*;
        match self {
            Int(i) => write!(f, "{}", i),
            UnsignedInt(u) => write!(f, "{}", u),
            Float(n) => write!(f, "{}", n),
            String(s) => {
                // let mut escaped = s
                //     .iter()
                //     .flat_map(|c| match c {
                //         b'\n' => "\\n".bytes().collect(),
                //         b'\r' => "\\r".bytes().collect(),
                //         b'\t' => "\\t".bytes().collect(),
                //         _ => vec![*c],
                //     })
                //     .collect::<Vec<_>>();

                // // Remove the null byte at the end,
                // // because this will break tests and
                // // it's not needed in debug output.
                // assert_eq!(escaped.pop(), Some(b'\0'));

                // write!(f, "\"{}\"", String::from_utf8_lossy(&escaped))

                todo!()
            }
            Char(c) => write!(f, "'{}'", char::from(*c).escape_default()),
        }
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

impl fmt::Display for AssignmentToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AssignmentToken::*;
        let s = match self {
            Equal => "=",
            AddEqual => "+=",
            SubEqual => "-=",
            MulEqual => "*=",
            DivEqual => "/=",
            ModEqual => "%=",
            ShlEqual => "<<=",
            ShrEqual => ">>=",
            AndEqual => "&=",
            OrEqual => "|=",
            XorEqual => "^=",
        };
        write!(f, "{}", s)
    }
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
