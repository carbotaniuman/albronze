pub mod ast;

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
