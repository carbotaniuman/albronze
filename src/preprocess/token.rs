use crate::get_str;
use crate::location::{Locatable, Location};
use crate::InternedStr;

use std::fmt;
use std::str::FromStr;

use thiserror::Error;

static_assertions::assert_eq_size!(TokenKind, [u8; 12]);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Unknown(char),
    Whitespace(WhitespaceKind),

    Hash(DigraphKind),
    HashHash(DigraphKind),

    LeftBrace(DigraphKind), // {
    RightBrace(DigraphKind),
    LeftBracket(DigraphKind), // [
    RightBracket(DigraphKind),
    LeftParen,
    RightParen,

    PlusPlus,
    MinusMinus,

    Equal,
    AddEqual,
    SubEqual,
    MulEqual,
    DivEqual,
    ModEqual,
    ShlEqual,
    ShrEqual,
    AndEqual,
    OrEqual,
    XorEqual,

    Less,
    Greater,
    EqualEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,

    Plus,
    Minus,
    Star,
    Divide,
    Mod,
    Xor,
    Ampersand,
    LogicalAnd,
    BitwiseOr,
    LogicalOr,
    BinaryNot,  // ~
    LogicalNot, // !
    ShiftLeft,
    ShiftRight,

    Semicolon,
    Colon,
    Comma,
    Dot,
    Question,
    StructDeref, // ->

    // Misc
    Ellipsis, // ...

    // TODO: This should be a seperate interner
    Literal(LiteralKind, InternedStr),
    Keyword(Keyword),
    Identifier(InternedStr),
    HeaderName { global: bool, name: InternedStr },
}

impl TokenKind {
    pub fn same_kind(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenKind::Keyword(left), TokenKind::Keyword(right)) => left == right,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }

    pub fn is_whitespace(&self) -> bool {
        matches!(self, TokenKind::Whitespace(_))
    }

    fn first_symbol_of(&self) -> TokenKind {
        use TokenKind::*;
        match *self {
            Hash(DigraphKind::Digraph) => Mod,
            HashHash(DigraphKind::Digraph) => Mod,
            HashHash(DigraphKind::Standard) => Hash(DigraphKind::Standard),
            LeftBrace(DigraphKind::Digraph) => Less,
            RightBrace(DigraphKind::Digraph) => Mod,
            LeftBracket(DigraphKind::Digraph) => Less,
            RightBracket(DigraphKind::Digraph) => Colon,

            AddEqual => Plus,
            SubEqual => Minus,
            MulEqual => Star,
            DivEqual => Divide,
            ModEqual => Mod,
            ShlEqual => Less,
            ShrEqual => Greater,
            AndEqual => Ampersand,
            OrEqual => BitwiseOr,
            XorEqual => Xor,

            EqualEqual => Equal,
            NotEqual => LogicalNot,
            LessEqual => Less,
            GreaterEqual => Greater,

            LogicalAnd => Ampersand,
            LogicalOr => BitwiseOr,
            ShiftLeft => Less,
            ShiftRight => Greater,

            StructDeref => Minus,

            Ellipsis => Dot,

            t => t,
        }
    }

    pub fn is_digraph(&self) -> bool {
        use TokenKind::*;
        match self {
            Hash(DigraphKind::Digraph)
            | HashHash(DigraphKind::Digraph)
            | LeftBrace(DigraphKind::Digraph)
            | RightBrace(DigraphKind::Digraph)
            | LeftBracket(DigraphKind::Digraph)
            | RightBracket(DigraphKind::Digraph) => true,
            _ => false,
        }
    }
    // whether preprocessor spacing can skip the space
    fn can_omit_space(&self, next: &TokenKind) -> bool {
        use LiteralKind::*;
        use TokenKind::*;

        // TODO: this is a mess
        match (self, next.first_symbol_of()) {
            // short circuit this first
            (Whitespace(_), _) | (_, Whitespace(_)) => true,

            (Identifier(s), Literal(String(_), _) | Literal(Char(_), _)) => {
                get_str!(s).parse::<EncodingKind>().is_err()
            }
            (Identifier(_), Identifier(_) | Literal(Number, _) | Unknown(_)) => false,
            (
                Literal(Number, _),
                Identifier(_) | Literal(Number, _) | Dot | Ellipsis | Plus | Minus,
            ) => false,

            (
                Equal | Less | Greater | Plus | Minus | Star | Divide | Mod | Xor | Ampersand
                | BitwiseOr | LogicalNot | ShiftLeft | ShiftRight,
                Equal | EqualEqual,
            ) => false,

            (Less, Colon | Less | Mod) => false,
            (Greater, Greater) => false,

            (Plus, Plus | PlusPlus) => false,
            (Minus, Minus | MinusMinus | Greater) => false,

            (Divide, Divide | Star) => false,
            (Mod, Colon | Greater) => false,
            (Ampersand, Ampersand) => false,
            (BitwiseOr, BitwiseOr) => false,

            (Colon, Greater) => false,
            (Hash(_), Hash(_) | Mod) => false,

            (Ellipsis, Dot | Literal(Number, _)) => false,

            _ => true,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use DigraphKind::*;
        use LiteralKind::*;
        use TokenKind::*;

        match self {
            Literal(Number, s) => write!(f, "{}", s),
            Literal(String(encoding), s) => write!(f, "{}\"{}\"", encoding, s),
            Literal(Char(encoding), s) => write!(f, "{}\'{}\'", encoding, s),
            Identifier(s) => write!(f, "{}", s),
            Keyword(s) => write!(f, "{}", s),
            HeaderName {
                global: true,
                name: s,
            } => write!(f, "<{}>", s),
            HeaderName {
                global: false,
                name: s,
            } => write!(f, "\"{}\"", s),
            Unknown(c) => write!(f, "{}", c),

            Whitespace(s) => write!(f, "{}", s),

            Hash(Standard) => write!(f, "#"),
            Hash(Digraph) => write!(f, "%:"),

            HashHash(Standard) => write!(f, "##"),
            HashHash(Digraph) => write!(f, "%:%:"),

            LeftBrace(Standard) => write!(f, "{{"),
            LeftBrace(Digraph) => write!(f, "<%"),
            RightBrace(Standard) => write!(f, "}}"),
            RightBrace(Digraph) => write!(f, "%>"),
            LeftBracket(Standard) => write!(f, "["),
            LeftBracket(Digraph) => write!(f, "<:"),
            RightBracket(Standard) => write!(f, "]"),
            RightBracket(Digraph) => write!(f, ":>"),
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),

            PlusPlus => write!(f, "++"),
            MinusMinus => write!(f, "--"),

            Equal => write!(f, "="),
            AddEqual => write!(f, "+="),
            SubEqual => write!(f, "-="),
            MulEqual => write!(f, "*="),
            DivEqual => write!(f, "/="),
            ModEqual => write!(f, "%="),
            ShlEqual => write!(f, "<<="),
            ShrEqual => write!(f, ">>="),
            AndEqual => write!(f, "&="),
            OrEqual => write!(f, "|="),
            XorEqual => write!(f, "^="),

            Less => write!(f, "<"),
            Greater => write!(f, ">"),
            EqualEqual => write!(f, "=="),
            NotEqual => write!(f, "!="),
            LessEqual => write!(f, "<="),
            GreaterEqual => write!(f, ">="),

            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Divide => write!(f, "/"),
            Mod => write!(f, "%"),
            Xor => write!(f, "^"),
            Ampersand => write!(f, "&"),
            LogicalAnd => write!(f, "&&"),
            BitwiseOr => write!(f, "|"),
            LogicalOr => write!(f, "||"),
            BinaryNot => write!(f, "~"),
            LogicalNot => write!(f, "!"),
            ShiftLeft => write!(f, "<<"),
            ShiftRight => write!(f, ">>"),

            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Question => write!(f, "?"),
            StructDeref => write!(f, "->"),

            Ellipsis => write!(f, "..."),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WhitespaceKind {
    Newline,
    MacroSpace,
    NonNewline(InternedStr),
    LineComment(InternedStr),
    BlockComment(InternedStr),
}

impl WhitespaceKind {
    pub fn is_comment(self) -> bool {
        use WhitespaceKind::*;
        matches!(self, LineComment(_) | BlockComment(_))
    }
}

impl fmt::Display for WhitespaceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use WhitespaceKind::*;

        match self {
            Newline => write!(f, "\n"),
            MacroSpace => write!(f, " "),
            NonNewline(s) | LineComment(s) | BlockComment(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EncodingKind {
    Normal,
    Utf8,
    Utf16,
    Utf32,
    Wide,
}

#[derive(Copy, Clone, Debug, Error, PartialEq, Eq)]
#[error("invalid encoding kind")]
pub struct InvalidEncodingKind;

impl FromStr for EncodingKind {
    type Err = InvalidEncodingKind;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        use EncodingKind::*;
        match value {
            "" => Ok(Normal),
            "u8" => Ok(Utf8),
            "u" => Ok(Utf16),
            "U" => Ok(Utf32),
            "L" => Ok(Wide),
            _ => Err(InvalidEncodingKind),
        }
    }
}

impl fmt::Display for EncodingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use EncodingKind::*;
        match self {
            Normal => write!(f, ""),
            Utf8 => write!(f, "u8"),
            Utf16 => write!(f, "u"),
            Utf32 => write!(f, "U"),
            Wide => write!(f, "L"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Number,
    String(EncodingKind),
    Char(EncodingKind),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DigraphKind {
    Standard,
    Digraph,
}

pub fn pretty_print<T: IntoIterator<Item = Locatable<TokenKind>> + Clone>(
    collection: T,
) -> impl fmt::Display {
    struct PrettyPrint<T: IntoIterator<Item = Locatable<TokenKind>> + Clone>(T);

    impl<T: IntoIterator<Item = Locatable<TokenKind>> + Clone> fmt::Display for PrettyPrint<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            let mut last = TokenKind::Whitespace(WhitespaceKind::Newline);

            let mut had_non_whitespace = false;
            let mut last_location = Location::generated(Default::default());
            let mut stored_whitespace = Vec::new();

            for token in self.0.clone() {
                // Code to skip empty lines or lines filled
                // with only whitespace
                match token.data {
                    // If we see a comment here, we're going to emit it
                    // because we checked for that earlier in the
                    // actual preprocessor.
                    TokenKind::Whitespace(kind) if !kind.is_comment() => {
                        if kind == WhitespaceKind::Newline {
                            stored_whitespace.clear();

                            if had_non_whitespace {
                                had_non_whitespace = false;
                                last = TokenKind::Whitespace(WhitespaceKind::Newline);
                                write!(f, "{}", token.data)?;
                            }
                        } else {
                            stored_whitespace.push(token.data);
                        }
                        continue;
                    }
                    _ => {
                        for i in stored_whitespace.drain(..) {
                            last = i;
                            write!(f, "{}", i)?;
                        }
                        had_non_whitespace = true;
                    }
                }

                // Check if the two tokens don't need a space or if,
                // the two tokens were next together in the source code.
                if !(last.can_omit_space(&token.data)
                    || last_location.is_directly_before(token.location))
                {
                    write!(f, " ")?;
                }

                write!(f, "{}", token.data)?;

                last = token.data;
                last_location = token.location;
            }

            Ok(())
        }
    }

    PrettyPrint(collection)
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Keyword {
    // statements
    If,
    Else,
    Do,
    While,
    For,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Return,
    Goto,

    // types
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Void,
    Signed,
    Unsigned,
    Typedef,

    // user-defined types
    Union,
    Struct,
    Enum,
    // the `i` in `typedef int i;`
    UserTypedef(InternedStr),

    // weird types
    Bool,
    Complex,
    Imaginary,
    VaList,

    // qualifiers
    Const,
    Volatile,
    Restrict,
    // weird qualifiers
    Atomic,
    ThreadLocal,
    // function qualifiers
    Inline,
    NoReturn,

    // storage classes
    Auto,
    Register,
    Static,
    Extern,

    // intrinsics
    Sizeof,
    Generic,
    StaticAssert,
    Alignas,
    Alignof,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: make this more efficient
        match self {
            Keyword::Alignas
            | Keyword::Alignof
            | Keyword::Bool
            | Keyword::Complex
            | Keyword::Imaginary
            | Keyword::Atomic
            | Keyword::Generic => write!(f, "_{:?}", self),
            Keyword::NoReturn => write!(f, "_Noreturn"),
            Keyword::ThreadLocal => write!(f, "_Thread_local"),
            Keyword::StaticAssert => write!(f, "_Static_assert"),
            Keyword::VaList => write!(f, "va_list"),
            _ => write!(f, "{}", &format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Copy, Clone, Debug, Error, PartialEq, Eq)]
#[error("invalid keyword")]
pub struct InvalidKeyword;

impl FromStr for Keyword {
    type Err = InvalidKeyword;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(match value {
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "do" => Keyword::Do,
            "while" => Keyword::While,
            "for" => Keyword::For,
            "switch" => Keyword::Switch,
            "case" => Keyword::Case,
            "default" => Keyword::Default,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            "return" => Keyword::Return,
            "goto" => Keyword::Goto,

            // types
            "__builtin_va_list" => Keyword::VaList,
            "_Bool" => Keyword::Bool,
            "char" => Keyword::Char,
            "short" => Keyword::Short,
            "int" => Keyword::Int,
            "long" => Keyword::Long,
            "float" => Keyword::Float,
            "double" => Keyword::Double,
            "_Complex" => Keyword::Complex,
            "_Imaginary" => Keyword::Imaginary,
            "void" => Keyword::Void,
            "signed" => Keyword::Signed,
            "unsigned" => Keyword::Unsigned,
            "typedef" => Keyword::Typedef,
            "enum" => Keyword::Enum,
            "union" => Keyword::Union,
            "struct" => Keyword::Struct,

            // qualifiers
            "const" => Keyword::Const,
            "volatile" => Keyword::Volatile,
            "restrict" => Keyword::Restrict,
            "_Atomic" => Keyword::Atomic,
            "_Thread_local" => Keyword::ThreadLocal,

            // function qualifiers
            "inline" => Keyword::Inline,
            "_Noreturn" => Keyword::NoReturn,

            // storage classes
            "auto" => Keyword::Auto,
            "register" => Keyword::Register,
            "static" => Keyword::Static,
            "extern" => Keyword::Extern,

            // compiler intrinsics
            "sizeof" => Keyword::Sizeof,
            "_Alignof" => Keyword::Alignof,
            "_Alignas" => Keyword::Alignas,
            "_Generic" => Keyword::Generic,
            "_Static_assert" => Keyword::StaticAssert,

            _ => return Err(InvalidKeyword),
        })
    }
}
