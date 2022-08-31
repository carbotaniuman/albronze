use crate::get_str;
use crate::location::{Locatable, Location};
use crate::InternedStr;

use std::fmt;
use std::str::FromStr;

use thiserror::Error;

static_assertions::assert_eq_size!(TokenKind, [u8; 12]);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // TODO: This should be a seperate interner
    Literal(LiteralKind, InternedStr),
    Identifier(InternedStr),
    HeaderName { global: bool, name: InternedStr },
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
}

impl TokenKind {
    pub fn is_whitespace(&self) -> bool {
        matches!(self, TokenKind::Whitespace(_))
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
        match (self, next) {
            // short circuit this first
            (Whitespace(_), _) | (_, Whitespace(_)) => true,

            // TODO: can be more precise
            (x, _) | (_, x) if x.is_digraph() => false,

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
                Equal,
            ) => false,

            (Less, Colon | Less | Mod) => false,
            (Greater, Greater) => false,

            (Plus, Plus) => false,
            (Minus, Minus | Greater) => false,

            (Divide, Divide | Star) => false,
            (Mod, Colon | Greater) => false,
            (Ampersand, Ampersand) => false,
            (BitwiseOr, BitwiseOr) => false,

            (Colon, Greater) => false,
            (Hash(_), Hash(_)) => false,

            (Dot | Ellipsis, Dot | Ellipsis | Literal(Number, _)) => false,

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
                let is_whitespace = match token.data {
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

                        false
                    }
                };

                // Check if the two tokens don't need a space or if,
                // the two tokens were next together in the source code.
                if !(last.can_omit_space(&token.data)
                    || last_location.is_directly_before(&token.location))
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
