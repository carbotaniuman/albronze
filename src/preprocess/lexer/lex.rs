use crate::location::Locatable;
use crate::InternedStr;

use crate::preprocess::error::LexError;
use crate::preprocess::lexer::Lexer;
use crate::preprocess::token::*;

use super::accumulator::WhitespaceAccumulator;
use super::LexResult;

use std::str::FromStr;

/// Returns whether the char is whitespace
/// according to the C standard.
fn is_c_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\u{0B}' || c == '\u{0C}'
}

impl Lexer {
    pub fn set_include_mode(&mut self, parse_header: bool) {
        self.parse_header = parse_header;
    }
    /// Remove all consecutive whitespace of the same type pending in the stream.
    /// The 4 types are newline, line comment, block comment, and all other whitespace.
    fn consume_whitespace(&mut self) -> Option<WhitespaceKind> {
        // TODO: Optimize
        if self.peek() == Some('\n') {
            self.next_char();
            return Some(WhitespaceKind::Newline);
        }

        {
            let mut ret = WhitespaceAccumulator::new(self.external_mode);
            let mut touched = false;

            while let Some(c) = self.peek() {
                if c == '\n' || !is_c_whitespace(c) {
                    break;
                };

                touched = true;
                ret.push(c);
                self.next_char();
            }

            if touched {
                return Some(WhitespaceKind::NonNewline(InternedStr::get_or_intern(ret)));
            }
        }

        if self.peek() == Some('/') {
            match self.peek_next() {
                Some('/') => {
                    let ret = self.consume_line_comment();
                    return Some(WhitespaceKind::LineComment(InternedStr::get_or_intern(ret)));
                }
                Some('*') => {
                    self.next_char();
                    self.next_char();
                    match self.consume_multi_comment() {
                        Ok(ret) => {
                            return Some(WhitespaceKind::BlockComment(InternedStr::get_or_intern(
                                ret,
                            )));
                        }
                        Err(err) => self.err(err),
                    }
                }
                _ => {}
            }
        }

        None
    }
    /// Remove all characters between now and the next '\n' character.
    ///
    /// Before: u8s{"blah `invalid tokens``\nhello // blah"}
    /// After:  chars{"hello // blah"}
    pub fn skip_line(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }

            self.next_char();
        }
    }

    /// Remove all characters between now and the next '\n' character.
    ///
    /// Before: u8s{"blah `invalid tokens``\nhello // blah"}
    /// After:  chars{"hello // blah"}
    fn consume_line_comment(&mut self) -> WhitespaceAccumulator {
        let mut ret = WhitespaceAccumulator::new(self.external_mode);

        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }

            ret.push(c);
            self.next_char();
        }
        ret
    }

    /// Remove a multi-line C-style comment, i.e. until the next '*/'.
    /// Assumes the leading `/*` has been eaten
    ///
    /// Before: u8s{"hello this is a lot of text */ int main(){}"}
    /// After:  chars{" int main(){}"}
    ///
    /// Return newlines occupied by the comment or a space if no newlines
    fn consume_multi_comment(&mut self) -> LexResult<WhitespaceAccumulator> {
        let mut ret = WhitespaceAccumulator::new(self.external_mode);
        ret.push('/');
        ret.push('*');

        let start = self.location().offset - 2;
        while let Some(c) = self.next_char() {
            if c == '*' && self.peek() == Some('/') {
                self.next_char();

                ret.push('*');
                ret.push('/');

                return Ok(ret);
            }

            ret.push(c);
        }
        Err(Locatable {
            location: self.span(start),
            data: LexError::UnterminatedComment,
        })
    }

    /// Parse an identifier or keyword, given the starting letter.
    ///
    /// Identifiers match the following regex: `[a-zA-Z_][a-zA-Z0-9_]*`
    fn parse_id(&mut self, start: char, _physical_line: usize) -> Result<String, LexError> {
        let mut ret = String::with_capacity(8);
        ret.push(start);

        while let Some(c) = self.peek() {
            match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    ret.push(c);
                    self.next_char();
                }
                _ => break,
            }
        }
        Ok(ret)
    }

    // Assumes the leading quote has been eaten
    fn parse_char(&mut self) -> Result<TokenKind, LexError> {
        let mut ret = String::with_capacity(8);

        loop {
            match self.next_char() {
                Some(c) => match c {
                    '\\' => {
                        ret.push(c);
                        if let Some('\'') = self.peek() {
                            self.next_char();
                            ret.push('\'');
                        } else if let Some('\\') = self.peek() {
                            self.next_char();
                            ret.push('\\');
                        }
                    }
                    '\'' => break,
                    '\n' => return Err(LexError::MissingEndQuote { string: false }),
                    _ => ret.push(c),
                },
                None => return Err(LexError::MissingEndQuote { string: false }),
            }
        }

        Ok(TokenKind::Literal(
            LiteralKind::Char(EncodingKind::Normal),
            InternedStr::get_or_intern(ret),
        ))
    }

    // Assumes the leading quote has been eaten
    fn parse_string(&mut self, encoding: EncodingKind) -> Result<TokenKind, LexError> {
        let mut ret = String::with_capacity(8);

        loop {
            match self.next_char() {
                Some(c) => match c {
                    '\\' => {
                        ret.push(c);
                        if let Some('\"') = self.peek() {
                            self.next_char();
                            ret.push('\"');
                        } else if let Some('\\') = self.peek() {
                            self.next_char();
                            ret.push('\\');
                        }
                    }
                    '\"' => break,
                    '\n' => return Err(LexError::MissingEndQuote { string: true }),
                    _ => ret.push(c),
                },
                None => return Err(LexError::MissingEndQuote { string: true }),
            }
        }

        Ok(TokenKind::Literal(
            LiteralKind::String(encoding),
            InternedStr::get_or_intern(ret),
        ))
    }
    fn parse_header_name(&mut self, global: bool) -> Result<TokenKind, LexError> {
        let mut name = String::with_capacity(8);

        while let Some(c) = self.next_char() {
            if (c == '>' && global) || (c == '"' && !global) {
                return Ok(TokenKind::HeaderName {
                    global,
                    name: InternedStr::get_or_intern(name),
                });
            } else {
                name.push(c);
            }
        }

        return Err(LexError::MissingHeaderEnd { global });
    }
    /// Parse a processor number
    /// If `start_digit` is false then the leading dot has already been stripped
    ///
    /// Assumes that a leading dot has already been stripped.
    fn parse_number(&mut self, start_char: char) -> TokenKind {
        let mut ret = String::with_capacity(8);
        ret.push(start_char);

        if !start_char.is_ascii_digit() {
            let next = self.next_char().unwrap();
            assert!(next.is_ascii_digit());
            ret.push(next);
        }

        loop {
            match self.peek() {
                Some(c @ ('e' | 'E' | 'p' | 'P')) => {
                    ret.push(c);
                    self.next_char();

                    match self.peek() {
                        Some(c @ ('+' | '-')) => {
                            ret.push(c);
                            self.next_char();
                        }
                        // we'll get it the next iteration of the loop
                        _ => {}
                    }
                }
                Some(c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.')) => {
                    ret.push(c);
                    self.next_char();
                }
                _ => break,
            }
        }

        TokenKind::Literal(LiteralKind::Number, InternedStr::get_or_intern(ret))
    }

    pub fn next_non_whitespace(&mut self) -> Option<LexResult<Locatable<TokenKind>>> {
        loop {
            match self.next() {
                Some(Ok(Locatable {
                    data: TokenKind::Whitespace(WhitespaceKind::NonNewline(_)),
                    ..
                })) => continue,
                other => break other,
            }
        }
    }

    pub fn next_non_whitespace_any(&mut self) -> Option<LexResult<Locatable<TokenKind>>> {
        loop {
            match self.next() {
                Some(Ok(Locatable {
                    data: TokenKind::Whitespace(_),
                    ..
                })) => continue,
                other => break other,
            }
        }
    }
}

impl Iterator for Lexer {
    // option: whether the stream is exhausted
    // result: whether the next lexeme is an error
    type Item = LexResult<Locatable<TokenKind>>;

    /// Return the next token in the stream.
    ///
    /// This iterator never resumes after it is depleted,
    /// i.e. once it returns None once, it will always return None.
    ///
    /// Any item may be an error, but items will always have an associated location.
    /// The file may be empty to start, in which case the iterator will return None.
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(err) = self.error_handler.pop_error() {
            return Some(Err(err));
        }

        let span_start = self.location().offset;

        match self.consume_whitespace() {
            Some(data) => {
                return Some(Ok(Locatable {
                    data: TokenKind::Whitespace(data),
                    location: self.span(span_start),
                }));
            }
            None => {}
        };

        if let Some(err) = self.error_handler.pop_error() {
            return Some(Err(err));
        }

        if self.peek().is_none() {
            return None;
        }

        let c = self
            .next_char()
            .map(|c| {
                use DigraphKind::*;
                use TokenKind::*;

                // this giant switch is most of the logic
                let start_line = self.line();
                let data = match c {
                    '#' => match self.peek() {
                        Some('#') => {
                            self.next_char();
                            HashHash(Standard)
                        }
                        _ => Hash(Standard),
                    },
                    '+' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            AddEqual
                        }
                        Some('+') => {
                            self.next_char();
                            PlusPlus
                        }
                        _ => Plus,
                    },
                    '-' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            SubEqual
                        }
                        Some('-') => {
                            self.next_char();
                            MinusMinus
                        }
                        Some('>') => {
                            self.next_char();
                            StructDeref
                        }
                        _ => Minus,
                    },
                    '*' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            MulEqual
                        }
                        _ => Star,
                    },
                    '/' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            DivEqual
                        }
                        _ => Divide,
                    },
                    '%' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            ModEqual
                        }
                        Some('>') => {
                            self.next_char();
                            RightBrace(Digraph)
                        }
                        Some(':') => {
                            self.next_char();
                            if self.peek() == Some('%') && self.peek_next() == Some(':') {
                                self.next_char();
                                self.next_char();
                                HashHash(Digraph)
                            } else {
                                Hash(Digraph)
                            }
                        }
                        _ => Mod,
                    },
                    '^' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            XorEqual
                        }
                        _ => Xor,
                    },
                    '=' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            EqualEqual
                        }
                        _ => Equal,
                    },
                    '!' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            NotEqual
                        }
                        _ => LogicalNot,
                    },
                    '>' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            GreaterEqual
                        }
                        Some('>') => {
                            self.next_char();
                            match self.peek() {
                                Some('=') => {
                                    self.next_char();
                                    ShrEqual
                                }
                                _ => ShiftRight,
                            }
                        }
                        _ => Greater,
                    },
                    '<' if self.parse_header => match self.parse_header_name(true) {
                        Ok(name) => name,
                        Err(err) => {
                            let span = self.span(span_start);
                            return Err(span.with(err));
                        }
                    },
                    '<' => match self.peek() {
                        Some('=') => {
                            self.next_char();
                            LessEqual
                        }
                        Some('<') => {
                            self.next_char();
                            match self.peek() {
                                Some('=') => {
                                    self.next_char();
                                    ShlEqual
                                }
                                _ => ShiftLeft,
                            }
                        }
                        Some(':') => {
                            self.next_char();
                            LeftBracket(Digraph)
                        }
                        Some('%') => {
                            self.next_char();
                            LeftBrace(Digraph)
                        }
                        _ => Less,
                    },
                    '&' => match self.peek() {
                        Some('&') => {
                            self.next_char();
                            LogicalAnd
                        }
                        Some('=') => {
                            self.next_char();
                            AndEqual
                        }
                        _ => Ampersand,
                    },
                    '|' => match self.peek() {
                        Some('|') => {
                            self.next_char();
                            LogicalOr
                        }
                        Some('=') => {
                            self.next_char();
                            OrEqual
                        }
                        _ => BitwiseOr,
                    },
                    '{' => LeftBrace(Standard),
                    '}' => RightBrace(Standard),
                    '(' => LeftParen,
                    ')' => RightParen,
                    '[' => LeftBracket(Standard),
                    ']' => RightBracket(Standard),
                    '~' => BinaryNot,
                    ':' => match self.peek() {
                        Some('>') => {
                            self.next_char();
                            RightBracket(Digraph)
                        }
                        _ => Colon,
                    },
                    ';' => Semicolon,
                    ',' => Comma,
                    '.' => match self.peek() {
                        Some('0'..='9') => self.parse_number('.'),
                        Some('.') => {
                            if self.peek_next() == Some('.') {
                                self.next_char();
                                self.next_char();
                                Ellipsis
                            } else {
                                Dot
                            }
                        }
                        _ => Dot,
                    },
                    '?' => Question,
                    c @ '0'..='9' => self.parse_number(c),
                    'a'..='z' | 'A'..='Z' | '_' => match self.parse_id(c, start_line) {
                        Ok(id) => match EncodingKind::from_str(&id) {
                            Ok(encoding) if self.peek() == Some('\"') => {
                                match self.parse_string(encoding) {
                                    Ok(id) => id,
                                    Err(err) => {
                                        let span = self.span(span_start);
                                        return Err(span.with(err));
                                    }
                                }
                            }
                            _ => TokenKind::Identifier(InternedStr::get_or_intern(id)),
                        },
                        Err(err) => {
                            let span = self.span(span_start);
                            return Err(span.with(err));
                        }
                    },
                    '\'' => match self.parse_char() {
                        Ok(id) => id,
                        Err(err) => {
                            let span = self.span(span_start);
                            return Err(span.with(err));
                        }
                    },
                    '"' if self.parse_header => match self.parse_header_name(false) {
                        Ok(name) => name,
                        Err(err) => {
                            let span = self.span(span_start);
                            return Err(span.with(err));
                        }
                    },
                    '"' => match self.parse_string(EncodingKind::Normal) {
                        Ok(id) => id,
                        Err(err) => {
                            let span = self.span(span_start);
                            return Err(span.with(err));
                        }
                    },
                    x => Unknown(x),
                };

                Ok(Locatable {
                    data,
                    location: self.span(span_start),
                })
            })
            .unwrap();

        if self.debug {
            println!("token: {:?}", c);
        }
        Some(c)
    }
}
