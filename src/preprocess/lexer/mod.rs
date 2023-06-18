//! Basic preprocessing lexer for Saltwater.
//!
//! This functions in this module handle the basics of parsing
//! C, namely 5.1.1.2 phases 1 and 2. This does not handle tokenizing
//! the characters into C preprocessing tokens, see `lex.rs` for that.

mod accumulator;
mod lex;

use super::error::LexError;

use crate::error::{ErrorHandler, Warning};
use crate::location::{Locatable, Location, SourceKind};

use arcstr::ArcStr;

pub type LexResult<T> = Result<T, Locatable<LexError>>;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct DiffData {
    offset: u32,
    line: usize,
}

#[derive(Clone, Debug)]
pub struct Lexer {
    initial: ArcStr,

    // chars offset
    offset: usize,
    // the current location of the parser
    location: SingleLocation,
    /// used for 2-character tokens
    current: Option<(char, DiffData)>,
    /// used for 3-character tokens
    lookahead: Option<(char, DiffData)>,

    // counts physical lines in the file, for __LINE__
    line: usize,

    pub error_handler: ErrorHandler<LexError>,

    // fields below here are only used for `lex.rs`
    /// whether we've a token on this line before or not
    /// used for EOF detection
    seen_line_token: bool,
    pub parse_header: bool,
    /// whether we've emitted an error for missing newline at EOF
    pub emitted_newline_error: bool,
    /// Whether or not to display each token as it is processed
    pub debug: bool,
    pub external_mode: bool,
}

#[derive(Copy, Clone, Debug)]
pub struct SingleLocation {
    pub offset: u32,
    pub source: SourceKind,
}

impl Lexer {
    pub fn noop() -> Lexer {
        Self::new(SourceKind::Generated, arcstr::literal!(""), false, false)
    }

    /// Creates a Lexer from a filename and the contents of a file
    pub fn new(source: SourceKind, initial: ArcStr, debug: bool, external_mode: bool) -> Lexer {
        Self {
            initial,

            offset: 0,
            location: SingleLocation { offset: 0, source },
            current: None,
            lookahead: None,
            parse_header: false,
            emitted_newline_error: false,

            line: 0,

            seen_line_token: false,
            error_handler: ErrorHandler::new(),
            debug,
            external_mode,
        }
    }

    pub fn location(&self) -> SingleLocation {
        self.location
    }

    pub fn offset(&self) -> u32 {
        self.location.offset
    }

    /// Given the start of a span as an offset,
    /// return a span lasting until the current location in the file.
    pub fn span(&self, start: u32) -> Location {
        Location {
            span: (start..self.location.offset).into(),
            source: self.location.source,
        }
    }

    /// Returns the physical line that the lexer
    /// is currently on
    pub fn line(&self) -> usize {
        self.line
    }

    /// This lexer is somewhat unique - it reads a single character at a time,
    /// unlike most lexers which read a token at a time (e.g. string literals).
    /// This makes some things harder to do than normal, for example integer and float parsing, because
    /// we can't use the standard library - it expects you to already have the entire string.
    ///
    /// This, along with `peek` and `unput` is sort of an iterator within an iterator:
    /// that loops over `char` instead of `Token`.
    ///
    /// Returns the next token in the stream, updating internal location information.
    /// If a lookahead already exists, use that instead.
    ///
    /// All functions should use this instead of `chars` directly.
    /// Using `chars` will not update location information and may discard lookaheads.
    ///
    /// This function should never set `self.location.offset` to an out-of-bounds location
    pub fn next_char(&mut self) -> Option<char> {
        if self.current.is_none() {
            self.peek();
        }
        if let Some((c, diff)) = self.current.take() {
            self.location.offset += diff.offset;
            self.line += diff.line;

            Some(c)
        } else {
            None
        }
    }

    // TODO: this _really_ needs to be refactored
    // TODO(carbotaniuman): IDK how this works tbh
    fn chars(&self) -> std::str::Chars<'_> {
        // if we're compiling on 16-bit, we have bigger problems
        static_assertions::const_assert!(
            std::mem::size_of::<usize>() >= std::mem::size_of::<u32>()
        );
        self.initial[self.offset as usize..].chars()
    }

    fn advance_char_lookaheads(&mut self) -> Option<(char, DiffData)> {
        // TODO(carbotaniuman): THIS IS A MESS

        // This gets the next token from the buffer
        // and updates the offset via the provided mutable reference.
        //
        // This does not apply any transformations, but rather simply
        // takes the next char present in `chars`.
        fn next_raw_char(this: &mut Lexer, peek: bool) -> Option<char> {
            let c = this.chars().next();
            if let Some(c) = c {
                if !peek {
                    this.offset += c.len_utf8();
                }
            }

            c
        }

        let mut diff_data = DiffData::default();

        let mut c = next_raw_char(self, false);
        if let Some(c) = c {
            diff_data.offset += c.len_utf8() as u32;
        }

        // Section 5.1.1.2 phase 2: discard backslashes before newlines
        while c == Some('\\') && next_raw_char(self, true) == Some('\n') {
            diff_data.offset += next_raw_char(self, false).unwrap().len_utf8() as u32;
            diff_data.line += 1;

            c = next_raw_char(self, false);
            if let Some(c) = c {
                diff_data.offset += c.len_utf8() as u32;
            }

            if c.is_none() {
                let location = self.span(self.initial.len() as u32);
                self.error_handler
                    .warn(Warning::BackslashNewlineAtEOF, location);
            }
        }

        if let Some(c) = c {
            if c == '\n' {
                self.seen_line_token = false;
                diff_data.line += 1;
            } else {
                self.seen_line_token = true;
            }
        }

        c.map(|x| (x, diff_data))
    }

    /// Return the character that would be returned by `next_char`.
    /// Can be called any number of times and will still return the same result.
    fn peek(&mut self) -> Option<char> {
        self.current = self
            .current
            .or_else(|| self.lookahead.take())
            .or_else(|| self.advance_char_lookaheads());

        self.current.map(|x| x.0)
    }

    /// Return the character that would be returned if you called `next_char()` twice in a row.
    /// Can be called any number of the times and will still return the same result.
    fn peek_next(&mut self) -> Option<char> {
        self.peek();
        self.lookahead = self.lookahead.or_else(|| self.advance_char_lookaheads());
        self.lookahead.map(|x| x.0)
    }

    pub fn err(&mut self, err: Locatable<LexError>) {
        self.error_handler.push_error(err);
    }

    pub fn warn(&mut self, err: Locatable<Warning>) {
        self.error_handler.push_warning(err);
    }

    pub fn err_loc<E: Into<LexError>>(&mut self, err: E, location: Location) {
        self.err(location.with(err.into()));
    }

    pub fn warn_loc<W: Into<Warning>>(&mut self, warning: W, location: Location) {
        self.warn(location.with(warning.into()));
    }
}
