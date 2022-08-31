pub struct WhitespaceAccumulator {
    buffer: Option<String>,
}

impl WhitespaceAccumulator {
    pub fn new(external_mode: bool) -> Self {
        WhitespaceAccumulator {
            buffer: if external_mode {
                Some(String::new())
            } else {
                None
            },
        }
    }

    pub fn push(&mut self, c: char) {
        if let Some(buffer) = &mut self.buffer {
            buffer.push(c);
        }
    }
}

impl std::convert::AsRef<str> for WhitespaceAccumulator {
    #[inline]
    fn as_ref(&self) -> &str {
        match &self.buffer {
            Some(buffer) => &buffer,
            None => &" ",
        }
    }
}

impl From<WhitespaceAccumulator> for String {
    fn from(acc: WhitespaceAccumulator) -> String {
        match acc.buffer {
            Some(buffer) => buffer,
            None => " ".to_string(),
        }
    }
}
