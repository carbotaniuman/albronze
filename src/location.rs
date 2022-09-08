use std::ops::Range;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

static_assertions::assert_eq_size!(SourceKind, [u8; 4]);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SourceKind {
    Generated,
    File(codespan::FileId),
}

impl Span {
    pub fn len(self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }
}

impl Into<codespan::Span> for Span {
    fn into(self) -> codespan::Span {
        codespan::Span::new(self.start, self.end)
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}

impl<T: Into<u32>> From<Range<T>> for Span {
    fn from(r: Range<T>) -> Span {
        Span {
            start: r.start.into(),
            end: r.end.into(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub span: Span,
    pub source: SourceKind,
}

impl Default for Location {
    fn default() -> Self {
        Self::generated(Span { start: 0, end: 0 })
    }
}

impl Location {
    pub fn generated(span: Span) -> Location {
        Self {
            span,
            source: SourceKind::Generated,
        }
    }
    pub fn with<T>(self, data: T) -> Locatable<T> {
        Locatable {
            data,
            location: self,
        }
    }

    pub fn len(&self) -> usize {
        self.span.len()
    }

    pub fn is_empty(&self) -> bool {
        self.span.is_empty()
    }

    pub fn is_directly_before(self, other: Location) -> bool {
        self.source != SourceKind::Generated
            && self.source == other.source
            && self.span.end == other.span.start
    }

    pub fn merge_span(&mut self, span: Span) {
        use std::cmp::{max, min};

        self.span = Span {
            start: min(self.span.start, span.start),
            end: max(self.span.end, span.end),
        };
    }

    // will only merge spans if the file matches
    #[must_use]
    pub fn maybe_merge(mut self, other: Location) -> Self {
        if self.source != SourceKind::Generated && self.source == other.source {
            self.merge_span(other.span);
        }
        self
    }

    #[must_use]
    pub fn maybe_merge_opt(mut self, other: Option<Location>) -> Self {
        if let Some(other) = other {
            self.maybe_merge(other)
        } else {
            self
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Locatable<T> {
    pub data: T,
    pub location: Location,
}

impl<T> Locatable<T> {
    pub fn new(data: T, location: Location) -> Locatable<T> {
        location.with(data)
    }

    pub fn map<S, F: FnOnce(T) -> S>(self, f: F) -> Locatable<S> {
        Locatable {
            data: f(self.data),
            location: self.location,
        }
    }
}

impl<T: PartialEq> PartialEq for Locatable<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T: Eq> Eq for Locatable<T> {}
