use miette::SourceSpan;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl std::ops::Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        Span {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl From<std::ops::RangeInclusive<usize>> for Span {
    fn from(r: std::ops::RangeInclusive<usize>) -> Self {
        Span::new(*r.start(), *r.end() + 1)
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(r: std::ops::Range<usize>) -> Self {
        Span::new(r.start, r.end)
    }
}

impl From<Span> for SourceSpan {
    fn from(s: Span) -> Self {
        SourceSpan::new(s.start.into(), (s.end - s.start).into())
    }
}
