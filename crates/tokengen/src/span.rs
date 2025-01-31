use std::{fmt, sync::Arc};

// TODO: Add method for getting the source span itself
pub trait Span {
    fn src(&self) -> &Arc<str>;

    fn start(&self) -> usize;

    fn end(&self) -> usize;

    fn span(&self) -> &SourceSpan;

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct SourceSpan {
    // TODO: Add an id for the file it came from
    src: Arc<str>,
    start: usize,
    end: usize,
}

impl SourceSpan {
    pub fn new(src: Arc<str>, start: usize, end: usize) -> Self {
        Self { src, start, end }
    }
    pub fn as_str(&self) -> &str {
        &self.src()[self.start..self.end]
    }
}

impl Span for SourceSpan {
    fn src(&self) -> &Arc<str> {
        &self.src
    }
    fn start(&self) -> usize {
        self.start
    }
    fn end(&self) -> usize {
        self.end
    }
    fn span(&self) -> &Self {
        self
    }
    fn len(&self) -> usize {
        (self.start..self.end).count()
    }
}

impl fmt::Debug for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceSpan")
            .field("src", &self.span().as_str())
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}
