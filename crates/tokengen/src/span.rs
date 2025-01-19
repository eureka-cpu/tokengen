use std::fmt;

pub trait Span {
    fn src(&self) -> &str;
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn span(&self) -> &str;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct SourceSpan {
    src_ptr: *const u8,
    src_len: usize,
    start: usize,
    end: usize,
}

impl SourceSpan {
    pub fn new<T: AsRef<str>>(src: T, start: usize, end: usize) -> Self {
        let s = src.as_ref();
        Self {
            src_ptr: s.as_ptr(),
            src_len: s.len(),
            start,
            end,
        }
    }
}

impl Span for SourceSpan {
    fn src(&self) -> &str {
        let slice: &[u8] = unsafe { std::slice::from_raw_parts(self.src_ptr, self.src_len) };
        std::str::from_utf8(slice).expect("source span failed to convert slice to &str")
    }
    fn start(&self) -> usize {
        self.start
    }
    fn end(&self) -> usize {
        self.end
    }
    fn span(&self) -> &str {
        &self.src()[self.start..self.end]
    }
    fn len(&self) -> usize {
        (self.start..self.end).count()
    }
}

impl fmt::Debug for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceSpan")
            .field("src", &self.span())
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}
