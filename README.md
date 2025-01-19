# tokengen

`tokengen` uses rust procedural derive and generative macros to make producing custom tokens and impls for lexers and parsers.
Be warned that this crate is purely for my own research and implementation purposes, but I am happy to receive feedback, issues and pull requests!

## Usage

Import the crate as a git dependency in your `Cargo.toml`. If you wish to use the derive features, enable them like so:

```toml
[dependencies]
tokengen = { git = "...", features = ["derive"] }
```

Generate single or multiples of keywords, symbols and more using the generative or derive macros:
```rust
keyword!([If, "if"]);

symbol!(
    [ExclamationMark, '!', [Bang]],
    [PoundSign, '#', [Hash]],
    [OpenParenthesis, '(', { Delimiter }],
    [ClosedParenthesis, ')', { Delimiter }]
);
```

> Using derive macros require the `derive` feature.

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Token)]
pub struct Ident {
    span: SourceSpan,
}
impl Ident {
    pub fn new(src: &str, start: usize, end: usize) -> Self {
        Self {
            span: SourceSpan::new(src, start, end),
        }
    }
}
impl Span for Ident {
    fn src(&self) -> &str {
        self.span.src()
    }
    fn start(&self) -> usize {
        self.span.start()
    }
    fn end(&self) -> usize {
        self.span.end()
    }
    fn span(&self) -> &str {
        self.span.span()
    }
    fn len(&self) -> usize {
        self.span.len()
    }
}
```

Now you have some generated types that automatically implement key features used in most lexing and parsing scenarios!
