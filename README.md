# tokengen

`tokengen` uses rust procedural derive and generative macros to make producing custom tokens and impls for lexers and parsers.
Be warned that this crate is purely for my own research and implementation purposes, but I am happy to receive feedback, issues and pull requests!

## Usage

Import the crate as a git dependency in your `Cargo.toml`. If you wish to use the derive features, enable them like so:

```toml
[dependencies]
tokengen = { git = "...", features = ["derive"] }
```

Generate single or multiples of keywords, symbols and more using the generative or derive macros. By doing this, all necessary implementations are provided,
and spans are automatically calculated based on their start position and the UTF-8 length of the `char` provided.
```rust
symbols!(
    [ExclamationMark, '!'],
    [PoundSign, '#'],
    [OpenParenthesis, '(', { SomeExtraDeriveTrait }],
    [ClosedParenthesis, ')', { SomeExtraDeriveTrait }]
);
keywords!([If, "if"], [Else, "else"]);
```

> Using derive macros require the `derive` feature.

You can then take your symbols and create token groups. Some are provided for you but everything is there to make a custom one if you wish.
Additionally supplied symbols will be considered part of the pattern of a token. These macros also generate enums for the _kind_ which is useful for
pattern matching, as well as a sum type of all structs generated by the macro. The sum types are named after the macros in non-plural form, ie. `symbols!` -> `Symbol`. 
```rust
symbols!([Plus, '+'], [Hyphen, '-'], [Semicolon, ';'], [OpenParenthesis, '(']);
operators!([Addition, [Plus]], [Subtraction, [Hyphen]]);
punctuators!([Semi, [Semicolon]]);
delimiters!([OpenParen, [OpenParenthesis]]);
```

Finally, with all of the token types declared, add them to the sum type of your language token. The identifiers enclosed in the curly braces
are the variants of the sum type and must already exist. The following are provided by the library, but can be extended so long as the trait
requirements are met. Both the traits and their derive macro components are available.
```rust
generate_token_sum_type!([MyLanguageToken, { Keyword, Operator, Punctuator, Delimiter, Ident }]);
```

General implementation of `Span`:
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    fn src(&self) -> &Arc<str> {
        self.span.src()
    }
    fn start(&self) -> usize {
        self.span.start()
    }
    fn end(&self) -> usize {
        self.span.end()
    }
    fn span(&self) -> &SourceSpan {
        self.span.span()
    }
    fn len(&self) -> usize {
        self.span.len()
    }
}
```

Derive `Span`:
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, tokengen::tokengen_derive::Span)]
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
```

> Note: There is also a derive macro for implementing Span for a type, and its only requirement is to have
> a field where the span is declared. In the future this will probably be expanded to cover more ground.

Now you have some generated types that automatically implement key features used in most lexing and parsing scenarios!
