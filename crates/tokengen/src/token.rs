use std::{fmt::Debug, sync::Arc};

use crate::span::{SourceSpan, Span};
pub use tokengen_derive::{
    DelimiterToken, OperatorToken, PunctuatorToken, Span as DeriveSpan, TokenSum,
};

pub struct TokenStream<T: Span>(Vec<T>);
impl<T: Span> TokenStream<T> {
    /// Create a new token stream from the length of the source to avoid reallocations
    pub fn new(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }
    pub fn push(&mut self, token: T) {
        self.0.push(token)
    }
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

// A little bit of pretty-printing in the debug printout helps with
// downstream consumers who will be using expect_test
impl<T: Span> Debug for TokenStream<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for tok in &self.0 {
            writeln!(f, "{} ({}..{})", tok.src(), tok.start(), tok.end())?;
        }
        Ok(())
    }
}


#[macro_export]
macro_rules! generate_token_sum_type {
    ( $([$name:ident, { $($variant:ident),* }]),+ ) => {
        $(
            #[allow(dead_code)]
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
            pub enum $name {
                $($variant($variant),)*
            }
            impl Span for $name {
                fn src(&self) -> &std::sync::Arc<str> {
                    match self {
                        $(Self::$variant(t) => t.span().src(),)*
                    }
                }
                fn start(&self) -> usize {
                    match self {
                        $(Self::$variant(t) => t.span().start(),)*
                    }
                }
                fn end(&self) -> usize {
                    match self {
                        $(Self::$variant(t) => t.span().end(),)*
                    }
                }
                fn span(&self) -> &$crate::span::SourceSpan {
                    match self {
                        $(Self::$variant(t) => t.span().span(),)*
                    }
                }
                fn len(&self) -> usize {
                    match self {
                        $(Self::$variant(t) => t.span().len(),)*
                    }
                }
            }
        )+
    };
}

// TODO: Add other token errors
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("'{0}' is not part of the set of defined symbols")]
    MissingSymbol(char),

    #[error("'{0}' is not part of the set of defined keywords")]
    MissingKeyword(String),
}

// TODO: See if there is a way to accept doc comments as part of the macro rule
// this way users can document their types they've created with the macro
/// Basic single character symbols that can later be used in crafting tokens,
/// or in match arms when lexing. This macro allows for common aliases to
/// be passed as a list for convenience, but is subject to change.
/// Additional derive traits can optionally be added at the end to extend
/// functionality without the need of explicit impl blocks.
///
/// Usage:
/// ```rust,ignore
/// // [StructIdentifier, 'char', { DeriveTrait1, DeriveTrait2 }]
/// symbols!(
///     [ExclamationMark, '!'],
///     [PoundSign, '#'],
///     [OpenParenthesis, '('],
///     [ClosedParenthesis, ')']
/// );
/// ```
#[macro_export]
macro_rules! symbols {
    ( $([$name:ident, $char:literal $(,{$($trait:ident),*})* ]),+ ) => {
        // $(
        //     #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan $(,$($trait,)*)*)]
        //     pub struct $name {
        //         span: $crate::span::SourceSpan,
        //     }
        //     impl $name {
        //         pub const AS_LITERAL: &'static char = &$char;

        //         #[allow(dead_code)] // Ignore warnings if constructor is never used
        //         pub fn new(src: std::sync::Arc<str>, start: usize) -> Self {
        //             Self { span: $crate::span::SourceSpan::new(src, start, start + $char.len_utf8()) }
        //         }
        //     }
        //     impl AsRef<char> for $name {
        //         fn as_ref(&self) -> &char {
        //             &$char
        //         }
        //     }
        //     impl std::fmt::Display for $name {
        //         fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //             write!(f, "{}", self.as_ref())
        //         }
        //     }
        // )+
        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct Symbol;

        impl Symbol {
            $(pub const $name: char = $char;)+
        }
        // impl AsRef<char> for Symbol {
        //     fn as_ref(&self) -> &char {
        //         match self {
        //             $(Self::$name => $name::AS_LITERAL,)+
        //         }
        //     }
        // }
        // impl TryFrom<char> for Symbol {
        //     type Error = $crate::token::Error;
        //     fn try_from(value: char) -> Result<Self, Self::Error> {
        //         match &value {
        //             $($name::AS_LITERAL => Ok(Self::$name),)+
        //             _ => Err($crate::token::Error::MissingSymbol(value))
        //         }
        //     }
        // }
        // impl std::fmt::Display for Symbol {
        //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //         match self {
        //             $(Self::$name => write!(f, "{}", self.as_ref()),)+
        //         }
        //     }
        // }
    };
}

/// A joined symbol is two or more symbols that together form a specific identifier.
#[macro_export]
macro_rules! joined_symbols {
    ( $([$name:ident, [$($symbol:ident),+] $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            #[allow(dead_code)]
            impl $name {
                /// The symbols that a joined symbol is made from.
                /// `Symbol` must be constructed and be in scope for this to work.
                /// See the `[symbol]` macro.
                pub const SYMBOLS: &'static [char] = &[$(Symbol::$symbol,)+];

                pub fn new(src: std::sync::Arc<str>, start: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, start + Self::SYMBOLS.iter().map(|s| s.len_utf8()).sum::<usize>()) }
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Ok(Self::SYMBOLS
                        .iter()
                        .for_each(|symbol| write!(f, "{symbol}")
                        .expect("failed to format joined symbol.")))
                }
            }
        )+
    };
}

pub trait OperatorToken: Debug + Clone + Span {}

#[macro_export]
macro_rules! operators {
    ( $([$name:ident, [$($symbol:ident),+] $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            #[allow(dead_code)]
            impl $name {
                /// The symbols that a joined symbol is made from.
                /// `Symbol` must be constructed and be in scope for this to work.
                /// See the `[symbol]` macro.
                pub const SYMBOLS: &'static [char] = &[$(Symbol::$symbol,)+];

                pub fn new(src: std::sync::Arc<str>, start: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, start + Self::SYMBOLS.iter().map(|s| s.len_utf8()).sum::<usize>()) }
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Ok(Self::SYMBOLS
                        .iter()
                        .for_each(|symbol| write!(f, "{symbol}")
                        .expect("failed to format joined symbol.")))
                }
            }
        )+
        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::OperatorToken)]
        pub enum Operator {
            $($name($name),)+
        }
        impl Span for Operator {
            fn src(&self) -> &std::sync::Arc<str> {
                match self {
                    $(Self::$name(o) => o.span.src(),)+
                }
            }
            fn start(&self) -> usize {
                match self {
                    $(Self::$name(o) => o.span.start(),)+
                }
            }
            fn end(&self) -> usize {
                match self {
                    $(Self::$name(o) => o.span.end(),)+
                }
            }
            fn span(&self) -> &$crate::span::SourceSpan {
                match self {
                    $(Self::$name(o) => o.span.span(),)+
                }
            }
            fn len(&self) -> usize {
                match self {
                    $(Self::$name(o) => o.span.len(),)+
                }
            }
        }

        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum OperatorKind {
            $($name,)+
        }
        impl std::fmt::Display for OperatorKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}", $name::SYMBOLS.iter().collect::<String>()),)+
                }
            }
        }
    };
}

pub trait PunctuatorToken: Debug + Clone + Span {}

#[macro_export]
macro_rules! punctuators {
    ( $([$name:ident, [$($symbol:ident),+] $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            #[allow(dead_code)]
            impl $name {
                /// The symbols that a joined symbol is made from.
                /// `Symbol` must be constructed and be in scope for this to work.
                /// See the `[symbol]` macro.
                pub const SYMBOLS: &'static [char] = &[$(Symbol::$symbol,)+];

                pub fn new(src: std::sync::Arc<str>, start: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, start + Self::SYMBOLS.iter().map(|s| s.len_utf8()).sum::<usize>()) }
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Ok(Self::SYMBOLS
                        .iter()
                        .for_each(|symbol| write!(f, "{symbol}")
                        .expect("failed to format joined symbol.")))
                }
            }
        )+

        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::PunctuatorToken)]
        pub enum Punctuator {
            $($name($name),)+
        }
        impl Span for Punctuator {
            fn src(&self) -> &std::sync::Arc<str> {
                match self {
                    $(Self::$name(p) => p.span.src(),)+
                }
            }
            fn start(&self) -> usize {
                match self {
                    $(Self::$name(p) => p.span.start(),)+
                }
            }
            fn end(&self) -> usize {
                match self {
                    $(Self::$name(p) => p.span.end(),)+
                }
            }
            fn span(&self) -> &$crate::span::SourceSpan {
                match self {
                    $(Self::$name(p) => p.span.span(),)+
                }
            }
            fn len(&self) -> usize {
                match self {
                    $(Self::$name(p) => p.span.len(),)+
                }
            }
        }

        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum PunctuatorKind {
            $($name,)+
        }
        impl std::fmt::Display for PunctuatorKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}", $name::SYMBOLS.iter().collect::<String>()),)+
                }
            }
        }
    };
}

/// A keyword is identifier that is reserved for a language
///
/// Usage:
/// ```rust,ignore
/// [StructIdentifier, "&str"]
/// keywords!([If, "if"]);
/// ```
#[macro_export]
macro_rules! keywords {
    ( $([$name:ident, $str:literal]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            impl $name {
                // TODO: Change this so that the name of the const is the name of the symbol or just a better name
                pub const AS_LITERAL: &'static str = $str;
                pub fn new(src: std::sync::Arc<str>, start: usize, end: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, end) }
                }
            }
            impl AsRef<str> for $name {
                fn as_ref(&self) -> &str {
                    &$str
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_ref())
                }
            }
        )+
        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum Keyword {
            $($name($name),)+
        }
        impl Span for Keyword {
            fn src(&self) -> &std::sync::Arc<str> {
                match self {
                    $(Self::$name(k) => k.span.src(),)+
                }
            }
            fn start(&self) -> usize {
                match self {
                    $(Self::$name(k) => k.span.start(),)+
                }
            }
            fn end(&self) -> usize {
                match self {
                    $(Self::$name(k) => k.span.end(),)+
                }
            }
            fn span(&self) -> &$crate::span::SourceSpan {
                match self {
                    $(Self::$name(k) => k.span.span(),)+
                }
            }
            fn len(&self) -> usize {
                match self {
                    $(Self::$name(k) => k.span.len(),)+
                }
            }
        }

        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum KeywordKind {
            $($name,)+
        }
        impl AsRef<str> for KeywordKind {
            fn as_ref(&self) -> &str {
                match self {
                    $(Self::$name => $name::AS_LITERAL,)+
                }
            }
        }
        impl std::fmt::Display for KeywordKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}", self.as_ref()),)+
                }
            }
        }
    };
}

/// An identifier is the name used to uniquely identify variables, functions, classes, modules, or other user-defined entities
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, DeriveSpan)]
pub struct Ident {
    span: SourceSpan,
}
impl Ident {
    pub fn new(src: Arc<str>, start: usize, end: usize) -> Self {
        Self {
            span: SourceSpan::new(src, start, end),
        }
    }
}

#[macro_export]
macro_rules! delimiters {
    ( $([$name:ident, [$($symbol:ident),+] $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan, $(,$($trait,)*)*)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            #[allow(dead_code)]
            impl $name {
                /// The symbols that a joined symbol is made from.
                /// `Symbol` must be constructed and be in scope for this to work.
                /// See the `[symbol]` macro.
                pub const SYMBOLS: &'static [char] = &[$(Symbol::$symbol,)+];

                pub fn new(src: std::sync::Arc<str>, start: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, start + Self::SYMBOLS.iter().map(|s| s.len_utf8()).sum::<usize>()) }
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Ok(Self::SYMBOLS
                        .iter()
                        .for_each(|symbol| write!(f, "{symbol}")
                        .expect("failed to format joined symbol.")))
                }
            }
        )+
        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DelimiterToken)]
        pub enum Delimiter {
            $($name($name),)+
        }
        impl Span for Delimiter {
            fn src(&self) -> &std::sync::Arc<str> {
                match self {
                    $(Self::$name(d) => d.span.src(),)+
                }
            }
            fn start(&self) -> usize {
                match self {
                    $(Self::$name(d) => d.span.start(),)+
                }
            }
            fn end(&self) -> usize {
                match self {
                    $(Self::$name(d) => d.span.end(),)+
                }
            }
            fn span(&self) -> &$crate::span::SourceSpan {
                match self {
                    $(Self::$name(d) => d.span.span(),)+
                }
            }
            fn len(&self) -> usize {
                match self {
                    $(Self::$name(d) => d.span.len(),)+
                }
            }
        }
        impl std::fmt::Display for Delimiter {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name(_) => write!(f, "{}", $name::SYMBOLS.iter().map(|s| s).collect::<String>()),)+
                }
            }
        }
    };
}

pub trait DelimiterToken: Debug + Clone + Span {}

/// This uses generics since the tokens and consequently, the union types
/// are not yet created. The only type DelimiterToken is implemented for is Delimiter.
#[derive(Debug, Clone, Copy)]
pub struct DelimitedTokenBuilder<D: DelimiterToken, T: Span> {
    open: Option<D>,
    token: Option<T>,
    close: Option<D>,
}

impl <D: DelimiterToken, T: Span> Default for DelimitedTokenBuilder<D, T>
{
    fn default() -> Self {
        Self {
            open: None,
            token: None,
            close: None,
        }
    }
}

impl<D: DelimiterToken, T: Span> DelimitedTokenBuilder<D, T> {
    pub fn new() -> Self {
        Self::default() 
    }
    pub fn open(self, open: Option<D>) -> Self {
        let mut buf = self;
        buf.open = open;
        buf
    }
    pub fn token(self, token: Option<T>) -> Self {
        let mut buf = self;
        buf.token = token;
        buf
    }
    pub fn close(self, close: Option<D>) -> Self {
        let mut buf = self;
        buf.close = close;
        buf
    }
    pub fn build(self) -> DelimitedToken<D, T> {
        let open = self.open;
        let token = self.token;
        let close = self.close;

        let open_ref = open
            .as_ref()
            .expect("Cannot build DelimitedToken without opening delimiter");
        let start = open_ref.start();
        let end = if let Some(close) = &close {
            close.end()
        } else if let Some(token) = &token {
            token.end()
        } else {
            open_ref.end()
        };
        let span = SourceSpan::new(open_ref.src().clone(), start, end);

        DelimitedToken::new(open, token, close, span)
    }
}

/// A [`Token`] delimited by some [`Symbol`] or [`JoinedSymbol`].
//
/// Delimiters are `Option` since we should try to recover if parsing fails.
/// [`DelimitedToken`]s are also considered [`Token`]s since they could potentially be nested.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DelimitedToken<D: DelimiterToken, T: Span> {
    open: Option<D>,
    token: Option<T>,
    close: Option<D>,
    /// The lexer will never not have the opening delimiter, since that is what we match for.
    /// Mostly the span here is just for testing and to appease the trait constraint.
    /// If/When the span is actually needed, eg. for error handling etc, each part of the token
    /// will be checked for anyway.
    span: SourceSpan,
}
impl<D: DelimiterToken, T: Span> DelimitedToken<D, T> {
    pub fn builder() -> DelimitedTokenBuilder<D, T> {
        DelimitedTokenBuilder::new()
    }
    pub fn new(open: Option<D>, token: Option<T>, close: Option<D>, span: SourceSpan) -> Self {
        Self {
            open,
            token,
            close,
            span,
        }
    }
    pub fn open(&self) -> Option<&D> {
        self.open.as_ref()
    }
    pub fn token(&self) -> Option<&T> {
        self.token.as_ref()
    }
    pub fn close(&self) -> Option<&D> {
        self.close.as_ref()
    }
}
impl<D: DelimiterToken, T: Span> Span for DelimitedToken<D, T> {
    fn src(&self) -> &Arc<str> {
        self.open()
            .map(|t| t.src())
            .or(self
                .token()
                .map(|t| t.src())
                .or(self.close().map(|t| t.src())))
            .expect("found empty delimited token group")
    }
    fn start(&self) -> usize {
        self.open().unwrap().start()
    }
    fn end(&self) -> usize {
        self.close().unwrap().end()
    }
    fn span(&self) -> &SourceSpan {
        &self.span
    }
    fn len(&self) -> usize {
        (self.start()..self.end()).count()
    }
}

#[allow(dead_code)]
#[cfg(test)]
mod token_tests {
    //! Tests for asserting that the macros expand as expected.

    use std::sync::Arc;

    use expect_test::{expect, Expect};

    use crate::{
        span::{SourceSpan, Span},
        token::{DelimitedToken, DelimiterToken, OperatorToken},
    };

    generate_token_sum_type!(
        [DummyToken, {
            Delimiter,
            DummyTokenInner
        }],
        [DummyTokenInner, {
            Keyword
        }]
    );
    symbols!(
        [PERIOD, '.'],
        [QUESTION_MARK, '?'],
        [EXCLAMATION_MARK, '!'],
        [COMMA, ','],
        [COLON, ':'],
        [SEMICOLON, ';'],
        [HYPHEN, '-'],
        [UNDERSCORE, '_'],
        [PLUS, '+'],
        [EQUALS, '='],
        [PIPE, '|'],
        [FORWARD_SLASH, '/'],
        [BACK_SLASH, '\\'],
        [ASTERISK, '*'],
        [AMPERSAND, '&'],
        [AT, '@'],
        [POUND_SIGN, '#'],
        [DOLLAR, '$'],
        [BACKTICK, '`'],
        [SINGLE_QUOTE, '\''],
        [DOUBLE_QUOTE, '\"'],
        [OPEN_ANGLE_BRACKET, '<'],
        [CLOSE_ANGLE_BRACKET, '>'],
        [OPEN_PARENTHESIS, '('],
        [CLOSE_PARENTHESIS, ')'],
        [OPEN_SQUARE_BRACKET, '['],
        [CLOSE_SQUARE_BRACKET, ']'],
        [OPEN_CURLY_BRACE, '{'],
        [CLOSE_CURLY_BRACE, '}']
    );

    operators!(
        [AttributeSelection, [PERIOD]],
        [ArithmeticNegation, [HYPHEN]],
        [HasAttribute, [QUESTION_MARK]],
        [ListConcatination, [PLUS, PLUS]],
        [Multiplication, [ASTERISK]],
        [Division, [FORWARD_SLASH]]
    );

    delimiters!(
        [DoubleQuote, [DOUBLE_QUOTE]],
        [DoubleSingleQuote, [SINGLE_QUOTE, SINGLE_QUOTE]],
        [OpenAngleBracket, [OPEN_ANGLE_BRACKET]],
        [CloseAngleBracket, [CLOSE_ANGLE_BRACKET]],
        [OpenParenthesis, [OPEN_PARENTHESIS]],
        [CloseParenthesis, [CLOSE_PARENTHESIS]],
        [OpenSquareBracket, [OPEN_SQUARE_BRACKET]],
        [CloseSquareBracket, [CLOSE_SQUARE_BRACKET]],
        [OpenCurlyBrace, [OPEN_CURLY_BRACE]],
        [CloseCurlyBrace, [CLOSE_CURLY_BRACE]],
        [DollarCurly, [DOLLAR, OPEN_CURLY_BRACE]]
    );
    joined_symbols!([HashBang, [POUND_SIGN, EXCLAMATION_MARK]]);
    keywords!([If, "if"]);

    fn check_spans<S: Span + std::fmt::Debug>(output: S, expect: Expect) {
        expect.assert_eq(&format!("{output:#?}"));
    }

    // TODO: Update tests with new symbol types
    // #[test]
    // fn test_symbol() {
    //     let symbol_str = Symbol::PoundSign.to_string();
    //     let src = r#"# Hello, World!"#;
    //     let hash = PoundSign::new(Arc::from(src), 0);

    //     assert_eq!(symbol_str.len(), hash.len());
    //     assert_eq!(symbol_str, format!("{hash}"));
    //     check_spans(
    //         hash,
    //         expect![[r##"
    //             PoundSign {
    //                 span: SourceSpan {
    //                     src: "#",
    //                     start: 0,
    //                     end: 1,
    //                 },
    //             }"##]],
    //     );
    // }

    #[test]
    fn test_joined_symbol() {
        let joined_symbol_str = HashBang::SYMBOLS.iter().collect::<String>();
        let src = r#"#!"#;
        let hashbang = HashBang::new(Arc::from(src), 0);

        assert_eq!(joined_symbol_str.len(), hashbang.len());
        assert_eq!(joined_symbol_str, format!("{hashbang}"));
        check_spans(
            hashbang,
            expect![[r##"
            HashBang {
                span: SourceSpan {
                    src: "#!",
                    start: 0,
                    end: 2,
                },
            }"##]],
        );
    }

    #[test]
    fn test_keyword() {
        let keyword_str = If::AS_LITERAL;
        let src = r#"if [ ! -e "$1" ]; then"#;
        let if_keyword = If::new(Arc::from(src), 0, keyword_str.len());

        assert_eq!(keyword_str.len(), if_keyword.len());
        assert_eq!(keyword_str, format!("{if_keyword}"));
        assert_eq!(keyword_str, if_keyword.span().as_str());
        check_spans(
            if_keyword,
            expect![[r#"
                If {
                    span: SourceSpan {
                        src: "if",
                        start: 0,
                        end: 2,
                    },
                }"#]],
        );
    }

    #[test]
    fn test_delimited_token() {
        let src = r#"()"#;
        let open = Delimiter::OpenParenthesis(OpenParenthesis::new(Arc::from(src), 0));
        let close = Delimiter::CloseParenthesis(CloseParenthesis::new(Arc::from(src), 1));
        let delimited_token: DelimitedToken<Delimiter, DummyToken> = DelimitedToken::builder()
            .open(Some(open.clone()))
            .close(Some(close.clone()))
            .build();

        let open_str = open.to_string();
        let close_str = close.to_string();

        assert_eq!(open_str.len(), delimited_token.open().unwrap().len());
        assert_eq!(close_str.len(), delimited_token.close().unwrap().len());
        assert_eq!(open_str, delimited_token.open().unwrap().span().as_str());
        assert_eq!(close_str, delimited_token.close().unwrap().span().as_str());
        check_spans(
            delimited_token,
            expect![[r#"
                DelimitedToken {
                    open: Some(
                        OpenParenthesis(
                            OpenParenthesis {
                                span: SourceSpan {
                                    src: "(",
                                    start: 0,
                                    end: 1,
                                },
                            },
                        ),
                    ),
                    token: None,
                    close: Some(
                        CloseParenthesis(
                            CloseParenthesis {
                                span: SourceSpan {
                                    src: ")",
                                    start: 1,
                                    end: 2,
                                },
                            },
                        ),
                    ),
                    span: SourceSpan {
                        src: "()",
                        start: 0,
                        end: 2,
                    },
                }"#]],
        );
    }
}
