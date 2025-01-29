use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use crate::span::{SourceSpan, Span};
pub use tokengen_derive::{Span as DeriveSpan, Token as DeriveToken};

pub trait Token: Debug + Span {}

#[derive(Debug)]
pub struct TokenStream(pub VecDeque<Box<dyn Token>>);
impl TokenStream {
    /// Create a new token stream from the length of the source to avoid reallocations
    pub fn new(capacity: usize) -> Self {
        Self(VecDeque::with_capacity(capacity))
    }
    pub fn push_back(&mut self, token: Box<dyn Token>) {
        self.0.push_back(token)
    }
    pub fn pop_back(&mut self) -> Option<Box<dyn Token>> {
        self.0.pop_back()
    }
    pub fn push_front(&mut self, token: Box<dyn Token>) {
        self.0.push_front(token)
    }
    pub fn pop_front(&mut self) -> Option<Box<dyn Token>> {
        self.0.pop_front()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

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
/// // [StructIdentifier, 'char', [Alias1, Alias2], { DeriveTrait1, DeriveTrait2 }]
/// symbols!(
///     [ExclamationMark, '!', [Bang]],
///     [PoundSign, '#', [Hash]],
///     [OpenParenthesis, '(', { Delimiter }],
///     [ClosedParenthesis, ')', { Delimiter }]
/// );
/// ```
#[macro_export]
macro_rules! symbols {
    ( $([$name:ident, $char:literal $(,[$($alias:ident),*]),* $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[allow(dead_code)] // Ignore warnings if alias is never used
            $($(pub type $alias = $name;)*)*

            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan, $crate::token::DeriveToken $(,$($trait,)*)*)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            impl $name {
                pub const AS_LITERAL: &'static char = &$char;

                #[allow(dead_code)] // Ignore warnings if constructor is never used
                pub fn new(src: std::sync::Arc<str>, start: usize, end: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, end) }
                }
            }
            impl AsRef<char> for $name {
                fn as_ref(&self) -> &char {
                    &$char
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
        pub enum Symbol {
            $($name,)+
        }
        impl AsRef<char> for Symbol {
            fn as_ref(&self) -> &char {
                match self {
                    $(Self::$name => $name::AS_LITERAL,)+
                }
            }
        }
        impl TryFrom<char> for Symbol {
            type Error = $crate::token::Error;
            fn try_from(value: char) -> Result<Self, Self::Error> {
                match &value {
                    $($name::AS_LITERAL => Ok(Self::$name),)+
                    _ => Err($crate::token::Error::MissingSymbol(value))
                }
            }
        }
        impl std::fmt::Display for Symbol {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}", self.as_ref()),)+
                }
            }
        }
    };
}

/// A joined symbol is two or more symbols that together form a specific identifier.
#[macro_export]
macro_rules! joined_symbols {
    ( $([$name:ident, [$($symbol:ident),+] $(,{$($trait:ident),*})* ]),+ ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan, $crate::token::DeriveToken)]
            pub struct $name {
                span: $crate::span::SourceSpan,
            }
            #[allow(dead_code)]
            impl $name {
                /// The symbols that a joined symbol is made from.
                /// `Symbol` must be constructed and be in scope for this to work.
                /// See the `[symbol]` macro.
                pub const SYMBOLS: &'static [Symbol] = &[$(Symbol::$symbol,)+];

                pub fn new(src: std::sync::Arc<str>, start: usize, end: usize) -> Self {
                    Self { span: $crate::span::SourceSpan::new(src, start, end) }
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
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, $crate::token::DeriveSpan, $crate::token::DeriveToken)]
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
            $($name,)+
        }
        impl AsRef<str> for Keyword {
            fn as_ref(&self) -> &str {
                match self {
                    $(Self::$name => $name::AS_LITERAL,)+
                }
            }
        }
        impl std::fmt::Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}", self.as_ref()),)+
                }
            }
        }
    };
}

/// An identifier is the name used to uniquely identify variables, functions, classes, modules, or other user-defined entities
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, DeriveSpan, DeriveToken)]
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

/// Denotes that a [`Symbol`] or [`JoinedSymbol`] is also classified as a potential [`Delimiter`].
pub trait Delimiter: Clone + Debug + Span + Token {}
/// A [`Token`] delimited by some [`Symbol`] or [`JoinedSymbol`].
//
/// Delimiters are `Option` since we should try to recover if parsing fails.
/// [`DelimitedToken`]s are also considered [`Token`]s since they could potentially be nested.
#[derive(Debug, Clone, PartialEq, Eq, DeriveToken)]
pub struct DelimitedToken<O, T, C>
where
    O: Delimiter,
    T: Token,
    C: Delimiter,
{
    open: Option<O>,
    token: Option<T>,
    close: Option<C>,
}
impl<O, T, C> DelimitedToken<O, T, C>
where
    O: Delimiter,
    T: Token,
    C: Delimiter,
{
    pub fn new(open: Option<O>, token: Option<T>, close: Option<C>) -> Self {
        Self { open, token, close }
    }
    pub fn open(&self) -> Option<&O> {
        self.open.as_ref()
    }
    pub fn token(&self) -> Option<&T> {
        self.token.as_ref()
    }
    pub fn close(&self) -> Option<&C> {
        self.close.as_ref()
    }
}
impl<O, T, C> Span for DelimitedToken<O, T, C>
where
    O: Delimiter,
    T: Token,
    C: Delimiter,
{
    fn src(&self) -> &str {
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
    fn span(&self) -> &str {
        &self.src()[self.start()..self.end()]
    }
    fn len(&self) -> usize {
        (self.start()..self.end()).count()
    }
}

#[cfg(test)]
mod token_tests {
    //! Tests for asserting that the macros expand as expected.

    use std::sync::Arc;

    use super::{DelimitedToken, Delimiter, Token};
    use crate::span::Span;
    use expect_test::{expect, Expect};
    use tokengen_derive::Delimiter;

    #[derive(Debug, Copy, Clone)]
    struct DummyToken;
    impl Token for DummyToken {}
    impl Span for DummyToken {
        fn src(&self) -> &str {
            ""
        }
        fn start(&self) -> usize {
            0
        }
        fn end(&self) -> usize {
            0
        }
        fn span(&self) -> &str {
            &self.src()[self.start()..self.end()]
        }
        fn len(&self) -> usize {
            self.src().len()
        }
    }

    symbols!(
        [ExclamationMark, '!', [Bang]],
        [PoundSign, '#', [Hash]],
        [OpenParenthesis, '(', { Delimiter }],
        [ClosedParenthesis, ')', { Delimiter }]
    );
    joined_symbols!([HashBang, [PoundSign, ExclamationMark]]);
    keywords!([If, "if"]);

    fn check_spans<S: Span + std::fmt::Debug>(output: S, expect: Expect) {
        expect.assert_eq(&format!("{output:#?}"));
    }

    #[test]
    fn test_symbol() {
        let symbol_str = PoundSign::AS_LITERAL.to_string();
        let src = r#"# Hello, World!"#;
        let hash = Hash::new(Arc::from(src), 0, symbol_str.len());

        assert_eq!(symbol_str.len(), hash.len());
        assert_eq!(symbol_str, format!("{hash}"));
        check_spans(
            hash,
            expect![[r##"
                PoundSign {
                    span: SourceSpan {
                        src: "#",
                        start: 0,
                        end: 1,
                    },
                }"##]],
        );
    }

    #[test]
    fn test_joined_symbol() {
        let joined_symbol_str = HashBang::SYMBOLS
            .iter()
            .map(|s| *s.as_ref())
            .collect::<String>();
        let src = r#"#!"#;
        let hashbang = HashBang::new(Arc::from(src), 0, joined_symbol_str.len());

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
        assert_eq!(keyword_str, if_keyword.span());
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
    fn test_delimiter() {
        let open_str = OpenParenthesis::AS_LITERAL.to_string();
        let close_str = ClosedParenthesis::AS_LITERAL.to_string();
        let src = r#"()"#;
        let open = OpenParenthesis::new(Arc::from(src), 0, open_str.len());
        let close =
            ClosedParenthesis::new(Arc::from(src), open.end(), open.end() + close_str.len());
        let delimited_token = DelimitedToken::new(Some(open), None::<DummyToken>, Some(close));

        assert_eq!(open_str.len(), delimited_token.open().unwrap().len());
        assert_eq!(close_str.len(), delimited_token.close().unwrap().len());
        assert_eq!(open_str, delimited_token.open().unwrap().span());
        assert_eq!(close_str, delimited_token.close().unwrap().span());
        check_spans(
            delimited_token,
            expect![[r#"
                DelimitedToken {
                    open: Some(
                        OpenParenthesis {
                            span: SourceSpan {
                                src: "(",
                                start: 0,
                                end: 1,
                            },
                        },
                    ),
                    token: None,
                    close: Some(
                        ClosedParenthesis {
                            span: SourceSpan {
                                src: ")",
                                start: 1,
                                end: 2,
                            },
                        },
                    ),
                }"#]],
        );
    }
}
