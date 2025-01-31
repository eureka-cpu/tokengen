use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(OperatorToken)]
pub fn derive_operator(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> OperatorToken for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> OperatorToken for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl OperatorToken for #name {}
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(PunctuatorToken)]
pub fn derive_punctuator(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> PunctuatorToken for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> PunctuatorToken for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl PunctuatorToken for #name {}
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(DelimiterToken)]
pub fn derive_delimiter(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> DelimiterToken for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> DelimiterToken for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl DelimiterToken for #name {}
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(TokenSum)]
pub fn derive_token_sum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> TokenSumType for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> TokenSumType for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl TokenSumType for #name {}
        }
    };

    TokenStream::from(expanded)
}

/// Only works on struct types, must have a field named `span` of type `SourceSpan`.
#[proc_macro_derive(Span)]
pub fn derive_span(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> Span for #name<#gen> #where_clause {
                    fn src(&self) -> &std::sync::Arc<str> {
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
            }
        } else {
            quote! {
                impl<#gen> Span for #name<#gen> {
                    fn src(&self) -> &std::sync::Arc<str> {
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
            }
        }
    } else {
        quote! {
            impl Span for #name {
                fn src(&self) -> &std::sync::Arc<str> {
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
        }
    };

    TokenStream::from(expanded)
}
