use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Delimiter)]
pub fn derive_delimiter(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> Delimiter for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> Delimiter for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl Delimiter for #name {}
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(Token)]
pub fn derive_token(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = if !input.generics.params.is_empty() {
        let gen = input.generics.params;
        if let Some(where_clause) = input.generics.where_clause {
            quote! {
                impl<#gen> Token for #name<#gen> #where_clause {}
            }
        } else {
            quote! {
                impl<#gen> Token for #name<#gen> {}
            }
        }
    } else {
        quote! {
            impl Token for #name {}
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
            }
        } else {
            quote! {
                impl<#gen> Span for #name<#gen> {
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
            }
        }
    } else {
        quote! {
            impl Span for #name {
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
        }
    };

    TokenStream::from(expanded)
}
