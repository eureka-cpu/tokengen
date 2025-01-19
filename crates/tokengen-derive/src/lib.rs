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
