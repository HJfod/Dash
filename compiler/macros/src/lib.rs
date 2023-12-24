
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::{FromMeta, ast::NestedMeta, Error};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{parse_macro_input, ItemStruct, Attribute, spanned::Spanned, LitStr, parse::Parse, Fields};

macro_rules! unwrap_macro_input {
    ($e: expr) => {
        match $e {
            Ok(v) => v,
            Err(e) => return TokenStream::from(Error::from(e).write_errors())
        }
    };
}

#[derive(Debug, FromMeta)]
struct TokenArgs {
    kind: Ident,
    #[darling(default)]
    raw: Option<LitStr>,
}

#[proc_macro_attribute]
pub fn token(args: TokenStream, stream: TokenStream) -> TokenStream {
    let attr_args = unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()));
    let mut target = parse_macro_input!(stream as ItemStruct);
    match &mut target.fields {
        Fields::Named(named) => {

        }
        _ => {
            return syn::Error::new(
                target.ident.span(),
                "AST nodes must be structs with named fields"
            ).to_compile_error().into()
        }
    }
    let args = unwrap_macro_input!(TokenArgs::from_list(&attr_args));
    let kind = args.kind;
    let test_raw = if let Some(raw) = args.raw {
        quote! { && token.raw == #raw }
    }
    else {
        quote! {}
    };
    let target_name = &target.ident;
    quote! {
        #target

        impl crate::parser::parse::Parse for #target_name {
            fn parse<'s, I>(
                src: std::sync::Arc<crate::shared::src::Src>,
                tokenizer: &mut crate::parser::tokenizer::TokenIterator<'s, I>
            ) -> Result<Self, ()>
                where I: Iterator<Item = crate::parser::tokenizer::Token<'s>>
            {
                let token = tokenizer.next().ok_or(())?;
                if matches!(token.kind, #kind) #test_raw {
                    Ok(Self {})
                }
            }
            
            fn peek<'s, I>(
                tokenizer: &crate::parser::tokenizer::TokenIterator<'s, I>
            ) -> bool
                where I: Iterator<Item = crate::parser::tokenizer::Token<'s>>
            {

            }
        }
    }.into()
}
