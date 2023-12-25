
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::{FromMeta, ast::NestedMeta};
use proc_macro::TokenStream;
use quote::quote;
use syn::Pat;
use syn::spanned::Spanned;
use syn::{parse_macro_input, ItemStruct, parse::Parser, Fields, Field};

macro_rules! unwrap_macro_input {
    ($e: expr) => {
        match $e {
            Ok(v) => v,
            Err(e) => return syn::Error::from(e).to_compile_error().into(),
        }
    };
}

#[derive(Debug, FromMeta)]
struct TokenArgs {
    kind: String,
    #[darling(default)]
    raw: Option<String>,
}

#[proc_macro_attribute]
pub fn token(args: TokenStream, stream: TokenStream) -> TokenStream {
    let attr_args = unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()));
    let mut target = parse_macro_input!(stream as ItemStruct);
    match &mut target.fields {
        Fields::Named(named) => {
            named.named.push(Field::parse_named.parse2(quote! {
                span: crate::shared::src::ArcSpan
            }).unwrap());
        }
        _ => {
            return syn::Error::new(
                target.ident.span(),
                "AST nodes must be structs with named fields"
            ).to_compile_error().into()
        }
    }
    let args = unwrap_macro_input!(TokenArgs::from_list(&attr_args));
    let kind: Pat = unwrap_macro_input!(Pat::parse_single.parse_str(&args.kind));
    let expected_kind = {
        let construct = match &kind {
            Pat::TupleStruct(t) => {
                let path = &t.path;
                quote!{ #path(Default::default()) }
            }
            Pat::Ident(i) => {
                let path = &i.ident;
                quote! { #path }
            }
            _ => {
                return syn::Error::new(
                    args.kind.span(), "kind must be a valid TokenKind pattern without the TokenKind prefix"
                ).to_compile_error().into();
            }
        };
        let raw = args.raw.as_deref().unwrap_or("");
        quote! { crate::parser::tokenizer::Token {
            kind: crate::parser::tokenizer::TokenKind::#construct,
            raw: #raw,
            span: crate::shared::src::Span::builtin(),
        } }
    };
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
                if let Some(token) = tokenizer.peek() {
                    if matches!(token.kind, crate::parser::tokenizer::TokenKind::#kind) #test_raw {
                        return Ok(Self { span: crate::shared::src::ArcSpan(
                            src, tokenizer.next().unwrap().span.1.clone()
                        ) });
                    }
                }
                tokenizer.expected(#expected_kind);
                Err(())
            }
            
            fn peek<'s, I>(
                tokenizer: &crate::parser::tokenizer::TokenIterator<'s, I>
            ) -> bool
                where I: Iterator<Item = crate::parser::tokenizer::Token<'s>>
            {
                if let Some(token) = tokenizer.peek() {
                    matches!(token.kind, crate::parser::tokenizer::TokenKind::#kind) #test_raw
                }
                else {
                    false
                }
            }
        }
    }.into()
}
