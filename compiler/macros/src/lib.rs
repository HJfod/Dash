
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::{FromMeta, ast::NestedMeta};
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Ident};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parse;
use syn::{Pat, ItemEnum, FieldsUnnamed};
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

macro_rules! get_named_fields {
    ($from: ident as $($borrow: tt)+) => {
        match $($borrow)+$from.fields {
            Fields::Named(named) => $($borrow)+named.named,
            _ => {
                return syn::Error::new(
                    $from.ident.span(), "AST nodes must be structs with named fields"
                ).to_compile_error().into()
            }
        }
    };
    (&$from: ident) => {
        get_named_fields!($from as &)
    };
    (&mut $from: ident) => {
        get_named_fields!($from as &mut)
    };
}

fn impl_ast_item(
    target: &impl ToTokens, target_name: &Ident,
    parse_impl: TokenStream2, peek_impl: TokenStream2
) -> TokenStream {
    quote! {
        #target
        impl crate::parser::parse::Parse for #target_name {
            fn parse<'s, I>(
                src: std::sync::Arc<crate::shared::src::Src>,
                tokenizer: &mut crate::parser::tokenizer::TokenIterator<'s, I>
            ) -> Result<Self, ()>
                where I: Iterator<Item = crate::parser::tokenizer::Token<'s>>
            {
                #parse_impl
            }
            
            fn peek<'s, I>(
                pos: usize,
                tokenizer: &crate::parser::tokenizer::TokenIterator<'s, I>
            ) -> bool
                where I: Iterator<Item = crate::parser::tokenizer::Token<'s>>
            {
                #peek_impl
            }
        }
    }.into()
}

fn impl_ast_struct(target: &mut ItemStruct, parse_impl: TokenStream2, peek_impl: TokenStream2) -> TokenStream {
    get_named_fields!(&mut target).push(
        Field::parse_named.parse2(quote! { span: crate::shared::src::ArcSpan }).unwrap()
    );
    impl_ast_item(target, &target.ident, parse_impl, peek_impl)
}

#[derive(Debug, FromMeta)]
struct TokenArgs {
    kind: String,
    #[darling(default)]
    raw: Option<String>,
}

#[proc_macro_attribute]
pub fn token(args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemStruct);
    let args = unwrap_macro_input!(TokenArgs::from_list(
        &unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()))
    ));
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
    let r: TokenStream2 = impl_ast_struct(&mut target,
        quote! {
            use crate::parser::tokenizer::TokenKind;
            use crate::shared::src::ArcSpan;
            if let Some(token) = tokenizer.peek(0) {
                if matches!(token.kind, TokenKind::#kind) #test_raw {
                    return Ok(Self { span: ArcSpan(src, tokenizer.next().unwrap().span.1.clone()) });
                }
            }
            tokenizer.expected(#expected_kind);
            Err(())
        },
        quote! {
            use crate::parser::tokenizer::TokenKind;
            if let Some(token) = tokenizer.peek(pos) {
                matches!(token.kind, TokenKind::#kind) #test_raw
            }
            else {
                false
            }
        }
    ).into();
    let name = target.ident;
    quote! {
        #r
        impl crate::parser::parse::IsToken for #name {}
    }.into()
}

fn peek_default() -> usize { 1 }

#[derive(Debug, FromMeta)]
struct NodeArgs {
    #[darling(default = "peek_default")]
    peek: usize,
}

#[proc_macro_attribute]
pub fn node(args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemStruct);
    let args = unwrap_macro_input!(NodeArgs::from_list(
        &unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()))
    ));
    let mut parse_impl = quote! {};
    let mut peek_impl = quote! {};
    let mut peeked = args.peek;
    for field in get_named_fields!(&target) {
        let i = field.ident.as_ref().unwrap();
        let t = &field.ty;
        parse_impl.extend(quote_spanned! {
            field.ty.span() => #i: Parse::parse(src.clone(), tokenizer)?,
        });
        if peeked > 0 {
            peek_impl.extend(quote_spanned! {
                field.ty.span() =>
                <#t as crate::parser::parse::IsToken>::is_token();
                if !<#t>::peek(#peeked, tokenizer) {
                    return false;
                }
            });
            peeked -= 1;
        }
    }
    impl_ast_struct(&mut target,
        quote! {
            use crate::parser::parse::Parse;
            use crate::shared::src::ArcSpan;
            let start = tokenizer.start_offset();
            Ok(Self {
                #parse_impl
                span: ArcSpan(src, start..tokenizer.end_offset())
            })
        },
        quote! {
            #peek_impl
            true
        }
    )
}

#[derive(Debug, FromMeta)]
struct GroupArgs {
    expected: String,
}

#[proc_macro_attribute]
pub fn group(args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemEnum);
    let args = unwrap_macro_input!(GroupArgs::from_list(
        &unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()))
    ));
    let mut parse_impl = quote! {};
    let mut peek_impl = quote! {};
    for variant in &mut target.variants {
        let v = &variant.ident;
        if !matches!(variant.fields, Fields::Unit) || variant.discriminant.is_some() {
            return syn::Error::new(
                variant.span(),
                "all variants of a group must be unit type without an explicit discriminant"
            ).to_compile_error().into();
        }
        variant.fields = Fields::Unnamed(unwrap_macro_input!(
            FieldsUnnamed::parse.parse2(quote! { (Box<#v>) })
        ));
        parse_impl.extend(quote! {
            if <#v>::peek(0, tokenizer) {
                return Ok(Self::#v(Box::from(<#v>::parse(src.clone(), tokenizer)?)));
            }
        });
        peek_impl.extend(quote! {
            if <#v>::peek(pos, tokenizer) {
                return true;
            }
        });
    }
    let expected = args.expected;
    impl_ast_item(&target, &target.ident,
        quote! {
            use crate::parser::parse::Parse;
            tokenizer.expected(#expected);
            Err(())
        },
        quote! {
            #peek_impl
            false
        }
    )
}
