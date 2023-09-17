
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate convert_case;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{parse_macro_input, Block, ItemFn, ItemStruct, Fields, Field, Error};
use convert_case::{Case, Casing};

#[proc_macro]
pub fn snake_case_ident(stream: TokenStream) -> TokenStream {
    let name = parse_macro_input!(stream as Ident);
    let snake = name.to_string().to_case(Case::Snake);
    quote! { #snake }.into()
}

#[proc_macro_attribute]
pub fn gdml_ast_node(args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemStruct);
    match &mut target.fields {
        Fields::Named(named) => {
            named.named.push(Field {
                attrs: vec![],
                mutability: syn::FieldMutability::None,
                vis: syn::Visibility::Inherited,
                ident: Some(format_ident!("span")),
                colon_token: None,
                ty: syn::Type::Verbatim(quote! { Span<'s> })
            });
        },
        _ => {
            return Error::new(
                target.ident.span(),
                "AST nodes must have named fields"
            ).to_compile_error().into();
        }
    };
    let name = &target.ident;
    quote! {
        #target

        impl<'s> crate::parser::node::ASTNode<'s> for #name<'s> {
            fn span(&self) -> &crate::shared::src::Span<'s> {
                &self.span
            }
        }
    }.into()
}

#[proc_macro_attribute]
pub fn gdml_log(_: TokenStream, stream: TokenStream) -> TokenStream {
    let mut fun = parse_macro_input!(stream as ItemFn);
    let name = fun.sig.ident.to_string();
    let oblock = &fun.block;
    let ty = &fun.sig.output;
    let nblock = quote! { {
        println!("call {}", #name);
        let ret = || #ty {
            #oblock
        }();
        println!("finish {}", #name);
        ret
    } }.into();
    fun.block = parse_macro_input!(nblock as Block).into();
    quote! { #fun }.into()
}
