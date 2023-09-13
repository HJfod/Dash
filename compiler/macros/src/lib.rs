
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate convert_case;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, Block, ItemFn};
use convert_case::{Case, Casing};

#[proc_macro]
pub fn snake_case_ident(stream: TokenStream) -> TokenStream {
    let name = parse_macro_input!(stream as Ident);
    let snake = name.to_string().to_case(Case::Snake);
    quote! { #snake }.into()
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
