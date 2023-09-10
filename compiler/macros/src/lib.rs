
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Block, ItemFn};

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
