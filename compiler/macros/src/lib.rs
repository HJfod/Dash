#![feature(proc_macro_diagnostic)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;

use defs::Gen;
use items::Rules;
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod clause;
mod typecheck;
mod defs;
mod gen;
mod items;
mod ty;

#[proc_macro]
pub fn define_rules(input: TokenStream) -> TokenStream {
    match parse_macro_input!(input as Rules).gen() {
        Ok(s) => s.into(),
        Err(e) => TokenStream::from(e.to_compile_error()),
    }
}
