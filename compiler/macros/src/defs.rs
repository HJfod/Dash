
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;
use std::{collections::HashSet, hash::Hash};
use unicode_xid::UnicodeXID;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::{quote, format_ident};
use syn::{
    parse_macro_input,
    parenthesized,
    ImplItemFn,
    Ident,
    Result,
    Token,
    token::{Paren, Bracket},
    parse::{Parse, ParseStream}, 
    LitStr, LitChar,
    braced, Error, ItemUse, Field, ExprBlock,
    punctuated::Punctuated, bracketed, ItemFn,
};

pub trait Gen {
    fn gen(&self) -> Result<TokenStream2>;
}

pub fn parse_list<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    let mut list = vec![];
    while let Ok(i) = input.parse() {
        list.push(i);
    }
    Ok(list)
}

pub mod kw {
    syn::custom_keyword!(keywords);
    syn::custom_keyword!(contextual);
    syn::custom_keyword!(reserve);
    syn::custom_keyword!(rule);
    syn::custom_keyword!(into);
    syn::custom_keyword!(while_peek);
    syn::custom_keyword!(afterwards);
    syn::custom_keyword!(until);
    syn::custom_keyword!(unless);
    syn::custom_keyword!(expected);
    syn::custom_keyword!(nofallthrough);
    syn::custom_keyword!(typecheck);
    syn::custom_keyword!(XID_Start);
    syn::custom_keyword!(XID_Continue);
    syn::custom_keyword!(OP_CHAR);
    syn::custom_keyword!(ANY_CHAR);
    syn::custom_keyword!(EOF);
}
