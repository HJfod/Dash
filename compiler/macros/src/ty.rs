extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Paren},
    Error, Ident, Result, Token,
};

use crate::defs::{kw, Gen};

#[derive(Clone)]
pub enum ClauseTy {
    List(Vec<ClauseTy>),
    Vec(Box<ClauseTy>),
    Option(Box<ClauseTy>),
    Rule(Ident),
    Enum(Ident),
    Char,
    String,
    Default,
}

impl ClauseTy {
    pub fn inner_ty(&self) -> &ClauseTy {
        match self {
            Self::Vec(a) => a.as_ref(),
            Self::Option(a) => a.as_ref(),
            a => a,
        }
    }

    pub fn is_convertible(&self, other: &ClauseTy) -> bool {
        match (self, other) {
            (Self::List(a), Self::List(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.is_convertible(b))
            }
            (Self::Vec(a), Self::Vec(b)) => a.is_convertible(b),
            (Self::Option(a), Self::Option(b)) => a.is_convertible(b),
            (Self::Rule(a), Self::Rule(b)) => a == b,
            (Self::Enum(a), Self::Enum(b)) => a == b,
            (Self::Char, Self::Char) => true,
            (Self::String, Self::String) => true,
            (_, Self::Default) => true,
            (Self::Default, _) => true,
            _ => false,
        }
    }
}

impl Parse for ClauseTy {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.lookahead1();
        let res;
        if ahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            res = match ident.to_string().as_str() {
                "String" => ClauseTy::String,
                "char" => ClauseTy::Char,
                _ => Err(Error::new(
                    ident.span(),
                    "unknown type (rules and enums must be qualified with keywords)",
                ))?,
            }
        } else if ahead.peek(kw::rule) {
            input.parse::<kw::rule>()?;
            res = ClauseTy::Rule(input.parse()?);
        } else if ahead.peek(Token![enum]) {
            input.parse::<Token![enum]>()?;
            res = ClauseTy::Enum(input.parse()?);
        } else if ahead.peek(Bracket) {
            let c;
            bracketed!(c in input);
            res = ClauseTy::Vec(c.parse()?);
        } else if ahead.peek(Paren) {
            let c;
            parenthesized!(c in input);
            res = ClauseTy::List(
                Punctuated::<ClauseTy, Token![,]>::parse_terminated(&c)?
                    .into_iter()
                    .collect(),
            );
        } else {
            return Err(ahead.error())?;
        }
        if input.parse::<Token![?]>().is_ok() {
            Ok(ClauseTy::Option(res.into()))
        } else {
            Ok(res)
        }
    }
}

impl Gen for ClauseTy {
    fn gen(&self) -> Result<TokenStream2> {
        match self {
            Self::List(tys) => {
                let mut variants = TokenStream2::new();
                for ty in tys {
                    let ty = ty.gen()?;
                    variants.extend(quote! { #ty, });
                }
                Ok(quote! { (#variants) })
            }
            Self::Vec(ty) => {
                let ty = ty.gen()?;
                Ok(quote! { Vec<#ty> })
            }
            Self::Option(ty) => {
                let ty = ty.gen()?;
                Ok(quote! { Option<#ty> })
            }
            Self::Rule(rule) => Ok(quote! { #rule<'s> }),
            Self::Enum(e) => Ok(quote! { #e }),
            Self::Char => Ok(quote! { char }),
            Self::String => Ok(quote! { String }),
            Self::Default => Ok(quote! { _ }),
        }
    }
}
