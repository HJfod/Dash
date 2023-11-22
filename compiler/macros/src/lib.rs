
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate convert_case;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{parse_macro_input, Block, ItemFn, ItemStruct, Fields, Field, Error, ItemEnum, Expr, parse::Parse, Token, braced, Signature, Type, Pat, Meta};
use convert_case::{Case, Casing};

#[proc_macro]
pub fn snake_case_ident(stream: TokenStream) -> TokenStream {
    let name = parse_macro_input!(stream as Ident);
    let snake = name.to_string().to_case(Case::Snake);
    quote! { #snake }.into()
}

#[proc_macro_attribute]
pub fn ast_node(_args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemStruct);
    let mut iter_chains = proc_macro2::TokenStream::new();
    match &mut target.fields {
        Fields::Named(named) => {
            for named in &named.named {
                if named.attrs.iter().any(|a| match a.meta {
                    Meta::Path(ref p) => p.get_ident() == Some(&format_ident!("ast_skip_child")),
                    _ => false,
                }) {
                    continue;
                }
                let name = named.ident.as_ref().unwrap();
                iter_chains.extend(quote! {
                    .chain(self.#name.to_iter_helper())
                });
            }
            named.named.push(Field {
                attrs: vec![],
                mutability: syn::FieldMutability::None,
                vis: syn::Visibility::Inherited,
                ident: Some(format_ident!("span")),
                colon_token: None,
                ty: syn::Type::Verbatim(quote! { Span })
            });
            named.named.push(Field {
                attrs: vec![],
                mutability: syn::FieldMutability::None,
                vis: syn::Visibility::Inherited,
                ident: Some(format_ident!("eval_ty")),
                colon_token: None,
                ty: syn::Type::Verbatim(quote! { Ty })
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

        impl crate::parser::node::ASTNode for #name {
            fn span(&self) -> &crate::shared::src::Span {
                &self.span
            }

            fn children(&mut self) -> Vec<ASTRef> {
                use crate::parser::node::ChildIterHelper;
                std::iter::empty()
                    #iter_chains
                    .collect()
            }

            fn eval_ty(&self) -> Ty {
                self.eval_ty.clone()
            }
        }
    }.into()
}

#[proc_macro_attribute]
pub fn log_fun_io(_: TokenStream, stream: TokenStream) -> TokenStream {
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

struct OpaqueFnImpl {
    sig: Signature,
    matchers: Vec<(Pat, Token![=>], Expr)>,
    rest: (Ident, Token![=>], Expr),
}

impl Parse for OpaqueFnImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let sig = input.parse()?;
        input.parse::<Token![:]>()?;
        let rest;
        let mut matchers = vec![];
        loop {
            if input.parse::<Token![..]>().is_ok() {
                rest = (input.parse()?, input.parse()?, input.parse()?);
                input.parse::<Token![;]>()?;
                break;
            }
            else {
                matchers.push((Pat::parse_multi(input)?, input.parse()?, input.parse()?));
                input.parse::<Token![;]>()?;
            }
        }
        Ok(Self { sig, matchers, rest })
    }
}

struct OpaqueImpl {
    name: Type,
    fns: Vec<OpaqueFnImpl>,
}

impl Parse for OpaqueImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;
        let name = input.parse()?;
        let content;
        braced!(content in input);
        let mut fns = vec![];
        while !content.is_empty() {
            fns.push(content.parse()?);
        }
        Ok(Self { name, fns })
    }
}

struct ManyOpaqueImpls {
    impls: Vec<OpaqueImpl>,
}

impl Parse for ManyOpaqueImpls {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut impls = vec![];
        while !input.is_empty() {
            impls.push(input.parse()?);
        }
        Ok(Self { impls })
    }
}

fn get_matched_variants(pats: Vec<Pat>) -> Vec<Ident> {
    let mut res = vec![];
    for pat in pats {
        match pat {
            Pat::Or(or) => res.extend(get_matched_variants(or.cases.into_iter().collect())),
            Pat::TupleStruct(str) => res.push(str.path.segments.last().unwrap().ident.clone()),
            Pat::Struct(str) => res.push(str.path.segments.last().unwrap().ident.clone()),
            _ => (),
        }
    }
    res
}

#[proc_macro_attribute]
pub fn impl_opaque(args: TokenStream, stream: TokenStream) -> TokenStream {
    let target = parse_macro_input!(stream as ItemEnum);
    let opaques = parse_macro_input!(args as ManyOpaqueImpls);
    let mut res = quote! {};
    for opaque in opaques.impls {
        let mut impl_res = quote! {};
        for fnc in opaque.fns {
            let sig = fnc.sig;
            let matched = get_matched_variants(fnc.matchers.iter().map(|(p, _, _)| p).cloned().collect());
            let mut matches = quote! {};
            for (pat, _, body) in fnc.matchers {
                matches.extend(quote! {
                    #pat => #body,
                });
            }
            for field in &target.variants {
                if matched.iter().any(|m| *m == field.ident) {
                    continue;
                }
                let field = &field.ident;
                let (ref param, _, ref body) = fnc.rest;
                matches.extend(quote! {
                    Self::#field(#param) => #body,
                });
            }
            impl_res.extend(quote! {
                #sig {
                    match self {
                        #matches
                    }
                }
            });
        }
        let trait_to_impl = opaque.name;
        let impl_to = &target.ident;
        res.extend(quote! {
            impl #trait_to_impl for #impl_to {
                #impl_res
            }
        });
    }
    quote! {
        #target
        #res
    }.into()
}
