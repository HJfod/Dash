
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate convert_case;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, format_ident};
use syn::{parse_macro_input, Block, ItemFn, ItemStruct, Fields, Field, Error, ItemEnum, Expr, parse::Parse, Token, braced, Signature, Type};
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

            fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode> {
                std::iter::empty()
                    #iter_chains
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
    param: Ident,
    body: Expr,
}

impl Parse for OpaqueFnImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let sig = input.parse()?;
        input.parse::<Token![:]>()?;
        let param = input.parse()?;
        input.parse::<Token![=>]>()?;
        let body = input.parse()?;
        Ok(Self { sig, param, body })
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
        Ok(Self {
            name,
            fns: content.parse_terminated(OpaqueFnImpl::parse, Token![;])?
                .into_iter()
                .collect()
        })
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

#[proc_macro_attribute]
pub fn impl_opaque(args: TokenStream, stream: TokenStream) -> TokenStream {
    let target = parse_macro_input!(stream as ItemEnum);
    let opaques = parse_macro_input!(args as ManyOpaqueImpls);
    let mut res = quote! {};
    for opaque in opaques.impls {
        let mut impl_res = quote! {};
        for fnc in opaque.fns {
            let sig = fnc.sig;
            let param = fnc.param;
            let body = fnc.body;
            let mut matches = quote! {};
            for field in &target.variants {
                let field = &field.ident;
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
