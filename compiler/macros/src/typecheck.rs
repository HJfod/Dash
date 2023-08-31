
use std::fmt::Display;
use proc_macro2::TokenStream as TokenStream2;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use syn::{Token, Error, parenthesized, Expr};
use syn::{Result, Path, braced};

use crate::defs::kw;

pub enum TypeOrIdent {
    Type(Token![type]),
    Ident(Ident),
}

impl Display for TypeOrIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(_) => f.write_str("type"),
            Self::Ident(i) => f.write_str(&i.to_string()),
        }
    }
}

impl TypeOrIdent {
    pub fn span(&self) -> Span {
        match self {
            Self::Type(t) => t.span,
            Self::Ident(i) => i.span(),
        }
    }
}

impl Parse for TypeOrIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(if input.peek(Token![type]) {
            Self::Type(input.parse()?)
        }
        else {
            Self::Ident(input.parse()?)
        })
    }
}

pub enum FindKind {
    Type,
    Entity,
}

pub enum EntityKind {
    Var,
    Fun,
    Type,
}

pub enum TypeClause {
    // A -> B
    Convertible(Box<TypeClause>, Box<TypeClause>),
    // find A as b
    Find(Ident, FindKind),
    // new kind(...)
    NewEntity(Span, EntityKind, Vec<Expr>),
    // type::name
    Type(Path),
}

impl Parse for TypeClause {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::find>().is_ok() {
            let name = input.parse()?;
            input.parse::<Token![as]>()?;
            let kind = input.parse::<TypeOrIdent>()?;
            Ok(Self::Find(name, match kind.to_string().as_str() {
                "type" => FindKind::Type,
                "entity" => FindKind::Entity,
                _ => Err(Error::new(kind.span(), "invalid find kind"))?
            }))
        }
        else if input.parse::<kw::new>().is_ok() {
            let kind = input.parse::<TypeOrIdent>()?;
            let params;
            parenthesized!(params in input);
            Ok(Self::NewEntity(
                kind.span(),
                match kind.to_string().as_str() {
                    "type" => EntityKind::Type,
                    "fun" => EntityKind::Fun,
                    "var" => EntityKind::Var,
                    _ => Err(Error::new(kind.span(), "invalid entity kind"))?
                },
                params.parse_terminated(Expr::parse, Token![,])?.into_iter().collect()
            ))
        }
        else {
            let a = Self::Type(input.parse()?);
            if input.parse::<Token![->]>().is_ok() {
                Ok(Self::Convertible(a.into(), input.parse()?))
            }
            else {
                Ok(a)
            }
        }
    }
}

pub struct TypeCtx {
    pub members: Vec<Ident>,
}

impl TypeClause {
    pub fn gen_with_ctx(&self, ctx: &TypeCtx) -> Result<TokenStream2> {
        match self {
            Self::Convertible(a, b) => {
                let a = a.gen_with_ctx(ctx)?;
                let b = b.gen_with_ctx(ctx)?;
                Ok(quote! { {
                    let a = &#a;
                    let b = &#b;
                    if !a.convertible_to(b) {
                        checker.emit_msg(&Message::from_meta(
                            Level::Error,
                            format!("Type '{a}' is not convertible to '{b}'"),
                            self.meta(),
                        ));
                    }
                    b
                } })
            }
            Self::NewEntity(_, kind, args) => {
                let mut args_stream = quote! {};
                for arg in args {
                    args_stream.extend(quote! { #arg, });
                }
                Ok(match kind {
                    EntityKind::Fun => quote! {
                        checker.push(compiler::Entity::new_fun(#args_stream)).ty()
                    },
                    EntityKind::Var => quote! {
                        checker.push(compiler::Entity::new_var(#args_stream)).ty()
                    },
                    EntityKind::Type => quote! {
                        checker.push(compiler::Ty::new(#args_stream)).clone()
                    },
                })
            }
            Self::Find(name, kind) => {
                match kind {
                    FindKind::Entity => {
                        Ok(quote! {
                            match checker.find::<compiler::Entity>(self.#name.path()) {
                                Some(e) => e.ty(),
                                None => {
                                    checker.emit_msg(&Message::from_meta(
                                        Level::Error,
                                        format!("Unknown entity '{}'", self.#name.path()),
                                        self.#name.meta(),
                                    ));
                                    Ty::Invalid
                                }
                            }
                        })
                    }
                    FindKind::Type => {
                        Ok(quote! {
                            match checker.find::<Ty>(self.#name.path()) {
                                Some(e) => e.clone(),
                                None => {
                                    checker.emit_msg(&Message::from_meta(
                                        Level::Error,
                                        format!("Unknown type '{}'", self.#name.path()),
                                        self.#name.meta(),
                                    ));
                                    Ty::Invalid
                                }
                            }
                        })
                    }
                }
            }
            Self::Type(path) => {
                Ok(quote! {
                    #path
                })
            }
        }
    }
}

pub struct TypeCheck {
    new_scope: bool,
    clauses: Vec<TypeClause>,
    return_clause: TypeClause,
}

impl Parse for TypeCheck {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::typecheck>()?;
        let new_scope = if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            content.parse::<kw::new>()?;
            content.parse::<kw::scope>()?;
            true
        }
        else {
            false
        };
        let content;
        braced!(content in input);
        let mut clauses = vec![];
        while let Ok(clause) = content.parse::<TypeClause>() {
            clauses.push(clause);
            content.parse::<Token![;]>()?;
        }
        content.parse::<Token![yield]>()?;
        let return_clause = content.parse()?;
        content.parse::<Token![;]>()?;
        Ok(Self { clauses, return_clause, new_scope })
    }
}

impl TypeCheck {
    pub fn gen_with_ctx(&self, ctx: &TypeCtx) -> Result<TokenStream2> {
        let mut stream = quote! {};
        for mem in &ctx.members {
            stream.extend(quote! {
                let #mem = self.#mem.typecheck(checker);
            });
        }
        for clause in &self.clauses {
            stream.extend(clause.gen_with_ctx(ctx)?);
            stream.extend(quote!{ ; });
        }
        let ret = self.return_clause.gen_with_ctx(ctx)?;
        stream.extend(quote! {
            let _gdml_type_ret = #ret;
        });
        if self.new_scope {
            stream = quote! {
                checker.push_scope();
                #stream
                checker.pop_scope();
            };
        }
        stream.extend(quote! { _gdml_type_ret });
        Ok(stream)
    }
}
