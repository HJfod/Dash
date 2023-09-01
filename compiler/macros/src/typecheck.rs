
use std::collections::HashSet;
use std::fmt::Display;
use proc_macro2::TokenStream as TokenStream2;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::token::{Paren, Brace};
use syn::{Token, Error, parenthesized, Expr, ExprBlock};
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

pub enum SpaceKind {
    Type,
    Entity,
}

pub enum TypeClause {
    // A -> B
    Convertible(Box<TypeClause>, Box<TypeClause>),
    // find A as b
    Find(Ident, SpaceKind),
    // new kind name?: ty
    NewEntity(SpaceKind, Ident, bool, Expr),
    // type::name
    Type(Path),
    // { ... }
    Code(ExprBlock),
    // scope { ... }
    Scope(Span, Vec<TypeClause>),
    // check member
    Check(Ident),
    // eval clause
    Eval(Box<TypeClause>),
}

impl Parse for TypeClause {
    fn parse(input: ParseStream) -> Result<Self> {
        let a = if input.parse::<kw::find>().is_ok() {
            let name = input.parse()?;
            input.parse::<Token![as]>()?;
            let kind = input.parse::<TypeOrIdent>()?;
            Self::Find(name, match kind.to_string().as_str() {
                "type" => SpaceKind::Type,
                "entity" => SpaceKind::Entity,
                _ => Err(Error::new(kind.span(), "invalid kind"))?
            })
        }
        else if input.parse::<kw::new>().is_ok() {
            let kind = input.parse::<TypeOrIdent>()?;
            let name = input.parse()?;
            let optional = input.parse::<Token![?]>().is_ok();
            input.parse::<Token![:]>()?;
            Self::NewEntity(
                match kind.to_string().as_str() {
                    "type" => SpaceKind::Type,
                    "entity" => SpaceKind::Entity,
                    _ => Err(Error::new(kind.span(), "invalid kind"))?
                },
                name, optional,
                input.parse()?
            )
        }
        else if input.parse::<kw::check>().is_ok() {
            Self::Check(input.parse()?)
        }
        else if input.parse::<kw::eval>().is_ok() {
            Self::Eval(input.parse()?)
        }
        else if let Ok(scope) = input.parse::<kw::scope>() {
            let c;
            braced!(c in input);
            Self::Scope(scope.span, c.parse_terminated(TypeClause::parse, Token![;])?.into_iter().collect())
        }
        else if input.peek(Brace) {
            Self::Code(input.parse()?)
        }
        else {
            Self::Type(input.parse()?)
        };
        if input.parse::<Token![->]>().is_ok() {
            Ok(Self::Convertible(a.into(), input.parse()?))
        }
        else {
            Ok(a)
        }
    }
}

pub struct TypeCtx {
    pub members: Vec<Ident>,
}

impl TypeClause {
    pub fn gen_with_ctx(&self, ctx: &TypeCtx, manual: bool, checked: &mut HashSet<String>) -> Result<TokenStream2> {
        match self {
            Self::Convertible(a, b) => {
                let a = a.gen_with_ctx(ctx, manual, checked)?;
                let b = b.gen_with_ctx(ctx, manual, checked)?;
                Ok(quote! { {
                    let a = #a.to_type();
                    let b = #b.to_type();
                    if !a.convertible_to(&b) {
                        checker.emit_msg(&Message::from_meta(
                            Level::Error,
                            format!("Type '{a}' is not convertible to '{b}'"),
                            self.meta(),
                        ));
                    }
                    b
                } })
            }
            Self::NewEntity(kind, name, optional, ty_expr) => {
                let find_ty;
                let found_fmt_string;
                let push = match kind {
                    SpaceKind::Entity => {
                        find_ty = quote! { compiler::Entity };
                        found_fmt_string = quote! { "Entity '{}' already exists in this scope" };
                        quote! {
                            checker.push(compiler::Entity::new(path, #ty_expr)).ty()
                        }
                    }
                    SpaceKind::Type => {
                        find_ty = quote! { Ty };
                        found_fmt_string = quote! { "Type '{}' already exists in this scope" };
                        quote! {
                            checker.push(compiler::Ty::new(path, #ty_expr)).clone()
                        }
                    }
                };
                let find_or_push = quote! {
                    let path = checker.resolve_new(name.path());
                    match checker.last_space::<#find_ty>().find(&path) {
                        None => {
                            #push
                        }
                        Some(e) => {
                            checker.emit_msg(&Message::from_meta(
                                Level::Error,
                                format!(#found_fmt_string, name.path()),
                                name.meta(),
                            ));
                            Ty::Invalid
                        }
                    }
                };
                if *optional {
                    Ok(quote! { {
                        if let Some(ref name) = self.#name {
                            #find_or_push
                        }
                        else {
                            #ty_expr
                        }
                    } })
                }
                else {
                    Ok(quote! { {                    
                        let name = &self.#name;
                        #find_or_push
                    } })
                }
            }
            Self::Find(name, kind) => {
                let find_ty;
                let notfound_fmt_string;
                let res_ty;
                match kind {
                    SpaceKind::Entity => {
                        find_ty = quote! { compiler::Entity };
                        notfound_fmt_string = quote! { "Unknown entity '{}'" };
                        res_ty = quote! { e.ty() };
                    }
                    SpaceKind::Type => {
                        find_ty = quote! { Ty };
                        notfound_fmt_string = quote! { "Unknown type '{}'" };
                        res_ty = quote! { e.clone() };
                    }
                }
                Ok(quote! { {
                    let path = self.#name.path();
                    match checker.find::<#find_ty, _>(&path) {
                        Some(e) => #res_ty,
                        None => {
                            checker.emit_msg(&Message::from_meta(
                                Level::Error,
                                format!(#notfound_fmt_string, path),
                                self.#name.meta(),
                            ));
                            Ty::Invalid
                        }
                    }
                } })
            }
            Self::Type(path) => {
                Ok(quote! {
                    #path
                })
            }
            Self::Code(expr) => {
                Ok(quote! {
                    #expr
                })
            }
            Self::Scope(span, clauses) => {
                if !manual {
                    Err(Error::new(span.clone(), "cannot have scope blocks in non-manual typecheck"))?;
                }
                let mut stream = quote! {};
                for clause in clauses {
                    stream.extend(clause.gen_with_ctx(ctx, manual, checked)?);
                }
                Ok(quote! {
                    checker.push_scope();
                    #stream;
                    checker.pop_scope();
                })
            }
            Self::Check(mem) => {
                if !manual {
                    Err(Error::new(mem.span(), "cannot have manual checks in non-manual typecheck"))?;
                }
                if !checked.contains(&mem.to_string()) {
                    Err(Error::new(mem.span(), "member doesn't exist or has already been checked"))?;
                }
                checked.remove(&mem.to_string());
                Ok(quote! {
                    let #mem = self.#mem.typecheck_helper(checker);
                })
            }
            Self::Eval(clause) => {
                let body = clause.gen_with_ctx(ctx, manual, checked)?;
                Ok(quote! {
                    #body.return_ty()
                })
            }
        }
    }
}

pub struct TypeCheck {
    name_span: Span,
    manual_members: bool,
    clauses: Vec<TypeClause>,
    return_clause: TypeClause,
}

impl Parse for TypeCheck {
    fn parse(input: ParseStream) -> Result<Self> {
        let name_span = input.parse::<kw::typecheck>()?.span;
        let manual_members = if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            content.parse::<kw::manual>()?;
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
        Ok(Self { name_span, clauses, return_clause, manual_members })
    }
}

impl TypeCheck {
    pub fn gen_with_ctx(&self, ctx: &TypeCtx) -> Result<TokenStream2> {
        let mut stream = quote! {};
        let mut checked = HashSet::new();
        for mem in &ctx.members {
            if !self.manual_members {
                stream.extend(quote! {
                    let #mem = self.#mem.typecheck_helper(checker);
                });
            }
            checked.insert(mem.to_string());
        }
        for clause in &self.clauses {
            stream.extend(clause.gen_with_ctx(ctx, self.manual_members, &mut checked)?);
            stream.extend(quote!{ ; });
        }
        let ret = self.return_clause.gen_with_ctx(ctx, self.manual_members, &mut checked)?;
        if !checked.is_empty() && self.manual_members {
            Err(Error::new(self.name_span.clone(), format!(
                "not all members checked, missing: {}",
                checked.into_iter().collect::<Vec<_>>().join(", ")
            )))?;
        }
        Ok(quote! {
            #stream;
            #ret
        })
    }
}
