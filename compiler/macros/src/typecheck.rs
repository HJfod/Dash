
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

pub enum NewEntityKind {
    Type,
    // mut
    Entity(bool),
}

pub enum ScopeKind {
    Opaque,
    Function,
}

impl ScopeKind {
    pub fn parse(ident: &Ident) -> std::result::Result<Self, Error> {
        match ident.to_string().as_str() {
            "opaque" => Ok(Self::Opaque),
            "function" => Ok(Self::Function),
            _ => Err(Error::new(ident.span(), "invalid scope kind"))?
        }
    }
}

pub enum TypeClause {
    // A -> B for a
    Convertible(Box<TypeClause>, Box<TypeClause>, Option<Box<Ident>>),
    // find A as b
    Find(Ident, SpaceKind),
    // new kind name?: ty
    NewEntity(NewEntityKind, Ident, bool, Expr),
    // type::name
    Type(Path),
    // { ... }
    Code(ExprBlock),
    // scope function { ... }
    Scope {
        span: Span,
        clauses: Vec<TypeClause>,
        return_ty: Option<Box<TypeClause>>,
        default_yield: Option<Box<TypeClause>>,
        kind: ScopeKind
    },
    // check member
    Check(Ident),
    // eval clause
    Eval(Box<TypeClause>),
    // return A from kind
    ReturnFromScope(Box<TypeClause>, ScopeKind),
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
            let k = input.parse::<TypeOrIdent>()?;
            let kind = match k.to_string().as_str() {
                "type" => NewEntityKind::Type,
                "entity" => NewEntityKind::Entity(input.parse::<Token![mut]>().is_ok()),
                _ => Err(Error::new(k.span(), "invalid kind"))?
            };
            let name = input.parse()?;
            let optional = input.parse::<Token![?]>().is_ok();
            input.parse::<Token![:]>()?;
            Self::NewEntity(kind, name, optional, input.parse()?)
        }
        else if input.parse::<kw::check>().is_ok() {
            Self::Check(input.parse()?)
        }
        else if input.parse::<kw::eval>().is_ok() {
            Self::Eval(input.parse()?)
        }
        else if input.parse::<Token![return]>().is_ok() {
            let name = input.parse()?;
            input.parse::<kw::from>()?;
            let kind = input.parse::<Ident>()?;
            Self::ReturnFromScope(name, ScopeKind::parse(&kind)?)
        }
        else if let Ok(scope) = input.parse::<kw::scope>() {
            let kind = if let Ok(ident) = input.parse::<Ident>() {
                ScopeKind::parse(&ident)?
            }
            else {
                ScopeKind::Opaque
            };
            let return_ty = if input.parse::<Token![->]>().is_ok() {
                Some(input.parse()?)
            }
            else {
                None
            };
            let c;
            braced!(c in input);
            let mut clauses = vec![];
            let mut default_yield = None;
            while !c.is_empty() {
                if c.parse::<Token![yield]>().is_ok() {
                    c.parse::<kw::default>()?;
                    default_yield = Some(c.parse()?);
                    c.parse::<Token![;]>()?;
                    break;
                }
                clauses.push(c.parse()?);
                c.parse::<Token![;]>()?;
            }
            Self::Scope {
                span: scope.span,
                clauses,
                return_ty,
                default_yield,
                kind
            }
        }
        else if input.peek(Brace) {
            Self::Code(input.parse()?)
        }
        else {
            Self::Type(input.parse()?)
        };
        if input.parse::<Token![->]>().is_ok() {
            let b = input.parse()?;
            let span = if input.parse::<Token![for]>().is_ok() {
                Some(input.parse()?)
            }
            else {
                None
            };
            Ok(Self::Convertible(a.into(), b, span))
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
            Self::Convertible(a, b, span) => {
                let a = a.gen_with_ctx(ctx, manual, checked)?;
                let b = b.gen_with_ctx(ctx, manual, checked)?;
                let span = span.as_ref().map(|s| quote! { self.#s }).unwrap_or(quote! { self });
                Ok(quote! { {
                    let a = #a.to_type();
                    let b = #b.to_type();
                    if !a.convertible_to(&b) {
                        checker.emit_msg(Message::from_meta(
                            Level::Error,
                            format!("Type '{a}' is not convertible to '{b}'"),
                            #span.meta(),
                        ));
                    }
                    b.or(a)
                } })
            }
            Self::NewEntity(kind, name, optional, ty_expr) => {
                let find_ty;
                let found_fmt_string;
                let push = match kind {
                    NewEntityKind::Entity(mutable) => {
                        find_ty = quote! { compiler::Entity };
                        found_fmt_string = quote! { "Entity '{}' already exists in this scope" };
                        quote! {
                            checker.push(compiler::Entity::new(
                                path, self.into(), #ty_expr, #mutable
                            )).ty()
                        }
                    }
                    NewEntityKind::Type => {
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
                            checker.emit_msg(Message::from_meta(
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
                let notaccessible_fmt_string;
                let res_ty;
                match kind {
                    SpaceKind::Entity => {
                        find_ty = quote! { compiler::Entity };
                        notfound_fmt_string = quote! { "Unknown entity '{path}'" };
                        notaccessible_fmt_string = quote! { "Entity '{path}' can not be used here" };
                        res_ty = quote! { e.ty() };
                    }
                    SpaceKind::Type => {
                        find_ty = quote! { Ty };
                        notfound_fmt_string = quote! { "Unknown type '{path}'" };
                        notaccessible_fmt_string = quote! { "Type '{path}' can not be used here" };
                        res_ty = quote! { e.clone() };
                    }
                }
                Ok(quote! { {
                    let path = self.#name.path();
                    match checker.find::<#find_ty, _>(&path) {
                        FindItem::Some(e) => #res_ty,
                        FindItem::NotAvailable(e) => {
                            checker.emit_msg(Message::from_meta(
                                Level::Error,
                                format!(#notaccessible_fmt_string),
                                self.#name.meta(),
                            ).note(Note::new_at(
                                format!("'{path}' declared here"),
                                e.decl().meta().src, e.decl().meta().range.clone()
                            )));
                            Ty::Invalid
                        }
                        FindItem::None => {
                            checker.emit_msg(Message::from_meta(
                                Level::Error,
                                format!(#notfound_fmt_string),
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
            Self::Scope { span, clauses, return_ty, default_yield, kind } => {
                if !manual {
                    Err(Error::new(span.clone(), "cannot have scope blocks in non-manual typecheck"))?;
                }
                if default_yield.is_none() {
                    Err(Error::new(span.clone(), "scope doesn't yield a default value"))?;
                }
                let mut stream = quote! {};
                for clause in clauses {
                    let gen = clause.gen_with_ctx(ctx, manual, checked)?;
                    stream.extend(quote! { #gen; });
                }
                let level = match kind {
                    ScopeKind::Function => quote! { Function },
                    ScopeKind::Opaque => quote! { Opaque },
                };
                let return_ty = match return_ty {
                    Some(ty) => {
                        let gen = ty.gen_with_ctx(ctx, manual, checked)?;
                        quote! { Some(#gen.clone()) }
                    }
                    None => quote! { None }
                };
                let default_yield = default_yield.as_ref().unwrap().gen_with_ctx(ctx, manual, checked)?;
                Ok(quote! { {
                    checker.push_scope(
                        crate::compiler::ScopeLevel::#level,
                        self.as_ref(), #return_ty
                    );
                    #stream;
                    checker.pop_scope(
                        #default_yield.to_type(),
                        self.as_ref()
                    )
                } })
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
                    #mem = self.#mem.typecheck_helper(checker);
                })
            }
            Self::Eval(clause) => {
                let body = clause.gen_with_ctx(ctx, manual, checked)?;
                Ok(quote! {
                    #body.return_ty()
                })
            }
            Self::ReturnFromScope(clause, kind) => {
                let ty = clause.gen_with_ctx(ctx, manual, checked)?;
                let level = match kind {
                    ScopeKind::Function => quote! { Function },
                    ScopeKind::Opaque => quote! { Opaque },
                };
                Ok(quote! { {
                    let ty = #ty.to_type();
                    checker.infer_return_type(
                        crate::compiler::FindScope::ByLevel(
                            crate::compiler::ScopeLevel::#level
                        ), ty, self.as_ref()
                    );
                    Ty::Never
                } })
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
            if self.manual_members {
                stream.extend(quote! {
                    let #mem;
                });
            }
            else {
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
