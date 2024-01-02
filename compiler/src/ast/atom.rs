
use dash_macros::{Parse, Resolve};
use super::{expr::{Expr, IdentPath, ExprList}, token::{lit, kw}};
use crate::{
    ast::token::delim,
    checker::{resolve::{Resolve, ResolveCache}, coherency::Checker, ty::Ty, path}, parser::parse::Parse
};

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum ItemUse {
    This(
        kw::This,
        #[parse(skip)]
        ResolveCache
    ),
    Ident(
        IdentPath,
        #[parse(skip)]
        ResolveCache
    ),
}

impl Resolve for ItemUse {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ent) = scope.entities().find(
                &match self {
                    Self::Ident(i, _) => i.to_path(),
                    Self::This(_, _) => path::IdentPath::new([path::Ident::from("this")], false)
                }
            ) {
                return Some(ent.ty());
            }
        }
        let msg_span = match self {
            Self::Ident(i, _) => (format!("Unknown item {}", i.to_path()), i.span()),
            Self::This(kw, _) => ("'this' is not valid in this scope".into(), kw.span()),
        };
        self.cache().unwrap().set_unresolved(msg_span.0, msg_span.1);
        None
    }

    fn cache(&mut self) -> Option<&mut ResolveCache> {
        match self {
            Self::Ident(_, c) | Self::This(_, c) => Some(c),
        }
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "expression")]
pub enum Atom {
    ClosedExpr(delim::Parenthesized<Expr>),
    Block(delim::Braced<ExprList>),
    ItemUse(Box<ItemUse>),
    String(lit::String),
    Float(lit::Float),
    Int(lit::Int),
    Bool(lit::Bool),
    Void(lit::Void),
}
