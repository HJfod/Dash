
use dash_macros::{Parse, Resolve};
use super::{expr::{Expr, IdentPath, ExprList}, token::{lit, kw}};
use crate::{
    ast::token::delim,
    checker::{resolve::Resolve, coherency::Checker, ty::Ty, path}
};
use crate::parser::parse::Parse;

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum ItemUse {
    This(kw::This),
    Ident(IdentPath),
}

impl Resolve for ItemUse {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ent) = scope.entities().find(
                &match self {
                    Self::Ident(i) => i.to_path(),
                    Self::This(_) => path::IdentPath::new([path::Ident::from("this")], false)
                },
                checker.namespace_stack()
            ) {
                return Some(ent.ty());
            }
        }
        match self {
            Self::Ident(i) => checker.push_unresolved(
                format!("Unknown item {}", i.to_path()), i.span()
            ),
            Self::This(kw) => checker.push_unresolved(
                "'this' is not valid in this scope", kw.span()
            ),
        }
        None
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
