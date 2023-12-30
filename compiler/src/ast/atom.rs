
use dash_macros::{Parse, Resolve};
use super::{expr::{Expr, IdentPath, ExprList}, token::{lit, kw}};
use crate::{ast::token::delim, checker::{resolve::{Resolve, Unresolved}, coherency::Checker, ty::Ty}, shared::logger::Message};

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum ItemUse {
    This(kw::This),
    Ident(IdentPath),
}

impl Resolve for ItemUse {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

impl Unresolved for ItemUse {
    fn unresolved_due_to(&self) -> Message {
        todo!()
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
