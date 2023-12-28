
use dash_macros::Parse;
use super::{expr::{Expr, IdentPath, ExprList}, token::lit};
use crate::ast::token::delim;

#[derive(Debug, Parse)]
pub struct ItemUse {
    path: IdentPath,
}

#[derive(Debug, Parse)]
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
