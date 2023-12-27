
use dash_macros::Parse;
use super::expr::{Expr, IdentPath, ExprList};
use crate::ast::token::delim;

#[derive(Parse)]
pub struct ClosedExpr {
    inner: delim::Parenthesized<Expr>,
}

#[derive(Parse)]
pub struct Block {
    exprs: delim::Braced<ExprList>,
}

#[derive(Parse)]
pub struct ItemUse {
    path: IdentPath,
}

#[derive(Parse)]
#[parse(expected = "expression")]
pub enum Atom {
    ClosedExpr(Box<ClosedExpr>),
    Block(Box<Block>),
    ItemUse(Box<ItemUse>),
}
