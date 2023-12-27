
use dash_macros::Parse;
use super::expr::{Expr, IdentPath, ExprList};
use crate::ast::token::delim;

#[derive(Debug, Parse)]
pub struct ClosedExpr {
    inner: delim::Parenthesized<Expr>,
}

#[derive(Debug, Parse)]
pub struct Block {
    exprs: delim::Braced<ExprList>,
}

#[derive(Debug, Parse)]
pub struct ItemUse {
    path: IdentPath,
}

#[derive(Debug, Parse)]
#[parse(expected = "expression")]
pub enum Atom {
    ClosedExpr(Box<ClosedExpr>),
    Block(Box<Block>),
    ItemUse(Box<ItemUse>),
}
