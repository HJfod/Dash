
use dash_macros::Parse;
use super::{token::{kw, delim}, expr::{Expr, ExprList}};

#[derive(Debug, Parse)]
pub struct If {
    if_kw: kw::If,
    cond: Expr,
    truthy: delim::Braced<ExprList>,
    falsy: Option<(kw::Else, Else)>
}

#[derive(Debug, Parse)]
#[parse(expected = "block or if statement")]
pub enum Else {
    Else(delim::Braced<ExprList>),
    ElseIf(Box<If>),
}

#[derive(Debug, Parse)]
pub struct Return {
    return_rw: kw::Return,
    expr: Option<Expr>,
}
