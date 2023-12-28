
use dash_macros::Parse;
use crate::parser::parse::{Separated, SeparatedWithTrailing};
use super::{token::{kw, delim, punct}, expr::{Expr, ExprList, IdentPath, IdentComponent}};

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
    return_kw: kw::Return,
    expr: Option<Expr>,
}

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum UsingComponent {
    Multi(delim::Braced<SeparatedWithTrailing<UsingComponent, punct::Comma>>),
    Single(IdentComponent),
}

#[derive(Debug, Parse)]
pub struct UsingPath {
    absolute: Option<punct::Namespace>,
    path: Separated<UsingComponent, punct::Namespace>,
}

#[derive(Debug, Parse)]
pub struct UsingItem {
    using_kw: kw::Using,
    path: UsingPath,
}

#[derive(Debug, Parse)]
#[parse(expected = "control flow expression")]
pub enum Flow {
    If(If),
    Return(Return),
    UsingItem(UsingItem),
}
