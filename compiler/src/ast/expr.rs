
use dash_macros::Parse;
use crate::parser::parse::Separated;
use super::{decl::Decl, token::{Ident, punct}, atom::Atom};

#[derive(Parse)]
pub struct IdentPath {
    absolute: Option<punct::Namespace>,
    path: Separated<Ident, punct::Namespace>,
}

#[derive(Parse)]
#[parse(expected = "expression")]
pub enum Expr {
    Decl(Box<Decl>),
    Atom(Box<Atom>),
}

#[derive(Parse)]
pub struct ExprList {
    exprs: Vec<(Expr, Vec<punct::Semicolon>)>,
}
