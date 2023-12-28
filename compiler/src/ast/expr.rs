
use dash_macros::Parse;
use crate::parser::parse::Separated;
use super::{decl::Decl, token::{Ident, punct::{self, TerminatingSemicolon}}, atom::Atom};

#[derive(Debug, Parse)]
pub struct IdentPath {
    absolute: Option<punct::Namespace>,
    path: Separated<Ident, punct::Namespace>,
}

#[derive(Debug, Parse)]
#[parse(expected = "expression")]
pub enum Expr {
    Decl(Box<Decl>),
    Atom(Box<Atom>),
}

#[derive(Debug, Parse)]
pub struct ExprList {
    exprs: Vec<(Expr, TerminatingSemicolon)>,
}
