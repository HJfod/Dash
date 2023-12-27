
use super::{token::{kw, op, punct}, ty::TypeExpr, expr::{Expr, IdentPath}};
use dash_macros::Parse;

#[derive(Debug, Parse)]
pub struct LetDecl {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}

#[derive(Debug, Parse)]
#[parse(expected = "item declaration")]
pub enum Decl {
    LetDecl(Box<LetDecl>),
}

