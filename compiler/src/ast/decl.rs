
use super::{token::{kw, op, punct}, ty::TypeExpr, expr::{Expr, IdentPath}};
use dash_macros::Parse;

#[derive(Parse)]
pub struct LetDecl {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}

#[derive(Parse)]
#[parse(expected = "item declaration")]
pub enum Decl {
    LetDecl,
}

