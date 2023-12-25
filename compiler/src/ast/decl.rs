
use super::{token::{kw, Ident, op}, ty::TypeExpr, expr::Expr};
use dash_macros::node;

#[node]
pub struct LetDecl {
    let_kw: kw::Let,
    name: Ident,
    ty: Option<(op::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}
