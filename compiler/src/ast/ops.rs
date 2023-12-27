
use dash_macros::Parse;
use super::{expr::Expr, token::op};

#[derive(Debug, Parse)]
pub struct BinOp {
    lhs: Expr,
    op: op::BinOp,
    rhs: Expr,
}
