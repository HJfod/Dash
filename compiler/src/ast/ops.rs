
use dash_macros::Parse;
use super::{expr::Expr, token::op};

#[derive(Parse)]
pub struct BinOp {
    lhs: Expr,
    op: op::BinOp,
    rhs: Expr,
}
