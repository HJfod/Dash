
use dash_macros::group;
use super::decl::LetDecl;

#[group(expected = "expression")]
pub enum Expr {
    LetDecl,
}
