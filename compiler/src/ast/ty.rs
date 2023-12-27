
use dash_macros::Parse;
use super::expr::IdentPath;

#[derive(Parse)]
#[parse(expected = "type")]
pub enum TypeExpr {
    TypeIdent(Box<TypeIdent>),
}

#[derive(Parse)]
pub struct TypeIdent {
    name: IdentPath,
}
