
use dash_macros::Parse;
use super::expr::IdentPath;

#[derive(Debug, Parse)]
#[parse(expected = "type")]
pub enum TypeExpr {
    TypeIdent(Box<TypeIdent>),
}

#[derive(Debug, Parse)]
pub struct TypeIdent {
    name: IdentPath,
}
