
use crate::{parser::parse::{SeparatedWithTrailing, DontExpect}, add_compile_message};

use super::{token::{kw, op, punct, delim}, ty::TypeExpr, expr::{Expr, IdentPath, ExprList}};
use dash_macros::Parse;

#[derive(Debug, Parse)]
pub struct LetDecl {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}

#[derive(Debug, Parse)]
pub struct NamedParam {
    name: IdentPath,
    ty: (punct::Colon, TypeExpr),
    default_value: Option<(op::Seq, Expr)>,
}

// mfw no &'static str in const generics 😢
add_compile_message!(ThisParamMayNotHaveValue: "the 'this' parameter may not have a default value");

#[derive(Debug, Parse)]
pub struct ThisParam {
    this_kw: kw::This,
    ty: Option<(punct::Colon, TypeExpr)>,
    _invalid_value: DontExpect<(op::Seq, Expr), ThisParamMayNotHaveValue>,
}

#[derive(Debug, Parse)]
#[parse(expected = "parameter")]
pub enum FunParam {
    NamedParam(NamedParam),
    ThisParam(ThisParam),
}

#[derive(Debug, Parse)]
pub struct FunDecl {
    fun_kw: kw::Fun,
    name: Option<IdentPath>,
    params: delim::Parenthesized<SeparatedWithTrailing<FunParam, punct::Comma>>,
    ret_ty: Option<(punct::Arrow, TypeExpr)>,
    body: delim::Braced<ExprList>,
}

#[derive(Debug, Parse)]
#[parse(expected = "item declaration")]
pub enum Decl {
    LetDecl(Box<LetDecl>),
    FunDecl(Box<FunDecl>),
}

