
use dash_macros::{Parse, Resolve};
use crate::{
    parser::parse::{Separated, SeparatedWithTrailing, Parse, Node, NodeList},
    checker::{resolve::Resolve, ty::Ty, coherency::Checker}
};
use super::{token::{kw, delim, punct}, expr::{Expr, ExprList, IdentComponent}};

#[derive(Debug, Parse)]
pub struct IfItem {
    if_kw: kw::If,
    cond: Expr,
    truthy: delim::Braced<ExprList>,
    falsy: Option<(kw::Else, Else)>,
}

impl Resolve for IfItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        let cond = self.cond.try_resolve(list, checker)?;
        let truthy = self.truthy.try_resolve(list, checker)?;
        let falsy = if let Some((_, e)) = &mut self.falsy {
            e.try_resolve(list, checker)?
        }
        else {
            Ty::Invalid
        };
        checker.expect_ty_eq(cond, Ty::Bool, self.cond.get(list).as_ref().span(list));
        checker.expect_ty_eq(truthy, falsy, self.span(list)).into()
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "block or if statement")]
pub enum ElseItem {
    Else(delim::Braced<ExprList>),
    ElseIf(Box<If>),
}

#[derive(Debug, Parse)]
pub struct ReturnItem {
    return_kw: kw::Return,
    expr: Option<Expr>,
}

impl Resolve for ReturnItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
enum UsingComponentItem {
    Multi(delim::Braced<SeparatedWithTrailing<UsingComponent, punct::Comma>>),
    Single(IdentComponent),
}

#[derive(Debug, Parse)]
struct UsingPathItem {
    absolute: Option<punct::Namespace>,
    path: Separated<UsingComponent, punct::Namespace>,
}

#[derive(Debug, Parse)]
pub struct UsingItem {
    using_kw: kw::Using,
    path: UsingPath,
}

impl Resolve for UsingItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "control flow expression")]
pub enum FlowItem {
    If(If),
    Return(Return),
    UsingItem(UsingItem),
}
