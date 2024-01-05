
use dash_macros::{ParseNode, ResolveNode};
use crate::{
    parser::parse::{Separated, SeparatedWithTrailing, ParseNode, Node, NodePool},
    checker::{resolve::ResolveNode, ty::Ty, coherency::Checker}
};
use super::{token::{kw, delim, punct}, expr::{Expr, ExprList, IdentComponent}};

#[derive(Debug, ParseNode)]
pub struct IfNode {
    if_kw: kw::If,
    cond: Expr,
    truthy: delim::Braced<ExprList>,
    falsy: Option<(kw::Else, Else)>,
}

impl ResolveNode for IfNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
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

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "block or if statement")]
pub enum ElseNode {
    Else(delim::Braced<ExprList>),
    ElseIf(Box<If>),
}

#[derive(Debug, ParseNode)]
pub struct ReturnNode {
    return_kw: kw::Return,
    expr: Option<Expr>,
}

impl ResolveNode for ReturnNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, ParseNode)]
#[parse(expected = "identifier")]
enum UsingComponentNode {
    Multi(delim::Braced<SeparatedWithTrailing<UsingComponent, punct::Comma>>),
    Single(IdentComponent),
}

#[derive(Debug, ParseNode)]
struct UsingPathNode {
    absolute: Option<punct::Namespace>,
    path: Separated<UsingComponent, punct::Namespace>,
}

#[derive(Debug, ParseNode)]
pub struct UsingNode {
    using_kw: kw::Using,
    path: UsingPath,
}

impl ResolveNode for UsingNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "control flow expression")]
pub enum FlowNode {
    If(If),
    Return(Return),
    UsingNode(UsingNode),
}
