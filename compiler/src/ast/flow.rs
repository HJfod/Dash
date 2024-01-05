
use dash_macros::{ParseNode, ResolveNode};
use crate::{
    parser::parse::{Separated, SeparatedWithTrailing, Node, NodePool},
    checker::{resolve::{ResolveNode, ResolveRef}, ty::Ty, coherency::Checker}, try_resolve_ref
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
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let cond = self.cond.try_resolve_ref(pool, checker)?;
        let truthy = self.truthy.try_resolve_ref(pool, checker)?;
        let falsy = try_resolve_ref!(self.falsy, (pool, checker), Some((_, e)) => e);
        checker.expect_ty_eq(cond, Ty::Bool, self.cond.get(pool).span(pool));
        checker.expect_ty_eq(truthy, falsy, self.span(pool)).into()
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "block or if statement")]
pub enum ElseNode {
    Else(delim::Braced<ExprList>),
    ElseIf(If),
}

#[derive(Debug, ParseNode)]
pub struct ReturnNode {
    return_kw: kw::Return,
    expr: Option<Expr>,
}

impl ResolveNode for ReturnNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, ParseNode)]
#[parse(expected = "identifier")]
enum UsingComponentNode {
    Multi(delim::Braced<SeparatedWithTrailing<UsingComponent, punct::Comma>>),
    Single(IdentComponent),
}

impl ResolveNode for UsingComponentNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug, ParseNode)]
struct UsingPathNode {
    absolute: Option<punct::Namespace>,
    path: Separated<UsingComponent, punct::Namespace>,
}

impl ResolveNode for UsingPathNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug, ParseNode)]
pub struct UsingNode {
    using_kw: kw::Using,
    path: UsingPath,
}

impl ResolveNode for UsingNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "control flow expression")]
pub enum FlowNode {
    If(If),
    Return(Return),
    Using(Using),
}
