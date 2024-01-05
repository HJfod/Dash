
use crate::parser::parse::{Node, NodePool, Ref};
use super::{ty::Ty, coherency::Checker};

pub trait ResolveNode: Node {
    /// Try to resolve this AST node
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty>;
}

pub trait ResolveRef: Ref {
    /// This should not be called manually, except for starting AST resolution
    fn try_resolve_ref(&self, pool: &NodePool, checker: &mut Checker) -> Option<Ty>;

    /// Get the resolved type of this Ref's Node
    fn resolved_ty(&self, pool: &NodePool) -> Ty;
}
