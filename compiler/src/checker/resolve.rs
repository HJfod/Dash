
use crate::parser::parse::{DontExpect, CompileMessage, Node, NodePool, ParseRef, Ref};
use super::{ty::Ty, coherency::Checker};

pub(crate) trait ResolveNode: Node {
    /// Try to resolve this AST node
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty>;
}

pub(crate) trait ResolveRef: Ref {
    /// This should not be called manually, except for starting AST resolution
    fn try_resolve_ref(&mut self, pool: &mut NodePool, checker: &mut Checker) -> Option<Ty>;

    /// Get the resolved type of this Ref's Node
    fn resolved_ty(&self, pool: &NodePool) -> Ty;
}

impl<T: ParseRef, M: CompileMessage> ResolveRef for DontExpect<T, M> {
    fn try_resolve_ref(&mut self, _: &mut NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}
