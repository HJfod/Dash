
use crate::{parser::parse::{Node, NodePool, Ref}, shared::logger::LoggerRef};
use super::{ty::Ty, coherency::Checker};

pub trait ResolveNode: Node {
    /// Try to resolve this AST node
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty>;

    /// If this Node was left unresolved, provide a reason
    #[allow(unused)]
    fn log_unresolved_reason(&self, pool: &NodePool, checker: &Checker, logger: LoggerRef) {
        // This node was not the reason compilation failed
    }
}

pub trait ResolveRef: Ref {
    /// This should not be called manually, except for starting AST resolution
    fn try_resolve_ref(&self, pool: &NodePool, checker: &mut Checker) -> Option<Ty>;

    /// Get the resolved type of this Ref's Node
    fn resolved_ty(&self, pool: &NodePool) -> Ty;
}
