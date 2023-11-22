
use crate::parser::node::{ASTNode, ASTRef};

pub trait TakeVisitor<V> {
    fn take_visitor(&mut self, visitor: &mut V) {}
}

pub trait Visit<V> {
    fn visit(&mut self, visitor: &mut V);
}

impl<V, A: ASTNode + TakeVisitor<V>> Visit<V> for A {
    fn visit(&mut self, visitor: &mut V) {
        self.children().into_iter().for_each(|c| match c {
            ASTRef::Ref(c) => unsafe { *c }.visit(visitor),
            ASTRef::Builtin => ()
        }); 
        self.take_visitor(visitor);
    }
}
