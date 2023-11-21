
use crate::parser::node::ASTNode;

pub trait TakeVisitor<V> {
    fn take_visitor(&mut self, visitor: &mut V) {}
}

pub trait Visit {
    fn visit<V>(&mut self, visitor: &mut V);
}

impl<'s, V, A: ASTNode<'s> + TakeVisitor<V>> Visit for A {
    fn visit(&mut self, visitor: &mut V) {
        self.iter_children().for_each(|c| c.visit(visitor)); 
        self.take_visitor(visitor);
    }
}
