
use crate::parser::node::ASTNode;

pub trait TakeVisitor<'n, V> {
    fn take_visitor(&'n mut self, visitor: &mut V) {}
}

pub trait Visit<'n> {
    fn visit<V>(&'n mut self, visitor: &mut V);
}

impl<'s, 'n, V, A: ASTNode<'s> + TakeVisitor<'n, V>> Visit<'n> for A {
    fn visit(&'n mut self, visitor: &mut V) {
        self.iter_children().for_each(|c| c.visit(visitor)); 
        self.take_visitor(visitor);
    }
}
