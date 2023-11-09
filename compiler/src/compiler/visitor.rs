
use crate::parser::node::ASTNode;

pub trait Visit<'n> {
    fn visit<V>(&'n mut self, visitor: &mut V);
}

impl<'s, 'n, A: ASTNode<'s>> Visit<'n> for A {
    fn visit<V>(&'n mut self, visitor: &mut V) {
        self.iter_children().for_each(|c| c.visit(visitor)); 
    }
}
