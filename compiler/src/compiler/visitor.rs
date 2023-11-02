
use super::typecheck::{TypeVisitor, Ty};

pub trait Welcome<'n, V> {
    fn welcome(&'n mut self, visitor: &mut V) {}
}

pub trait Visit<'n> {
    fn send_visitor<V>(&'n mut self, visitor: &mut V);
    fn visit<V>(&'n mut self, visitor: &mut V) {
        self.send_visitor(visitor);
        (self as Welcome<'n, V>).welcome(visitor);
    }
}
