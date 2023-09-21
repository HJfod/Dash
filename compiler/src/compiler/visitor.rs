
pub trait Visitor {
    type Result;
}

pub trait Visit<V: Visitor> {
    fn visit(&self, visitor: &mut V) -> <V as Visitor>::Result;
}

pub trait VisitProxy<V: Visitor> {
    type Result;
    fn visit_proxy(&self, visitor: &mut V) -> Self::Result;
}

impl<V: Visitor, P: Visit<V>> VisitProxy<V> for P {
    type Result = <V as Visitor>::Result;
    fn visit_proxy(&self, visitor: &mut V) -> Self::Result {
        self.visit(visitor)
    }
}

impl<V: Visitor, T: VisitProxy<V>> VisitProxy<V> for Vec<T> {
    type Result = Vec<T::Result>;
    fn visit_proxy(&self, visitor: &mut V) -> Self::Result {
        self.iter().map(|t| t.visit(visitor)).collect()
    }
}

impl<V: Visitor, T: VisitProxy<V>> VisitProxy<V> for Option<T> {
    type Result = Option<T::Result>;
    fn visit_proxy(&self, visitor: &mut V) -> Self::Result {
        self.map(|c| c.visit(visitor))
    }
}
