
use super::typecheck::{TypeVisitor, Ty};

pub trait Visitors<'s, 'n> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n>;
}

// pub trait ProxyVisitors<'s, 'n> {
//     type TypeFullResult;
//     fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Self::TypeFullResult;
// }

// impl<'s, 'n, T: Visitors<'s, 'n>> ProxyVisitors<'s, 'n> for T {
//     type TypeFullResult = Ty<'s, 'n>;
//     fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Self::TypeFullResult {
//         self.visit_type_full(visitor)
//     }
// }

// impl<'s, 'n, T: ProxyVisitors<'s, 'n>> ProxyVisitors<'s, 'n> for Option<T> {
//     fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Self::TypeFullResult {
//         self.map(|v| v.visit_type_full(visitor))
//     }
// }

// impl<'s, 'n, T: ProxyVisitors<'s, 'n>> ProxyVisitors<'s, 'n> for Vec<T> {
//     fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Self::TypeFullResult {
//         self.iter().map(|v| v.visit_type_full(visitor)).collect()
//     }
// }
