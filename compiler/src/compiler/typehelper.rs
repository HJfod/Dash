
use super::typecheck::{TypeChecker, TypeCheck, Ty};

pub trait TypeCheckHelper<'s, 'n> {
    type Ret;
    fn typecheck_helper(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Self::Ret;
}

impl<'n, 's: 'n, T: TypeCheck<'s, 'n>> TypeCheckHelper<'s, 'n> for T {
    type Ret = Ty<'s, 'n>;
    fn typecheck_helper(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        self.typecheck(checker)
    }
}

impl<'s, 'n> TypeCheckHelper<'s, 'n> for String {
    type Ret = &'n String;
    fn typecheck_helper(&'n self, _: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        self
    }
}

impl<'s, 'n> TypeCheckHelper<'s, 'n> for i64 {
    type Ret = i64;
    fn typecheck_helper(&'n self, _: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        *self
    }
}

impl<'s, 'n> TypeCheckHelper<'s, 'n> for f64 {
    type Ret = f64;
    fn typecheck_helper(&'n self, _: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        *self
    }
}

impl<'s, 'n, T: TypeCheckHelper<'s, 'n>> TypeCheckHelper<'s, 'n> for Option<T> {
    type Ret = Option<T::Ret>;
    fn typecheck_helper(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        self.as_ref().map(|v| v.typecheck_helper(checker))
    }
}

impl<'s, 'n, T: TypeCheckHelper<'s, 'n>> TypeCheckHelper<'s, 'n> for Vec<T> {
    type Ret = Vec<T::Ret>;
    fn typecheck_helper(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Self::Ret {
        self.iter().map(|v| v.typecheck_helper(checker)).collect()
    }
}
