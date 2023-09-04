use std::{hash::Hash, ops::Deref};

use crate::{parser::Rule, compiler::{TypeCheck, Ty, TypeChecker}};

#[macro_export]
macro_rules! rule_peek {
    ($parser: ident, $expr: expr) => {{
        let start = $parser.pos();
        match || -> Result<_, Message<'s>> { Ok($expr) }() {
            Ok(_) => {
                $parser.goto(start);
                true
            }
            Err(_) => {
                $parser.goto(start);
                false
            }
        }
    }};
}

#[macro_export]
macro_rules! rule_try {
    ($parser: ident, $expr: expr) => {{
        let start = $parser.pos();
        match || -> Result<_, Message<'s>> { Ok($expr) }() {
            Ok(r) => Ok(r),
            Err(e) => {
                $parser.goto(start);
                Err(e)
            }
        }
    }};
}

pub trait ConcatInto<T> {
    fn concat_into(self, target: &mut Vec<T>);
}

impl<'s, 'n, T: Rule<'s>> ConcatInto<T> for T {
    fn concat_into(self, target: &mut Vec<T>) {
        target.push(self);
    }
}

impl<A, T: ConcatInto<A>> ConcatInto<A> for Vec<T> {
    fn concat_into(self, target: &mut Vec<A>) {
        self.into_iter().for_each(|t| t.concat_into(target));
    }
}

impl<A, T: ConcatInto<A>> ConcatInto<A> for Option<T> {
    fn concat_into(self, target: &mut Vec<A>) {
        if let Some(v) = self {
            v.concat_into(target);
        }
    }
}

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

pub trait EvalTypeHelper<'s, 'n> {
    fn to_type(&self) -> Ty<'s, 'n>;
}

impl<'s, 'n> EvalTypeHelper<'s, 'n> for Ty<'s, 'n> {
    fn to_type(&self) -> Ty<'s, 'n> {
        self.clone()
    }
}

impl<'s, 'n> EvalTypeHelper<'s, 'n> for Option<Ty<'s, 'n>> {
    fn to_type(&self) -> Ty<'s, 'n> {
        self.clone().unwrap_or(Ty::Inferred)
    }
}

#[derive(Debug)]
pub struct RefWrapper<'t, T> {
    value: &'t T,
}

impl<'t, T> Copy for RefWrapper<'t, T> {}

impl<'t, T> Clone for RefWrapper<'t, T> {
    fn clone(&self) -> Self {
        Self { value: self.value }
    }
}

impl<'t, T> PartialEq for RefWrapper<'t, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'t, T> Eq for RefWrapper<'t, T> {}

impl<'t, T> Hash for RefWrapper<'t, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.value as *const T).hash(state)
    }
}

impl<'t, T> From<&'t T> for RefWrapper<'t, T> {
    fn from(value: &'t T) -> Self {
        Self { value }
    }
}

impl<'t, T> Deref for RefWrapper<'t, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}
