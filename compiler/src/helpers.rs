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

impl<'s, T: Rule<'s>> ConcatInto<T> for T {
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

pub trait TypeCheckHelper<'r, 's: 'r, 'l> {
    type Ret;
    fn typecheck_helper(&'r self, checker: &mut TypeChecker<'s, 'l>) -> Self::Ret;
}

impl<'r, 's: 'r, 'l, T: TypeCheck<'s, 'l>> TypeCheckHelper<'r, 's, 'l> for T {
    type Ret = Ty;
    fn typecheck_helper(&'r self, checker: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        self.typecheck(checker)
    }
}

impl<'r, 's: 'r, 'l> TypeCheckHelper<'r, 's, 'l> for String {
    type Ret = &'r String;
    fn typecheck_helper(&'r self, _: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        self
    }
}

impl<'r, 's: 'r, 'l> TypeCheckHelper<'r, 's, 'l> for i64 {
    type Ret = i64;
    fn typecheck_helper(&'r self, _: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        *self
    }
}

impl<'r, 's: 'r, 'l> TypeCheckHelper<'r, 's, 'l> for f64 {
    type Ret = f64;
    fn typecheck_helper(&'r self, _: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        *self
    }
}

impl<'r, 's: 'r, 'l, T: TypeCheckHelper<'r, 's, 'l>> TypeCheckHelper<'r, 's, 'l> for Option<T> {
    type Ret = Option<T::Ret>;
    fn typecheck_helper(&'r self, checker: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        self.as_ref().map(|v| v.typecheck_helper(checker))
    }
}

impl<'r, 's: 'r, 'l, T: TypeCheckHelper<'r, 's, 'l>> TypeCheckHelper<'r, 's, 'l> for Vec<T> {
    type Ret = Vec<T::Ret>;
    fn typecheck_helper(&'r self, checker: &mut TypeChecker<'s, 'l>) -> Self::Ret {
        self.iter().map(|v| v.typecheck_helper(checker)).collect()
    }
}

pub trait EvalTypeHelper {
    fn to_type(&self) -> Ty;
}

impl EvalTypeHelper for Ty {
    fn to_type(&self) -> Ty {
        self.clone()
    }
}

impl EvalTypeHelper for Option<Ty> {
    fn to_type(&self) -> Ty {
        self.clone().unwrap_or(Ty::Invalid)
    }
}
