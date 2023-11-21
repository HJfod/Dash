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

pub trait EvalTypeHelper<'s> {
    fn to_type(&self) -> Ty<'s>;
}

impl<'s> EvalTypeHelper<'s> for Ty<'s> {
    fn to_type(&self) -> Ty<'s> {
        self.clone()
    }
}

impl<'s> EvalTypeHelper<'s> for Option<Ty<'s>> {
    fn to_type(&self) -> Ty<'s> {
        self.clone().unwrap_or(Ty::Inferred)
    }
}
