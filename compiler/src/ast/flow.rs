
use dash_macros::{Parse, Resolve};
use crate::{parser::parse::{Separated, SeparatedWithTrailing, Parse}, checker::{resolve::{Resolve, ResolveCache}, ty::Ty, coherency::Checker}};
use super::{token::{kw, delim, punct}, expr::{Expr, ExprList, IdentComponent}};

#[derive(Debug, Parse)]
pub struct If {
    if_kw: kw::If,
    cond: Expr,
    truthy: delim::Braced<ExprList>,
    falsy: Option<(kw::Else, Else)>,
    #[parse(skip)]
    cache: ResolveCache,
}

impl Resolve for If {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let cond = self.cond.try_resolve(checker)?;
        let truthy = self.truthy.try_resolve(checker)?;
        let falsy = if let Some((_, e)) = &mut self.falsy {
            e.try_resolve(checker)?
        }
        else {
            Ty::Invalid
        };
        checker.expect_ty_eq(cond, Ty::Bool, self.cond.span());
        checker.expect_ty_eq(truthy, falsy, self.span()).into()
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "block or if statement")]
pub enum Else {
    Else(delim::Braced<ExprList>),
    ElseIf(Box<If>),
}

#[derive(Debug, Parse)]
pub struct Return {
    return_kw: kw::Return,
    expr: Option<Expr>,
}

impl Resolve for Return {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        todo!()
    }
}

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
enum UsingComponent {
    Multi(delim::Braced<SeparatedWithTrailing<UsingComponent, punct::Comma>>),
    Single(IdentComponent),
}

#[derive(Debug, Parse)]
struct UsingPath {
    absolute: Option<punct::Namespace>,
    path: Separated<UsingComponent, punct::Namespace>,
}

#[derive(Debug, Parse)]
pub struct UsingItem {
    using_kw: kw::Using,
    path: UsingPath,
}

impl Resolve for UsingItem {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        todo!()
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "control flow expression")]
pub enum Flow {
    If(If),
    Return(Return),
    UsingItem(UsingItem),
}
