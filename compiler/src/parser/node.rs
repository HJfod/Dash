
use std::fmt::Debug;
use crate::shared::{src::{Src, Range}, logging::Message, wrappers::RefWrapper};
use super::{stream::{TokenStream, Token}, ast::{expr::Expr, ty::Type, decls::{VarDecl, FunDecl, FunParam}}};
use std::hash::Hash;

pub trait ASTNode<'s>: Debug {
    fn span(&self) -> &Span<'s>;
}

impl<'s, T: ASTNode<'s>> ASTNode<'s> for Box<T> {
    fn span(&self) -> &Span<'s> {
        self.as_ref().span()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ASTRef<'s, 'n> {
    Builtin,
    VarDecl(RefWrapper<'n, VarDecl<'s>>),
    FunDecl(RefWrapper<'n, FunDecl<'s>>),
    FunParam(RefWrapper<'n, FunParam<'s>>),
    Expr(RefWrapper<'n, Expr<'s>>),
    Type(RefWrapper<'n, Type<'s>>),
}

impl<'s, 'n> ASTNode<'s> for ASTRef<'s, 'n> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Builtin => Span::builtin(),
            Self::VarDecl(e) => e.span(),
            Self::FunDecl(e) => e.span(),
            Self::FunParam(e) => e.span(),
            Self::Expr(e) => e.span(),
            Self::Type(e) => e.span(),
        }
    }
}

pub trait Parse<'s>: Sized + ASTNode<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>>;
}

#[derive(PartialEq, Clone)]
pub struct Span<'s> {
    pub src: &'s Src,
    pub range: Range,
}

static BUILTIN_SPAN: Span<'static> = Span {
    src: &Src::Builtin,
    range: Range::zero(),
};

impl<'s> Span<'s> {
    pub fn builtin() -> &'static Self {
        &BUILTIN_SPAN
    }

    pub fn join(&self, other: &Span<'s>) -> Span<'s> {
        let mut new = self.clone();
        new.range.end = other.range.end;
        new
    }
}

impl Debug for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({:?}:{})", self.src, self.range))
    }
}
