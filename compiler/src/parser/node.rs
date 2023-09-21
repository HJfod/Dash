
use std::fmt::Debug;
use crate::shared::{src::{Span, Src}, logging::Message, wrappers::RefWrapper};
use super::{stream::{TokenStream, Token}, ast::{expr::Expr, ty::Type, decls::{VarDecl, FunDecl, FunParam}, flow::Return}};
use std::hash::Hash;

pub trait ASTNode<'s>: Debug {
    fn span(&self) -> &Span<'s>;
    fn src(&self) -> &'s Src {
        self.span().src()
    }
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
    Return(RefWrapper<'n, Return<'s>>),
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
            Self::Return(e) => e.span(),
        }
    }
}

pub trait Parse<'s>: Sized + ASTNode<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>>;
}
