
use std::fmt::Debug;
use crate::shared::{src::{Span, Src, Spanful, BUILTIN_SPAN}, logging::Message};
use super::stream::{TokenStream, Token};
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

#[derive(Debug, Clone, Copy)]
pub enum ASTRef<'s, 'n> {
    Builtin,
    Ref(&'n dyn ASTNode<'s>),
}

impl<'s, 'n> PartialEq for ASTRef<'s, 'n> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ASTRef::Ref(a), ASTRef::Ref(b)) => std::ptr::eq(a, b),
            (ASTRef::Builtin, ASTRef::Builtin) => true,
            (_, _) => false,
        }
    }
}

impl<'s, 'n> Eq for ASTRef<'s, 'n> {}

impl<'s, 'n> Hash for ASTRef<'s, 'n> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin => 0.hash(state),
            Self::Ref(r) => (*r as *const dyn ASTNode<'s>).hash(state),
        }
    }
}

impl<'s, 'n> ASTNode<'s> for ASTRef<'s, 'n> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Builtin => &BUILTIN_SPAN,
            Self::Ref(e) => e.span(),
        }
    }
}

pub trait Parse<'s>: Sized + ASTNode<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>>;
}
