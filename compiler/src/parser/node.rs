
use std::fmt::Debug;
use crate::{shared::{src::{Span, Src, Spanful, BUILTIN_SPAN}, logging::Message}, compiler::ty::Ty};
use super::stream::{TokenStream, Token};
use std::hash::Hash;

pub trait ASTNode<'s>: Debug {
    fn span(&self) -> &Span<'s>;
    fn src(&self) -> &'s Src {
        self.span().src()
    }
    fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>>;

    /// Get the resulting type for this expression.
    /// If the expression has not yet been definitely evaluated, the type is 
    /// `Ty::Unresolved`. Compilation should never finish if any AST node returns 
    /// `Unresolved`!
    fn eval_ty(&self) -> Ty<'s>;
}

impl<'s, T: ASTNode<'s>> ASTNode<'s> for Box<T> {
    fn span(&self) -> &Span<'s> {
        self.as_ref().span()
    }
    fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>> {
        self.as_ref().iter_children()
    }
    fn eval_ty(&self) -> Ty<'s> {
        self.as_ref().eval_ty()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ASTRef<'s> {
    Builtin,
    Ref(*mut dyn ASTNode<'s>),
}

impl<'s, T: ASTNode<'s>> From<&T> for ASTRef<'s> {
    fn from(value: &T) -> Self {
        Self::Ref(value as *mut dyn ASTNode<'s>)
    }
}

impl<'s> PartialEq for ASTRef<'s> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ASTRef::Ref(a), ASTRef::Ref(b)) => std::ptr::eq(a, b),
            (ASTRef::Builtin, ASTRef::Builtin) => true,
            (_, _) => false,
        }
    }
}

impl<'s> Eq for ASTRef<'s> {}

impl<'s> Hash for ASTRef<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin => 0.hash(state),
            Self::Ref(r) => (*r).hash(state),
        }
    }
}

impl<'s> ASTNode<'s> for ASTRef<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Builtin => &BUILTIN_SPAN,
            Self::Ref(e) => e.span(),
        }
    }
    fn iter_children(&mut self) -> impl Iterator<Item = &mut impl ASTNode<'s>> {
        match self {
            Self::Builtin => std::iter::empty(),
            Self::Ref(e) => e.iter_children(),
        }
    }
    fn eval_ty(&self) -> Ty<'s> {
        match self {
            Self::Builtin => Ty::Invalid,
            Self::Ref(r) => r.eval_ty(),
        }
    }
}

pub trait Parse<'s>: Sized + ASTNode<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>>;
}

pub trait ChildIterHelper<'s> {
    fn to_iter_helper(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>>;
}

impl<'s, T: ASTNode<'s>> ChildIterHelper<'s> for T {
    fn to_iter_helper(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>> {
        std::iter::once(self)
    }
}

impl<'s, T: ASTNode<'s>> ChildIterHelper<'s> for Vec<T> {
    fn to_iter_helper(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>> {
        self.iter_mut()
    }
}

impl<'s, T: ASTNode<'s>> ChildIterHelper<'s> for Option<T> {
    fn to_iter_helper(&mut self) -> impl Iterator<Item = &mut dyn ASTNode<'s>> {
        self.iter_mut()
    }
}
