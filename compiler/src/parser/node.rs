
use std::{fmt::Debug, sync::Arc};
use crate::{shared::{src::{Span, Src, Spanful, BUILTIN_SPAN}, logging::Message}, compiler::ty::Ty};
use super::stream::{TokenStream, Token};
use std::hash::Hash;

pub trait ASTNode: Debug {
    fn span(&self) -> &Span;
    fn src(&self) -> Arc<Src> {
        self.span().src()
    }
    fn children(&mut self) -> Vec<ASTRef>;

    /// Get the resulting type for this expression.
    /// If the expression has not yet been definitely evaluated, the type is 
    /// `Ty::Unresolved`. Compilation should never finish if any AST node returns 
    /// `Unresolved`!
    fn eval_ty(&self) -> Ty;
}

impl<T: ASTNode> ASTNode for Box<T> {
    fn span(&self) -> &Span {
        self.as_ref().span()
    }
    fn children(&mut self) -> Vec<ASTRef> {
        self.as_mut().children()
    }
    fn eval_ty(&self) -> Ty {
        self.as_ref().eval_ty()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ASTRef {
    Builtin,
    Ref(*mut dyn ASTNode),
}

impl<T: ASTNode> From<&mut T> for ASTRef {
    fn from(value: &mut T) -> Self {
        Self::Ref(value as *mut dyn ASTNode)
    }
}

impl PartialEq for ASTRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ASTRef::Ref(a), ASTRef::Ref(b)) => std::ptr::eq(a, b),
            (ASTRef::Builtin, ASTRef::Builtin) => true,
            (_, _) => false,
        }
    }
}

impl Eq for ASTRef {}

impl Hash for ASTRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin => 0.hash(state),
            Self::Ref(r) => (*r).hash(state),
        }
    }
}

impl ASTNode for ASTRef {
    fn span(&self) -> &Span {
        match self {
            Self::Builtin => &BUILTIN_SPAN,
            Self::Ref(e) => unsafe { **e }.span(),
        }
    }
    fn children(&mut self) -> Vec<ASTRef> {
        match self {
            Self::Builtin => vec![],
            Self::Ref(e) => unsafe { **e }.children(),
        }
    }
    fn eval_ty(&self) -> Ty {
        match self {
            Self::Builtin => Ty::Invalid,
            Self::Ref(r) => unsafe { **r }.eval_ty(),
        }
    }
}

pub trait Parse: Sized + ASTNode {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message>;
}

pub trait ChildIterHelper {
    fn to_iter_helper(&mut self) -> Vec<ASTRef>;
}

impl<T: ASTNode> ChildIterHelper for T {
    fn to_iter_helper(&mut self) -> Vec<ASTRef> {
        std::iter::once(self)
    }
}

impl<T: ASTNode> ChildIterHelper for Vec<T> {
    fn to_iter_helper(&mut self) -> Vec<ASTRef> {
        self.iter_mut()
    }
}

impl<T: ASTNode> ChildIterHelper for Option<T> {
    fn to_iter_helper(&mut self) -> Vec<ASTRef> {
        self.iter_mut()
    }
}
