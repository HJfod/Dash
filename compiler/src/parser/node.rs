
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
    fn parse_impl<S>(stream: &mut S) -> Result<Self, Message<'s>>
        where S: IntoIterator<Item = Token<'s>>;
    fn parse<S>(stream: &mut S) -> Result<Self, Message<'s>>
        where S: IntoIterator<Item = Token<'s>>
    {
        let start = stream.pos();
        match Self::parse_impl(stream) {
            Ok(node) => Ok(node),
            Err(e) => {
                stream.goto(start);
                Err(e)
            }
        }
    }
    fn peek(stream: &mut TokenStream<'s>) -> bool {
        let start = stream.pos();
        let node = Self::parse_impl(stream).ok();
        stream.goto(start);
        node.is_some()
    }
}

pub trait ParseValue<'s>: Sized {
    fn parse_value_impl(self, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>>;
    fn parse_value(self, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        match self.parse_value_impl(stream) {
            Ok(node) => Ok(node),
            Err(e) => {
                stream.goto(start);
                Err(e)
            }
        }
    }
    fn peek_value(self, stream: &mut TokenStream<'s>) -> bool {
        let start = stream.pos();
        let node = self.parse_value_impl(stream).ok();
        stream.goto(start);
        node.is_some()
    }
}

#[derive(PartialEq, Clone)]
pub struct Span<'s> {
    pub src: &'s Src,
    pub range: Range,
}

static BUILTIN_META: Span<'static> = Span {
    src: &Src::Builtin,
    range: Range::zero(),
};

impl<'s> Span<'s> {
    pub fn builtin() -> &'static Self {
        &BUILTIN_META
    }
}

impl Debug for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({:?}:{})", self.src, self.range))
    }
}
