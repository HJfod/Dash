
use crate::{
    parser::{
        stream::TokenStream,
        node::{ASTNode, Span}
    },
    shared::logging::Message,
    compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};
use super::{expr::Expr, token::Op};

#[derive(Debug)]
pub struct BinOp<'s> {
    lhs: Box<Expr<'s>>,
    op: Op,
    rhs: Box<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> BinOp<'s> {
    pub fn parse_with<F>(lhs: Expr<'s>, mut rhs: F, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>>
        where F: FnMut(&mut TokenStream<'s>) -> Result<Expr<'s>, Message<'s>>
    {
        let start = lhs.span().range.start.offset;
        Ok(Self {
            lhs: lhs.into(),
            op: stream.parse()?,
            rhs: rhs(stream)?.into(),
            span: stream.span(start)
        })
    }
}

impl<'s> ASTNode<'s> for BinOp<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for BinOp<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
