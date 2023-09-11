
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
    pub fn parse_with<F, S>(lhs: Expr<'s>, mut rhs: F, stream: &mut S) -> Result<Self, Message<'s>>
        where
            S: TokenStream<'s>,
            F: FnMut(&mut S) -> Result<Expr<'s>, Message<'s>>
    {
        let lhs = lhs.into();
        let op = stream.parse()?;
        let rhs = rhs(stream)?.into();
        Ok(Self {
            lhs, op, rhs, span: lhs.span().join(rhs.span()),
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
