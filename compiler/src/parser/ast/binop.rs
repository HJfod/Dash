
use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::ASTNode
    },
    shared::{logging::Message, src::Span},
    compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};
use super::{expr::Expr, token::Op};

#[derive(Debug)]
pub struct BinOp<'s> {
    lhs: Box<Expr<'s>>,
    op: Op<'s>,
    rhs: Box<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> BinOp<'s> {
    pub fn parse_with<F, I>(
        lhs: Expr<'s>,
        mut rhs: F,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>>
        where
            I: Iterator<Item = Token<'s>>,
            F: FnMut(&mut TokenStream<'s, I>) -> Result<Expr<'s>, Message<'s>>
    {
        let start = stream.pos();
        let lhs = lhs.into();
        let op = stream.parse()?;
        let rhs = rhs(stream)?.into();
        Ok(Self {
            lhs, op, rhs, span: Span::new(stream.src(), start, stream.pos()),
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
