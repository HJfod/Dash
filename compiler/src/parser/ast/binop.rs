
use gs_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::ASTNode
    },
    shared::{logging::{Message, Level}, src::Span},
    compiler::{typecheck::{TypeVisitor, Ty}, visitor::Visit}
};
use super::{expr::Expr, token::Op};

#[derive(Debug)]
#[ast_node]
pub struct BinOp<'s> {
    lhs: Box<Expr<'s>>,
    op: Op<'s>,
    rhs: Box<Expr<'s>>,
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

impl<'s, 'n> Visit<TypeVisitor<'s, 'n>> for BinOp<'s> {
    fn visit(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let lhs_ty = self.lhs.typecheck_helper(visitor);
        let rhs_ty = self.rhs.typecheck_helper(visitor);
        match visitor.binop_ty(&lhs_ty, &self.op, &rhs_ty) {
            Some(ty) => ty,
            None => {
                visitor.emit_msg(Message::from_span(
                    Level::Error,
                    format!("Cannot apply '{}' to '{lhs_ty}' and '{rhs_ty}'", self.op),
                    self.span()
                ));
                Ty::Invalid
            }
        }
    }
}
