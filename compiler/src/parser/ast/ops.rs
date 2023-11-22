
use dash_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        ast::token::Op,
        node::{Parse, ASTNode, ASTRef}
    },
    shared::{logging::{Message, Level}, src::Span},
    compiler::{ty::Ty, visitor::TakeVisitor, coherency::CoherencyVisitor}
};
use super::{expr::Expr, token::{Parenthesized, Bracketed, self}};

#[derive(Debug)]
#[ast_node]
pub struct UnOp {
    op: Op,
    target: Box<Expr>,
}

impl Parse for UnOp {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let op = Op::parse(stream)?;
        let target = Expr::parse_unop(stream)?.into();
        Ok(Self {
            op,
            target,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl TakeVisitor<CoherencyVisitor> for UnOp {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        todo!()
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Call {
    target: Box<Expr>,
    args: Vec<Expr>,
}

impl Call {
    pub fn parse_with<I: Iterator<Item = Token>>(
        target: Expr, stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
        let start = stream.pos();
        let mut args_stream = Parenthesized::parse(stream)?.into_stream();
        let mut args = Vec::new();
        while !args_stream.eof() {
            args.push(args_stream.parse()?);
            if args_stream.eof() {
                break;
            }
            token::Comma::parse(&mut args_stream)?;
        }
        Ok(Self {
            target: target.into(),
            args,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for Call {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for Call {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let target_ty = self.target.visit_coherency(visitor);
        let args_ty = self.args.iter().map(|v| v.visit_coherency(visitor)).collect::<Vec<_>>();
        match target_ty {
            Ty::Function { params, ret_ty, decl: _ } => {
                let mut params_iter = params.into_iter();
                for ((arg, arg_ty), (_, param_ty)) in self.args.iter().zip(args_ty).zip(&mut params_iter) {
                    visitor.expect_eq(arg_ty, param_ty, arg.span());
                }
                for rest in params_iter {
                    visitor.emit_msg(Message::from_span(
                        Level::Error,
                        format!("Argument '{}' not passed", rest.0),
                        &self.span
                    ));
                }
                ret_ty.as_ref().clone()
            }
            Ty::Invalid => Ty::Invalid,
            other => {
                visitor.emit_msg(Message::from_span(
                    Level::Error,
                    format!("Attempted to call an expression of type {other}"),
                    &self.span
                ));
                Ty::Invalid
            }
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Index {
    target: Box<Expr>,
    index: Box<Expr>,
}

impl Index {
    pub fn parse_with<I: Iterator<Item = Token>>(target: Expr, stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let mut args_stream = Bracketed::parse(stream)?.into_stream();
        let index = args_stream.parse::<Expr>()?.into();
        match args_stream.next() {
            Token::EOF(_, _) => {},
            other => Err(Message::from_span(
                Level::Error, 
                format!("Did not expect {other} at this position"),
                other.span()
            ))?,
        }
        Ok(Self {
            target: target.into(),
            index,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for Index {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for Index {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        todo!()
    }
}

#[derive(Debug)]
#[ast_node]
pub struct BinOp {
    lhs: Box<Expr>,
    op: Op,
    rhs: Box<Expr>,
}

impl BinOp {
    pub fn parse_with<F, I>(
        lhs: Expr,
        mut rhs: F,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message>
        where
            I: Iterator<Item = Token>,
            F: FnMut(&mut TokenStream<I>) -> Result<Expr, Message>
    {
        let start = stream.pos();
        let lhs = lhs.into();
        let op = stream.parse()?;
        let rhs = rhs(stream)?.into();
        Ok(Self {
            lhs, op, rhs,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl TakeVisitor<CoherencyVisitor> for BinOp {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let lhs_ty = self.lhs.visit_coherency(visitor);
        let rhs_ty = self.rhs.visit_coherency(visitor);
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
