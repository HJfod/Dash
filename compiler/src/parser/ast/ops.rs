
use gdml_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        ast::token::Op,
        node::{Parse, ASTNode}
    },
    shared::{logging::{Message, Level}, src::Span},
    compiler::{typecheck::{TypeVisitor, Ty}, visitor::Visitors}
};
use super::{expr::Expr, token::{Parenthesized, Bracketed, self}};

#[derive(Debug)]
#[ast_node]
pub struct UnOp<'s> {
    op: Op<'s>,
    target: Box<Expr<'s>>,
}

impl<'s> Parse<'s> for UnOp<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let op = Op::parse(stream)?;
        let target = Expr::parse_unop(stream)?.into();
        Ok(Self { op, target, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for UnOp<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Call<'s> {
    target: Box<Expr<'s>>,
    args: Vec<Expr<'s>>,
}

impl<'s> Call<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        target: Expr<'s>, stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
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
        Ok(Self { target: target.into(), args, span: start..stream.pos() })
    }
}

impl<'s> Parse<'s> for Call<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Call<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
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
pub struct Index<'s> {
    target: Box<Expr<'s>>,
    index: Box<Expr<'s>>,
}

impl<'s> Index<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(target: Expr<'s>, stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let mut args_stream = Bracketed::parse(stream)?.into_stream();
        let index = args_stream.parse::<Expr<'s>>()?.into();
        match args_stream.next() {
            Token::EOF(_, _) => {},
            other => Err(Message::from_span(
                Level::Error, 
                format!("Did not expect {other} at this position"),
                other.span()
            ))?,
        }
        Ok(Self { target: target.into(), index, span: start..stream.pos() })
    }
}

impl<'s> Parse<'s> for Index<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Index<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}

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
            lhs, op, rhs, span: start..stream.pos(),
        })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for BinOp<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
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
