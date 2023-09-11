
use crate::{
    parser::{
        stream::TokenStream,
        ast::token::Op,
        node::{Parse, ASTNode, Span, ParseValue}
    },
    shared::logging::Message,
    compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};
use super::{expr::Expr, token::{Parenthesized, Bracketed}};

#[derive(Debug)]
pub struct UnOp<'s> {
    op: Op,
    target: Box<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for UnOp<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let op = Op::parse(stream)?;
        let target = Expr::parse_unop(stream)?.into();
        Ok(Self { op, target, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for UnOp<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for UnOp<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Call<'s> {
    target: Box<Expr<'s>>,
    args: Vec<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Call<'s> {
    pub fn parse_with(target: Expr<'s>, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = target.span().range.start.offset;
        let mut args_stream = Parenthesized::parse(stream)?.into_stream();
        let mut args = Vec::new();
        while !args_stream.is_eof() {
            args.push(args_stream.parse()?);
            if args_stream.is_eof() {
                break;
            }
            ",".parse_value(&mut args_stream)?;
        }
        Ok(Self { target: target.into(), args, span: stream.span(start) })
    }
}

impl<'s> Parse<'s> for Call<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s> ASTNode<'s> for Call<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Call<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Index<'s> {
    target: Box<Expr<'s>>,
    index: Box<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Index<'s> {
    pub fn parse_with(target: Expr<'s>, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = target.span().range.start.offset;
        let mut args_stream = Bracketed::parse(stream)?.into_stream();
        let index = args_stream.parse::<Expr<'s>>()?.into();
        args_stream.expect_eof()?;
        Ok(Self { target: target.into(), index, span: stream.span(start) })
    }
}

impl<'s> Parse<'s> for Index<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s> ASTNode<'s> for Index<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Index<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
