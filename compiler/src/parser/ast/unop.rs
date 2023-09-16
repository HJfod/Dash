
use crate::{
    parser::{
        stream::{TokenStream, Token},
        ast::token::Op,
        node::{Parse, ASTNode}
    },
    shared::{logging::Message, src::Span},
    compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};
use super::{expr::Expr, token::{Parenthesized, Bracketed, self}};

#[derive(Debug)]
pub struct UnOp<'s> {
    op: Op<'s>,
    target: Box<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for UnOp<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let op = Op::parse(stream)?;
        let target = Expr::parse_unop(stream)?.into();
        Ok(Self { op, target, span: Span::new(stream.src(), start, stream.pos()) })
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
        Ok(Self { target: target.into(), args, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s> Parse<'s> for Call<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
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
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(target: Expr<'s>, stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let mut args_stream = Bracketed::parse(stream)?.into_stream();
        let index = args_stream.parse::<Expr<'s>>()?.into();
        todo!("expect eof");
        Ok(Self { target: target.into(), index, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s> Parse<'s> for Index<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
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
