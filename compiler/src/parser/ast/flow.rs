
use crate::{
    parser::{
        node::{Parse, ASTNode},
        stream::{TokenStream, Token}
    },
    shared::{logging::Message, src::Span}, compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};
use super::{expr::Expr, token::{self, Tokenize}};

#[derive(Debug)]
pub struct If<'s> {
    cond: Box<Expr<'s>>,
    truthy: Box<Expr<'s>>,
    falsy: Option<Box<Expr<'s>>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for If<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        token::If::parse(stream)?;
        let cond = Box::from(stream.parse::<Expr<'s>>()?);
        let truthy = Box::from(Expr::Block(stream.parse()?));
        let falsy = if token::Else::parse(stream).is_ok() {
            Some(Box::from(if token::If::peek(stream).is_some() {
                Expr::If(stream.parse()?)
            }
            else {
                Expr::Block(stream.parse()?)
            }))
        }
        else {
            None
        };
        Ok(Self { cond, truthy, falsy, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s> ASTNode<'s> for If<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for If<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Return<'s> {
    expr: Option<Box<Expr<'s>>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for Return<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        token::Return::parse(stream)?;
        let expr = Expr::parse(stream).ok().map(|e| e.into());
        Ok(Self { expr, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s> ASTNode<'s> for Return<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Return<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
