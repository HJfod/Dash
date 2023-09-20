
use gs_macros::ast_node;

use crate::{
    parser::{
        node::{Parse, ASTNode, ASTRef},
        stream::{TokenStream, Token}
    },
    shared::{logging::Message, src::Span}, compiler::{typecheck::{TypeCheck, TypeChecker, Ty, ScopeLevel, FindScope}, typehelper::TypeCheckHelper}
};
use super::{expr::Expr, token::{self, Tokenize}};

#[derive(Debug)]
#[ast_node]
pub struct If<'s> {
    cond: Box<Expr<'s>>,
    truthy: Box<Expr<'s>>,
    falsy: Option<Box<Expr<'s>>>,
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

impl<'s, 'n> TypeCheck<'s, 'n> for If<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        checker.push_scope(ScopeLevel::Opaque, ASTRef::Expr(self.cond.as_ref().into()), None);
        let cond_ty = self.cond.typecheck_helper(checker);
        let truthy_ty = self.truthy.typecheck_helper(checker);
        checker.pop_scope(truthy_ty.clone(), ASTRef::Expr(self.truthy.as_ref().into()));
        let falsy_ty = self.falsy.typecheck_helper(checker);
        checker.expect_eq(cond_ty, Ty::Bool, self.cond.span());
        checker.expect_eq(falsy_ty.unwrap_or(Ty::Inferred), truthy_ty.clone(), &self.span);
        truthy_ty
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Return<'s> {
    expr: Option<Box<Expr<'s>>>,
}

impl<'s> Parse<'s> for Return<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        token::Return::parse(stream)?;
        let expr = Expr::parse(stream).ok().map(|e| e.into());
        Ok(Self { expr, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Return<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        let expr_ty = self.expr.typecheck_helper(checker);
        checker.infer_return_type(
            FindScope::ByLevel(ScopeLevel::Function),
            expr_ty.unwrap_or(Ty::Void),
            ASTRef::Return(self.into())
        );
        Ty::Never
    }
}
