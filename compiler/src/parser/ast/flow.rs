
use dash_macros::ast_node;

use crate::{
    parser::{
        node::{Parse, ASTNode, ASTRef},
        stream::{TokenStream, Token}
    },
    shared::{logging::Message, src::Span},
    compiler::{ty::Ty, visitor::TakeVisitor, coherency::{CoherencyVisitor, ScopeLevel, FindScope, Scope}}
};
use super::{expr::Expr, token::{self, Tokenize}};

#[derive(Debug)]
#[ast_node]
pub struct If {
    cond: Box<Expr>,
    truthy: Box<Expr>,
    falsy: Option<Box<Expr>>,
    #[ast_skip_child]
    scope: *mut Scope,
}

impl Parse for If {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        token::If::parse(stream)?;
        let cond = Box::from(stream.parse::<Expr>()?);
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
        Ok(Self { cond, truthy, falsy, span: start..stream.pos(), eval_ty: Ty::Unresolved })
    }
}

impl TakeVisitor<CoherencyVisitor> for If {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        visitor.enter_scope(&mut self.scope, || (ScopeLevel::Block, self.into(), None));
        let cond_ty = self.cond.visit_coherency(visitor);
        let truthy_ty = self.truthy.visit_coherency(visitor);
        visitor.pop_scope(truthy_ty.clone(), self.into());
        let falsy_ty = self.falsy.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.expect_eq(cond_ty, Ty::Bool, self.cond.span());
        visitor.expect_eq(falsy_ty.unwrap_or(Ty::Unresolved), truthy_ty.clone(), &self.span);
        truthy_ty
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Return {
    expr: Option<Box<Expr>>,
}

impl Parse for Return {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        token::Return::parse(stream)?;
        let expr = Expr::parse(stream).ok().map(|e| e.into());
        Ok(Self { expr, span: start..stream.pos(), eval_ty: Ty::Unresolved })
    }
}

impl TakeVisitor<CoherencyVisitor> for Return {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let expr_ty = self.expr.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.infer_return_type(
            FindScope::ByLevel(ScopeLevel::Function),
            expr_ty.unwrap_or(Ty::Void),
            self.into()
        );
        Ty::Never
    }
}
