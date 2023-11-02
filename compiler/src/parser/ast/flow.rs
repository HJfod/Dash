
use gdml_macros::ast_node;

use crate::{
    parser::{
        node::{Parse, ASTNode, ASTRef},
        stream::{TokenStream, Token}
    },
    shared::{logging::Message, src::Span},
    compiler::{ty::{TypeVisitor, Ty, ScopeLevel, FindScope}, visitor::Visitors,}
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
        Ok(Self { cond, truthy, falsy, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for If<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        visitor.push_scope(ScopeLevel::Opaque, ASTRef::Ref(self as &'n dyn ASTNode<'s>), None);
        let cond_ty = self.cond.visit_coherency(visitor);
        let truthy_ty = self.truthy.visit_coherency(visitor);
        visitor.pop_scope(truthy_ty.clone(), ASTRef::Ref(self as &'n dyn ASTNode<'s>));
        let falsy_ty = self.falsy.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.expect_eq(cond_ty, Ty::Bool, self.cond.span());
        visitor.expect_eq(falsy_ty.unwrap_or(Ty::Inferred), truthy_ty.clone(), &self.span);
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
        Ok(Self { expr, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Return<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let expr_ty = self.expr.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.infer_return_type(
            FindScope::ByLevel(ScopeLevel::Function),
            expr_ty.unwrap_or(Ty::Void),
            ASTRef::Ref(self as &'n dyn ASTNode<'s>)
        );
        Ty::Never
    }
}
