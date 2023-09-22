
use gs_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode, ASTRef}
    },
    shared::{logging::Message, is_none_or::IsNoneOr, src::Span},
    compiler::{typecheck::{TypeVisitor, Ty, Entity, ScopeLevel}, visitor::Visitors}
};
use super::{ty::Type, expr::{Expr, Visibility}, token::{Ident, Parenthesized, Braced, self, Colon, Tokenize}, if_then_some};

#[derive(Debug)]
#[ast_node]
pub struct VarDecl<'s> {
    public: Visibility<'s>,
    ident: Ident<'s>,
    ty: Option<Type<'s>>,
    value: Option<Box<Expr<'s>>>,
}

impl<'s> Parse<'s> for VarDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let public = stream.parse()?;
        token::Var::parse(stream)?;
        let ident = Ident::parse(stream)?;
        let ty = if Colon::peek_and_parse(stream).is_some() {
            Some(Type::parse(stream)?)
        }
        else {
            None
        };
        let value = if token::Seq::peek_and_parse(stream).is_some() {
            Some(Expr::parse(stream)?.into())
        }
        else {
            None
        };
        Ok(VarDecl { public, ident, ty, value, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for VarDecl<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.as_ref().map(|ty| ty.visit_type_full(visitor));
        let value = self.value.as_ref().map(|value| value.visit_type_full(visitor));
        let eval_ty = match (ty, value) {
            (Some(a), Some(b)) => visitor.expect_eq(a, b, self.span()),
            (Some(a), None)    => a,
            (None,    Some(b)) => b,
            (None,    None)    => Ty::Inferred,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, ASTRef::VarDecl(self.into()), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
#[ast_node]
pub struct FunParam<'s> {
    ident: Ident<'s>,
    ty: Type<'s>,
    default_value: Option<Expr<'s>>,
}

impl<'s> Parse<'s> for FunParam<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let ident = Ident::parse(stream)?;
        token::Colon::parse(stream)?;
        let ty = Type::parse(stream)?;
        let default_value = if token::Seq::peek_and_parse(stream).is_some() {
            Some(Expr::parse(stream)?)
        }
        else {
            None
        };
        Ok(FunParam { ident, ty, default_value, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for FunParam<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.visit_type_full(visitor);
        let value = self.default_value.as_ref().map(|v| v.visit_type_full(visitor));
        let eval_ty = match (ty, value) {
            (a, Some(b)) => visitor.expect_eq(a, b, self.span()),
            (a, None)    => a,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, ASTRef::FunParam(self.into()), eval_ty, true), &self.span)
            .map(|t| t.ty())
            .unwrap_or(Ty::Invalid)
    }
}

#[derive(Debug)]
#[ast_node]
pub struct FunDecl<'s> {
    public: Visibility<'s>,
    ident: Option<Ident<'s>>,
    params: Vec<FunParam<'s>>,
    ret_ty: Option<Type<'s>>,
    body: Option<Box<Expr<'s>>>,
}

impl<'s> FunDecl<'s> {
    pub fn requires_semicolon(&self) -> bool {
        self.body.as_ref().is_none_or(|b| b.requires_semicolon())
    }
}

impl<'s> Parse<'s> for FunDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let public = stream.parse()?;
        token::Fun::parse(stream)?;
        let ident = Ident::parse(stream).ok();
        let mut params_stream = Parenthesized::parse(stream)?.into_stream();
        let mut params = Vec::new();
        while !params_stream.eof() {
            params.push(params_stream.parse()?);
            if params_stream.eof() {
                break;
            }
            token::Comma::parse(&mut params_stream)?;
        }
        let ret_ty = if_then_some(
            token::Arrow::peek_and_parse(stream).is_some(),
            || Type::parse(stream)
        )?;
        let body = if token::FatArrow::peek_and_parse(stream).is_some() {
            Some(Box::from(stream.parse::<Expr<'s>>()?))
        }
        else if Braced::peek(stream).is_some() {
            Some(Box::from(Expr::Block(stream.parse()?)))
        }
        else {
            None
        };
        Ok(FunDecl { public, ident, params, ret_ty, body, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for FunDecl<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ret_ty = self.ret_ty.as_ref().map(|v| v.visit_type_full(visitor));
        visitor.push_scope(ScopeLevel::Function, ASTRef::FunDecl(self.into()), ret_ty.clone());
        let param_tys = self.params.iter().map(|v| v.visit_type_full(visitor)).collect::<Vec<_>>();
        let body_ty = self.body.as_ref().map(|v| v.visit_type_full(visitor));
        visitor.pop_scope(body_ty.unwrap_or(Ty::Inferred), ASTRef::FunDecl(self.into()));
        if let Some(ref ident) = self.ident {
            let name = visitor.resolve_new(ident.path());
            visitor.try_push(
                Entity::new(
                    name, ASTRef::FunDecl(self.into()), Ty::Function {
                        params: self.params.iter()
                            .zip(param_tys)
                            .map(|(p, ty)| (p.ident.value().clone(), ty))
                            .collect(),
                        ret_ty: ret_ty.unwrap_or(Ty::Inferred).into(),
                        decl: ASTRef::FunDecl(self.into())
                    },
                    false
                ),
                &self.span
            );
        }
        Ty::Void
    }
}
