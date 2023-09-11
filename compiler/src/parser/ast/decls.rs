
use crate::{
    parser::{
        stream::TokenStream,
        node::{Parse, ParseValue, Span, ASTNode, ASTRef}
    },
    shared::{logging::Message, is_none_or::IsNoneOr},
    compiler::{typecheck::{TypeCheck, TypeChecker, Ty, Entity, ScopeLevel},
    typehelper::TypeCheckHelper}
};
use super::{ty::Type, expr::Expr, token::{Ident, Kw, Op, Parenthesized, Braced}};

#[derive(Debug)]
pub struct VarDecl<'s> {
    ident: Ident<'s>,
    ty: Option<Type<'s>>,
    value: Option<Box<Expr<'s>>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for VarDecl<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let var = Kw::Var.parse_value(stream)?;
        let ident = Ident::parse(stream)?;
        let ty = if_then_some!(
            ":".parse_value(stream).is_ok() => Type::parse(stream)?
        );
        let value = if_then_some!(
            Op::Seq.parse_value(stream).is_ok() => Expr::parse(stream)?.into()
        );
        Ok(VarDecl { ident, ty, value, span: var.span() })
    }
}

impl<'s> ASTNode<'s> for VarDecl<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for VarDecl<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.typecheck_helper(checker);
        let value = self.value.typecheck_helper(checker);
        let eval_ty = match (ty, value) {
            (Some(a), Some(b)) => checker.expect_eq(a, b, self.span()),
            (Some(a), None)    => a,
            (None,    Some(b)) => b,
            (None,    None)    => Ty::Inferred,
        };
        let name = checker.resolve_new(self.ident.path());
        checker.try_push(Entity::new(name, ASTRef::VarDecl(self.into()), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
pub struct FunParam<'s> {
    ident: Ident<'s>,
    ty: Type<'s>,
    default_value: Option<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for FunParam<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let ident = Ident::parse(stream)?;
        ":".parse_value(stream)?;
        let ty = Type::parse(stream)?;
        let default_value = if_then_some!(
            Op::Seq.parse_value(stream).is_ok() => Expr::parse(stream)?.into()
        );
        Ok(FunParam { ident, ty, default_value, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for FunParam<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for FunParam<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.typecheck_helper(checker);
        let value = self.default_value.typecheck_helper(checker);
        let eval_ty = match (ty, value) {
            (a, Some(b)) => checker.expect_eq(a, b, self.span()),
            (a, None)    => a,
        };
        let name = checker.resolve_new(self.ident.path());
        checker.try_push(Entity::new(name, ASTRef::FunParam(self.into()), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
pub struct FunDecl<'s> {
    ident: Option<Ident<'s>>,
    params: Vec<FunParam<'s>>,
    ret_ty: Option<Type<'s>>,
    body: Option<Box<Expr<'s>>>,
    span: Span<'s>,
}

impl<'s> FunDecl<'s> {
    pub fn requires_semicolon(&self) -> bool {
        self.body.as_ref().is_none_or(|b| b.requires_semicolon())
    }
}

impl<'s> Parse<'s> for FunDecl<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        Kw::Fun.parse_value(stream)?;
        let ident = Ident::parse(stream).ok();
        let mut params_stream = Parenthesized::parse(stream)?.into_stream();
        let mut params = Vec::new();
        while !params_stream.is_eof() {
            params.push(params_stream.parse()?);
            if params_stream.is_eof() {
                break;
            }
            ",".parse_value(&mut params_stream)?;
        }
        let ret_ty = if_then_some!(
            "->".parse_value(stream).is_ok() => Type::parse(stream)?
        );
        let body = if "=>".parse_value(stream).is_ok() {
            Some(Box::from(stream.parse::<Expr<'s>>()?))
        }
        else if Braced::peek(stream) {
            Some(Box::from(Expr::Block(stream.parse()?)))
        }
        else {
            None
        };
        Ok(FunDecl { ident, params, ret_ty, body, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for FunDecl<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for FunDecl<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        let ret_ty = self.ret_ty.typecheck_helper(checker);
        checker.push_scope(ScopeLevel::Function, ASTRef::FunDecl(self.into()), ret_ty.clone());
        let param_tys = self.params.typecheck_helper(checker);
        let body_ty = self.body.typecheck_helper(checker);
        checker.pop_scope(body_ty.unwrap_or(Ty::Inferred), ASTRef::FunDecl(self.into()));
        if let Some(ref ident) = self.ident {
            let name = checker.resolve_new(ident.path());
            checker.try_push(
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
