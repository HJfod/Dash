
use gs_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode, ASTRef}
    },
    shared::{logging::{Message, Level}, is_none_or::IsNoneOr, src::{Span, Loc}},
    compiler::{typecheck::{TypeVisitor, Ty, Entity, ScopeLevel}, visitor::Visitors}
};
use super::{ty::Type, expr::{Expr, Visibility}, token::{Ident, Parenthesized, Braced, self, Colon, Tokenize}, if_then_some, Path};

#[derive(Debug)]
#[ast_node]
pub struct ItemAttrs<'s> {
    visibility: Visibility<'s>,
    is_extern: Option<token::Extern<'s>>,
}

impl<'s> Parse<'s> for ItemAttrs<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        Ok(Self {
            visibility: stream.parse()?,
            is_extern: token::Extern::peek_and_parse(stream),
            span: start..stream.pos(),
        })
    }
}

#[derive(Debug)]
#[ast_node]
pub struct VarDecl<'s> {
    item_attrs: ItemAttrs<'s>,
    ident: Ident<'s>,
    ty: Option<Type<'s>>,
    value: Option<Box<Expr<'s>>>,
}

impl<'s> VarDecl<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        let start = item_attrs.span().start;
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
        Ok(VarDecl {
            item_attrs,
            ident,
            ty,
            value,
            span: start..stream.pos()
        })
    }
}

impl<'s> Parse<'s> for VarDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
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
        Ok(FunParam { ident, ty, default_value, span: start..stream.pos() })
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
    item_attrs: ItemAttrs<'s>,
    ident: Option<Ident<'s>>,
    params: Vec<FunParam<'s>>,
    ret_ty: Option<Type<'s>>,
    body: Option<Box<Expr<'s>>>,
}

impl<'s> FunDecl<'s> {
    pub fn requires_semicolon(&self) -> bool {
        self.body.as_ref().is_none_or(|b| b.requires_semicolon())
    }

    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        let start = item_attrs.span().start;
        token::Fun::parse(stream)?;
        let ident = Ident::peek_and_parse(stream);
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
        Ok(FunDecl {
            item_attrs,
            ident,
            params,
            ret_ty,
            body,
            span: start..stream.pos()
        })
    }
}

impl<'s> Parse<'s> for FunDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
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

#[derive(Debug)]
#[ast_node]
pub struct TypeAliasDecl<'s> {
    item_attrs: ItemAttrs<'s>,
    ident: Ident<'s>,
    value: Type<'s>,
}

impl<'s> TypeAliasDecl<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        let start = item_attrs.span().start;
        token::Type::parse(stream)?;
        let ident = stream.parse()?;
        token::Seq::parse(stream)?;
        let value = stream.parse()?;
        Ok(TypeAliasDecl {
            item_attrs,
            ident,
            value,
            span: start..stream.pos()
        })
    }
}

impl<'s> Parse<'s> for TypeAliasDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for TypeAliasDecl<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.value.visit_type_full(visitor);
        visitor.try_push(Ty::Alias {
            name: self.ident.to_string(),
            ty: ty.clone().into(),
            decl: ASTRef::TypeAliasDecl(self.into()),
        }, self.span());
        ty
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Item<'s> {
    VarDecl(VarDecl<'s>),
    FunDecl(FunDecl<'s>),
    TypeAliasDecl(TypeAliasDecl<'s>),
}

impl<'s> Item<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        if token::Var::peek(stream).is_some() {
            Ok(Self::VarDecl(VarDecl::parse_with(item_attrs, stream)?))
        }
        else if token::Fun::peek(stream).is_some() {
            Ok(Self::FunDecl(FunDecl::parse_with(item_attrs, stream)?))
        }
        else if token::Type::peek(stream).is_some() {
            Ok(Self::TypeAliasDecl(TypeAliasDecl::parse_with(item_attrs, stream)?))
        }
        else {
            Err(Message::from_span(
                Level::Error,
                "Expected item declaration",
                stream.peek().span()
            ))
        }
    }

    pub fn peek<I: Iterator<Item = Token<'s>>>(stream: &TokenStream<'s, I>) -> bool {
        token::Var::peek(stream).is_some() || 
        token::Fun::peek(stream).is_some() ||
        token::Public::peek(stream).is_some() ||
        token::Private::peek(stream).is_some() ||
        token::Extern::peek(stream).is_some()
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::FunDecl(f) => f.requires_semicolon(),
            Self::VarDecl(_) => true,
            Self::TypeAliasDecl(_) => true,
        }
    }
}

impl<'s> Parse<'s> for Item<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Item<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::FunDecl(t) => t.visit_type_full(visitor),
            Self::VarDecl(t) => t.visit_type_full(visitor),
            Self::TypeAliasDecl(t) => t.visit_type_full(visitor),
        }
    }
}

impl<'s> ASTNode<'s> for Item<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::VarDecl(t) => t.span(),
            Self::FunDecl(t) => t.span(),
            Self::TypeAliasDecl(t) => t.span(),
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct UsingItem<'s> {
    path: Path<'s>,
}

impl<'s> Parse<'s> for UsingItem<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        token::Using::parse(stream)?;
        let path = stream.parse()?;
        Ok(Self { path, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for UsingItem<'s> {
    fn visit_type_full(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
