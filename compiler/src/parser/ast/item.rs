
use dash_macros::ast_node;

// notes to self on how to eval macros:
// step 1. during typechecking, if a type can't be found yet, don't error but add it 
// to a list of unresolved type expressions instead
// step 2. during typechecking, if a macro contains unresolved types, do not evaluate 
// any calls to that macro yet but instead add them to a list of unresolved macro calls
// step 3. immediately evaluate any macro calls that only have resolved types
// step 4. at the end of typechecking, if there are any unresolved macro calls, keep 
// trying to resolve them, and if we got through typechecking without any new macro call 
// resolutions happening, then we know there's a loop somewhere or the macro calls contain 
// nonexistant types and send out compile errors for every unresolved type expression and 
// macro
// step 5. if all macro calls were resolved, go through one more time to try to resolve 
// all remaining unresolved type expressions. if any are left unresolved, send out errors
// step 6. if we got through typechecking with all macro calls and types resolved, then 
// step 7. we are done :33

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode, ASTRef}
    },
    shared::{logging::{Message, Level}, is_none_or::IsNoneOr, src::{Span, Loc}},
    compiler::{ty::{TypeVisitor, Ty, Entity, ScopeLevel}, visitor::Visitors}
};
use super::{ty::Type, expr::{Expr, Visibility, Block}, token::{Ident, Parenthesized, Braced, self, Colon, Tokenize}, Path};

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

        let ty = Colon::peek_and_parse(stream)
            .map(|_| Type::parse(stream))
            .transpose()?;

        let value = token::Seq::peek_and_parse(stream)
            .map(|_| Expr::parse(stream).map(|e| e.into()))
            .transpose()?;

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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.as_ref().map(|ty| ty.visit_coherency(visitor));
        let value = self.value.as_ref().map(|value| value.visit_coherency(visitor));
        let eval_ty = match (ty, value) {
            (Some(a), Some(b)) => visitor.expect_eq(a, b, self.span()),
            (Some(a), None)    => a,
            (None,    Some(b)) => b,
            (None,    None)    => Ty::Inferred,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, ASTRef::Ref(self as &'n dyn ASTNode<'s>), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
#[ast_node]
pub struct ConstDecl<'s> {
    item_attrs: ItemAttrs<'s>,
    ident: Ident<'s>,
    ty: Option<Type<'s>>,
    value: Box<Expr<'s>>,
}

impl<'s> ConstDecl<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        let start = item_attrs.span().start;
        token::Let::parse(stream)?;
        let ident = Ident::parse(stream)?;
        let ty = if Colon::peek_and_parse(stream).is_some() {
            Some(Type::parse(stream)?)
        }
        else {
            None
        };
        token::Seq::parse(stream)?;
        let value = Expr::parse(stream)?.into();
        Ok(ConstDecl {
            item_attrs,
            ident,
            ty,
            value,
            span: start..stream.pos()
        })
    }
}

impl<'s> Parse<'s> for ConstDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for ConstDecl<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.as_ref().map(|ty| ty.visit_coherency(visitor));
        let value = self.value.visit_coherency(visitor);
        let eval_ty = match (ty, value) {
            (Some(a), b) => visitor.expect_eq(a, b, self.span()),
            (None,    b) => b,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, ASTRef::Ref(self as &'n dyn ASTNode<'s>), eval_ty, true), &self.span);
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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.ty.visit_coherency(visitor);
        let value = self.default_value.as_ref().map(|v| v.visit_coherency(visitor));
        let eval_ty = match (ty, value) {
            (a, Some(b)) => visitor.expect_eq(a, b, self.span()),
            (a, None)    => a,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, ASTRef::Ref(self as &'n dyn ASTNode<'s>), eval_ty, true), &self.span)
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
        let ret_ty = token::Arrow::peek_and_parse(stream)
            .map(|_| Type::parse(stream))
            .transpose()?;
        
        let body = token::FatArrow::peek_and_parse(stream)
            .map(|_| Expr::parse(stream).map(|e| e.into()))
            .or_else(||
                Braced::peek(stream)
                    .map(|_| Block::parse(stream).map(|e| Expr::Block(e).into()))
            )
            .transpose()?;
            
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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ret_ty = self.ret_ty.as_ref().map(|v| v.visit_coherency(visitor));
        visitor.push_scope(ScopeLevel::Function, ASTRef::Ref(self as &'n dyn ASTNode<'s>), ret_ty.clone());
        let param_tys = self.params.iter().map(|v| v.visit_coherency(visitor)).collect::<Vec<_>>();
        let body_ty = self.body.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.pop_scope(body_ty.unwrap_or(Ty::Inferred), ASTRef::Ref(self as &'n dyn ASTNode<'s>));
        if let Some(ref ident) = self.ident {
            let name = visitor.resolve_new(ident.path());
            visitor.try_push(
                Entity::new(
                    name, ASTRef::Ref(self as &'n dyn ASTNode<'s>), Ty::Function {
                        params: self.params.iter()
                            .zip(param_tys)
                            .map(|(p, ty)| (p.ident.value().clone(), ty))
                            .collect(),
                        ret_ty: ret_ty.unwrap_or(Ty::Inferred).into(),
                        decl: ASTRef::Ref(self as &'n dyn ASTNode<'s>)
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
pub struct FieldDecl<'s> {
    ident: Ident<'s>,
    ty: Type<'s>,
}

impl<'s> FieldDecl<'s> {
    pub fn requires_semicolon(&self) -> bool {
        true
    }
}

impl<'s> Parse<'s> for FieldDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let ident = stream.parse()?;
        token::Colon::parse(stream)?;
        let ty = stream.parse()?;
        Ok(Self { ident, ty, span: start..stream.pos() })
    }
}

#[derive(Debug)]
#[ast_node]
pub struct StructDecl<'s> {
    item_attrs: ItemAttrs<'s>,
    ident: Ident<'s>,
    fields: Vec<FieldDecl<'s>>,
}

impl<'s> StructDecl<'s> {
    pub fn parse_with<I: Iterator<Item = Token<'s>>>(
        item_attrs: ItemAttrs<'s>,
        stream: &mut TokenStream<'s, I>
    ) -> Result<Self, Message<'s>> {
        let start = item_attrs.span().start;
        token::Struct::parse(stream)?;
        let ident = stream.parse()?;
        let mut fields_stream = Braced::parse(stream)?.into_stream();
        let mut fields: Vec<FieldDecl<'s>> = vec![];
        while !fields_stream.eof() {
            fields.push(fields_stream.parse()?);
            if fields_stream.eof() {
                break;
            }
            if fields.last().unwrap().requires_semicolon() {
                token::Semicolon::parse(&mut fields_stream)?;
            }
            else {
                token::Semicolon::peek_and_parse(&mut fields_stream);
            }
        }
        Ok(Self {
            item_attrs,
            ident,
            fields,
            span: start..stream.pos()
        })
    }
}

impl<'s> Parse<'s> for StructDecl<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl<'s, 'n> Visitors<'s, 'n> for StructDecl<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let ty = self.value.visit_coherency(visitor);
        visitor.try_push(Ty::Alias {
            name: self.ident.to_string(),
            ty: ty.clone().into(),
            decl: ASTRef::Ref(self as &'n dyn ASTNode<'s>),
        }, self.span());
        ty
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Item<'s> {
    VarDecl(VarDecl<'s>),
    ConstDecl(ConstDecl<'s>),
    FunDecl(FunDecl<'s>),
    StructDecl(StructDecl<'s>),
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
        else if token::Let::peek(stream).is_some() {
            Ok(Self::ConstDecl(ConstDecl::parse_with(item_attrs, stream)?))
        }
        else if token::Fun::peek(stream).is_some() {
            Ok(Self::FunDecl(FunDecl::parse_with(item_attrs, stream)?))
        }
        else if token::Type::peek(stream).is_some() {
            Ok(Self::TypeAliasDecl(TypeAliasDecl::parse_with(item_attrs, stream)?))
        }
        else if token::Struct::peek(stream).is_some() {
            Ok(Self::StructDecl(StructDecl::parse_with(item_attrs, stream)?))
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
        token::Let::peek(stream).is_some() || 
        token::Fun::peek(stream).is_some() ||
        token::Struct::peek(stream).is_some() ||
        token::Class::peek(stream).is_some() ||
        token::Trait::peek(stream).is_some() ||
        token::Type::peek(stream).is_some() ||
        token::Public::peek(stream).is_some() ||
        token::Private::peek(stream).is_some() ||
        token::Extern::peek(stream).is_some()
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::FunDecl(f) => f.requires_semicolon(),
            Self::VarDecl(_) => true,
            Self::ConstDecl(_) => true,
            Self::StructDecl(_) => false,
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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::FunDecl(t) => t.visit_coherency(visitor),
            Self::VarDecl(t) => t.visit_coherency(visitor),
            Self::ConstDecl(t) => t.visit_coherency(visitor),
            Self::TypeAliasDecl(t) => t.visit_coherency(visitor),
            Self::StructDecl(t) => t.visit_coherency(visitor),
        }
    }
}

impl<'s> ASTNode<'s> for Item<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::VarDecl(t) => t.span(),
            Self::ConstDecl(t) => t.span(),
            Self::FunDecl(t) => t.span(),
            Self::StructDecl(t) => t.span(),
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
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
