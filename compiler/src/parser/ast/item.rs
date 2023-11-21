
use dash_macros::{ast_node, impl_opaque};

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
        node::{Parse, ASTNode}
    },
    shared::{logging::{Message, Level}, is_none_or::IsNoneOr, src::Span},
    compiler::{ty::Ty, visitor::TakeVisitor, coherency::{CoherencyVisitor, ScopeLevel}, entity::Entity}
};
use super::{ty::Type, expr::{Expr, Visibility, Block}, token::{Ident, Parenthesized, Braced, self, Colon, Tokenize}, Path};

#[derive(Debug)]
#[ast_node]
pub struct ItemAttrs {
    visibility: Visibility,
    is_extern: Option<token::Extern>,
}

impl Parse for ItemAttrs {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        Ok(Self {
            visibility: stream.parse()?,
            is_extern: token::Extern::peek_and_parse(stream),
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

#[derive(Debug)]
#[ast_node]
pub struct VarDecl {
    item_attrs: ItemAttrs,
    ident: Ident,
    ty: Option<Type>,
    value: Option<Box<Expr>>,
}

impl VarDecl {
    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
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
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for VarDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for VarDecl {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let ty = self.ty.as_ref().map(|ty| ty.visit_coherency(visitor));
        let value = self.value.as_ref().map(|value| value.visit_coherency(visitor));
        let eval_ty = match (ty, value) {
            (Some(a), Some(b)) => visitor.expect_eq(a, b, self.span()),
            (Some(a), None)    => a,
            (None,    Some(b)) => b,
            (None,    None)    => Ty::Unresolved,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, self.into(), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
#[ast_node]
pub struct ConstDecl {
    item_attrs: ItemAttrs,
    ident: Ident,
    ty: Option<Type>,
    value: Box<Expr>,
}

impl ConstDecl {
    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
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
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for ConstDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for ConstDecl {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let ty = self.ty.as_ref().map(|ty| ty.visit_coherency(visitor));
        let value = self.value.visit_coherency(visitor);
        let eval_ty = match (ty, value) {
            (Some(a), b) => visitor.expect_eq(a, b, self.span()),
            (None,    b) => b,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, self.into(), eval_ty, true), &self.span);
        Ty::Void
    }
}

#[derive(Debug)]
#[ast_node]
pub struct FunParam {
    ident: Ident,
    ty: Type,
    default_value: Option<Expr>,
}

impl Parse for FunParam {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
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
        Ok(FunParam {
            ident,
            ty,
            default_value,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl TakeVisitor<CoherencyVisitor> for FunParam {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let ty = self.ty.visit_coherency(visitor);
        let value = self.default_value.as_ref().map(|v| v.visit_coherency(visitor));
        let eval_ty = match (ty, value) {
            (a, Some(b)) => visitor.expect_eq(a, b, self.span()),
            (a, None)    => a,
        };
        let name = visitor.resolve_new(self.ident.path());
        visitor.try_push(Entity::new(name, self.into(), eval_ty, true), &self.span)
            .map(|t| t.ty())
            .unwrap_or(Ty::Invalid)
    }
}

#[derive(Debug)]
#[ast_node]
pub struct FunDecl {
    item_attrs: ItemAttrs,
    ident: Option<Ident>,
    params: Vec<FunParam>,
    ret_ty: Option<Type>,
    body: Option<Box<Expr>>,
}

impl FunDecl {
    pub fn requires_semicolon(&self) -> bool {
        self.body.as_ref().is_none_or(|b| b.requires_semicolon())
    }

    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
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
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for FunDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for FunParam {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let ret_ty = self.ret_ty.as_ref().map(|v| v.visit_coherency(visitor));
        visitor.push_scope(ScopeLevel::Function, self.into(), ret_ty.clone());
        let param_tys = self.params.iter().map(|v| v.visit_coherency(visitor)).collect::<Vec<_>>();
        let body_ty = self.body.as_ref().map(|v| v.visit_coherency(visitor)).flatten();
        visitor.pop_scope(body_ty.unwrap_or(Ty::Unresolved), self.into());
        if let Some(ref ident) = self.ident {
            let name = visitor.resolve_new(ident.path());
            visitor.try_push(
                Entity::new(
                    name, self.into(), Ty::Function {
                        params: self.params.iter()
                            .zip(param_tys)
                            .map(|(p, ty)| (p.ident.value().clone(), ty))
                            .collect(),
                        ret_ty: ret_ty.unwrap_or(Ty::Unresolved).into(),
                        decl: self.into()
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
pub struct FieldDecl {
    ident: Ident,
    ty: Type,
}

impl FieldDecl {
    pub fn requires_semicolon(&self) -> bool {
        true
    }
}

impl Parse for FieldDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let ident = stream.parse()?;
        token::Colon::parse(stream)?;
        let ty = stream.parse()?;
        Ok(Self {
            ident,
            ty,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

#[derive(Debug)]
#[ast_node]
pub struct StructDecl {
    item_attrs: ItemAttrs,
    ident: Ident,
    fields: Vec<FieldDecl>,
}

impl StructDecl {
    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
        let start = item_attrs.span().start;
        token::Struct::parse(stream)?;
        let ident = stream.parse()?;
        let mut fields_stream = Braced::parse(stream)?.into_stream();
        let mut fields: Vec<FieldDecl> = vec![];
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
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for StructDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for StructDecl {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        todo!()
    }
}

#[derive(Debug)]
#[ast_node]
pub struct TypeAliasDecl {
    item_attrs: ItemAttrs,
    ident: Ident,
    value: Type,
}

impl TypeAliasDecl {
    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
        let start = item_attrs.span().start;
        token::Type::parse(stream)?;
        let ident = stream.parse()?;
        token::Seq::parse(stream)?;
        let value = stream.parse()?;
        Ok(TypeAliasDecl {
            item_attrs,
            ident,
            value,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl Parse for TypeAliasDecl {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for TypeAliasDecl {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let ty = self.value.visit_coherency(visitor);
        visitor.try_push(Ty::Alias {
            name: self.ident.to_string(),
            ty: ty.clone().into(),
            decl: self.into(),
        }, self.span());
        ty
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
#[impl_opaque {
    impl ASTNode {
        fn span(&self) -> &Span:
            e => e.span();
        fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode>:
            e => e.iter_children();
        fn eval_ty(&self) -> Ty:
            e => e.eval_ty();
    }

    impl TakeVisitor<CoherencyVisitor> {
        fn take_visitor(&mut self, visitor: &mut CoherencyVisitor):
            e => e.take_visitor(visitor);
    }
}]
pub enum Item {
    VarDecl(VarDecl),
    ConstDecl(ConstDecl),
    FunDecl(FunDecl),
    StructDecl(StructDecl),
    TypeAliasDecl(TypeAliasDecl),
}

impl Item {
    pub fn parse_with<I: Iterator<Item = Token>>(
        item_attrs: ItemAttrs,
        stream: &mut TokenStream<I>
    ) -> Result<Self, Message> {
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

    pub fn peek<I: Iterator<Item = Token>>(stream: &TokenStream<I>) -> bool {
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

impl Parse for Item {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_with(stream.parse()?, stream)
    }
}

#[derive(Debug)]
#[ast_node]
pub struct UsingItem {
    path: Path,
}

impl Parse for UsingItem {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        token::Using::parse(stream)?;
        let path = stream.parse()?;
        Ok(Self {
            path,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl TakeVisitor<CoherencyVisitor> for UsingItem {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        todo!()
    }
}
