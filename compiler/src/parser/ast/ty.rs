
use dash_macros::{ast_node, impl_opaque};

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode, ASTRef}, ast::token::VoidLit
    },
    shared::{logging::{Message, Level, Note}, src::Span},
    compiler::{ty::Ty, coherency::{CoherencyVisitor, FoundItem}, visitor::TakeVisitor}
};

use super::token::{Ident, Tokenize};

#[derive(Debug)]
#[impl_opaque {
    impl ASTNode {
        fn span(&self) -> &Span:
            ..e => e.span();
        fn children(&mut self) -> Vec<ASTRef>:
            ..e => e.children();
        fn eval_ty(&self) -> Ty:
            ..e => e.eval_ty();
    }
}]
pub enum Type {
    Void(Span),
    TypeName(TypeName),
}

impl Parse for Type {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        if let Some(tk) = VoidLit::peek_and_parse(stream) {
            Ok(Type::Void(tk.span().clone()))
        }
        else {
            Ok(Type::TypeName(TypeName::parse(stream)?))
        }
    }
}

impl TakeVisitor<CoherencyVisitor> for Type {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        match self {
            Self::Void(_) => Ty::Void,
            Self::TypeName(t) => t.visit_coherency(visitor),
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct TypeName {
    ident: Ident,
}

impl Parse for TypeName {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let ident = Ident::parse(stream)?;
        Ok(TypeName {
            ident,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

impl TakeVisitor<CoherencyVisitor> for TypeName {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        let path = self.ident.path();
        match visitor.find::<Ty, _>(&path) {
            FoundItem::Some(e) => e.clone(),
            FoundItem::NotAvailable(e) => {
                visitor.emit_msg(Message::from_span(
                    Level::Error,
                    format!("Type '{path}' can not be used here"),
                    self.ident.span(),
                ).note(Note::from_span(
                    format!("'{path}' declared here"),
                    e.decl().span()
                )));
                Ty::Invalid
            }
            FoundItem::None => {
                visitor.emit_msg(Message::from_span(
                    Level::Error,
                    format!("Unknown type '{path}'"),
                    self.ident.span(),
                ));
                Ty::Invalid
            }
        }
    }
}
