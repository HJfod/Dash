
use gdml_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode}, ast::token::VoidLit
    },
    shared::{logging::{Message, Level, Note}, src::Span},
    compiler::{typecheck::{TypeVisitor, Ty, FindItem}, visitor::Visitors}
};

use super::token::{Ident, Tokenize};

#[derive(Debug)]
pub enum Type<'s> {
    Void(Span<'s>),
    TypeName(TypeName<'s>),
}

impl<'s> Parse<'s> for Type<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        if let Some(tk) = VoidLit::peek_and_parse(stream) {
            Ok(Type::Void(tk.span().clone()))
        }
        else {
            Ok(Type::TypeName(TypeName::parse(stream)?))
        }
    }
}

impl<'s> ASTNode<'s> for Type<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Void(t) => t,
            Self::TypeName(t) => t.span(),
        }
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Type<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::Void(_) => Ty::Void,
            Self::TypeName(t) => t.visit_coherency(visitor),
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct TypeName<'s> {
    ident: Ident<'s>,
}

impl<'s> Parse<'s> for TypeName<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let ident = Ident::parse(stream)?;
        Ok(TypeName { ident, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for TypeName<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        let path = self.ident.path();
        match visitor.find::<Ty, _>(&path) {
            FindItem::Some(e) => e.clone(),
            FindItem::NotAvailable(e) => {
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
            FindItem::None => {
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
