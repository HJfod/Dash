
use geo_macros::ast_node;

use crate::{
    parser::{
        stream::{TokenStream, Token},
        node::{Parse, ASTNode}
    },
    shared::{logging::{Message, Level, Note}, src::Span},
    compiler::typecheck::{TypeCheck, TypeChecker, Ty, FindItem}
};

use super::token::Ident;

#[derive(Debug)]
pub enum Type<'s> {
    TypeName(TypeName<'s>),
}

impl<'s> Parse<'s> for Type<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Ok(Type::TypeName(TypeName::parse(stream)?))
    }
}

impl<'s> ASTNode<'s> for Type<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::TypeName(t) => t.span(),
        }
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Type<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::TypeName(t) => t.typecheck(checker),
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
        Ok(TypeName { ident, span: Span::new(stream.src(), start, stream.pos()) })
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for TypeName<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        let path = self.ident.path();
        match checker.find::<Ty, _>(&path) {
            FindItem::Some(e) => e.clone(),
            FindItem::NotAvailable(e) => {
                checker.emit_msg(Message::from_span(
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
                checker.emit_msg(Message::from_span(
                    Level::Error,
                    format!("Unknown type '{path}'"),
                    self.ident.span(),
                ));
                Ty::Invalid
            }
        }
    }
}
