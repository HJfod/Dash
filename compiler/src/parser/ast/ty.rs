
use crate::{
    parser::{
        stream::TokenStream,
        node::{Parse, ASTNode, Span}
    },
    shared::logging::Message,
    compiler::typecheck::{TypeCheck, TypeChecker, Ty}
};

use super::token::Ident;

#[derive(Debug)]
pub enum Type<'s> {
    TypeName(TypeName<'s>),
}

impl<'s> Parse<'s> for Type<'s> {
    fn parse_impl<S: TokenStream<'s>>(stream: &mut S) -> Result<Self, Message<'s>> {
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
pub struct TypeName<'s> {
    ident: Ident<'s>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for TypeName<'s> {
    fn parse_impl<S: TokenStream<'s>>(stream: &mut S) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let ident = Ident::parse(stream)?;
        Ok(TypeName { ident, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for TypeName<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for TypeName<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        todo!()
    }
}
