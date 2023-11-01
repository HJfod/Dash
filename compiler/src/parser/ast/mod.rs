
use gdml_macros::ast_node;

use crate::{shared::{logging::Message, src::Span}, compiler::typecheck};
use self::token::{Ident, Dicolon, Tokenize};
use super::{node::Parse, stream::{Token, TokenStream}};

fn if_then_some<'s, R, F>(t: bool, then: F) -> Result<Option<R>, Message<'s>>
    where
        R: Parse<'s>,
        F: FnOnce() -> Result<R, Message<'s>>
{
    if t {
        Ok(Some(then()?))
    }
    else {
        Ok(None)
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Path<'s> {
    components: Vec<Ident<'s>>,
    absolute: bool,
}

impl<'s> Path<'s> {
    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new(
            self.components.iter().map(|i| i.value().clone()).collect::<Vec<_>>(),
            self.absolute
        )
    }
}

impl<'s> Parse<'s> for Path<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let absolute = Dicolon::peek_and_parse(stream).is_some();
        let mut components = vec![];
        loop {
            components.push(stream.parse()?);
            if Dicolon::peek_and_parse(stream).is_none() {
                break;
            }
        }
        Ok(Self { components, absolute, span: start..stream.pos() })
    }
}

pub mod token;
pub mod expr;
pub mod ty;
pub mod item;
pub mod ops;
pub mod flow;
