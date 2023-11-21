
use dash_macros::ast_node;

use crate::{shared::{logging::Message, src::Span}, compiler::ty::IdentPath};
use self::token::{Ident, Dicolon, Tokenize};
use super::{node::Parse, stream::{Token, TokenStream}};
use crate::compiler::ty::Ty;
use crate::parser::node::ASTNode;

#[derive(Debug)]
#[ast_node]
pub struct Path {
    components: Vec<Ident>,
    absolute: bool,
}

impl Path {
    pub fn path(&self) -> IdentPath {
        IdentPath::new(
            self.components.iter().map(|i| i.value().clone()).collect::<Vec<_>>(),
            self.absolute
        )
    }
}

impl Parse for Path {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let absolute = Dicolon::peek_and_parse(stream).is_some();
        let mut components = vec![];
        loop {
            components.push(stream.parse()?);
            if Dicolon::peek_and_parse(stream).is_none() {
                break;
            }
        }
        Ok(Self {
            components,
            absolute,
            span: start..stream.pos(),
            eval_ty: Ty::Unresolved,
        })
    }
}

pub mod token;
pub mod expr;
pub mod ty;
pub mod item;
pub mod ops;
pub mod flow;
