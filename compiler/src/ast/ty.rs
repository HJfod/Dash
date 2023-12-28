
use std::sync::Arc;

use dash_macros::Parse;
use crate::{parser::{parse::{Parse, FatalParseError, calculate_span}, tokenizer::{TokenIterator, Token}}, shared::src::{Src, ArcSpan}};
use super::{expr::IdentPath, token::op};

#[derive(Debug)]
pub enum TypeExpr {
    Optional(Box<TypeExpr>, op::Question),
    Atom(TypeAtom),
}

impl Parse for TypeExpr {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
        where I: Iterator<Item = Token<'s>>
    {
        let mut res = TypeExpr::Atom(Parse::parse(src.clone(), tokenizer)?);
        while let Some(q) = op::Question::peek_and_parse(src.clone(), tokenizer)? {
            res = TypeExpr::Optional(res.into(), q);
        }
        Ok(res)
    }

    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        TypeAtom::peek(pos, tokenizer)
    }

    fn span(&self) -> Option<ArcSpan> {
        match self {
            Self::Optional(ty, q) => calculate_span([ty.span(), q.span()]),
            Self::Atom(atom) => atom.span(),
        }
    }
}

#[derive(Debug, Parse)]
#[parse(expected = "type")]
pub enum TypeAtom {
    TypeIdent(TypeIdent),
}

#[derive(Debug, Parse)]
pub struct TypeIdent {
    name: IdentPath,
}
