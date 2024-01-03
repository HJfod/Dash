
use std::sync::Arc;

use dash_macros::{Parse, Resolve};
use crate::{
    parser::{parse::{Parse, FatalParseError, calculate_span, RefToNode, NodeList, Node}, tokenizer::TokenIterator},
    shared::src::{Src, ArcSpan},
    checker::{resolve::Resolve, coherency::Checker, ty::Ty}
};
use super::{expr::IdentPath, token::op};

#[derive(Debug)]
pub enum TypeExprItem {
    Optional(TypeExpr, op::Question),
    Atom(TypeAtom),
}
pub type TypeExpr = RefToNode<TypeExprItem>;

impl Node for TypeExprItem {
    fn span(&self) -> Option<ArcSpan> {
        match self {
            Self::Optional(ty, q) => calculate_span([ty.span(), q.span()]),
            Self::Atom(atom) => atom.span(),
        }
    }
}

impl Parse for TypeExprItem {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut res = Self::Atom(Parse::parse(list, src.clone(), tokenizer)?);
        while let Some(q) = op::Question::peek_and_parse(list, src.clone(), tokenizer)? {
            res = Self::Optional(list.add(res), q);
        }
        Ok(res)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        TypeAtom::peek(pos, tokenizer)
    }
}

impl Resolve for TypeExprItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        match self {
            Self::Optional(opt, _) => Some(Ty::Option {
                ty: Box::new(opt.try_resolve(list, checker)?)
            }),
            Self::Atom(atom) => atom.try_resolve(list, checker),
        }
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "type")]
pub enum TypeAtomItem {
    TypeIdent(TypeIdent),
}

#[derive(Debug, Parse)]
pub struct TypeIdentItem {
    name: IdentPath,
}

impl Resolve for TypeIdentItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ty) = scope.types().find(&self.name.get(list).as_ref().to_path(list)) {
                return Some(ty.clone());
            }
        }
        // self.cache.set_unresolved(
        //     format!("Unknown type {}", self.name.to_path()),
        //     self.name.span()
        // );
        None
    }
}
