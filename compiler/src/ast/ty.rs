
use std::sync::Arc;

use dash_macros::{Parse, Resolve};
use crate::{
    parser::{parse::{Parse, FatalParseError, calculate_span}, tokenizer::{TokenIterator, Token}},
    shared::src::{Src, ArcSpan},
    checker::{resolve::{Resolve, ResolveCache}, coherency::Checker, ty::Ty}
};
use super::{expr::IdentPath, token::op};

#[derive(Debug)]
pub enum TypeExpr {
    Optional(Box<TypeExpr>, op::Question, ResolveCache),
    Atom(TypeAtom),
}

impl Parse for TypeExpr {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
        where I: Iterator<Item = Token<'s>>
    {
        let mut res = TypeExpr::Atom(Parse::parse(src.clone(), tokenizer)?);
        while let Some(q) = op::Question::peek_and_parse(src.clone(), tokenizer)? {
            res = TypeExpr::Optional(res.into(), q, Default::default());
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
            Self::Optional(ty, q, _) => calculate_span([ty.span(), q.span()]),
            Self::Atom(atom) => atom.span(),
        }
    }
}

impl Resolve for TypeExpr {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        match self {
            TypeExpr::Optional(opt, _, _) => Some(Ty::Option {
                ty: Box::new(opt.try_resolve(checker)?)
            }),
            TypeExpr::Atom(atom) => atom.try_resolve(checker),
        }
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        match self {
            Self::Optional(_, _, cache) => Some(cache),
            Self::Atom(atom) => atom.cache(),
        }
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "type")]
pub enum TypeAtom {
    TypeIdent(TypeIdent),
}

#[derive(Debug, Parse)]
pub struct TypeIdent {
    name: IdentPath,
    #[parse(skip)]
    cache: ResolveCache,
}

impl Resolve for TypeIdent {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ty) = scope.types().find(
                &self.name.to_path(),
                checker.namespace_stack()
            ) {
                return Some(ty.clone());
            }
        }
        self.cache.set_unresolved(
            format!("Unknown type {}", self.name.to_path()),
            self.name.span()
        );
        None
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}
