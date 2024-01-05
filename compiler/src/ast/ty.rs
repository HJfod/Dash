
use std::sync::Arc;

use dash_macros::{ParseNode, Resolve};
use crate::{
    parser::{parse::{ParseNode, FatalParseError, calculate_span, RefToNode, NodePool, Node, Ref, NodeID, ParseRef}, tokenizer::TokenIterator},
    shared::src::{Src, ArcSpan},
    checker::{resolve::ResolveNode, coherency::Checker, ty::Ty}
};
use super::{expr::IdentPath, token::op};

#[derive(Debug)]
pub enum TypeExprNode {
    Optional(TypeExpr, op::Question),
    Atom(TypeAtom),
}
pub type TypeExpr = RefToNode<TypeExprNode>;

impl Node for TypeExprNode {
    fn children(&self) -> Vec<crate::parser::parse::NodeID> {
        match self {
            Self::Optional(ty, q) => ty.ids().into_iter().chain(q.ids()).collect(),
            Self::Atom(atom) => atom.ids(),
        }
    }
}

impl ParseNode for TypeExprNode {
    fn parse_node(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<NodeID, FatalParseError> {
        let mut res = Self::Atom(ParseRef::parse_ref(pool, src.clone(), tokenizer)?);
        while let Some(q) = op::Question::peek_and_parse(pool, src.clone(), tokenizer)? {
            res = Self::Optional(pool.add(res), q);
        }
        Ok(res)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        TypeAtom::peek(pos, tokenizer)
    }
}

impl ResolveNode for TypeExprNode {
    fn try_resolve(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        match self {
            Self::Optional(opt, _) => Some(Ty::Option {
                ty: Box::new(opt.try_resolve(list, checker)?)
            }),
            Self::Atom(atom) => atom.try_resolve(list, checker),
        }
    }
}

#[derive(Debug, ParseNode, Resolve)]
#[parse(expected = "type")]
pub enum TypeAtomNode {
    TypeIdent(TypeIdent),
}

#[derive(Debug, ParseNode)]
pub struct TypeIdentNode {
    name: IdentPath,
}

impl ResolveNode for TypeIdentNode {
    fn try_resolve(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ty) = scope.types().find(&self.name.get(list).as_ref().to_path(list)) {
                return Some(ty.clone());
            }
        }
        // self.cache.set_unresolved(
        //     format!("Unknown type {}", self.name.to_path()),
        //     self.name.span(list)
        // );
        None
    }
}
