
use std::sync::Arc;

use dash_macros::{ParseNode, ResolveNode};
use crate::{
    parser::{parse::{ParseNode, FatalParseError, RefToNode, NodePool, Node, Ref, NodeID, ParseRef}, tokenizer::TokenIterator},
    shared::src::Src,
    checker::{resolve::{ResolveNode, ResolveRef}, coherency::Checker, ty::Ty}
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
            res = Self::Optional(RefToNode::new(pool, res), q);
        }
        Ok(pool.add(res))
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        TypeAtom::peek(pos, tokenizer)
    }
}

impl ResolveNode for TypeExprNode {
    fn try_resolve_node(&mut self, pool: &NodePool, _: &mut Checker) -> Option<Ty> {
        match self {
            Self::Optional(opt, _) => Some(Ty::Option {
                ty: Box::new(opt.resolved_ty(pool))
            }),
            Self::Atom(atom) => Some(atom.resolved_ty(pool)),
        }
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "type")]
pub enum TypeAtomNode {
    TypeIdent(TypeIdent),
}

#[derive(Debug, ParseNode)]
pub struct TypeIdentNode {
    name: IdentPath,
}

impl ResolveNode for TypeIdentNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ty) = scope.types().find(&self.name.get(pool).to_path(pool)) {
                return Some(ty.clone());
            }
        }
        // self.cache.set_unresolved(
        //     format!("Unknown type {}", self.name.to_path()),
        //     self.name.span(pool)
        // );
        None
    }
}
