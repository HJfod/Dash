
use std::sync::Arc;

use dash_macros::{ParseNode, ResolveNode};
use crate::{
    parser::{parse::{Separated, ParseNode, FatalParseError, ParseNodeFn, RefToNode, NodePool, Node, ParseRef, NodeID, Ref}, tokenizer::TokenIterator},
    shared::src::Src,
    checker::{resolve::{ResolveNode, ResolveRef}, coherency::{Checker, ScopeID}, ty::Ty, path}
};
use super::{
    decl::Decl,
    token::{Ident, punct::{self, TerminatingSemicolon}, op::{Prec, self}, delim},
    atom::Atom,
    flow::Flow,
    ops::{BinOp, UnOp, Call, Index, CallNode, IndexNode, UnOpNode, BinOpNode}
};

#[derive(Debug, ParseNode)]
#[parse(expected = "identifier")]
pub enum IdentComponentNode {
    Attribute(punct::At, Ident),
    Ident(Ident),
}

impl ResolveNode for IdentComponentNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug, ParseNode)]
pub struct IdentPathNode {
    absolute: Option<punct::Namespace>,
    path: Separated<IdentComponent, punct::Namespace>,
}

impl IdentPathNode {
    pub(crate) fn to_path(&self, pool: &NodePool) -> path::IdentPath {
        path::IdentPath::new(
            self.path.iter().map(|i| path::Ident::from(match *i.get(pool) {
                IdentComponentNode::Ident(i) => i.get(pool).to_string(),
                IdentComponentNode::Attribute(_, i) => format!("@{}", i.get(pool)),
            })).collect::<Vec<_>>(),
            self.absolute.is_some()
        )
    }
}

impl ResolveNode for IdentPathNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "expression")]
pub enum ScalarExprNode {
    Decl(Decl),
    Flow(Flow),
    Atom(Atom),
}

#[derive(Debug, ResolveNode)]
pub enum ExprNode {
    BinOp(BinOp),
    UnOp(UnOp),
    Call(Call),
    Index(Index),
    Scalar(ScalarExpr),
}
pub type Expr = RefToNode<ExprNode>;

impl ExprNode {
    fn parse_postfix(
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError> {
        let mut expr = Self::Scalar(ParseRef::parse_ref(pool, src.clone(), tokenizer)?);
        loop {
            if delim::Parenthesized::<delim::P>::peek(0, tokenizer) {
                expr = Self::Call(RefToNode::new_raw(
                    CallNode::parse_with(RefToNode::new_raw(pool.add(expr)), pool, src.clone(), tokenizer)?
                ));
            }
            else if delim::Bracketed::<delim::P>::peek(0, tokenizer) {
                expr = Self::Index(RefToNode::new_raw(
                    IndexNode::parse_with(RefToNode::new_raw(pool.add(expr)), pool, src.clone(), tokenizer)?
                ));
            }
            else {
                break;
            }
        }
        Ok(pool.add(expr))
    }
    fn parse_unop(
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError> {
        if op::Unary::peek(0, tokenizer) {
            let res = Self::UnOp(RefToNode::new_raw(
                UnOpNode::parse_with(Self::parse_postfix, pool, src, tokenizer)?
            ));
            Ok(pool.add(res))
        }
        else {
            Self::parse_postfix(pool, src, tokenizer)
        }
    }
    fn parse_binop_prec<F>(
        prec: Prec, sides: &mut F,
        pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError>
        where F: ParseNodeFn
    {
        let mut lhs = sides(pool, src.clone(), tokenizer)?;
        while prec.peek(tokenizer) {
            let bop = Self::BinOp(RefToNode::new_raw(
                BinOpNode::parse_with(
                    RefToNode::new_raw(lhs),
                    &mut *sides, pool, src.clone(), tokenizer
                )?
            ));
            lhs = pool.add(bop);
        }
        Ok(lhs)
    }
}

impl Node for ExprNode {
    fn children(&self) -> Vec<NodeID> {
        match self {
            Self::BinOp(binop) => binop.ids(),
            Self::UnOp(unop) => unop.ids(),
            Self::Call(call) => call.ids(),
            Self::Index(index) => index.ids(),
            Self::Scalar(scalar) => scalar.ids(),
        }
    }
}

impl ParseNode for ExprNode {
    fn parse_node(
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError> {
        let mut sides: Box<dyn ParseNodeFn> = Box::from(Self::parse_unop);
        for prec in Prec::order() {
            sides = Box::from(
                move |pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator|
                    Self::parse_binop_prec(prec, &mut sides, pool, src, tokenizer)
            );
        }
        sides(pool, src, tokenizer)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        ScalarExpr::peek(pos, tokenizer)
    }
}

#[derive(Debug, ParseNode)]
pub struct ExprListNode {
    exprs: Vec<(Expr, TerminatingSemicolon)>,
    #[parse(skip)]
    scope: Option<ScopeID>,
}

impl ResolveNode for ExprListNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let _handle = checker.enter_scope(&mut self.scope);
        self.exprs.iter_mut()
            .map(|(e, c)| (e.resolved_ty(pool), c.get(pool).has_semicolon()))
            .collect::<Vec<_>>()
            .into_iter()
            .last()
            .and_then(|(e, c)| (!c).then_some(e).or(Some(Ty::Void)))
    }
}
