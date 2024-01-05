
use std::sync::Arc;

use dash_macros::{ParseNode, ResolveNode};
use crate::{
    parser::{parse::{Separated, ParseNode, FatalParseError, ParseNodeFn, RefToNode, NodePool, Node, ParseRef, NodeID, Ref}, tokenizer::TokenIterator},
    shared::src::Src,
    checker::{resolve::ResolveNode, coherency::{Checker, ScopeID}, ty::Ty, path}
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

#[derive(Debug, ParseNode)]
pub struct IdentPathNode {
    absolute: Option<punct::Namespace>,
    path: Separated<IdentComponent, punct::Namespace>,
}

impl IdentPathNode {
    pub(crate) fn to_path(&self, list: &NodePool) -> path::IdentPath {
        path::IdentPath::new(
            self.path.iter().map(|i| path::Ident::from(match i.get(list).as_ref() {
                IdentComponentNode::Ident(i) => i.get(list).as_ref().to_string(),
                IdentComponentNode::Attribute(_, i) => format!("@{}", i.get(list).as_ref()),
            })).collect::<Vec<_>>(),
            self.absolute.is_some()
        )
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
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Self, FatalParseError> {
        let mut expr = Self::Scalar(ParseRef::parse_ref(list, src.clone(), tokenizer)?);
        loop {
            if delim::Parenthesized::<delim::P>::peek(0, tokenizer) {
                expr = Self::Call(
                    CallNode::parse_with(list.add(expr), list, src.clone(), tokenizer)?
                );
            }
            else if delim::Bracketed::<delim::P>::peek(0, tokenizer) {
                expr = Self::Index(
                    IndexNode::parse_with(list.add(expr), list, src.clone(), tokenizer)?
                );
            }
            else {
                break;
            }
        }
        Ok(expr)
    }
    fn parse_unop(
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Self, FatalParseError> {
        if op::Unary::peek(0, tokenizer) {
            Ok(Self::UnOp(
                UnOpNode::parse_with(
                    |list, src, tokenizer| {
                        let t = Self::parse_postfix(list, src, tokenizer)?;
                        Ok(list.add(t))
                    },
                    list, src, tokenizer
                )?
            ))
        }
        else {
            Self::parse_postfix(list, src, tokenizer)
        }
    }
    fn parse_binop_prec<'s, F>(
        prec: Prec, sides: &mut F,
        list: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>
    ) -> Result<Self, FatalParseError>
        where F: ParseNodeFn
    {
        let mut lhs = sides(list, src.clone(), tokenizer)?;
        while prec.peek(tokenizer) {
            lhs = Self::BinOp(
                BinOpNode::parse_with(
                    list.add(lhs),
                    |l, s, t| {
                        let t = sides(l, s, t)?;
                        Ok(l.add(t))
                    },
                    list, src.clone(), tokenizer
                )?
            );
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
    fn parse_node<'s>(list: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut sides: Box<dyn ParseNodeFn<'s, Self>> = Box::from(Self::parse_unop);
        for prec in Prec::order() {
            sides = Box::from(
                move |list: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>|
                    Self::parse_binop_prec(prec, &mut sides, list, src, tokenizer)
            );
        }
        sides(list, src, tokenizer)
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
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        let _handle = checker.enter_scope(&mut self.scope);
        self.exprs.iter_mut()
            .map(|(e, c)| (e.try_resolve(list, checker), c.has_semicolon()))
            .map(|(e, c)| e.map(|e| (e, c)))
            .collect::<Option<Vec<_>>>()?
            .into_iter()
            .last()
            .and_then(|(e, c)| (!c).then_some(e).or(Some(Ty::Void)))
    }
}
