
use std::{sync::Arc, collections::HashMap};
use dash_macros::ParseNode;
use crate::{
    parser::{parse::{FatalParseError, ParseNodeFn, SeparatedWithTrailing, NodePool, RefToNode, Node, ParseRef, NodeID}, tokenizer::TokenIterator},
    shared::{src::{Src, ArcSpan}, logger::{Message, Level, Note, LoggerRef}},
    checker::{resolve::{ResolveNode, ResolveRef}, coherency::Checker, ty::Ty, path}, ice
};
use super::{expr::Expr, token::{op, delim, Ident, punct}};

#[derive(Debug, ParseNode)]
#[parse(expected = "expression or named argument")]
pub enum ArgNode {
    Named(Ident, #[parse(peek_point)] punct::Colon, Expr),
    Unnamed(Expr),
}

impl ResolveNode for ArgNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug)]
pub struct CallNode {
    target: Expr,
    args: delim::Parenthesized<SeparatedWithTrailing<Arg, punct::Comma>>,
}
pub type Call = RefToNode<CallNode>;

impl CallNode {
    pub(crate) fn parse_with(
        target: Expr,
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError> {
        let res = Self {
            target,
            args: ParseRef::parse_ref(pool, src, tokenizer)?,
        };
        Ok(pool.add(res))
    }
}

impl Node for CallNode {
    fn children(&self) -> Vec<&dyn ResolveRef> {
        vec![&self.target, &self.args]
    }
}

impl ResolveNode for CallNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.resolved_ty(pool);
        let args = self.args.get(pool).value.iter()
            .map(|arg| match *arg.get(pool) {
                ArgNode::Unnamed(value) => {
                    (None, value.resolved_ty(pool), value.get(pool).span(pool))
                }
                ArgNode::Named(name, _, value) => {
                    (Some(name.get(pool).to_string()), value.resolved_ty(pool), value.get(pool).span(pool))
                }
            })
            .collect::<Vec<_>>();
        match target {
            Ty::Function { params, ret_ty } => {
                let mut arg_ix = 0usize;
                let mut encountered_named = None;
                let mut passed: HashMap<String, ArcSpan> = HashMap::new();
                for (name, ty, span) in &args {
                    arg_ix += 1;
                    if let Some(name) = name {
                        encountered_named = Some(span.clone());
                        match passed.get(name) {
                            Some(old) => {
                                checker.logger().lock().unwrap().log(Message::new(
                                    Level::Error,
                                    format!("Parameter '{name}' has already been passed"),
                                    span.clone().unwrap_or(ArcSpan::builtin()).as_ref()
                                ).note(Note::new_at(
                                    "Previous passing here",
                                    old.as_ref()
                                )));
                            }
                            None => {
                                match params.iter().find(|p| p.0.as_ref() == Some(name)) {
                                    Some((_, pty)) => {
                                        checker.expect_ty_eq(ty.clone(), pty.clone(), span.clone());
                                    }
                                    None => {
                                        checker.logger().lock().unwrap().log(Message::new(
                                            Level::Error,
                                            format!("Unknown parameter '{name}'"),
                                            span.clone().unwrap_or(ArcSpan::builtin()).as_ref()
                                        ));
                                    }
                                }
                                passed.insert(name.clone(), span.clone().unwrap_or(ArcSpan::builtin()));
                            }
                        }
                    }
                    else {
                        match encountered_named.clone() {
                            Some(e_span) => {
                                checker.logger().lock().unwrap().log(Message::new(
                                    Level::Error,
                                    "Cannot pass positional arguments after named arguments \
                                    have been passed",
                                    span.clone().unwrap_or(ArcSpan::builtin()).as_ref()
                                ).note(Note::hint(
                                    "Move this named argument to the end of the arguments pool",
                                    e_span.unwrap_or(ArcSpan::builtin()).as_ref()
                                )));
                            }
                            None => {
                                match params.get(arg_ix) {
                                    Some((name, pty)) => {
                                        if let Some(name) = name {
                                            passed.insert(name.clone(), span.clone().unwrap_or(ArcSpan::builtin()));
                                        }
                                        checker.expect_ty_eq(ty.clone(), pty.clone(), span.clone());
                                    }
                                    None => {
                                        checker.logger().lock().unwrap().log(Message::new(
                                            Level::Error,
                                            "Too many positional arguments",
                                            span.clone().unwrap_or(ArcSpan::builtin()).as_ref()
                                        ).note(Note::new(format!(
                                            "Function has only {} parameters, but {} were passed",
                                            params.len(), args.len()
                                        ), false)));
                                    }
                                }
                            }
                        }
                    }
                }
                if arg_ix < params.len() {
                    checker.logger().lock().unwrap().log(Message::new(
                        Level::Error,
                        "Missing arguments",
                        self.span(pool).unwrap_or(ArcSpan::builtin()).as_ref()
                    ).note(Note::new(format!(
                        "Function has {} parameters, but only {} were passed",
                        params.len(), args.len()
                    ), false)));
                }
                Some(ret_ty.as_ref().clone())
            }
            other => {
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Cannot call an expression of type {other}"),
                    self.span(pool).unwrap_or(ArcSpan::builtin()).as_ref()
                ));
                Some(Ty::Invalid)
            }
        }
    }
}

#[derive(Debug)]
pub struct IndexNode {
    target: Expr,
    index: delim::Bracketed<Expr>,
    trailing_comma: Option<punct::Comma>,
}
pub type Index = RefToNode<IndexNode>;

impl IndexNode {
    pub(crate) fn parse_with(
        target: Expr,
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError> {
        let res = Self {
            target,
            index: ParseRef::parse_ref(pool, src.clone(), tokenizer)?,
            trailing_comma: ParseRef::parse_ref(pool, src.clone(), tokenizer)?,
        };
        Ok(pool.add(res))
    }
}

impl Node for IndexNode {
    fn children(&self) -> Vec<&dyn ResolveRef> {
        vec![&self.target, &self.index, &self.trailing_comma]
    }
}

impl ResolveNode for IndexNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug)]
pub struct UnOpNode {
    op: op::Unary,
    target: Expr,
}
pub type UnOp = RefToNode<UnOpNode>;

impl UnOpNode {
    pub(crate) fn parse_with<F>(
        mut target: F,
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError>
        where F: ParseNodeFn
    {
        let res = Self {
            op: ParseRef::parse_ref(pool, src.clone(), tokenizer)?,
            target: RefToNode::new_raw(target(pool, src, tokenizer)?),
        };
        Ok(pool.add(res))
    }
}

impl Node for UnOpNode {
    fn children(&self) -> Vec<&dyn ResolveRef> {
        vec![&self.op, &self.target]
    }
}

impl ResolveNode for UnOpNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.resolved_ty(pool);
        let op = self.op.get(pool);
        if target.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            let name = path::IdentPath::new([path::Ident::UnOp(op.op(), target.clone())], false);
            if let Some(fun) = scope.entities().find(&name) {
                match fun.ty() {
                    Ty::Function { params: _, ret_ty } => return Some(ret_ty.as_ref().clone()),
                    _ => ice!(
                        "encountered entity with unop name '{name}' \
                        that wasn't a function type, but {}",
                        fun.ty()
                    )
                }
            }
        }
        None
    }
    fn log_unresolved_reason(&self, pool: &NodePool, _checker: &Checker, logger: LoggerRef) {
        logger.lock().unwrap().log(Message::new(
            Level::Error,
            format!(
                "Cannot use operator '{}' on type {}",
                self.op.get(pool).op(), self.target.resolved_ty(pool)
            ),
            self.span_or_builtin(pool).as_ref()
        ))
    }
}

#[derive(Debug)]
pub struct BinOpNode {
    lhs: Expr,
    op: op::Binary,
    rhs: Expr,
}
pub type BinOp = RefToNode<BinOpNode>;

impl BinOpNode {
    pub(crate) fn parse_with<F>(
        lhs: Expr,
        mut rhs: F,
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError>
        where F: ParseNodeFn
    {
        let res = Self {
            lhs,
            op: ParseRef::parse_ref(pool, src.clone(), tokenizer)?,
            rhs: RefToNode::new_raw(rhs(pool, src, tokenizer)?),
        };
        Ok(pool.add(res))
    }
}

impl Node for BinOpNode {
    fn children(&self) -> Vec<&dyn ResolveRef> {
        vec![&self.lhs, &self.op, &self.rhs]
    }
}

impl ResolveNode for BinOpNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let a = self.lhs.resolved_ty(pool);
        let b = self.rhs.resolved_ty(pool);
        let op = self.op.get(pool);
        if a.is_unreal() || b.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            // todo: handle symmetrive ops, like a + b <=> b + a
            // todo: synthesize ops, like a == b <=> a != b
            let name = path::IdentPath::new([
                path::Ident::BinOp(a.clone(), op.op(), b.clone())
            ], false);
            if let Some(fun) = scope.entities().find(&name) {
                match fun.ty() {
                    Ty::Function { params: _, ret_ty } => return Some(ret_ty.as_ref().clone()),
                    _ => ice!(
                        "encountered entity with binop name '{name}' \
                        that wasn't a function type, but {}",
                        fun.ty()
                    )
                }
            }
        }
        None
    }
    fn log_unresolved_reason(&self, pool: &NodePool, _checker: &Checker, logger: LoggerRef) {
        logger.lock().unwrap().log(Message::new(
            Level::Error,
            format!(
                "Cannot use operator '{}' on types {} and {}",
                self.op.get(pool).op(),
                self.lhs.resolved_ty(pool),
                self.rhs.resolved_ty(pool),
            ),
            self.span_or_builtin(pool).as_ref()
        ))
    }
}
