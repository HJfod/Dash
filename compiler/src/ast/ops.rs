
use std::{sync::Arc, collections::HashMap};
use dash_macros::ParseNode;
use crate::{
    parser::{parse::{ParseNode, FatalParseError, ParseNodeFn, SeparatedWithTrailing, NodePool, RefToNode, Node, ParseRef, NodeID, Ref}, tokenizer::TokenIterator},
    shared::{src::{Src, ArcSpan}, logger::{Message, Level, Note}},
    checker::{resolve::ResolveNode, coherency::Checker, ty::Ty, path}, ice
};
use super::{expr::Expr, token::{op, delim, Ident, punct}};

#[derive(Debug, ParseNode)]
#[parse(expected = "expression or named argument")]
pub enum ArgNode {
    Named(Ident, #[parse(peek_point)] punct::Colon, Expr),
    Unnamed(Expr),
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
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Call, FatalParseError> {
        let res = Self {
            target,
            args: ParseRef::parse_ref(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for CallNode {
    fn children(&self) -> Vec<NodeID> {
        self.target.ids().into_iter().chain(self.args.ids().into_iter()).collect()
    }
}

impl ResolveNode for CallNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(list, checker)?;
        let args = self.args.get(list).as_mut().value.iter_mut()
            .map(|arg| match arg.get(list).as_mut() {
                ArgNode::Unnamed(value) => {
                    (None, value.try_resolve(list, checker), value.span(list))
                }
                ArgNode::Named(name, _, value) => {
                    (Some(name.get(list).as_ref().to_string()), value.try_resolve(list, checker), value.span(list))
                }
            })
            .map(|(name, expr, span)| expr.map(|e| (name, e, span)))
            .collect::<Option<Vec<_>>>()?;
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
                                    "Move this named argument to the end of the arguments list",
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
                        self.span(list).unwrap_or(ArcSpan::builtin()).as_ref()
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
                    self.span(list).unwrap_or(ArcSpan::builtin()).as_ref()
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
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Index, FatalParseError> {
        let res = Self {
            target,
            index: ParseRef::parse_ref(list, src.clone(), tokenizer)?,
            trailing_comma: ParseRef::parse_ref(list, src.clone(), tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for IndexNode {
    fn children(&self) -> Vec<NodeID> {
        self.target.ids().into_iter()
            .chain(self.index.ids())
            .chain(self.trailing_comma.ids())
            .collect()
    }
}

impl ResolveNode for IndexNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
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
    pub(crate) fn parse_with<'s, F>(
        mut target: F,
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s>
    ) -> Result<UnOp, FatalParseError>
        where F: ParseNodeFn
    {
        let res = Self {
            op: ParseRef::parse_ref(list, src.clone(), tokenizer)?,
            target: target(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for UnOpNode {
    fn children(&self) -> Vec<NodeID> {
        self.op.ids().into_iter()
            .chain(self.target.ids())
            .collect()
    }
}

impl ResolveNode for UnOpNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(list, checker)?;
        let op = self.op.get(list);
        if target.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            let name = path::IdentPath::new([path::Ident::UnOp(op.as_ref().op(), target.clone())], false);
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
        // self.cache.set_unresolved(
        //     format!("Cannot use operator '{op}' on type {target}"),
        //     self.span(list)
        // );
        None
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
    pub(crate) fn parse_with<'s, F>(
        lhs: Expr,
        mut rhs: F,
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s>
    ) -> Result<BinOp, FatalParseError>
        where F: ParseNodeFn
    {
        let res = Self {
            lhs,
            op: ParseRef::parse_ref(list, src.clone(), tokenizer)?,
            rhs: rhs(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for BinOpNode {
    fn children(&self) -> Vec<NodeID> {
        self.lhs.ids().into_iter()
            .chain(self.op.ids())
            .chain(self.rhs.ids())
    }
}

impl ResolveNode for BinOpNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        let a = self.lhs.try_resolve(list, checker)?;
        let b = self.rhs.try_resolve(list, checker)?;
        let op = self.op.get(list);
        if a.is_unreal() || b.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            // todo: handle symmetrive ops, like a + b <=> b + a
            // todo: synthesize ops, like a == b <=> a != b
            let name = path::IdentPath::new([
                path::Ident::BinOp(a.clone(), op.as_ref().op(), b.clone())
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
        // self.cache.set_unresolved(
        //     format!("Cannot use operator '{op}' on types {a} and {b}"),
        //     self.span(list)
        // );
        None
    }
}
