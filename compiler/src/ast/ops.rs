
use std::{sync::Arc, collections::HashMap};
use dash_macros::Parse;
use crate::{
    parser::{parse::{Parse, FatalParseError, calculate_span, ParseFn, SeparatedWithTrailing, NodeList, RefToNode, Node}, tokenizer::TokenIterator},
    shared::{src::{Src, ArcSpan}, logger::{Message, Level, Note}},
    checker::{resolve::Resolve, coherency::Checker, ty::Ty, path}, ice
};
use super::{expr::Expr, token::{op, delim, Ident, punct}};

#[derive(Debug, Parse)]
#[parse(expected = "expression or named argument")]
pub enum ArgItem {
    Named(Ident, #[parse(peek_point)] punct::Colon, Expr),
    Unnamed(Expr),
}

#[derive(Debug)]
pub struct CallItem {
    target: Expr,
    args: delim::Parenthesized<SeparatedWithTrailing<Arg, punct::Comma>>,
}
pub type Call = RefToNode<CallItem>;

impl CallItem {
    pub(crate) fn parse_with(
        target: Expr,
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Call, FatalParseError> {
        let res = Self {
            target,
            args: Parse::parse(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for CallItem {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.target.span(), self.args.span()])
    }
}

impl Resolve for CallItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(list, checker)?;
        let args = self.args.get(list).as_mut().value.iter_mut()
            .map(|arg| match arg.get(list).as_mut() {
                ArgItem::Unnamed(value) => {
                    (None, value.try_resolve(list, checker), value.span())
                }
                ArgItem::Named(name, _, value) => {
                    (Some(name.get(list).as_ref().to_string()), value.try_resolve(list, checker), value.span())
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
                        self.span().unwrap_or(ArcSpan::builtin()).as_ref()
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
                    self.span().unwrap_or(ArcSpan::builtin()).as_ref()
                ));
                Some(Ty::Invalid)
            }
        }
    }
}

#[derive(Debug)]
pub struct IndexItem {
    target: Expr,
    index: delim::Bracketed<Expr>,
    trailing_comma: Option<punct::Comma>,
}
pub type Index = RefToNode<IndexItem>;

impl IndexItem {
    pub(crate) fn parse_with(
        target: Expr,
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Index, FatalParseError> {
        let res = Self {
            target,
            index: Parse::parse(list, src.clone(), tokenizer)?,
            trailing_comma: Parse::parse(list, src.clone(), tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for IndexItem {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.target.span(), self.index.span(), self.trailing_comma.span()])
    }
}

impl Resolve for IndexItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug)]
pub struct UnOpItem {
    op: op::Unary,
    target: Expr,
}
pub type UnOp = RefToNode<UnOpItem>;

impl UnOpItem {
    pub(crate) fn parse_with<'s, F>(
        mut target: F,
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s>
    ) -> Result<UnOp, FatalParseError>
        where F: ParseFn<'s, Expr>
    {
        let res = Self {
            op: Parse::parse(list, src.clone(), tokenizer)?,
            target: target(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for UnOpItem {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.op.span(), self.target.span()])
    }
}

impl Resolve for UnOpItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
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
        //     self.span()
        // );
        None
    }
}

#[derive(Debug)]
pub struct BinOpItem {
    lhs: Expr,
    op: op::Binary,
    rhs: Expr,
}
pub type BinOp = RefToNode<BinOpItem>;

impl BinOpItem {
    pub(crate) fn parse_with<'s, F>(
        lhs: Expr,
        mut rhs: F,
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s>
    ) -> Result<BinOp, FatalParseError>
        where F: ParseFn<'s, Expr>
    {
        let res = Self {
            lhs,
            op: Parse::parse(list, src.clone(), tokenizer)?,
            rhs: rhs(list, src, tokenizer)?,
        };
        Ok(list.add(res))
    }
}

impl Node for BinOpItem {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.lhs.span(), self.op.span(), self.rhs.span()])
    }
}

impl Resolve for BinOpItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
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
        //     self.span()
        // );
        None
    }
}
