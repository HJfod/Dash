
use std::{sync::Arc, collections::HashMap};
use dash_macros::Parse;
use crate::{
    parser::{parse::{Parse, FatalParseError, calculate_span, ParseFn, SeparatedWithTrailing}, tokenizer::{TokenIterator, Token}},
    shared::{src::{Src, ArcSpan}, logger::{Message, Level, Note}},
    checker::{resolve::{Resolve, ResolveCache}, coherency::Checker, ty::Ty, path}, ice
};
use super::{expr::Expr, token::{op, delim, Ident, punct}};

#[derive(Debug, Parse)]
#[parse(expected = "expression or named argument")]
pub enum Arg {
    Named(Ident, #[parse(peek_point)] punct::Colon, Expr),
    Unnamed(Expr),
}

#[derive(Debug)]
pub struct Call {
    target: Expr,
    args: delim::Parenthesized<SeparatedWithTrailing<Arg, punct::Comma>>,
    cache: ResolveCache,
}

impl Call {
    pub(crate) fn parse_with<'s, I>(
        target: Expr,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, I>
    ) -> Result<Self, FatalParseError>
        where
            I: Iterator<Item = Token<'s>>
    {
        Ok(Self {
            target,
            args: Parse::parse(src, tokenizer)?,
            cache: Default::default(),
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.target.span(), self.args.span()])
    }
}

impl Resolve for Call {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(checker)?;
        let args = self.args.value.iter_mut()
            .map(|arg| match arg {
                Arg::Unnamed(value) => {
                    (None, value.try_resolve(checker), value.span())
                }
                Arg::Named(name, _, value) => {
                    (Some(name.to_string()), value.try_resolve(checker), value.span())
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
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}

#[derive(Debug)]
pub struct Index {
    target: Expr,
    index: delim::Bracketed<Expr>,
    trailing_comma: Option<punct::Comma>,
}

impl Index {
    pub(crate) fn parse_with<'s, I>(
        target: Expr,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, I>
    ) -> Result<Self, FatalParseError>
        where
            I: Iterator<Item = Token<'s>>
    {
        Ok(Self {
            target,
            index: Parse::parse(src.clone(), tokenizer)?,
            trailing_comma: Parse::parse(src.clone(), tokenizer)?,
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.target.span(), self.index.span(), self.trailing_comma.span()])
    }
}

impl Resolve for Index {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        todo!()
    }
}

#[derive(Debug)]
pub struct UnOp {
    op: op::Unary,
    target: Expr,
    cache: ResolveCache,
}

impl UnOp {
    pub(crate) fn parse_with<'s, F, I>(
        mut target: F,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, I>
    ) -> Result<Self, FatalParseError>
        where
            I: Iterator<Item = Token<'s>>,
            F: ParseFn<'s, I, Expr>
    {
        Ok(Self {
            op: Parse::parse(src.clone(), tokenizer)?,
            target: target(src, tokenizer)?,
            cache: Default::default(),
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.op.span(), self.target.span()])
    }
}

impl Resolve for UnOp {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(checker)?;
        let op = self.op.clone();
        if target.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            let name = path::IdentPath::new([path::Ident::UnOp(op.clone(), target.clone())], false);
            if let Some(fun) = scope.entities().find(&name, checker.namespace_stack()) {
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
        self.cache.set_unresolved(
            format!("Cannot use operator '{op}' on type {target}"),
            self.span()
        );
        None
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}

#[derive(Debug)]
pub struct BinOp {
    lhs: Expr,
    op: op::Binary,
    rhs: Expr,
    cache: ResolveCache,
}

impl BinOp {
    pub(crate) fn parse_with<'s, F, I>(
        lhs: Expr,
        mut rhs: F,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, I>
    ) -> Result<Self, FatalParseError>
        where
            I: Iterator<Item = Token<'s>>,
            F: ParseFn<'s, I, Expr>
    {
        Ok(Self {
            lhs,
            op: Parse::parse(src.clone(), tokenizer)?,
            rhs: rhs(src, tokenizer)?,
            cache: Default::default(),
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.lhs.span(), self.op.span(), self.rhs.span()])
    }
}

impl Resolve for BinOp {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let a = self.lhs.try_resolve(checker)?;
        let b = self.rhs.try_resolve(checker)?;
        let op = self.op.clone();
        if a.is_unreal() || b.is_unreal() {
            return Some(Ty::Invalid);
        }
        for scope in checker.scopes() {
            let name = path::IdentPath::new([path::Ident::BinOp(a.clone(), op.clone(), b.clone())], false);
            if let Some(fun) = scope.entities().find(&name, checker.namespace_stack()) {
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
        self.cache.set_unresolved(
            format!("Cannot use operator '{op}' on types {a} and {b}"),
            self.span()
        );
        None
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}
