
use std::sync::Arc;
use dash_macros::Parse;
use crate::{
    parser::{parse::{Parse, FatalParseError, calculate_span, ParseFn, SeparatedWithTrailing}, tokenizer::{TokenIterator, Token}},
    shared::{src::{Src, ArcSpan}, logger::{Message, Level}},
    checker::{resolve::Resolve, coherency::Checker, ty::Ty, path}, ice
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
            args: Parse::parse(src, tokenizer)?
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.target.span(), self.args.span()])
    }
}

impl Resolve for Call {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
        let target = self.target.try_resolve(checker)?;
        let args = self.args.value.iter_mut()
            .map(|arg| match arg {
                Arg::Unnamed(value) => {
                    (None, value.try_resolve(checker))
                }
                Arg::Named(name, _, value) => {
                    (Some(name), value.try_resolve(checker))
                }
            })
            .map(|(name, expr)| expr.map(|e| (name, e)))
            .collect::<Option<Vec<_>>>()?;
        match target {
            Ty::Function { params, ret_ty } => {
                Some(ret_ty.as_ref().clone())
            }
            other => {
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Cannot call an expression of type {other}"),
                    self.span().into()
                ));
                Some(Ty::Invalid)
            }
        }
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
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
        todo!()
    }
}

#[derive(Debug)]
pub struct UnOp {
    op: op::Unary,
    target: Expr,
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
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.op.span(), self.target.span()])
    }
}

impl Resolve for UnOp {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
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
        checker.push_unresolved(
            format!("Cannot use operator '{op}' on type {target}"),
            self.span()
        );
        None
    }
}

#[derive(Debug)]
pub struct BinOp {
    lhs: Expr,
    op: op::Binary,
    rhs: Expr,
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
        })
    }
    pub fn span(&self) -> Option<ArcSpan> {
        calculate_span([self.lhs.span(), self.op.span(), self.rhs.span()])
    }
}

impl Resolve for BinOp {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
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
        checker.push_unresolved(
            format!("Cannot use operator '{op}' on types {a} and {b}"),
            self.span()
        );
        None
    }
}
