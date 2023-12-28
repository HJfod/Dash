
use std::sync::Arc;
use dash_macros::Parse;
use crate::{parser::{parse::{Parse, FatalParseError, calculate_span, ParseFn, SeparatedWithTrailing}, tokenizer::{TokenIterator, Token}}, shared::src::{Src, ArcSpan}};

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
