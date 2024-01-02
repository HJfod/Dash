
use std::sync::Arc;

use dash_macros::{Parse, Resolve};
use crate::{
    parser::{parse::{Separated, Parse, FatalParseError, ParseFn}, tokenizer::{TokenIterator, Token}},
    shared::src::{Src, ArcSpan},
    checker::{resolve::{Resolve, ResolveCache}, coherency::{Checker, ScopeID}, ty::Ty, path}
};
use super::{
    decl::Decl,
    token::{Ident, punct::{self, TerminatingSemicolon}, op::{Prec, self}, delim},
    atom::Atom,
    flow::Flow,
    ops::{BinOp, UnOp, Call, Index}
};

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum IdentComponent {
    Attribute(punct::At, Ident),
    Ident(Ident),
}

#[derive(Debug, Parse)]
pub struct IdentPath {
    absolute: Option<punct::Namespace>,
    path: Separated<IdentComponent, punct::Namespace>,
}

impl IdentPath {
    pub(crate) fn to_path(&self) -> path::IdentPath {
        path::IdentPath::new(
            self.path.iter().map(|i| path::Ident::from(match i {
                IdentComponent::Ident(i) => i.to_string(),
                IdentComponent::Attribute(_, i) => format!("@{i}"),
            })).collect::<Vec<_>>(),
            self.absolute.is_some()
        )
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "expression")]
pub enum ScalarExpr {
    Decl(Decl),
    Flow(Flow),
    Atom(Atom),
}

#[derive(Debug, Resolve)]
pub enum Expr {
    BinOp(Box<BinOp>),
    UnOp(Box<UnOp>),
    Call(Box<Call>),
    Index(Box<Index>),
    Scalar(Box<ScalarExpr>),
}

impl Expr {
    fn parse_postfix<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
        where I: Iterator<Item = Token<'s>>
    {
        let mut expr = Self::Scalar(Parse::parse(src.clone(), tokenizer)?);
        loop {
            if delim::Parenthesized::<delim::P>::peek(0, tokenizer) {
                expr = Expr::Call(Box::from(
                    Call::parse_with(expr, src.clone(), tokenizer)?
                ));
            }
            else if delim::Bracketed::<delim::P>::peek(0, tokenizer) {
                expr = Expr::Index(Box::from(
                    Index::parse_with(expr, src.clone(), tokenizer)?
                ));
            }
            else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unop<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
        where I: Iterator<Item = Token<'s>>
    {
        if op::Unary::peek(0, tokenizer) {
            Ok(Self::UnOp(Box::from(
                UnOp::parse_with(Self::parse_postfix, src, tokenizer)?
            )))
        }
        else {
            Self::parse_postfix(src, tokenizer)
        }
    }

    fn parse_binop_prec<'s, F, I>(
        prec: Prec, sides: &mut F,
        src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>
    ) -> Result<Self, FatalParseError>
        where
            I: Iterator<Item = Token<'s>>,
            F: ParseFn<'s, I, Self>
    {
        let mut lhs = sides(src.clone(), tokenizer)?;
        while prec.peek(tokenizer) {
            lhs = Expr::BinOp(Box::from(
                BinOp::parse_with(lhs, |s, t| sides(s, t), src.clone(), tokenizer)?
            ));
        }
        Ok(lhs)
    }
}

impl Parse for Expr {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
        where I: Iterator<Item = Token<'s>>
    {
        let mut sides: Box<dyn ParseFn<'s, I, Self>> = Box::from(Self::parse_unop);
        for prec in Prec::order() {
            sides = Box::from(
                move |src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>|
                    Self::parse_binop_prec(prec, &mut sides, src, tokenizer)
            );
        }
        sides(src, tokenizer)
    }

    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        ScalarExpr::peek(pos, tokenizer)
    }

    fn span(&self) -> Option<ArcSpan> {
        match self {
            Self::BinOp(binop) => binop.span(),
            Self::UnOp(unop) => unop.span(),
            Self::Call(call) => call.span(),
            Self::Index(index) => index.span(),
            Self::Scalar(scalar) => scalar.span(),
        }
    }
}

#[derive(Debug, Parse)]
pub struct ExprList {
    exprs: Vec<(Expr, TerminatingSemicolon)>,
    #[parse(skip)]
    scope: Option<ScopeID>,
    #[parse(skip)]
    cache: ResolveCache,
}

impl Resolve for ExprList {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let _handle = checker.enter_scope(&mut self.scope);
        self.exprs.iter_mut()
            .map(|(e, c)| (e.try_resolve(checker), c.has_semicolon()))
            .map(|(e, c)| e.map(|e| (e, c)))
            .collect::<Option<Vec<_>>>()?
            .into_iter()
            .last()
            .and_then(|(e, c)| (!c).then_some(e).or(Some(Ty::Void)))
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}
