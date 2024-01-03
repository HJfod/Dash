
use std::sync::Arc;

use dash_macros::{Parse, Resolve};
use crate::{
    parser::{parse::{Separated, Parse, FatalParseError, ParseFn, RefToNode, NodeList, Node}, tokenizer::TokenIterator},
    shared::src::{Src, ArcSpan},
    checker::{resolve::Resolve, coherency::{Checker, ScopeID}, ty::Ty, path}
};
use super::{
    decl::Decl,
    token::{Ident, punct::{self, TerminatingSemicolon}, op::{Prec, self}, delim},
    atom::Atom,
    flow::Flow,
    ops::{BinOp, UnOp, Call, Index, CallItem, IndexItem, UnOpItem, BinOpItem}
};

#[derive(Debug, Parse)]
#[parse(expected = "identifier")]
pub enum IdentComponentItem {
    Attribute(punct::At, Ident),
    Ident(Ident),
}

#[derive(Debug, Parse)]
pub struct IdentPathItem {
    absolute: Option<punct::Namespace>,
    path: Separated<IdentComponent, punct::Namespace>,
}

impl IdentPathItem {
    pub(crate) fn to_path(&self, list: &NodeList) -> path::IdentPath {
        path::IdentPath::new(
            self.path.iter().map(|i| path::Ident::from(match i.get(list).as_ref() {
                IdentComponentItem::Ident(i) => i.get(list).as_ref().to_string(),
                IdentComponentItem::Attribute(_, i) => format!("@{}", i.get(list).as_ref()),
            })).collect::<Vec<_>>(),
            self.absolute.is_some()
        )
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "expression")]
pub enum ScalarExprItem {
    Decl(Decl),
    Flow(Flow),
    Atom(Atom),
}

#[derive(Debug, Resolve)]
pub enum ExprItem {
    BinOp(BinOp),
    UnOp(UnOp),
    Call(Call),
    Index(Index),
    Scalar(ScalarExpr),
}
pub type Expr = RefToNode<ExprItem>;

impl ExprItem {
    fn parse_postfix(
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Self, FatalParseError> {
        let mut expr = Self::Scalar(Parse::parse(list, src.clone(), tokenizer)?);
        loop {
            if delim::Parenthesized::<delim::P>::peek(0, tokenizer) {
                expr = Self::Call(
                    CallItem::parse_with(list.add(expr), list, src.clone(), tokenizer)?
                );
            }
            else if delim::Bracketed::<delim::P>::peek(0, tokenizer) {
                expr = Self::Index(
                    IndexItem::parse_with(list.add(expr), list, src.clone(), tokenizer)?
                );
            }
            else {
                break;
            }
        }
        Ok(expr)
    }
    fn parse_unop(
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Self, FatalParseError> {
        if op::Unary::peek(0, tokenizer) {
            Ok(Self::UnOp(
                UnOpItem::parse_with(
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
        list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>
    ) -> Result<Self, FatalParseError>
        where F: ParseFn<'s, Self>
    {
        let mut lhs = sides(list, src.clone(), tokenizer)?;
        while prec.peek(tokenizer) {
            lhs = Self::BinOp(
                BinOpItem::parse_with(
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

impl Node for ExprItem {
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

impl Parse for ExprItem {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut sides: Box<dyn ParseFn<'s, Self>> = Box::from(Self::parse_unop);
        for prec in Prec::order() {
            sides = Box::from(
                move |list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>|
                    Self::parse_binop_prec(prec, &mut sides, list, src, tokenizer)
            );
        }
        sides(list, src, tokenizer)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        ScalarExpr::peek(pos, tokenizer)
    }
}

#[derive(Debug, Parse)]
pub struct ExprListItem {
    exprs: Vec<(Expr, TerminatingSemicolon)>,
    #[parse(skip)]
    scope: Option<ScopeID>,
}

impl Resolve for ExprListItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
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
