extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;
use std::collections::HashSet;
use unicode_xid::UnicodeXID;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    token::{Bracket, Paren},
    Error, ExprBlock, Ident, LitChar, LitStr, Result, Token,
};

use crate::{defs::kw, items::Keyword, ty::ClauseTy};

#[derive(Clone)]
pub enum MaybeBinded {
    Drop(Clause),
    Unnamed(Clause),
    Named(Ident, Clause),
    Arg(Ident, Clause),
}

impl MaybeBinded {
    fn clause(&self) -> &Clause {
        match self {
            Self::Drop(c) => c,
            Self::Unnamed(c) => c,
            Self::Named(_, c) => c,
            Self::Arg(_, c) => c,
        }
    }

    fn is_binded(&self) -> bool {
        !matches!(self, Self::Drop(_))
    }
}

#[derive(Clone)]
pub enum Char {
    Single(LitChar),
    Not(LitChar),
    Previous(LitChar),
    Range(LitChar, LitChar),
    XidStart,
    XidContinue,
    OpChar,
    Any,
    EOF,
}

#[derive(Clone)]
pub enum RepeatMode {
    OneOrMore,
    ZeroOrMore,
    Until(Box<Clause>),
}

#[derive(Clone)]
pub struct RuleClause {
    name: Ident,
    matcher: Option<Ident>,
    cast_as: Vec<Ident>,
}

impl RuleClause {
    pub fn gen(
        &self,
        fun: &str,
        specific_fun: &str,
        args: Option<TokenStream2>,
    ) -> Result<TokenStream2> {
        let name = if let Some(ref which) = self.matcher {
            format_ident!("{specific_fun}{which}")
        } else {
            format_ident!("{fun}")
        };
        let cls = &self.name;
        let mut stream = quote! {
            #cls::#name(parser, #args)?
        };
        for rule in &self.cast_as {
            stream = quote! {
                #rule::from(#stream)
            };
        }
        Ok(stream)
    }
}

impl Parse for RuleClause {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        let which = if input.peek(Bracket) {
            let contents;
            bracketed!(contents in input);
            Some(contents.parse::<Ident>()?)
        } else {
            None
        };
        let mut into = Vec::new();
        while input.parse::<Token![as]>().is_ok() {
            into.push(input.parse()?);
        }
        if !into.is_empty() {
            Ok(Self {
                name: ident,
                matcher: which,
                cast_as: into,
            })
        } else {
            Ok(Self {
                name: ident,
                matcher: which,
                cast_as: vec![],
            })
        }
    }
}

#[derive(Clone)]
pub enum Clause {
    // (?a b c) => { ... }
    List {
        peek_condition: Vec<Clause>,
        items: Vec<MaybeBinded>,
        rust: Option<ExprBlock>,
    },
    // a | b    as Enum (a | b)
    OneOf(Span, Vec<Clause>, Option<Ident>),
    // a? a unless B
    Option(Box<Clause>, Option<Box<Clause>>),
    // a & b
    Concat(Vec<Clause>),
    // a && b
    ConcatVec(Vec<Clause>),
    // A* A+ A until B
    Repeat(Box<Clause>, RepeatMode),
    // "string literal"
    String(LitStr),
    // 'c'
    Char(Char),
    // A[_] as B as C
    Rule(RuleClause),
    // fn -> A { ... }
    FnMatcher {
        ret_ty: ClauseTy,
        body: ExprBlock,
    },
    // E.V
    EnumVariant(Ident, Option<Ident>),
    // _
    Default,
}

impl Clause {
    fn parse_single(input: ParseStream) -> Result<Self> {
        let res;
        let ahead = input.lookahead1();
        if ahead.peek(kw::XID_Start) {
            input.parse::<kw::XID_Start>()?;
            res = Clause::Char(Char::XidStart)
        }
        else if ahead.peek(kw::XID_Continue) {
            input.parse::<kw::XID_Continue>()?;
            res = Clause::Char(Char::XidContinue)
        }
        else if ahead.peek(kw::OP_CHAR) {
            input.parse::<kw::OP_CHAR>()?;
            res = Clause::Char(Char::OpChar)
        }
        else if ahead.peek(kw::ANY_CHAR) {
            input.parse::<kw::ANY_CHAR>()?;
            res = Clause::Char(Char::Any)
        }
        else if ahead.peek(kw::EOF) {
            input.parse::<kw::EOF>()?;
            res = Clause::Char(Char::EOF)
        }
        else if ahead.peek(Ident) {
            // enum variants are `Enum.Variant`
            if input.peek2(Token![.]) {
                let ident = input.parse::<Ident>()?;
                input.parse::<Token![.]>()?;
                res = if input.parse::<Token![*]>().is_ok() {
                    Clause::EnumVariant(ident, None)
                } else {
                    Clause::EnumVariant(ident, Some(input.parse()?))
                };
            }
            // otherwise this is a rule name
            else {
                res = Clause::Rule(RuleClause::parse(input)?)
            }
        }
        else if ahead.peek(Paren) {
            let contents;
            parenthesized!(contents in input);
            res = contents.parse()?;
        }
        else if ahead.peek(LitStr) {
            res = Clause::String(input.parse()?);
        }
        else if ahead.peek(LitChar) {
            let ch = input.parse()?;
            if input.parse::<Token![..]>().is_ok() {
                res = Clause::Char(Char::Range(ch, input.parse()?));
            } else {
                res = Clause::Char(Char::Single(ch));
            }
        }
        else if ahead.peek(Token![<=]) {
            input.parse::<Token![<=]>()?;
            res = Clause::Char(Char::Previous(input.parse()?))
        }
        else if ahead.peek(Token![^]) {
            input.parse::<Token![^]>()?;
            res = Clause::Char(Char::Not(input.parse()?));
        }
        else if ahead.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            res = Clause::Default;
        }
        else if ahead.peek(Token![fn]) {
            input.parse::<Token![fn]>()?;
            input.parse::<Token![->]>()?;
            let ret_ty = input.parse()?;
            let body = input.parse()?;
            res = Clause::FnMatcher { ret_ty, body };
        }
        else {
            return Err(ahead.error());
        }
        if input.parse::<Token![*]>().is_ok() {
            Ok(Clause::Repeat(res.into(), RepeatMode::ZeroOrMore))
        }
        else if input.parse::<Token![+]>().is_ok() {
            Ok(Clause::Repeat(res.into(), RepeatMode::OneOrMore))
        }
        else if input.parse::<kw::until>().is_ok() {
            Ok(Clause::Repeat(
                res.into(),
                RepeatMode::Until(Self::parse_one_of(input)?.into()),
            ))
        }
        else if input.parse::<Token![?]>().is_ok() {
            Ok(Clause::Option(res.into(), None))
        }
        else if input.parse::<kw::unless>().is_ok() {
            Ok(Clause::Option(
                res.into(),
                Some(Self::parse_one_of(input)?.into()),
            ))
        }
        else {
            Ok(res)
        }
    }

    fn parse_concat_string(input: ParseStream) -> Result<Self> {
        let mut res = vec![Self::parse_single(input)?];
        while input.parse::<Token![&]>().is_ok() {
            res.push(Self::parse_single(input)?);
        }
        if res.len() == 1 {
            Ok(res.remove(0))
        } else {
            Ok(Clause::Concat(res))
        }
    }

    fn parse_concat(input: ParseStream) -> Result<Self> {
        let mut res = vec![Self::parse_concat_string(input)?];
        while input.parse::<Token![~]>().is_ok() {
            res.push(Self::parse_concat_string(input)?);
        }
        if res.len() == 1 {
            Ok(res.remove(0))
        } else {
            Ok(Clause::ConcatVec(res))
        }
    }

    fn parse_one_of(input: ParseStream) -> Result<Self> {
        let mut pos = input.span();
        if input.parse::<Token![as]>().is_ok() {
            let as_ = input.parse()?;
            let c;
            parenthesized!(c in input);
            Ok(Clause::OneOf(
                pos,
                c.parse_terminated(Self::parse_concat, Token![|])?.into_iter().collect(),
                Some(as_)
            ))
        }
        else {
            let mut res = vec![Self::parse_concat(input)?];
            while input.parse::<Token![|]>().is_ok() {
                pos = pos.join(input.span()).unwrap();
                res.push(Self::parse_concat(input)?);
            }
            if res.len() == 1 {
                Ok(res.remove(0))
            } else {
                Ok(Clause::OneOf(pos, res, None))
            }
        }
    }

    pub fn parse_one(input: ParseStream) -> Result<Self> {
        Self::parse_one_of(input)
    }

    pub fn is_functional(&self) -> bool {
        match self {
            Clause::List {
                peek_condition: _,
                items: _,
                rust,
            } => rust.is_some(),
            _ => false,
        }
    }

    pub fn get_arg_tys(&self) -> Result<Vec<(Ident, ClauseTy)>> {
        match self {
            Self::List {
                peek_condition: _,
                items,
                rust: _,
            } => {
                let mut res = vec![];
                for i in items {
                    if let MaybeBinded::Arg(n, c) = i {
                        res.push((n.clone(), c.eval_ty()?));
                    }
                }
                Ok(res)
            }
            _ => Ok(vec![]),
        }
    }

    pub fn eval_ty(&self) -> Result<ClauseTy> {
        match self {
            Self::List {
                peek_condition,
                items,
                rust,
            } => {
                if rust.is_some() {
                    return Err(Error::new(
                        Span::call_site(),
                        "code blocks cannot be used for non-functional matchers",
                    ));
                }
                let mut res = vec![];
                for opt in peek_condition {
                    opt.eval_ty()?;
                }
                for item in items {
                    let ty = item.clause().eval_ty()?;
                    // todo: error if bind is an arg and not top level
                    if item.is_binded() {
                        res.push(ty);
                    }
                }
                let list = if res.len() == 1 {
                    res.remove(0)
                } else {
                    ClauseTy::List(res)
                };
                if peek_condition.is_empty() {
                    Ok(list)
                } else {
                    Ok(ClauseTy::Option(list.into()))
                }
            }
            Self::OneOf(span, list, as_) => {
                if list.is_empty() {
                    Err(Error::new(span.clone(), "internal error: empty one-of"))?;
                }
                if let Some(as_enum) = as_ {
                    Ok(ClauseTy::Rule(as_enum.clone()))
                }
                else {
                    let first = list.first().unwrap().eval_ty()?;
                    for a in list.iter().skip(1) {
                        if !a.eval_ty()?.is_convertible(&first) {
                            Err(Error::new(span.clone(), "all one-of options must result in the same type"))?
                        }
                    }
                    Ok(first)
                }
            }
            Self::Option(clause, _) => {
                match clause.eval_ty()? {
                    vec@ClauseTy::Vec(_) => Ok(vec),
                    other => Ok(ClauseTy::Option(other.into()))
                }
            }
            Self::Concat(list) => {
                for a in list {
                    if !matches!(a.eval_ty()?, ClauseTy::String | ClauseTy::Char) {
                        Err(Error::new(
                            Span::call_site(),
                            "you can only concat chars and strings",
                        ))?
                    }
                }
                Ok(ClauseTy::String)
            }
            Self::ConcatVec(list) => Ok(ClauseTy::Vec(
                list.first().unwrap().eval_ty()?.inner_ty().clone().into(),
            )),
            Self::Repeat(r, _) => {
                let ty = r.eval_ty()?;
                // automatically concat char* to string
                if matches!(ty, ClauseTy::Char) {
                    Ok(ClauseTy::String)
                } else {
                    Ok(ClauseTy::Vec(ty.into()))
                }
            }
            Self::String(_) => Ok(ClauseTy::String),
            Self::Char(c) => Ok(if matches!(c, Char::EOF) {
                ClauseTy::Default
            } else {
                ClauseTy::Char
            }),
            Self::Rule(rule) => Ok(ClauseTy::Rule(
                rule.cast_as.last().unwrap_or(&rule.name).clone(),
            )),
            Self::EnumVariant(e, _) => Ok(ClauseTy::Enum(e.clone())),
            Self::FnMatcher { ret_ty, body: _ } => Ok(ret_ty.clone()),
            Self::Default => Ok(ClauseTy::Default),
        }
    }

    pub fn verify_keywords(&self, kws: &HashSet<Keyword>) -> Result<()> {
        match self {
            Self::List {
                peek_condition,
                items,
                rust: _,
            } => {
                peek_condition
                    .iter()
                    .map(|i| i.verify_keywords(kws))
                    .collect::<Result<_>>()?;
                items
                    .iter()
                    .map(|i| i.clause().verify_keywords(kws))
                    .collect::<Result<_>>()?;
            }
            Self::OneOf(_, items, _) | Self::Concat(items) | Self::ConcatVec(items) => {
                items
                    .iter()
                    .map(|i| i.verify_keywords(kws))
                    .collect::<Result<_>>()?;
            }
            Self::Option(clause, unless) => {
                clause.verify_keywords(kws)?;
                unless
                    .iter()
                    .map(|i| i.verify_keywords(kws))
                    .collect::<Result<_>>()?;
            }
            Self::Repeat(clause, _) => {
                clause.verify_keywords(kws)?;
            }
            Self::String(lit) => {
                let value = lit.value();
                if value.chars().take(1).all(|c| c.is_xid_start())
                    && value.chars().skip(1).all(|c| c.is_xid_continue())
                {
                    if let Some(kw) = kws.iter().find(|kw| kw.to_string() == lit.value()) {
                        if let Keyword::Reserved(_) = kw {
                            Err(Error::new(lit.span(), "keyword is only reserved"))?;
                        }
                    } else {
                        Err(Error::new(lit.span(), "unknown keyword"))?;
                    }
                }
            }
            Self::Char(_)
            | Self::Rule(_)
            | Self::EnumVariant(_, _)
            | Self::Default
            | Self::FnMatcher { ret_ty: _, body: _ } => {}
        }
        Ok(())
    }
}

impl Parse for Clause {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut peek_condition = Vec::new();
        let mut items = Vec::new();
        if input.parse::<Token![?]>().is_ok() {
            if input.parse::<Token![?]>().is_ok() {
                peek_condition.push(Self::parse_one_of(input)?);
            } else {
                let c = Self::parse_one_of(input)?;
                peek_condition.push(c.clone());
                items.push(MaybeBinded::Drop(c));
            }
        }
        loop {
            let ahead = input.lookahead1();
            if ahead.peek(kw::nofallthrough) || ahead.peek(kw::afterwards) {
                break;
            }
            if ahead.peek(Ident) {
                if input.peek2(Token![:]) {
                    let name = input.parse()?;
                    input.parse::<Token![:]>()?;
                    items.push(MaybeBinded::Named(name, Self::parse_one_of(input)?));
                } else {
                    items.push(MaybeBinded::Drop(Self::parse_one_of(input)?));
                }
            } else if ahead.peek(Token![$]) {
                input.parse::<Token![$]>()?;
                let name = input.parse()?;
                input.parse::<Token![:]>()?;
                items.push(MaybeBinded::Arg(name, Self::parse_one_of(input)?));
            } else if ahead.peek(Token![_]) {
                input.parse::<Token![_]>()?;
                input.parse::<Token![:]>()?;
                items.push(MaybeBinded::Unnamed(Self::parse_one_of(input)?));
            } else if ahead.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                items.push(MaybeBinded::Unnamed(Self::parse_one_of(input)?));
            } else {
                if let Ok(p) = Self::parse_one_of(input) {
                    items.push(MaybeBinded::Drop(p));
                } else {
                    break;
                }
            }
        }
        let mut rust = None;
        if input.parse::<Token![=>]>().is_ok() {
            rust = Some(input.parse()?);
        }
        Ok(Clause::List {
            peek_condition,
            items,
            rust,
        })
    }
}
