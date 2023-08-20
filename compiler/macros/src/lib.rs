
#![feature(proc_macro_diagnostic)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::{quote, format_ident};
use syn::{
    parse_macro_input,
    parenthesized,
    ImplItemFn,
    Ident,
    Result,
    Token,
    token::Paren,
    parse::{Parse, ParseStream}, LitStr, LitChar, braced, Error, ItemUse, Field, ExprBlock,
};

trait Gen {
    fn gen(&self) -> Result<TokenStream2>;
}

fn parse_list<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    let mut list = vec![];
    while let Ok(i) = input.parse() {
        list.push(i);
    }
    Ok(list)
}

mod kw {
    syn::custom_keyword!(rule);
}

#[derive(Clone, PartialEq)]
enum ClauseTy {
    List(Vec<ClauseTy>),
    Vec(Box<ClauseTy>),
    Option(Box<ClauseTy>),
    Rule(Ident),
    Char,
    String,
}

impl ClauseTy {
    fn is_convertible(&self, other: &ClauseTy) -> bool {
        *self == *other
    }
}

impl Gen for ClauseTy {
    fn gen(&self) -> Result<TokenStream2> {
        match self {
            Self::List(tys) => {
                let mut variants = TokenStream2::new();
                for ty in tys {
                    let ty = ty.gen()?;
                    variants.extend(quote! { #ty, });
                }
                Ok(quote! { (#variants) })
            }
            Self::Vec(ty) => {
                let ty = ty.gen()?;
                Ok(quote! { Vec<#ty> })
            }
            Self::Option(ty) => {
                let ty = ty.gen()?;
                Ok(quote! { Option<#ty> })
            }
            Self::Rule(rule) => {
                Ok(quote! { #rule<'s> })
            }
            Self::Char => {
                Ok(quote! { char })
            }
            Self::String => {
                Ok(quote! { String })
            }
        }
    }
}

#[derive(Clone)]
enum MaybeBinded {
    Drop(Clause),
    Unnamed(Clause),
    Named(Ident, Clause),
}

impl MaybeBinded {
    fn clause(&self) -> &Clause {
        match self {
            Self::Drop(c) => c,
            Self::Unnamed(c) => c,
            Self::Named(_, c) => c,
        }
    }

    fn is_binded(&self) -> bool {
        !matches!(self, Self::Drop(_))
    }
}

#[derive(Clone)]
enum Clause {
    // (?a b c) => { ... }
    List(Vec<Clause>, Vec<MaybeBinded>, Option<ExprBlock>),
    // a & b
    Concat(Vec<Clause>),
    // A* A+
    Repeat(Box<Clause>, bool),
    // "string literal"
    String(LitStr),
    // 'c'
    Char(LitChar),
    // 'a'..'b'
    CharRange(LitChar, LitChar),
    // A
    Rule(Ident),
    // A as B
    RuleAs(Ident, Ident),
}

impl Clause {
    fn parse_single(input: ParseStream) -> Result<Self> {
        let res;
        let ahead = input.lookahead1();
        if ahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            if input.parse::<Token![as]>().is_ok() {
                res = Clause::RuleAs(ident, input.parse()?);
            }
            else {
                res = Clause::Rule(ident);
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
                res = Clause::CharRange(ch, input.parse()?);
            }
            else {
                res = Clause::Char(ch);
            }
        }
        else {
            return Err(ahead.error());
        }
        if input.parse::<Token![*]>().is_ok() {
            Ok(Clause::Repeat(res.into(), false))
        }
        else if input.parse::<Token![+]>().is_ok() {
            Ok(Clause::Repeat(res.into(), true))
        }
        else {
            Ok(res)
        }
    }

    fn parse_concat(input: ParseStream) -> Result<Self> {
        let mut res = vec![Self::parse_single(input)?];
        while input.parse::<Token![&]>().is_ok() {
            res.push(Self::parse_single(input)?);
        }
        if res.len() == 1 {
            Ok(res.remove(0))
        }
        else {
            Ok(Clause::Concat(res))
        }
    }

    fn is_functional(&self) -> bool {
        match self {
            Clause::List(_, _, rust) => rust.is_some(),
            _ => false,
        }
    }

    fn eval_ty(&self) -> Result<ClauseTy> {
        match self {
            Clause::List(opts, list, rust) => {
                if rust.is_some() {
                    return Err(Error::new(
                        Span::call_site(),
                        "code blocks cannot be used for non-functional matchers"
                    ));
                }
                let mut res = vec![];
                for opt in opts {
                    opt.eval_ty()?;
                }
                for item in list {
                    let ty = item.clause().eval_ty()?;
                    if item.is_binded() {
                        res.push(ty);
                    }
                }
                let list = if res.len() == 1 {
                    res.remove(0)
                }
                else {
                    ClauseTy::List(res)
                };
                if opts.is_empty() {
                    Ok(list)
                }
                else {
                    Ok(ClauseTy::Option(list.into()))
                }
            }
            Clause::Concat(list) => {
                for a in list {
                    if !matches!(a.eval_ty()?, ClauseTy::String | ClauseTy::Char) {
                        Err(Error::new(Span::call_site(), "you can only concat chars and strings"))?
                    }
                }
                Ok(ClauseTy::String)
            }
            Clause::Repeat(r, _) => {
                let ty = r.eval_ty()?;
                // automatically concat char* to string
                if ty == ClauseTy::Char {
                    Ok(ClauseTy::String)
                }
                else {
                    Ok(ClauseTy::Vec(ty.into()))
                }
            }
            Clause::String(_) => {
                Ok(ClauseTy::String)
            }
            Clause::Char(_) | Clause::CharRange(_, _) => {
                Ok(ClauseTy::Char)
            }
            Clause::Rule(rule) => {
                Ok(ClauseTy::Rule(rule.clone()))
            }
            Clause::RuleAs(_, rule) => {
                Ok(ClauseTy::Rule(rule.clone()))
            }
        }
    }
}

impl Parse for Clause {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut opts = Vec::new();
        if input.parse::<Token![?]>().is_ok() {
            opts.push(Self::parse_concat(input)?);
        }
        let mut list = Vec::new();
        loop {
            let ahead = input.lookahead1();
            if ahead.peek(Ident) {
                if input.peek2(Token![:]) {
                    let name = input.parse()?;
                    input.parse::<Token![:]>()?;
                    list.push(MaybeBinded::Named(name, Self::parse_concat(input)?));
                }
                else {
                    list.push(MaybeBinded::Drop(Self::parse_concat(input)?));
                }
            }
            else if ahead.peek(Token![_]) {
                input.parse::<Token![_]>()?;
                input.parse::<Token![:]>()?;
                list.push(MaybeBinded::Unnamed(Self::parse_concat(input)?));
            }
            else if ahead.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                list.push(MaybeBinded::Unnamed(Self::parse_concat(input)?));
            }
            else {
                if let Ok(p) = Self::parse_concat(input) {
                    list.push(MaybeBinded::Drop(p));
                }
                else {
                    break;
                }
            }
        }
        let mut rust = None;
        if input.parse::<Token![=>]>().is_ok() {
            rust = Some(input.parse()?);
        }
        Ok(Clause::List(opts, list, rust))
    }
}

impl Clause {
    fn gen_with_ctx(&self, top: bool) -> Result<TokenStream2> {
        match self {
            Self::List(opts, clauses, rust) => {
                let mut body = TokenStream2::new();
                let mut cond = TokenStream2::new();
                for c in opts {
                    let b = c.gen_with_ctx(false)?;
                    cond.extend(quote! {
                        #b;
                    });
                }
                let mut binded_vars = vec![];
                for (i, c) in clauses.iter().enumerate() {
                    match c {
                        MaybeBinded::Named(name, clause) => {
                            let b = clause.gen()?;
                            body.extend(quote! {
                                let #name = #b;
                            });
                            binded_vars.push(name.clone());
                        }
                        MaybeBinded::Unnamed(clause) => {
                            let name = format_ident!("gdml_bind_{i}");
                            let b = clause.gen()?;
                            body.extend(quote! {
                                let #name = #b;
                            });
                            binded_vars.push(name);
                        }
                        MaybeBinded::Drop(clause) => {
                            let b = clause.gen()?;
                            body.extend(quote! {
                                #b;
                            })
                        }
                    }
                }
                let mut result_stream = TokenStream2::new();
                if let Some(rust) = rust {
                    result_stream = quote! { #rust };
                }
                else {
                    if top {
                        for r in binded_vars {
                            result_stream.extend(quote! { #r, });
                        }
                        result_stream = quote! { Ok(Self {
                            #result_stream
                            meta: parser.get_meta(start),
                        }) };
                    }
                    else {
                        if binded_vars.len() == 1 {
                            let f = binded_vars.first().unwrap();
                            result_stream.extend(quote! { #f });
                        }
                        else {
                            for r in binded_vars {
                                result_stream.extend(quote! { #r, });
                            }
                            result_stream = quote! { (#result_stream) };
                        }
                    }
                }
                if !opts.is_empty() {
                    Ok(quote! { {
                        let start = parser.skip_ws();
                        let cond = || -> Result<(), Message<'s>> {
                            #cond
                            Ok(())
                        }();
                        if cond.is_ok() {
                            #body
                            Some(#result_stream)
                        }
                        else {
                            parser.goto(start);
                            None
                        }
                    } })
                }
                else {
                    Ok(quote! { {
                        let start = parser.skip_ws();
                        #body
                        #result_stream
                    } })
                }
            }
            Self::Concat(list) => {
                let mut stream = quote! {
                    let mut res = String::new();
                };
                for l in list {
                    let b = l.gen()?;
                    if l.eval_ty()? == ClauseTy::Char {
                        stream.extend(quote! {
                            res.push(#b);
                        });
                    }
                    else {
                        stream.extend(quote! {
                            res.push_str(#b);
                        });
                    }
                }
                Ok(quote! {
                    {
                        #stream
                        res
                    }
                })
            }
            Self::Repeat(clause, require_one) => {
                let body = clause.gen()?;
                let stream = if *require_one {
                    quote! {
                        #body;
                    }
                }
                else {
                    quote! {}
                };
                // concat chars to a string
                if clause.eval_ty()? == ClauseTy::Char {
                    Ok(quote! {
                        {
                            #stream
                            let mut res = String::new();
                            while let Ok(b) = || -> Result<_, Message<'s>> {
                                Ok(#body)
                            }() {
                                res.push(b);
                            }
                            res
                        }
                    })
                }
                else {
                    Ok(quote! {
                        {
                            #stream
                            let mut res = Vec::new();
                            while let Ok(b) = || -> Result<_, Message<'s>> {
                                Ok(#body)
                            }() {
                                res.push(b);
                            }
                            res
                        }
                    })
                }
            }
            Self::String(lit) => {
                Ok(quote! {
                    parser.expect_word(#lit)?
                })
            }
            Self::Char(ch) => {
                Ok(quote! {
                    parser.expect_ch(#ch)?
                })
            }
            Self::CharRange(a, b) => {
                Ok(quote! {
                    parser.expect_ch_range(#a..#b)?
                })
            }
            Self::Rule(rule) => {
                Ok(quote! {
                    parser.expect_rule::<#rule>()?
                })
            }
            Self::RuleAs(rule, into) => {
                Ok(quote! {
                    #into::from(parser.expect_rule::<#rule>()?)
                })
            }
        }
    }

    fn gen_members(&self) -> Result<TokenStream2> {
        match self {
            Self::List(opts, list, _) => {
                let mut stream = TokenStream2::new();
                if opts.is_empty() {
                    for item in list {
                        if let MaybeBinded::Named(name, clause) = item {
                            let ty = clause.eval_ty()?.gen()?;
                            stream.extend(quote! {
                                #name: #ty,
                            });
                        }
                    }
                }
                Ok(stream)
            }
            _ => Ok(TokenStream2::new())
        }
    }
}

impl Gen for Clause {
    fn gen(&self) -> Result<TokenStream2> {
        self.gen_with_ctx(false)
    }
}

struct Match {
    clause: Clause,
}

impl Parse for Match {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![match]>()?;
        let list = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Match { clause: list })
    }
}

impl Gen for Match {
    fn gen(&self) -> Result<TokenStream2> {
        self.clause.gen_with_ctx(true)
    }
}

struct EnumRule {
    name: Ident,
    variants: Vec<Ident>,
}

impl Parse for EnumRule {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![enum]>()?;
        input.parse::<kw::rule>()?;
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        let mut variants = vec![input.parse()?];
        while input.parse::<Token![|]>().is_ok() {
            variants.push(input.parse()?);
        }
        input.parse::<Token![;]>()?;
        Ok(Self { name, variants })
    }
}

impl Gen for EnumRule {
    fn gen(&self) -> Result<TokenStream2> {
        let name = &self.name;
        let mut variants = TokenStream2::new();
        let mut impls = TokenStream2::new();
        let mut match_options = quote! {
            let mut furthest_match: Option<(Loc, Message<'s>)> = None;
        };
        let mut meta_variants = TokenStream2::new();
        for var in &self.variants {
            variants.extend(quote! {
                #var(Box<#var<'s>>),
            });
            match_options.extend(quote! {
                match parser.expect_rule::<#var>() {
                    Ok(r) => return Ok(Self::#var(r.into())),
                    Err(e) => {
                        if !furthest_match.as_ref().is_some_and(|m| e.range.end <= m.0) {
                            furthest_match = Some((e.range.end.clone(), e));
                        }
                    },
                }
            });
            impls.extend(quote! {
                impl<'s> From<#var<'s>> for #name<'s> {
                    fn from(from: #var<'s>) -> Self {
                        Self::#var(from.into())
                    }
                }
            });
            meta_variants.extend(quote! {
                Self::#var(v) => &v.meta,
            });
        }
        Ok(quote! {
            #[derive(Debug)]
            pub enum #name<'s> {
                #variants
            }

            #impls

            impl<'s> Rule<'s> for #name<'s> {
                fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #match_options
                    Err(furthest_match.unwrap().1)
                }

                fn meta(&self) -> &ExprMeta {
                    match self {
                        #meta_variants
                    }
                }
            }
        })
    }
}

struct ParseRule(Vec<Field>, Vec<Match>, Vec<ImplItemFn>);

impl Parse for ParseRule {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut fields = vec![];
        let mut matches = vec![];
        let mut impls = vec![];
        loop {
            if input.peek(Token![match]) {
                matches.push(input.parse::<Match>()?);
                continue;
            }
            if input.parse::<Token![impl]>().is_ok() {
                impls.push(input.parse::<ImplItemFn>()?);
                continue;
            }
            if input.peek(Ident) {
                fields.push(Field::parse_named(input)?);
                input.parse::<Token![;]>()?;
                continue;
            }
            break;
        }
        Ok(Self(fields, matches, impls))
    }
}

struct MatchRule {
    name: Ident,
    fields: Vec<Field>,
    matches: Vec<Match>,
    impls: Vec<ImplItemFn>,
}

impl Parse for MatchRule {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::rule>()?;
        let name = input.parse()?;
        let contents;
        braced!(contents in input);
        let ParseRule(fields, matches, impls) = contents.parse()?;
        Ok(Self { name, fields, matches, impls })
    }
}

impl Gen for MatchRule {
    fn gen(&self) -> Result<TokenStream2> {
        if self.matches.is_empty() {
            return Err(Error::new(Span::call_site(), "rules must have at least one match statement"));
        }
        let name = &self.name;
        let mut members = TokenStream2::new();
        let first = &self.matches.first().unwrap().clause;
        if first.is_functional() {
            for mat in self.matches.iter().skip(1) {
                if !mat.clause.is_functional() {
                    return Err(Error::new(Span::call_site(), "all matchers must be functional"));
                }
            }
        }
        else {
            let first_ty = first.eval_ty()?;
            for mat in self.matches.iter().skip(1) {
                if !mat.clause.eval_ty()?.is_convertible(&first_ty) {
                    return Err(Error::new(Span::call_site(), "all matches must evaluate to the same type"));
                }
            }
            members.extend(first.gen_members()?);
        }
        for field in &self.fields {
            members.extend(quote! {
                #field,
            });
        }
        members.extend(quote! {
            meta: ExprMeta<'s>,
        });

        let mut fns = TokenStream2::new();
        let mut trait_impls = TokenStream2::new();

        let mut match_options = quote! {
            let mut furthest_match: Option<(Loc, Message<'s>)> = None;
        };

        for (i, mat) in self.matches.iter().enumerate() {
            let name = format_ident!("match_impl_{i}");
            let body = mat.gen()?;
            fns.extend(quote! {
                fn #name(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #body
                }
            });
            match_options.extend(quote! {
                match Self::#name(parser) {
                    Ok(r) => return Ok(r),
                    Err(e) => {
                        if !furthest_match.as_ref().is_some_and(|m| e.range.end <= m.0) {
                            furthest_match = Some((e.range.end.clone(), e));
                        }
                    },
                }
            });
        }

        trait_impls.extend(quote! {
            impl<'s> Rule<'s> for #name<'s> {
                fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #match_options
                    Err(furthest_match.unwrap().1)
                }

                fn meta(&self) -> &ExprMeta {
                    &self.meta
                }
            }
        });

        for fun in &self.impls {
            match fun.sig.ident.to_string().as_str() {
                "typecheck" => {
                    // impls.extend(quote! {
                    //     impl TypeCheck for #name {
                    //         #fun
                    //     }
                    // });
                }
                _ => {
                    return Err(Error::new(Span::call_site(), "unknown impl"));
                }
            }
        }

        Ok(quote! {
            #[derive(Debug)]
            pub struct #name<'s> {
                #members
            }

            impl<'s> #name<'s> {
                #fns
            }

            #trait_impls
        })
    }
}

enum Rule {
    MatchRule(MatchRule),
    EnumRule(EnumRule),
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::rule) {
            Ok(Rule::MatchRule(input.parse()?))
        }
        else if input.peek(Token![enum]) {
            Ok(Rule::EnumRule(input.parse()?))
        }
        else {
            Err(Error::new(Span::call_site(), "expected rule"))
        }
    }
}

impl Gen for Rule {
    fn gen(&self) -> Result<TokenStream2> {
        match self {
            Rule::MatchRule(r) => r.gen(),
            Rule::EnumRule(r) => r.gen(),
        }
    }
}

struct Rules {
    uses: Vec<ItemUse>,
    rules: Vec<Rule>,
}

impl Parse for Rules {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            uses: parse_list(input)?,
            rules: parse_list(input)?,
        })
    }
}

impl Gen for Rules {
    fn gen(&self) -> Result<TokenStream2> {
        let mut stream = TokenStream2::new();
        for use_ in &self.uses {
            stream.extend(quote! { #use_ });
        }
        for rule in &self.rules {
            stream.extend(rule.gen()?);
        }
        Ok(quote! {
            pub mod ast {
                use crate::src::{Loc, Message};
                use crate::parser::{Parser, Rule, ExprMeta, XID_Start, XID_Continue};
                #stream
            }
        })
    }
}

#[proc_macro]
pub fn define_rules(input: TokenStream) -> TokenStream {
    match parse_macro_input!(input as Rules).gen() {
        Ok(s) => s.into(),
        Err(e) => TokenStream::from(e.to_compile_error())
    }
}
