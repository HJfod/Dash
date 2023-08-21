
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
    parse::{Parse, ParseStream}, LitStr, LitChar, braced, Error, ItemUse, Field, ExprBlock, punctuated::Punctuated,
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
    syn::custom_keyword!(expected);
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
enum Char {
    Single(LitChar),
    Range(LitChar, LitChar),
    XidStart,
    XidContinue,
    OpChar,
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
    Char(Char),
    // A
    Rule(Ident),
    // A as B as C
    RuleAs(Vec<Ident>),
}

impl Clause {
    fn parse_single(input: ParseStream) -> Result<Self> {
        let res;
        let ahead = input.lookahead1();
        if ahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            let mut into = Vec::new();
            while input.parse::<Token![as]>().is_ok() {
                into.push(input.parse()?);
            }
            if !into.is_empty() {
                into.insert(0, ident);
                res = Clause::RuleAs(into);
            }
            else {
                res = match ident.to_string().as_str() {
                    "XID_Start"     => Clause::Char(Char::XidStart),
                    "XID_Continue"  => Clause::Char(Char::XidContinue),
                    "OP_CHAR"       => Clause::Char(Char::OpChar),
                    _ => Clause::Rule(ident),
                };
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
            }
            else {
                res = Clause::Char(Char::Single(ch));
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
            Clause::Char(_) => {
                Ok(ClauseTy::Char)
            }
            Clause::Rule(rule) => {
                Ok(ClauseTy::Rule(rule.clone()))
            }
            Clause::RuleAs(rules) => {
                Ok(ClauseTy::Rule(rules.last().unwrap().clone()))
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
                            res.push_str(&#b);
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
                // concat chars to a string
                let mut stream = if clause.eval_ty()? == ClauseTy::Char {
                    quote! {
                        let mut res = String::new();
                    }
                }
                else {
                    quote! {
                        let mut res = Vec::new();
                    }
                };
                if *require_one {
                    stream.extend(quote! {
                        res.push(#body);
                    });
                }
                Ok(quote! {
                    {
                        #stream
                        while let Ok(b) = || -> Result<_, Message<'s>> {
                            Ok(#body)
                        }() {
                            res.push(b);
                        }
                        res
                    }
                })
            }
            Self::String(lit) => {
                Ok(quote! {
                    parser.expect_word(#lit)?
                })
            }
            Self::Char(ch) => {
                Ok(match ch {
                    Char::Single(ch) => {
                        quote! {
                            parser.expect_ch(#ch)?
                        }
                    }
                    Char::Range(a, b) => {
                        quote! {
                            parser.expect_ch_range(#a..#b)?
                        }
                    }
                    Char::XidStart => {
                        quote! {
                            parser.expect_ch_with(UnicodeXID::is_xid_start, "identifier")?
                        }
                    }
                    Char::XidContinue => {
                        quote! {
                            parser.expect_ch_with(UnicodeXID::is_xid_continue, "identifier")?
                        }
                    }
                    Char::OpChar => {
                        quote! {
                            parser.expect_ch_with(crate::parser::is_op_char, "operator")?
                        }
                    }
                })
            }
            Self::Rule(rule) => {
                Ok(quote! {
                    parser.expect_rule::<#rule>()?
                })
            }
            Self::RuleAs(rules) => {
                let first = rules.first().unwrap();
                let mut stream = quote! {
                    parser.expect_rule::<#first>()?
                };
                for rule in rules.iter().skip(1) {
                    stream = quote! {
                        #rule::from(#stream)
                    };
                }
                Ok(stream)
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

struct Variant {
    lookahead: Option<Clause>,
    name: Ident,
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = if input.parse::<Token![?]>().is_ok() {
            let v = input.parse()?;
            input.parse::<Token![->]>()?;
            Some(v)
        }
        else {
            None
        };
        Ok(Self { lookahead, name: input.parse()? })
    }
}

struct EnumRule {
    name: Ident,
    variants: Vec<Variant>,
    expected: LitStr,
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
        input.parse::<kw::expected>()?;
        let expected = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Self { name, variants, expected })
    }
}

impl Gen for EnumRule {
    fn gen(&self) -> Result<TokenStream2> {
        let name = &self.name;
        let mut variants = TokenStream2::new();
        let mut impls = TokenStream2::new();
        let mut match_options = quote! {
        };
        let mut meta_variants = TokenStream2::new();
        for var in &self.variants {
            let var_name = &var.name;
            variants.extend(quote! {
                #var_name(Box<#var_name<'s>>),
            });
            if let Some(ref la) = var.lookahead {
                let la = la.gen()?;
                match_options.extend(quote! {
                    {
                        let start = parser.skip_ws();
                        let cond = || -> Result<(), Message<'s>> {
                            #la
                            Ok(())
                        }().is_ok();
                        parser.goto(start);
                        if cond {
                            return Ok(Self::#var_name(parser.expect_rule::<#var_name>()?.into()));
                        }
                    }
                });
            }
            else {
                match_options.extend(quote! {
                    if let Ok(r) = parser.expect_rule::<#var_name>() {
                        return Ok(Self::#var_name(r.into()));
                    }
                });
            }
            impls.extend(quote! {
                impl<'s> From<#var_name<'s>> for #name<'s> {
                    fn from(from: #var_name<'s>) -> Self {
                        Self::#var_name(from.into())
                    }
                }
            });
            meta_variants.extend(quote! {
                Self::#var_name(v) => &v.meta(),
            });
        }
        let expected = format!("Expected {}", self.expected.value());
        Ok(quote! {
            #[derive(Debug)]
            pub enum #name<'s> {
                #variants
            }

            #impls

            impl<'s> Rule<'s> for #name<'s> {
                fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #match_options
                    Err(parser.error(parser.pos(), #expected))
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

struct EnumField {
    name: Ident,
    string: LitStr,
}

impl Parse for EnumField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![->]>()?;
        let string = input.parse()?;
        Ok(Self { name, string })
    }
}

struct Enum {
    name: Ident,
    fields: Vec<EnumField>,
}

impl Parse for Enum {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![enum]>()?;
        let name = input.parse()?;
        let contents;
        braced!(contents in input);
        let fields = Punctuated::<EnumField, Token![,]>::parse_terminated(&contents)?
            .into_iter().collect();
        Ok(Self { name, fields })
    }
}

impl Gen for Enum {
    fn gen(&self) -> Result<TokenStream2> {
        let name = &self.name;
        let mut variants = TokenStream2::new();
        let mut try_from_str = TokenStream2::new();
        let mut into_str = TokenStream2::new();
        for field in &self.fields {
            let name = &field.name;
            let string = &field.string;
            variants.extend(quote! {
                #name,
            });
            into_str.extend(quote! {
                Self::#name => f.write_str(#string),
            });
            try_from_str.extend(quote! {
                #string => Ok(Self::#name),
            });
        }
        Ok(quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #variants
            }

            impl std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #into_str
                    }
                }
            }

            impl TryFrom<&str> for #name {
                type Error = ();
                fn try_from(value: &str) -> Result<Self, Self::Error> {
                    match value {
                        #try_from_str
                        _ => Err(()),
                    }
                }
            }
        })
    }
}

enum Item {
    MatchRule(MatchRule),
    EnumRule(EnumRule),
    Enum(Enum),
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::rule) {
            Ok(Item::MatchRule(input.parse()?))
        }
        else if input.peek(Token![enum]) {
            if input.peek2(kw::rule) {
                Ok(Item::EnumRule(input.parse()?))
            }
            else {
                Ok(Item::Enum(input.parse()?))
            }
        }
        else {
            Err(Error::new(Span::call_site(), "expected rule"))
        }
    }
}

impl Gen for Item {
    fn gen(&self) -> Result<TokenStream2> {
        match self {
            Item::MatchRule(r) => r.gen(),
            Item::EnumRule(r) => r.gen(),
            Item::Enum(e) => e.gen(),
        }
    }
}

struct Rules {
    uses: Vec<ItemUse>,
    items: Vec<Item>,
}

impl Parse for Rules {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            uses: parse_list(input)?,
            items: parse_list(input)?,
        })
    }
}

impl Gen for Rules {
    fn gen(&self) -> Result<TokenStream2> {
        let mut stream = TokenStream2::new();
        for use_ in &self.uses {
            stream.extend(quote! { #use_ });
        }
        for rule in &self.items {
            stream.extend(rule.gen()?);
        }
        Ok(quote! {
            pub mod ast {
                use unicode_xid::UnicodeXID;
                use crate::src::{Loc, Message};
                use crate::parser::{Parser, Rule, ExprMeta};
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
