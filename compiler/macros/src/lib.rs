
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
    token::{Paren, Bracket},
    parse::{Parse, ParseStream}, 
    LitStr, LitChar,
    braced, Error, ItemUse, Field, ExprBlock,
    punctuated::Punctuated, bracketed, ItemFn,
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
    syn::custom_keyword!(until);
    syn::custom_keyword!(unless);
    syn::custom_keyword!(expected);
    syn::custom_keyword!(nofallthrough);
}

#[derive(Clone)]
enum ClauseTy {
    List(Vec<ClauseTy>),
    Vec(Box<ClauseTy>),
    Option(Box<ClauseTy>),
    Rule(Ident),
    Enum(Ident),
    Char,
    String,
    Default,
}

impl ClauseTy {
    fn inner_ty(&self) -> &ClauseTy {
        match self {
            Self::Vec(a) => a.as_ref(),
            Self::Option(a) => a.as_ref(),
            a => a,
        }
    }

    fn is_convertible(&self, other: &ClauseTy) -> bool {
        match (self, other) {
            (Self::List(a), Self::List(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.is_convertible(b))
            }
            (Self::Vec(a), Self::Vec(b)) => a.is_convertible(b),
            (Self::Option(a), Self::Option(b)) => a.is_convertible(b),
            (Self::Rule(a), Self::Rule(b)) => a == b,
            (Self::Enum(a), Self::Enum(b)) => a == b,
            (Self::Char, Self::Char) => true,
            (Self::String, Self::String) => true,
            (_, Self::Default) => true,
            (Self::Default, _) => true,
            _ => false,
        }
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
            Self::Enum(e) => {
                Ok(quote! { #e })
            }
            Self::Char => {
                Ok(quote! { char })
            }
            Self::String => {
                Ok(quote! { String })
            }
            Self::Default => {
                Ok(quote! { _ })
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
    EOF,
}

#[derive(Clone)]
enum RepeatMode {
    OneOrMore,
    ZeroOrMore,
    Until(Box<Clause>),
}

#[derive(Clone)]
enum Clause {
    // (?a b c) => { ... }
    List {
        peek_condition: Vec<Clause>,
        items: Vec<MaybeBinded>,
        rust: Option<ExprBlock>,
    },
    // a | b
    OneOf(Vec<Clause>),
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
    Rule {
        name: Ident,
        matcher: Option<Ident>,
        cast_as: Vec<Ident>
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
        if ahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            // enum variants are `Enum.Variant`
            if input.parse::<Token![.]>().is_ok() {
                res = if input.parse::<Token![*]>().is_ok() {
                    Clause::EnumVariant(ident, None)
                }
                else {
                    Clause::EnumVariant(ident, Some(input.parse()?))
                };
            }
            // otherwise this is a rule name
            else {
                let which = if input.peek(Bracket) {
                    let contents;
                    bracketed!(contents in input);
                    Some(contents.parse::<Ident>()?)
                }
                else {
                    None
                };
                let mut into = Vec::new();
                while input.parse::<Token![as]>().is_ok() {
                    into.push(input.parse()?);
                }
                if !into.is_empty() {
                    res = Clause::Rule {
                        name: ident,
                        matcher: which,
                        cast_as: into,
                    };
                }
                else {
                    res = match ident.to_string().as_str() {
                        "XID_Start"     => Clause::Char(Char::XidStart),
                        "XID_Continue"  => Clause::Char(Char::XidContinue),
                        "OP_CHAR"       => Clause::Char(Char::OpChar),
                        "EOF"           => Clause::Char(Char::EOF),
                        _ => Clause::Rule {
                            name: ident,
                            matcher: which,
                            cast_as: vec![],
                        },
                    };
                }
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
        else if ahead.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            res = Clause::Default;
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
            Ok(Clause::Repeat(res.into(), RepeatMode::Until(Self::parse_one_of(input)?.into())))
        }
        else if input.parse::<Token![?]>().is_ok() {
            Ok(Clause::Option(res.into(), None))
        }
        else if input.parse::<kw::unless>().is_ok() {
            Ok(Clause::Option(res.into(), Some(Self::parse_one_of(input)?.into())))
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
        }
        else {
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
        }
        else {
            Ok(Clause::ConcatVec(res))
        }
    }

    fn parse_one_of(input: ParseStream) -> Result<Self> {
        let mut res = vec![Self::parse_concat(input)?];
        while input.parse::<Token![|]>().is_ok() {
            res.push(Self::parse_concat(input)?);
        }
        if res.len() == 1 {
            Ok(res.remove(0))
        }
        else {
            Ok(Clause::OneOf(res))
        }
    }

    fn is_functional(&self) -> bool {
        match self {
            Clause::List { peek_condition: _, items: _, rust } => rust.is_some(),
            _ => false,
        }
    }

    fn eval_ty(&self) -> Result<ClauseTy> {
        match self {
            Clause::List { peek_condition, items, rust } => {
                if rust.is_some() {
                    return Err(Error::new(
                        Span::call_site(),
                        "code blocks cannot be used for non-functional matchers"
                    ));
                }
                let mut res = vec![];
                for opt in peek_condition {
                    opt.eval_ty()?;
                }
                for item in items {
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
                if peek_condition.is_empty() {
                    Ok(list)
                }
                else {
                    Ok(ClauseTy::Option(list.into()))
                }
            }
            Clause::OneOf(list) => {
                if list.is_empty() {
                    Err(Error::new(Span::call_site(), "internal error: empty one-of"))?;
                }
                let first = list.first().unwrap().eval_ty()?;
                for a in list.iter().skip(1) {
                    if !a.eval_ty()?.is_convertible(&first) {
                        Err(Error::new(Span::call_site(), "all one-of options must result in the same type"))?
                    }
                }
                Ok(first)
            }
            Clause::Option(clause, _) => {
                Ok(ClauseTy::Option(clause.eval_ty()?.into()))
            }
            Clause::Concat(list) => {
                for a in list {
                    if !matches!(a.eval_ty()?, ClauseTy::String | ClauseTy::Char) {
                        Err(Error::new(Span::call_site(), "you can only concat chars and strings"))?
                    }
                }
                Ok(ClauseTy::String)
            }
            Clause::ConcatVec(list) => {
                Ok(ClauseTy::Vec(list.first().unwrap().eval_ty()?.inner_ty().clone().into()))
            }
            Clause::Repeat(r, _) => {
                let ty = r.eval_ty()?;
                // automatically concat char* to string
                if matches!(ty, ClauseTy::Char) {
                    Ok(ClauseTy::String)
                }
                else {
                    Ok(ClauseTy::Vec(ty.into()))
                }
            }
            Clause::String(_) => {
                Ok(ClauseTy::String)
            }
            Clause::Char(c) => {
                Ok(if matches!(c, Char::EOF) {
                    ClauseTy::Default
                }
                else {
                    ClauseTy::Char
                })
            }
            Clause::Rule { name, matcher: _, cast_as } => {
                Ok(ClauseTy::Rule(cast_as.last().unwrap_or(name).clone()))
            }
            Clause::EnumVariant(e, _) => {
                Ok(ClauseTy::Enum(e.clone()))
            }
            Clause::Default => Ok(ClauseTy::Default)
        }
    }
}

impl Parse for Clause {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut peek_condition = Vec::new();
        let mut items = Vec::new();
        if input.parse::<Token![?]>().is_ok() {
            if input.parse::<Token![?]>().is_ok() {
                peek_condition.push(Self::parse_one_of(input)?);
            }
            else {
                let c = Self::parse_one_of(input)?;
                peek_condition.push(c.clone());
                items.push(MaybeBinded::Drop(c));
            }
        }
        loop {
            let ahead = input.lookahead1();
            if ahead.peek(kw::nofallthrough) {
                break;
            }
            if ahead.peek(Ident) {
                if input.peek2(Token![:]) {
                    let name = input.parse()?;
                    input.parse::<Token![:]>()?;
                    items.push(MaybeBinded::Named(name, Self::parse_one_of(input)?));
                }
                else {
                    items.push(MaybeBinded::Drop(Self::parse_one_of(input)?));
                }
            }
            else if ahead.peek(Token![_]) {
                input.parse::<Token![_]>()?;
                input.parse::<Token![:]>()?;
                items.push(MaybeBinded::Unnamed(Self::parse_one_of(input)?));
            }
            else if ahead.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                items.push(MaybeBinded::Unnamed(Self::parse_one_of(input)?));
            }
            else {
                if let Ok(p) = Self::parse_one_of(input) {
                    items.push(MaybeBinded::Drop(p));
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
        Ok(Clause::List { peek_condition, items, rust })
    }
}

enum GenCtx {
    None,
    TopLevel,
    TopLevelEnum,
}

impl Clause {
    fn gen_with_ctx(&self, ctx: GenCtx) -> Result<TokenStream2> {
        match self {
            Self::List { peek_condition, items, rust } => {
                let mut body = TokenStream2::new();
                let mut cond = TokenStream2::new();
                for c in peek_condition {
                    let b = c.gen_with_ctx(GenCtx::None)?;
                    cond.extend(quote! {
                        #b;
                    });
                }
                let mut binded_vars = vec![];
                for (i, c) in items.iter().enumerate() {
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
                    match ctx {
                        GenCtx::TopLevelEnum => {
                            for r in binded_vars {
                                result_stream.extend(quote! { #r, });
                            }
                            result_stream = quote! { Ok(Self::from(#result_stream)) };
                        }
                        GenCtx::TopLevel => {
                            for r in binded_vars {
                                result_stream.extend(quote! { #r, });
                            }
                            result_stream = quote! { Ok(Self {
                                #result_stream
                                meta: parser.get_meta(start),
                            }) };
                        }
                        GenCtx::None => {
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
                }
                cond = quote! {
                    {
                        #cond
                        ()
                    }
                };
                if !peek_condition.is_empty() {
                    if matches!(ctx, GenCtx::None) {
                        Ok(quote! { {
                            if crate::rule_peek!(parser, #cond) {
                                let start = parser.skip_ws();
                                #body
                                Some(#result_stream)
                            }
                            else {
                                None
                            }
                        } })
                    }
                    else {
                        Ok(quote! { {
                            let start = parser.skip_ws();
                            if crate::rule_peek!(parser, #cond) {
                                #body
                                #result_stream
                            }
                            else {
                                Err(parser.error(start, format!("Expected {}", Self::expected())))
                            }
                        } })
                    }
                }
                else {
                    Ok(quote! { {
                        let start = parser.skip_ws();
                        #body
                        #result_stream
                    } })
                }
            }
            Self::OneOf(list) => {
                let mut match_options = quote! {
                    let mut furthest_match: Option<(Loc, Message<'s>)> = None;
                };

                let ty = list.first().unwrap().eval_ty()?.gen()?;

                for mat in list {
                    let body = mat.gen()?;
                    match_options.extend(quote! {
                        match crate::rule_try!(parser, #body) {
                            Ok(r) => return Ok(r),
                            Err(e) => {
                                if !furthest_match.as_ref().is_some_and(|m| e.range.end <= m.0) {
                                    furthest_match = Some((e.range.end.clone(), e));
                                }
                            },
                        }
                    });
                }
                
                Ok(quote! {
                    || -> Result<#ty, Message<'s>> {
                        #match_options
                        Err(furthest_match.unwrap().1)
                    }()?
                })
            }
            Self::Option(clause, unless) => {
                let body = clause.gen()?;
                if let Some(unless) = unless {
                    let unless = unless.gen()?;
                    Ok(quote! {
                        if !crate::rule_peek!(parser, #unless) {
                            Some(#body)
                        }
                        else {
                            None
                        }
                    })
                }
                else {
                    Ok(quote! {
                        crate::rule_try!(parser, #body).ok()
                    })
                }
            }
            Self::Concat(list) => {
                let mut stream = quote! {
                    let mut res = String::new();
                };
                for l in list {
                    let b = l.gen()?;
                    if matches!(l.eval_ty()?, ClauseTy::Char) {
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
            Self::ConcatVec(list) => {
                let mut stream = quote! {
                    let mut res = Vec::new();
                };
                for l in list {
                    let b = l.gen()?;
                    stream.extend(quote! {
                        crate::helpers::ConcatInto::concat_into(#b, &mut res);
                    });
                }
                Ok(quote! {
                    {
                        #stream
                        res
                    }
                })
            }
            Self::Repeat(clause, mode) => {
                let body = clause.gen()?;
                // concat chars to a string
                let mut stream = if matches!(clause.eval_ty()?, ClauseTy::Char) {
                    quote! {
                        let mut res = String::new();
                    }
                }
                else {
                    quote! {
                        let mut res = Vec::new();
                    }
                };
                match mode {
                    RepeatMode::OneOrMore | RepeatMode::ZeroOrMore => {
                        if matches!(mode, RepeatMode::OneOrMore) {
                            stream.extend(quote! {
                                res.push(#body);
                            });
                        }
                        Ok(quote! {
                            {
                                #stream
                                while let Ok(b) = crate::rule_try!(parser, #body) {
                                    res.push(b);
                                }
                                res
                            }
                        })
                    }
                    RepeatMode::Until(until) => {
                        let until = until.gen()?;
                        Ok(quote! {
                            {
                                #stream
                                while crate::rule_try!(parser, #until).is_err() {
                                    res.push(#body);
                                }
                                res
                            }
                        })
                    }
                }
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
                    Char::EOF => {
                        quote! {
                            { parser.expect_eof()?; Default::default() }
                        }
                    }
                })
            }
            Self::Rule { name: rule, matcher, cast_as } => {
                let name = if let Some(which) = matcher {
                    format_ident!("expect_impl_{which}")
                }
                else {
                    format_ident!("expect")
                };
                let mut stream = quote! {
                    #rule::#name(parser)?
                };
                for rule in cast_as {
                    stream = quote! {
                        #rule::from(#stream)
                    };
                }
                Ok(stream)
            }
            Self::EnumVariant(e, v) => {
                if let Some(v) = v {
                    Ok(quote! {
                        #e::try_from(parser.expect_word(&#e::#v.to_string())?.as_str()).unwrap()
                    })
                }
                else {
                    Ok(quote! {
                        #e::expect_any(parser)?
                    })
                }
            }
            Self::Default => {
                Ok(quote! {
                    Default::default()
                })
            }
        }
    }

    fn gen_members(&self) -> Result<TokenStream2> {
        match self {
            Self::List { peek_condition, items, rust } => {
                let mut stream = TokenStream2::new();
                if peek_condition.is_empty() && rust.is_none() {
                    for item in items {
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
        self.gen_with_ctx(GenCtx::None)
    }
}

struct Match {
    result_type: Option<Ident>,
    name: Option<Ident>,
    clause: Clause,
}

impl Match {
    fn gen_with_ctx(&self, ctx: GenCtx) -> Result<TokenStream2> {
        self.clause.gen_with_ctx(ctx)
    }
}

impl Parse for Match {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![match]>()?;
        let name = if input.peek(Bracket) {
            let c;
            bracketed!(c in input);
            Some(c.parse::<Ident>()?)
        }
        else {
            None
        };
        let result_type = if input.parse::<Token![as]>().is_ok() {
            Some(input.parse()?)
        }
        else {
            None
        };
        let clause: Clause = input.parse()?;
        if !clause.is_functional() {
            input.parse::<Token![;]>()?;
            if result_type.is_some() {
                Err(Error::new(Span::call_site(), "as matchers must be functional"))?;
            }
        }
        Ok(Match { result_type, name, clause })
    }
}

struct ParseRule(RuleKind, Option<LitStr>, Vec<Match>, Vec<ItemFn>, Vec<ImplItemFn>);

impl Parse for ParseRule {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut kind = RuleKind::Struct { fields: vec![] };
        let mut matches = vec![];
        let mut fns = vec![];
        let mut impls = vec![];
        let mut expected = None;
        loop {
            if input.peek(Token![match]) {
                matches.push(input.parse::<Match>()?);
                continue;
            }
            if input.peek(Token![fn]) {
                fns.push(input.parse::<ItemFn>()?);
                continue;
            }
            if input.parse::<Token![impl]>().is_ok() {
                impls.push(input.parse::<ImplItemFn>()?);
                continue;
            }
            if input.parse::<Token![enum]>().is_ok() {
                if let RuleKind::Struct { ref fields } = kind {
                    if !fields.is_empty() {
                        Err(Error::new(Span::call_site(), "cannot have enums in struct rules"))?;
                    }
                }
                let variants = Punctuated::<Ident, Token![,]>::parse_separated_nonempty(input)?
                    .into_iter()
                    .collect();
                input.parse::<Token![;]>()?;
                kind = RuleKind::Enum { variants, };
                continue;
            }
            if input.parse::<kw::expected>().is_ok() {
                if expected.is_some() {
                    Err(Error::new(Span::call_site(), "cannot have multiple expected names"))?;
                }
                expected = Some(input.parse()?);
                input.parse::<Token![;]>()?;
                continue;
            }
            if input.peek(Ident) {
                if let RuleKind::Struct { ref mut fields } = kind {
                    fields.push(Field::parse_named(input)?);
                    input.parse::<Token![;]>()?;
                }
                else {
                    Err(Error::new(Span::call_site(), "cannot have members in enum rules"))?;
                }
                continue;
            }
            break;
        }
        Ok(Self(kind, expected, matches, fns, impls))
    }
}

enum RuleKind {
    Struct {
        fields: Vec<Field>,
    },
    Enum {
        variants: Vec<Ident>,
    },
}

struct Rule {
    name: Ident,
    expected: Option<LitStr>,
    kind: RuleKind,
    matches: Vec<Match>,
    fns: Vec<ItemFn>,
    impls: Vec<ImplItemFn>,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::rule>()?;
        let name = input.parse()?;
        let contents;
        braced!(contents in input);
        let ParseRule(kind, expected, matches, fns, impls) = contents.parse()?;
        Ok(Self { name, expected, kind, matches, fns, impls })
    }
}

impl Gen for Rule {
    fn gen(&self) -> Result<TokenStream2> {
        if self.matches.is_empty() {
            return Err(Error::new(self.name.span(), "rules must have at least one match statement"));
        }
        let name = &self.name;
        let first = &self.matches.first().unwrap().clause;

        let mut fns = TokenStream2::new();
        let mut trait_impls = TokenStream2::new();

        let (decl, meta, is_enum) = match &self.kind {
            RuleKind::Struct { fields } => {
                let mut members = TokenStream2::new();
                members.extend(first.gen_members()?);
                for field in fields {
                    members.extend(quote! {
                        #field,
                    });
                }
                members.extend(quote! {
                    meta: ExprMeta<'s>,
                });
                (quote! {
                    struct #name<'s> {
                        #members
                    }
                }, quote! {
                    &self.meta
                }, false)
            }
            RuleKind::Enum { variants } => {
                let mut meta_variants = TokenStream2::new();
                let mut variants_stream = TokenStream2::new();
                for var in variants {
                    let var_name = &var;
                    variants_stream.extend(quote! {
                        #var_name(Box<#var_name<'s>>),
                    });
                    trait_impls.extend(quote! {
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
                (quote! {
                    enum #name<'s> {
                        #variants_stream
                    }
                }, quote! {
                    match self {
                        #meta_variants
                    }
                }, true)
            }
        };

        if first.is_functional() {
            for mat in self.matches.iter().skip(1) {
                if !mat.clause.is_functional() {
                    return Err(Error::new(self.name.span(), "all matchers must be functional"));
                }
            }
        }
        else if !is_enum {
            let first_ty = first.eval_ty()?;
            for mat in self.matches.iter().skip(1) {
                if !mat.clause.eval_ty()?.is_convertible(&first_ty) {
                    return Err(Error::new(self.name.span(), "all matches must evaluate to the same type"));
                }
            }
        }
        
        let fallthrough_matcher_count = self.matches.iter().filter(|f| f.result_type.is_none()).count();
        if fallthrough_matcher_count > 1 && self.expected.is_none() {
            return Err(Error::new(self.name.span(), "an expected clause is required with multiple matchers"));
        }

        for fun in &self.fns {
            fns.extend(quote! { #fun });
        }

        if let Some(ref expected) = self.expected {
            fns.extend(quote! {
                pub fn expected() -> &'static str {
                    #expected
                }
            });
        }

        let mut match_options = quote! {
        };
        if fallthrough_matcher_count > 1 {
            match_options.extend(quote! {
                let start = parser.skip_ws();
            });
        }
        let mut prev_matcher: Option<Ident> = None;

        for (i, mat) in self.matches.iter().rev().enumerate() {
            let expect_name = format_ident!(
                "expect_impl_{}",
                mat.name.as_ref()
                    .map(|s| s.to_string())
                    .unwrap_or(i.to_string())
            );
            let ty = mat.result_type.clone()
                .map(|e| quote! { #e<'s> })
                .unwrap_or(quote! { Self });
            let body = mat.gen_with_ctx(
                if is_enum {
                    GenCtx::TopLevelEnum
                }
                else {
                    GenCtx::TopLevel
                }
            )?;
            let prev_match_call = if prev_matcher.is_some() && mat.result_type.is_none() {
                let call = prev_matcher.as_ref().unwrap();
                quote! {
                    Err(_) => Self::#call(parser)
                }
            }
            else {
                quote! {
                    Err(e) => Err(e)
                }
            };
            if !(mat.result_type.is_none() && fallthrough_matcher_count == 1) {
                fns.extend(quote! {
                    fn #expect_name(parser: &mut Parser<'s>) -> Result<#ty, Message<'s>> {
                        match crate::rule_try!(parser, #body?) {
                            Ok(r) => Ok(r),
                            #prev_match_call,
                        }
                    }
                });
            }
            if mat.result_type.is_none() {
                if fallthrough_matcher_count == 1 {
                    match_options.extend(quote! {
                        #body
                    });
                }
                else {
                    prev_matcher = Some(expect_name.clone());
                    match_options.extend(quote! {
                        match Self::#expect_name(parser) {
                            Ok(r) => return Ok(r),
                            Err(e) => {},
                        }
                    });
                }
            }
        }

        if fallthrough_matcher_count > 1 {
            match_options.extend(quote! {
                Err(parser.error(start, format!("Expected {}", Self::expected())))
            });
        }

        trait_impls.extend(quote! {
            impl<'s> Rule<'s> for #name<'s> {
                fn expect(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #match_options
                }

                fn meta(&self) -> &ExprMeta<'s> {
                    #meta
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
                    return Err(Error::new(fun.sig.ident.span(), "unknown impl"));
                }
            }
        }

        Ok(quote! {
            #[derive(Debug)]
            pub #decl

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
    lit_name: LitStr,
    fields: Vec<EnumField>,
}

impl Parse for Enum {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![enum]>()?;
        let name = input.parse()?;
        input.parse::<Token![as]>()?;
        let lit_name = input.parse()?;
        let contents;
        braced!(contents in input);
        let fields = Punctuated::<EnumField, Token![,]>::parse_terminated(&contents)?
            .into_iter().collect();
        Ok(Self { name, lit_name, fields })
    }
}

impl Gen for Enum {
    fn gen(&self) -> Result<TokenStream2> {
        let name = &self.name;
        let lit_name = &self.lit_name;
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

            impl #name {
                fn expect_any<'s>(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    let start = parser.skip_ws();
                    let word = parser.next_word(#lit_name)?;
                    match Self::try_from(word.as_str()) {
                        Ok(s) => Ok(s),
                        Err(e) => {
                            let msg = parser.error(start, format!("Expected '{}', got '{word}'", #lit_name));
                            parser.goto(start);
                            Err(msg)
                        }
                    }
                }
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
    MatchRule(Rule),
    Enum(Enum),
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::rule) {
            Ok(Item::MatchRule(input.parse()?))
        }
        else if input.peek(Token![enum]) {
            Ok(Item::Enum(input.parse()?))
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
