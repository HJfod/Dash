
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;
use std::{collections::HashSet, hash::Hash};
use unicode_xid::UnicodeXID;

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

use crate::{clause::{Clause, RuleClause}, gen::GenCtx, defs::{kw, Gen, parse_list}};

pub struct Match {
    result_type: Option<Ident>,
    name: Option<Ident>,
    clause: Clause,
    afterwards: Vec<(Box<Clause>, RuleClause)>,
}

impl Match {
    fn gen_with_ctx(&self, ctx: GenCtx) -> Result<TokenStream2> {
        let body = self.clause.gen_with_ctx(ctx)?;
        if !self.afterwards.is_empty() {
            let mut stream = quote! {};
            for (peek, rule) in &self.afterwards {
                let peek = peek.gen()?;
                let rule = rule.gen("expect_with", "expect_with_", Some(quote! { res }))?;
                stream.extend(quote! {
                    if crate::rule_peek!(parser, #peek) {
                        res = #rule.into();
                        continue;
                    }
                });
            }
            Ok(quote! { {
                let mut res = { #body }?;
                loop {
                    #stream
                    break;
                }
                Ok(res)
            } })
        }
        else {
            Ok(body)
        }
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
        let mut afterwards = Vec::new();
        if input.parse::<kw::afterwards>().is_ok() {
            while input.parse::<kw::while_peek>().is_ok() {
                let clause = Clause::parse_one(input)?.into();
                input.parse::<kw::into>()?;
                let rule = input.parse()?;
                afterwards.push((clause, rule));
            }
        }
        if !clause.is_functional() {
            input.parse::<Token![;]>()?;
            if result_type.is_some() {
                Err(Error::new(Span::call_site(), "as matchers must be functional"))?;
            }
        }
        Ok(Match { result_type, name, clause, afterwards })
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

pub struct TypeCheck {}

pub struct Rule {
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

        let mut matcher_body = quote! {};
        let mut with_matcher_fn = None;
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
            let err_branch = if prev_matcher.is_some() && mat.result_type.is_none() {
                let call = prev_matcher.as_ref().unwrap();
                quote! {
                    Self::#call(parser)
                }
            }
            else {
                if self.expected.is_some() {
                    quote! {
                        Err(parser.error(start, format!("Expected {}", Self::expected())))
                    }
                }
                else {
                    quote! {
                        Err(e)
                    }
                }
            };
            let body;
            let args = mat.clause.get_arg_tys()?;
            if !args.is_empty() {
                let expect_with_name = format_ident!(
                    "expect_with_{}",
                    mat.name.as_ref()
                        .map(|s| s.to_string())
                        .unwrap_or(i.to_string())
                );
                let mut pass_args_stream = quote! {};
                let mut args_stream = quote! {};
                for (name, ty) in args {
                    let ty = ty.gen()?;
                    pass_args_stream.extend(quote! {
                        #name,
                    });
                    args_stream.extend(quote! {
                        #name: #ty,
                    });
                }
                body = mat.clause.gen_top_prefun(expect_with_name.clone())?;
                let with_body = mat.gen_with_ctx(GenCtx::TopLevel { is_enum, err_branch })?;
                fns.extend(quote! {
                    fn #expect_with_name(parser: &mut Parser<'s>, #args_stream) -> Result<#ty, Message<'s>> {
                        #with_body
                    }
                });
                with_matcher_fn = Some(quote! {
                    fn expect_with(parser: &mut Parser<'s>, #args_stream) -> Result<#ty, Message<'s>> {
                        Self::#expect_with_name(parser, #pass_args_stream)
                    }
                });
            }
            else {
                body = mat.gen_with_ctx(GenCtx::TopLevel { is_enum, err_branch })?;
            }
            if !(mat.result_type.is_none() && fallthrough_matcher_count == 1) {
                fns.extend(quote! {
                    fn #expect_name(parser: &mut Parser<'s>) -> Result<#ty, Message<'s>> {
                        #body
                    }
                });
            }
            if mat.result_type.is_none() {
                prev_matcher = Some(expect_name.clone());
                matcher_body = body;
            }
        }

        if let Some(with_matcher_fn) = with_matcher_fn {
            fns.extend(with_matcher_fn);
        }

        trait_impls.extend(quote! {
            impl<'s> Rule<'s> for #name<'s> {
                fn expect(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
                    #matcher_body
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

pub struct EnumField {
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

pub struct Enum {
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
                            let msg = parser.error(start, format!("Invalid {} '{word}'", #lit_name));
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

#[derive(Clone)]
pub enum Keyword {
    Strict(LitStr),
    Contextual(LitStr),
    Reserved(LitStr),
}

impl Keyword {
    pub fn lit(&self) -> &LitStr {
        match self {
            Self::Strict(lit) | Self::Contextual(lit) | Self::Reserved(lit) => &lit,
        }
    }

    pub fn to_string(&self) -> String {
        self.lit().value()
    }
}

impl PartialEq for Keyword {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Keyword {}

impl Hash for Keyword {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl Parse for Keyword {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::reserve>().is_ok() {
            Ok(Keyword::Reserved(input.parse()?))
        }
        else if input.parse::<kw::contextual>().is_ok() {
            Ok(Keyword::Contextual(input.parse()?))
        }
        else {
            Ok(Keyword::Strict(input.parse()?))
        }
    }
}

pub enum Item {
    MatchRule(Rule),
    Enum(Enum),
    Keywords(Vec<Keyword>),
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::rule) {
            Ok(Item::MatchRule(input.parse()?))
        }
        else if input.peek(Token![enum]) {
            Ok(Item::Enum(input.parse()?))
        }
        else if input.parse::<kw::keywords>().is_ok() {
            let kws;
            braced!(kws in input);
            Ok(Item::Keywords(
                Punctuated::<Keyword, Token![,]>::parse_terminated(&kws)?
                    .into_iter().collect()
            ))
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
            Item::Keywords(_) => Ok(quote! {}),
        }
    }
}

pub struct Rules {
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

        let mut all_kws = HashSet::new();
        for rule in &self.items {
            match rule {
                Item::Keywords(kws) => {
                    for kw in kws {
                        if all_kws.contains(kw) {
                            Err(Error::new(kw.lit().span(), "keyword is already defined"))?;
                        }
                        all_kws.insert(kw.clone());
                    }
                }
                _ => {}
            }
        }
        
        for rule in &self.items {
            stream.extend(rule.gen()?);
            match rule {
                Item::MatchRule(rule) => {
                    rule.matches.iter()
                        .map(|m| m.clause.verify_keywords(&all_kws))
                        .collect::<Result<_>>()?;
                }
                _ => {}
            }
        }

        let mut kw_stream = quote! {};
        let mut contextual_kw_stream = quote! {};
        let mut reserved_kw_stream = quote! {};
        for kw in all_kws {
            match kw {
                Keyword::Strict(kw) => kw_stream.extend(quote! { #kw => true, }),
                Keyword::Contextual(kw) => contextual_kw_stream.extend(quote! { #kw => true, }),
                Keyword::Reserved(kw) => reserved_kw_stream.extend(quote! { #kw => true, }),
            }
        }

        Ok(quote! {
            pub mod ast {
                use unicode_xid::UnicodeXID;
                use crate::src::{Loc, Message};
                use crate::parser::{Parser, Rule, ExprMeta};

                fn is_keyword(value: &str) -> bool {
                    match value {
                        #kw_stream
                        _ => false
                    }
                }

                fn is_contextual_keyword(value: &str) -> bool {
                    match value {
                        #contextual_kw_stream
                        _ => false
                    }
                }

                fn is_reserved_keyword(value: &str) -> bool {
                    match value {
                        #reserved_kw_stream
                        _ => false
                    }
                }

                #stream
            }
        })
    }
}
