
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
    parse::{Parse, ParseStream}, LitStr, LitChar, braced, ExprBlock,
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

enum Clause {
    // (a b c)
    List(Vec<Clause>),
    // a | b | c
    OneOf(Vec<Clause>),
    // a || b || c
    Enum(Vec<Clause>),
    // A*
    Repeat(Box<Clause>),
    // A?
    Optional(Box<Clause>),
    // "string literal"
    String(LitStr),
    // 'c'
    Char(LitChar),
    // a:A
    NamedBind(Ident, Box<Clause>),
    // :A
    UnnamedBind(Box<Clause>),
    // A
    Rule(Ident),
    // A as B
    RuleAs(Ident, Ident),
}

enum ClauseTy {
    List(Vec<ClauseTy>),
    Rule(Ident),
}

impl Clause {
    fn parse_inner(input: ParseStream) -> Result<Self> {
        let ahead = input.lookahead1();
        if ahead.peek(Paren) {
            let content;
            parenthesized!(content in input);
            Ok(Clause::List(parse_list(&content)?))
        }
        else if ahead.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Clause::UnnamedBind(input.parse()?))
        }
        else if ahead.peek(Ident) {
            if input.peek2(Token![:]) {
                let name = input.parse::<Ident>()?;
                input.parse::<Token![:]>()?;
                Ok(Clause::NamedBind(name, input.parse()?))
            }
            else {
                let ident = input.parse::<Ident>()?;
                Ok(Clause::Rule(ident))
            }
        }
        else if ahead.peek(LitStr) {
            Ok(Clause::String(input.parse()?))
        }
        else if ahead.peek(LitChar) {
            Ok(Clause::Char(input.parse()?))
        }
        else {
            return Err(ahead.error());
        }
    }
}

impl Parse for Clause {
    fn parse(input: ParseStream) -> Result<Self> {
        let first = Self::parse_inner(input)?;
        if input.parse::<Token![*]>().is_ok() {
            Ok(Clause::Repeat(first.into()))
        }
        else if input.parse::<Token![?]>().is_ok() {
            Ok(Clause::Optional(first.into()))
        }
        else if input.parse::<Token![|]>().is_ok() {
            let mut clauses = vec![];
            clauses.push(first);
            loop {
                match input.parse()? {
                    Clause::OneOf(o) => clauses.extend(o),
                    o => clauses.push(o),
                }
                if input.parse::<Token![|]>().is_err() {
                    break;
                }
            }
            Ok(Clause::OneOf(clauses))
        }
        else if input.parse::<Token![||]>().is_ok() {
            let mut clauses = vec![];
            clauses.push(first);
            loop {
                match input.parse()? {
                    Clause::Enum(o) => clauses.extend(o),
                    o => clauses.push(o),
                }
                if input.parse::<Token![||]>().is_err() {
                    break;
                }
            }
            Ok(Clause::Enum(clauses))
        }
        else {
            Ok(first)
        }
    }
}

struct Match {
    clause: Clause,
}

impl Parse for Match {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![match]>()?;
        Ok(Match { clause: Clause::List(parse_list(input)?) })
    }
}

struct Rule {
    name: Ident,
    matches: Vec<Match>,
    impls: Vec<ImplItemFn>,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::rule>()?;
        let name = input.parse()?;
        let contents;
        braced!(contents in input);
        let mut matches = vec![];
        let mut impls = vec![];
        loop {
            if contents.peek(Token![match]) {
                matches.push(contents.parse::<Match>()?);
            }
            if contents.parse::<Token![impl]>().is_ok() {
                impls.push(contents.parse::<ImplItemFn>()?);
            }
            break;
        }
        Ok(Self { name, matches, impls })
    }
}

impl Gen for Rule {
    fn gen(&self) -> Result<TokenStream2> {
        let mut stream = TokenStream2::new();
        let name = self.name;
        Ok(quote! {
            struct #name {

            }
        })
    }
}

struct Rules {
    rules: Vec<Rule>,
}

impl Parse for Rules {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self { rules: parse_list(input)? })
    }
}

impl Gen for Rules {
    fn gen(&self) -> Result<TokenStream2> {
        let mut stream = TokenStream2::new();
        for rule in &self.rules {
            stream.extend(rule.gen()?);
        }
        Ok(stream)
    }
}

#[proc_macro]
pub fn define_rules(input: TokenStream) -> TokenStream {
    match parse_macro_input!(input as Rules).gen() {
        Ok(s) => s.into(),
        Err(e) => TokenStream::from(e.to_compile_error())
    }
}
