
use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use syn::{Token, Error, parenthesized};
use syn::{Result, Path, braced};

use crate::defs::kw;

pub enum FindKind {
    Type,
    Entity,
}

pub enum TypeClause {
    // A -> B
    Convertible(Box<TypeClause>, Box<TypeClause>),
    // find A as b
    Find(Ident, FindKind),
    // type::name
    Type(Path),
}

impl Parse for TypeClause {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::find>().is_ok() {
            let name = input.parse()?;
            input.parse::<Token![as]>()?;
            let kind = input.parse::<Ident>()?;
            Ok(Self::Find(name, match kind.to_string().as_str() {
                "type" => FindKind::Type,
                "entity" => FindKind::Entity,
                _ => Err(Error::new(kind.span(), "invalid find kind"))?
            }))
        }
        else {
            let a = Self::Type(input.parse()?);
            if input.parse::<Token![->]>().is_ok() {
                Ok(Self::Convertible(a.into(), input.parse()?))
            }
            else {
                Ok(a)
            }
        }
    }
}

pub struct TypeCheck {
    new_scope: bool,
    clauses: Vec<TypeClause>,
    return_clause: TypeClause,
}

impl Parse for TypeCheck {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::typecheck>()?;
        let new_scope = if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            content.parse::<kw::new>()?;
            content.parse::<kw::scope>()?;
            true
        }
        else {
            false
        };
        let content;
        braced!(content in input);
        let mut clauses = vec![];
        while let Ok(clause) = content.parse::<TypeClause>() {
            clauses.push(clause);
            content.parse::<Token![;]>()?;
        }
        content.parse::<Token![yield]>()?;
        let return_clause = content.parse()?;
        content.parse::<Token![;]>()?;
        Ok(Self { clauses, return_clause, new_scope })
    }
}
