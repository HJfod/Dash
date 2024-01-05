
use dash_macros::{ParseNode, ResolveNode};
use super::{expr::{Expr, IdentPath, ExprList}, token::{lit, kw}};
use crate::{
    ast::token::delim,
    checker::{resolve::ResolveNode, coherency::Checker, ty::Ty, path}, parser::parse::NodePool
};

#[derive(Debug, ParseNode)]
#[parse(expected = "identifier")]
pub enum ItemUseNode {
    This(kw::This),
    Ident(IdentPath),
}

impl ResolveNode for ItemUseNode {
    fn try_resolve_node(&mut self, list: &mut NodePool, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ent) = scope.entities().find(
                &match self {
                    Self::Ident(i) => i.get(list).as_ref().to_path(list),
                    Self::This(_) => path::IdentPath::new([path::Ident::from("this")], false)
                }
            ) {
                return Some(ent.ty());
            }
        }
        // let msg_span = match self {
        //     Self::Ident(i) => (format!("Unknown item {}", i.to_path()), i.span(list)),
        //     Self::This(kw) => ("'this' is not valid in this scope".into(), kw.span(list)),
        // };
        // self.cache().unwrap().set_unresolved(msg_span.0, msg_span.1);
        None
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "expression")]
pub enum AtomNode {
    ClosedExpr(delim::Parenthesized<Expr>),
    Block(delim::Braced<ExprList>),
    ItemUse(ItemUse),
    String(lit::String),
    Float(lit::Float),
    Int(lit::Int),
    Bool(lit::Bool),
    Void(lit::Void),
}
