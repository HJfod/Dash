
use dash_macros::{ParseNode, ResolveNode};
use super::{expr::{Expr, IdentPath, ExprList}, token::{lit, kw}};
use crate::{
    ast::token::delim,
    checker::{resolve::ResolveNode, coherency::Checker, ty::Ty, path}, parser::parse::{NodePool, Node}, shared::logger::{Message, Level, LoggerRef}
};

#[derive(Debug, ParseNode)]
#[parse(expected = "identifier")]
pub enum ItemUseNode {
    This(kw::This),
    Ident(IdentPath),
}

impl ResolveNode for ItemUseNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        for scope in checker.scopes() {
            if let Some(ent) = scope.entities().find(
                &match self {
                    Self::Ident(i) => i.get(pool).to_path(pool),
                    Self::This(_) => path::IdentPath::new([path::Ident::from("this")], false)
                }
            ) {
                return Some(ent.ty());
            }
        }
        None
    }
    fn log_unresolved_reason(&self, pool: &NodePool, _checker: &Checker, logger: LoggerRef) {
        match self {
            Self::Ident(i) => logger.lock().unwrap().log(Message::new(
                Level::Error,
                format!("Unknown item {}", i.get(pool).to_path(pool)),
                i.get(pool).span_or_builtin(pool).as_ref()
            )),
            Self::This(kw) => logger.lock().unwrap().log(Message::new(
                Level::Error,
                "'this' is not valid in this scope",
                kw.get(pool).span_or_builtin(pool).as_ref()
            )),
        }
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
