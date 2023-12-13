
use std::{sync::Arc, ops::Range, fmt::Debug};
use crate::shared::src::{Src, SrcPool, Span};
use crate::shared::logger::LoggerRef;
use crate::parser::grammar::GrammarFile;
use crate::parser::ParseOptions;

use super::path::IdentPath;
use super::tests::Check;
use super::ty::Ty;

#[derive(Clone)]
pub struct ArcSpan(pub Arc<Src>, pub Range<usize>);

impl Debug for ArcSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl ArcSpan {
    pub fn as_ref(&self) -> Span {
        Span(self.0.as_ref(), self.1.clone())
    }
}

pub enum Child {
    Node(Node),
    Maybe(Option<Node>),
    List(Vec<Node>),
}

impl Debug for Child {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Node(n) => Debug::fmt(n, f),
            Self::Maybe(n) => if let Some(n) = n {
                Debug::fmt(n, f)
            }
            else {
                write!(f, "<none>")
            },
            Self::List(list) => f.debug_list()
                .entries(list)
                .finish()
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Default)]
pub struct Children(Vec<(String, Child)>);

impl Children {
    pub fn get(&self, name: &str) -> Option<&Child> {
        self.0.iter().find(|c| c.0 == name).map(|c| &c.1)
    }
}

impl From<Vec<(String, Child)>> for Children {
    fn from(value: Vec<(String, Child)>) -> Self {
        Self(value)
    }
}

impl<'a> IntoIterator for &'a Children {
    type IntoIter = <&'a Vec<(String, Child)> as IntoIterator>::IntoIter;
    type Item = <&'a Vec<(String, Child)> as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut Children {
    type IntoIter = <&'a mut Vec<(String, Child)> as IntoIterator>::IntoIter;
    type Item = <&'a mut Vec<(String, Child)> as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

pub struct Node {
    pub(super) name: String,
    // not a HashMap because ast nodes have at most like 7 children
    // in the future this could be replaced by a stack-allocated max-size
    // arrayvector if the performance benefit becomes necessary
    pub(super) children: Children,
    // for AST nodes representing literals
    pub(super) value: Option<Value>,
    pub(super) span: ArcSpan,
    pub(super) resolved_ty: Option<Ty>,
    pub(super) check: Check,
}

impl Node {
    pub(crate) fn new_empty<S: Into<String>>(name: S, span: ArcSpan) -> Self {
        Self {
            name: name.into(), children: Default::default(),
            value: None, span, resolved_ty: None,
            check: Check::default(),
        }
    }
    pub(crate) fn new<S: Into<String>>(
        name: S,
        children: Children,
        span: ArcSpan,
        check: Check,
    ) -> Self {
        Self {
            name: name.into(), children, value: None,
            span, resolved_ty: None, check,
        }
    }
    pub(crate) fn new_with_value<S: Into<String>>(name: S, value: Value, span: ArcSpan) -> Self {
        Self {
            name: name.into(), children: Default::default(),
            value: Some(value), span, resolved_ty: None,
            check: Check::default(),
        }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn span(&self) -> ArcSpan {
        self.span.clone()
    }
    pub(crate) fn to_ident_path(&self) -> IdentPath {
        IdentPath::parse(&self.span.0.data()[self.span.1.clone()])
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct(&self.name);
        for child in &self.children {
            dbg.field(&child.0, &child.1);
        }
        if let Some(value) = &self.value {
            dbg.field("value", value);
        }
        dbg.field("span", &self.span);
        dbg.finish()
    }
}

pub struct ASTPool {
    asts: Vec<Node>,
}

impl<'s: 'g, 'g> ASTPool {
    pub fn parse_src_pool(
        pool: &SrcPool,
        grammar: &'s GrammarFile<'g>,
        logger: LoggerRef,
        options: ParseOptions
    ) -> Self {
        Self {
            asts: pool.iter()
                .map(|src| grammar.exec(src.clone(), logger.clone(), options.clone()))
                .collect()
        }
    }

    pub fn iter(&self) -> <&Vec<Node> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a ASTPool {
    type Item = &'a Node;
    type IntoIter = <&'a Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter()
    }
}

impl<'a> IntoIterator for &'a mut ASTPool {
    type Item = &'a mut Node;
    type IntoIter = <&'a mut Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter_mut()
    }
}
