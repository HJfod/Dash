
use std::{sync::Arc, ops::Range, fmt::Debug};
use crate::shared::src::{Src, SrcPool, Span};
use crate::shared::logger::LoggerRef;
use crate::parser::grammar::GrammarFile;
use crate::parser::ParseOptions;

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

pub struct Node {
    name: String,
    // not a HashMap because ast nodes have at most like 7 children
    // in the future this could be replaced by a stack-allocated max-size
    // arrayvector if the performance benefit becomes necessary
    children: Vec<(String, Child)>,
    // for AST nodes representing literals
    value: Option<Value>,
    span: ArcSpan,
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

impl Node {
    pub fn new_empty<S: Into<String>>(name: S, span: ArcSpan) -> Self {
        Self { name: name.into(), children: vec![], value: None, span }
    }
    pub fn new<S: Into<String>>(name: S, children: Vec<(String, Child)>, span: ArcSpan) -> Self {
        Self { name: name.into(), children, value: None, span }
    }
    pub fn new_with_value<S: Into<String>>(name: S, value: Value, span: ArcSpan) -> Self {
        Self { name: name.into(), children: vec![], value: Some(value), span }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn span(&self) -> ArcSpan {
        self.span.clone()
    }
}

pub struct ASTPool {
    asts: Vec<Node>,
}

impl ASTPool {
    pub fn parse_src_pool(
        pool: &SrcPool,
        grammar: &GrammarFile,
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
