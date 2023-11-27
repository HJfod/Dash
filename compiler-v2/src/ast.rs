
use std::{sync::Arc, ops::Range};
use crate::{src::{Src, SrcPool}, logger::LoggerRef, grammar::GrammarFile};

#[derive(Debug, Clone)]
pub struct ArcSpan(pub Arc<Src>, pub Range<usize>);

#[derive(Debug)]
pub enum Child {
    Node(Node),
    Maybe(Option<Node>),
    List(Vec<Node>),
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub struct Node {
    // not a HashMap because ast nodes have at most like 7 children
    // in the future this could be replaced by a stack-allocated max-size
    // arrayvector if the performance benefit becomes necessary
    children: Vec<(String, Child)>,
    // for AST nodes representing literals
    value: Option<Value>,
    span: ArcSpan,
}

impl Node {
    pub fn new_empty(span: ArcSpan) -> Self {
        Self { children: vec![], value: None, span }
    }
    pub fn new(children: Vec<(String, Child)>, span: ArcSpan) -> Self {
        Self { children, value: None, span }
    }
    pub fn new_with_value(value: Value, span: ArcSpan) -> Self {
        Self { children: vec![], value: Some(value), span }
    }
    pub fn span(&self) -> ArcSpan {
        self.span.clone()
    }
}

pub struct ASTPool {
    asts: Vec<Node>,
}

impl ASTPool {
    pub fn parse_src_pool(pool: &SrcPool, grammar: &GrammarFile, logger: LoggerRef) -> Self {
        Self {
            asts: pool.iter()
                .map(|src| grammar.exec(src.clone(), logger.clone()))
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
