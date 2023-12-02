
use std::{sync::Arc, ops::Range, fmt::Debug};
use crate::shared::src::{Src, SrcPool, Span};
use crate::shared::logger::LoggerRef;
use crate::parser::grammar::{GrammarFile, Check};
use crate::parser::ParseOptions;

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

pub enum Child<'n, 'g> {
    Node(Node<'n, 'g>),
    Maybe(Option<Node<'n, 'g>>),
    List(Vec<Node<'n, 'g>>),
}

impl Debug for Child<'_, '_> {
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

pub struct Node<'n, 'g> {
    pub(crate) name: String,
    // not a HashMap because ast nodes have at most like 7 children
    // in the future this could be replaced by a stack-allocated max-size
    // arrayvector if the performance benefit becomes necessary
    pub(crate) children: Vec<(String, Child<'n, 'g>)>,
    // for AST nodes representing literals
    pub(crate) value: Option<Value>,
    pub(crate) span: ArcSpan,
    pub(crate) resolved_ty: Option<Ty<'n, 'g>>,
    pub(crate) check: Option<&'g Check<'g>>,
}

impl<'n, 'g> Node<'n, 'g> {
    pub fn new_empty<S: Into<String>>(name: S, span: ArcSpan) -> Self {
        Self {
            name: name.into(), children: vec![], value: None,
            span, resolved_ty: None, check: None,
        }
    }
    pub fn new<S: Into<String>>(
        name: S,
        children: Vec<(String, Child<'n, 'g>)>,
        span: ArcSpan,
        check: Option<&'g Check<'g>>,
    ) -> Self {
        Self {
            name: name.into(), children, value: None,
            span, resolved_ty: None, check,
        }
    }
    pub fn new_with_value<S: Into<String>>(name: S, value: Value, span: ArcSpan) -> Self {
        Self {
            name: name.into(), children: vec![], value: Some(value),
            span, resolved_ty: None, check: None,
        }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn span(&self) -> ArcSpan {
        self.span.clone()
    }
    pub fn child(&self, name: &str) -> Option<&Child<'n, 'g>> {
        self.children.iter().find(|c| c.0 == name).map(|c| &c.1)
    }
}

impl Debug for Node<'_, '_> {
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

pub struct ASTPool<'n, 'g> {
    asts: Vec<Node<'n, 'g>>,
}

impl<'n, 's: 'g, 'g> ASTPool<'n, 'g> {
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

    pub fn iter(&self) -> <&Vec<Node<'n, 'g>> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'n, 'a, 'g> IntoIterator for &'a ASTPool<'n, 'g> {
    type Item = &'a Node<'n, 'g>;
    type IntoIter = <&'a Vec<Node<'n, 'g>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter()
    }
}
