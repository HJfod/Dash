
use std::ops::RangeBounds;
use std::{collections::HashMap, fmt::Display, sync::Arc};
use crate::logger::LoggerRef;

use super::ast::{Node, Child, Value, ArcSpan};
use super::grammar::{Rule, GrammarFile, MemberKind, Grammar, Item, IfGrammar, TokenItem};
use super::tokenizer::{Tokenizer, TokenKind, Token};
use super::src::Src;
use super::logger::{Message, Level};

impl Node {
    pub fn from(value: Token<'_>, src: Arc<Src>) -> Self {
        Node::new_with_value(
            match value.kind {
                TokenKind::Keyword | TokenKind::Op |
                TokenKind::Punct | TokenKind::Ident |
                TokenKind::Parentheses(_) | TokenKind::Braces(_) |
                TokenKind::Brackets(_) => None,
                TokenKind::Int(n) => Some(Value::Int(n)),
                TokenKind::Float(n) => Some(Value::Float(n)),
                TokenKind::String(s) => Some(Value::String(s)),
                TokenKind::Error(_) => unreachable!("errors should never be matched")
            },
            ArcSpan(src, value.span.1)
        )
    }
}

enum Var {
    None,
    Node(Node),
    List(Vec<Node>),
    Maybe(Option<Node>),
}

impl Var {
    fn assign(&mut self, node: Node) {
        match self {
            Var::None => *self = Var::Node(node),
            Var::Node(_) => panic!(
                "internal compiler error: attempted to assign a child more than once \
                - grammar file is invalid"
            ),
            Var::List(list) => list.push(node),
            Var::Maybe(opt) => *opt = Some(node),
        }
    }
}

impl<'s, 'g> IfGrammar<'g> {
    fn peek(token: &TokenItem<'_>, tokenizer: &Tokenizer<'s, 'g>) -> bool {
        let Some(tk) = tokenizer.peek() else { return false; };
        match token {
            TokenItem::Braces => matches!(tk.kind, TokenKind::Braces(_)),
            TokenItem::Brackets => matches!(tk.kind, TokenKind::Brackets(_)),
            TokenItem::Parentheses => matches!(tk.kind, TokenKind::Parentheses(_)),
            TokenItem::Ident => matches!(tk.kind, TokenKind::Ident),
            TokenItem::Int => matches!(tk.kind, TokenKind::Int(_)),
            TokenItem::Float => matches!(tk.kind, TokenKind::Float(_)),
            TokenItem::String => matches!(tk.kind, TokenKind::String(_)),
            TokenItem::Token(s) => tk.raw == *s,
        }
    }

    fn exec(
        &self,
        src: Arc<Src>,
        tokenizer: &mut Tokenizer<'s, 'g>,
        vars: &mut HashMap<String, Var>
    ) -> bool {
        match self {
            IfGrammar::Match { match_: token, into } => {
                if Self::peek(token, tokenizer) {
                    let token = tokenizer.next().unwrap();
                    if let Some(into) = into {
                        vars.get_mut(*into).expect(
                            "internal compiler error: unknown variable {r} \
                            - grammar file is invalid"
                        ).assign(Node::from(token, src));
                    }
                    true
                }
                else {
                    false
                }
            }
            IfGrammar::Peek { peek } => {
                Self::peek(peek, tokenizer)
            }
        }
    }
}

impl<'s, 'g> Item<'g> {
    fn parse(
        &self,
        src: Arc<Src>,
        tokenizer: &mut Tokenizer<'s, 'g>,
    ) -> Option<Node> {
        match self {
            Item::Token(token) => {
                if IfGrammar::peek(token, tokenizer) {
                    let token = tokenizer.next().unwrap();
                    Some(Node::from(token, src.clone()))
                }
                else {
                    Grammar::error_next_token(token, tokenizer);
                    None
                }
            }
            Item::Rule(r) => Some(
                tokenizer.grammar().rules.get(*r)
                    .expect(
                        "internal compiler error: unknown rule {r} \
                        - grammar file is invalid"
                    )
                    .exec(src.clone(), tokenizer)
            )
        }
    }
}

impl<'s, 'g> Grammar<'g> {
    fn error_next_token<F: Display>(expected: F, tokenizer: &mut Tokenizer<'s, 'g>) {
        let (token, eof) = tokenizer.next()
            .map(|t| (t, false))
            .unwrap_or((tokenizer.last().unwrap(), true));
        tokenizer.logger().lock().unwrap().log(Message::new(
            Level::Error,
            if eof {
                format!("Expected {expected}, got end-of-file")
            }
            else {
                format!("Expected {expected}, got {token}")
            },
            token.span
        ));
    }

    fn exec(
        &self,
        src: Arc<Src>,
        tokenizer: &mut Tokenizer<'s, 'g>,
        vars: &mut HashMap<String, Var>
    ) -> Option<Node> {
        match self {
            Grammar::Match { match_, into, inner } => {
                let node = match_.parse(src.clone(), tokenizer);
                if let (Some(into), Some(node)) = (into, node) {
                    vars.get_mut(*into).expect(
                        "internal compiler error: unknown variable {r} \
                        - grammar file is invalid"
                    ).assign(node);
                }
                for g in inner {
                    g.exec(src.clone(), tokenizer, vars);
                }
                None
            }
            Grammar::If { if_, then, else_ } => {
                if if_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        g.exec(src.clone(), tokenizer, vars);
                    }
                }
                else {
                    for g in else_ {
                        g.exec(src.clone(), tokenizer, vars);
                    }
                }
                None
            }
            Grammar::While { while_, then } => {
                while while_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        g.exec(src.clone(), tokenizer, vars);
                    }
                }
                None
            }
            Grammar::Return { return_ } => {
                return_.parse(src, tokenizer)
            }
            Grammar::Expected { expected } => {
                Self::error_next_token(expected, tokenizer);
                None
            }
        }
    }
}

impl<'s, 'g> Rule<'g> {
    fn exec(&self, src: Arc<Src>, tokenizer: &mut Tokenizer<'s, 'g>) -> Node {
        let mut vars = self.layout.iter()
            .map(|(name, kind)| (name.to_owned(), match kind {
                MemberKind::List  => Var::List(vec![]),
                MemberKind::Maybe => Var::Maybe(None),
                MemberKind::Rule  => Var::None,
            }))
            .collect::<HashMap<_, _>>();
            
        let start = tokenizer.start_offset();
        for stmt in &self.grammar {
            stmt.exec(src.clone(), tokenizer, &mut vars);
        }

        Node::new(vars.into_iter()
            .map(|(name, value)| {
                let value = match value {
                    Var::Node(node) => Child::Node(node),
                    Var::List(list) => Child::List(list),
                    Var::Maybe(opt) => Child::Maybe(opt),
                    Var::None => panic!(
                        "internal compiler error: required child {name} \
                        was not assigned - grammar file is invalid"
                    ),
                };
                (name, value)
            })
            .collect(),
            ArcSpan(
                src.clone(),
                start..tokenizer.last()
                    .map(|t| t.span.1.end)
                    .unwrap_or(0usize)
            )
        )
    }
}

impl<'g> GrammarFile<'g> {
    pub(super) fn exec(&self, src: Arc<Src>, logger: LoggerRef) -> Node {
        let mut tokenizer = Tokenizer::new(src.as_ref(), self, logger);
        let ast = self.rules.get("main")
            .expect("internal compiler error: no 'main' rule provide - grammar file is invalid")
            .exec(src.clone(), &mut tokenizer);

        let leftover = tokenizer.count();
        if leftover > 0 {
            panic!(
                "internal compiler error: grammar rule 'main' did not \
                exhaustively match source, {leftover} tokens were unaccounted for"
            );
        }
        ast
    }
}
