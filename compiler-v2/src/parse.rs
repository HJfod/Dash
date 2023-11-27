
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

#[derive(Debug)]
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
        let Some(tk) = tokenizer.peek() else { return matches!(token, TokenItem::Eof(_)); };
        match token {
            TokenItem::Braces => matches!(tk.kind, TokenKind::Braces(_)),
            TokenItem::Brackets => matches!(tk.kind, TokenKind::Brackets(_)),
            TokenItem::Parentheses => matches!(tk.kind, TokenKind::Parentheses(_)),
            TokenItem::Ident => matches!(tk.kind, TokenKind::Ident),
            TokenItem::Int => matches!(tk.kind, TokenKind::Int(_)),
            TokenItem::Float => matches!(tk.kind, TokenKind::Float(_)),
            TokenItem::String => matches!(tk.kind, TokenKind::String(_)),
            TokenItem::Token(s) => tk.raw == *s,
            TokenItem::Eof(_) => false,
        }
    }

    fn exec(
        &self,
        src: Arc<Src>,
        tokenizer: &mut Tokenizer<'s, 'g>,
        vars: &mut HashMap<String, Var>
    ) -> bool {
        match self {
            IfGrammar::NotEOF => {
                tokenizer.peek().is_some()
            }
            IfGrammar::Match { match_, into } => {
                if Self::peek(match_, tokenizer) {
                    if matches!(match_, TokenItem::Eof(_)) {
                        if into.is_some() {
                            panic!("can't use \"into\" with {{ \"match\": \"eof\" }}");
                        }
                        return true;
                    }
                    let token = tokenizer.next().unwrap();
                    if !matches!(token.kind, TokenKind::Error(_)) {
                        if let Some(into) = into {
                            vars.get_mut(*into).expect(
                                "internal compiler error: unknown variable {r} \
                                - grammar file is invalid"
                            ).assign(Node::from(token, src));
                        }
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
                    // can't store EOF
                    if matches!(token, TokenItem::Eof(_)) {
                        return None;
                    }
                    let token = tokenizer.next().unwrap();
                    (!matches!(token.kind, TokenKind::Error(_))).then(|| Node::from(token, src.clone()))
                }
                else {
                    Grammar::error_next_token(token, tokenizer);
                    None
                }
            }
            Item::Rule(r) => Some(
                tokenizer.grammar().rules.get(*r)
                    .unwrap_or_else(|| panic!(
                        "internal compiler error: unknown rule {r} \
                        - grammar file is invalid"
                    ))
                    .exec(src.clone(), tokenizer)
            )
        }
    }
}

impl<'s, 'g> Grammar<'g> {
    fn error_next_token<F: Display>(expected: F, tokenizer: &mut Tokenizer<'s, 'g>) {
        if let Some(token) = tokenizer.next() {
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Error,
                format!("Expected {expected}, got {token}"),
                token.span
            ));
        }
        else {
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Error,
                format!("Expected {expected}, got end-of-file"),
                tokenizer.eof_span()
            ));
        }
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
        let mut vars = self.members.iter()
            .map(|kind| match kind {
                MemberKind::List(a)  => (a.to_string(), Var::List(vec![])),
                MemberKind::Maybe(a) => (a.to_string(), Var::Maybe(None)),
                MemberKind::Rule(a)  => (a.to_string(), Var::None),
            })
            .collect::<HashMap<_, _>>();
            
        let start = tokenizer.start_offset();
        for stmt in &self.grammar {
            if let Some(ret) = stmt.exec(src.clone(), tokenizer, &mut vars) {
                return ret;
            }
        }

        Node::new(vars.into_iter()
            .filter_map(|(name, value)| {
                let value = match value {
                    Var::Node(node) => Child::Node(node),
                    Var::List(list) => Child::List(list),
                    Var::Maybe(opt) => Child::Maybe(opt),
                    Var::None       => return None,
                    // Var::None => panic!(
                    //     "internal compiler error: required child '{name}' \
                    //     was not assigned - grammar file is invalid"
                    // ),
                };
                Some((name, value))
            })
            .collect(),
            ArcSpan(src.clone(), start..tokenizer.last_non_ws)
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
