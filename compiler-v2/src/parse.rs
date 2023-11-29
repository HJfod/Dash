
use std::{collections::HashMap, fmt::Display, sync::Arc};
use crate::logger::LoggerRef;
use crate::tokenizer::TokenIterator;

use super::ast::{Node, Child, Value, ArcSpan};
use super::grammar::{Rule, GrammarFile, MemberKind, Grammar, Item, IfGrammar, TokenItem};
use super::tokenizer::{Tokenizer, TokenKind, Token};
use super::src::Src;
use super::logger::{Message, Level};

impl Node {
    fn from(token: Token, src: Arc<Src>) -> Node {
        match token.kind {
            TokenKind::Keyword |
            TokenKind::Op |
            TokenKind::Punct |
            TokenKind::Ident |
            TokenKind::Parentheses(_) |
            TokenKind::Braces(_) |
            TokenKind::Brackets(_) => None,
            TokenKind::Int(n) => Some(Value::Int(n)),
            TokenKind::Float(n) => Some(Value::Float(n)),
            TokenKind::String(s) => Some(Value::String(s)),
            TokenKind::Error(_) => unreachable!("errors should never be matched")
        }
            .map(|v| Node::new_with_value(v, ArcSpan(src.clone(), token.span.1.clone())))
            .unwrap_or_else(|| Node::new_empty(ArcSpan(src, token.span.1)))
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
    fn peek<I>(token: &TokenItem<'_>, tokenizer: &TokenIterator<'s, 'g, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        let Some(tk) = tokenizer.peek() else { return matches!(token, TokenItem::Eof(_)); };
        match token {
            TokenItem::Braces => matches!(tk.kind, TokenKind::Braces(_)),
            TokenItem::Brackets => matches!(tk.kind, TokenKind::Brackets(_)),
            TokenItem::Parentheses => matches!(tk.kind, TokenKind::Parentheses(_)),
            TokenItem::Ident => matches!(tk.kind, TokenKind::Ident),
            TokenItem::Int => matches!(tk.kind, TokenKind::Int(_)),
            TokenItem::Float => matches!(tk.kind, TokenKind::Float(_)),
            TokenItem::String => matches!(tk.kind, TokenKind::String(_)),
            TokenItem::Keyword(s) | TokenItem::Op(s) | TokenItem::Punct(s) => tk.raw == *s,
            TokenItem::Eof(_) => false,
        }
    }

    fn exec<I>(
        &self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut HashMap<String, Var>
    ) -> bool
        where I: Iterator<Item = Token<'s>>
    {
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

struct InvalidToken;

impl<'s, 'g> Item<'g> {
    fn parse<I>(
        &self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
    ) -> Result<(Node, Option<Vec<Token<'s>>>), InvalidToken>
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            Item::Token(token) => {
                if IfGrammar::peek(token, tokenizer) {
                    // can't store EOF
                    if matches!(token, TokenItem::Eof(_)) {
                        return Err(InvalidToken);
                    }
                    let mut token = tokenizer.next().unwrap();
                    if matches!(token.kind, TokenKind::Error(_)) {
                        return Err(InvalidToken);
                    }
                    let inner = token.kind.take_inner();
                    Ok((Node::from(token, src.clone()), inner))
                }
                else {
                    Grammar::error_next_token(token, tokenizer);
                    Err(InvalidToken)
                }
            }
            Item::Rule(r) => Ok((
                tokenizer.grammar().rules.get(*r)
                    .unwrap_or_else(|| panic!(
                        "internal compiler error: unknown rule {r} \
                        - grammar file is invalid"
                    ))
                    .exec(src.clone(), tokenizer),
                None
            ))
        }
    }
}

impl<'s, 'g> Grammar<'g> {
    fn error_next_token<I, F>(expected: F, tokenizer: &mut TokenIterator<'s, 'g, I>)
        where I: Iterator<Item = Token<'s>>, F: Display
    {
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

    #[must_use]
    fn exec<I>(
        &self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut HashMap<String, Var>
    ) -> Option<Node>
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            Grammar::Match { match_, into, inner } => {
                if let Ok((node, inner_tokens)) = match_.parse(src.clone(), tokenizer) {
                    if let Some(into) = into {
                        vars.get_mut(*into).expect(
                            "internal compiler error: unknown variable {r} \
                            - grammar file is invalid"
                        ).assign(node);
                    }
                    if !inner.is_empty() {
                        if let Some(inner_tokens) = inner_tokens {
                            let mut iter = tokenizer.fork(inner_tokens.into_iter());
                            for g in inner {
                                if let Some(ret) = g.exec(src.clone(), &mut iter, vars) {
                                    return Some(ret);
                                }
                            }
                        }
                        else {
                            panic!(
                                "internal compiler error: attempted to use \"inner\" \
                                on a node without inner contents - grammar file is invalid"
                            );
                        }
                    }
                }
                None
            }
            Grammar::If { if_, then, else_ } => {
                if if_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars) {
                            return Some(ret);
                        }
                    }
                }
                else {
                    for g in else_ {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::While { while_, then } => {
                while while_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::Return { return_ } => {
                return_.parse(src, tokenizer).ok().map(|n| n.0)
            }
            Grammar::DebugLog { debug_log } => {
                println!("{debug_log}; peek: {:?}", tokenizer.peek());
                None
            }
            Grammar::Expected { expected } => {
                Self::error_next_token(expected, tokenizer);
                None
            }
        }
    }
}

impl<'s, 'g> Rule<'g> {
    fn exec<I>(&self, src: Arc<Src>, tokenizer: &mut TokenIterator<'s, 'g, I>) -> Node
        where I: Iterator<Item = Token<'s>>
    {
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
            ArcSpan(src.clone(), start..tokenizer.end_offset())
        )
    }
}

impl<'g> GrammarFile<'g> {
    pub(super) fn exec(&self, src: Arc<Src>, logger: LoggerRef) -> Node {
        let mut tokenizer = Tokenizer::new(src.as_ref(), self, logger).into();
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
