
use std::{collections::HashMap, fmt::Display, sync::Arc};
use crate::grammar::ItemOrMember;
use crate::logger::LoggerRef;
use crate::src::Span;
use crate::tokenizer::TokenIterator;

use super::ast::{Node, Child, Value, ArcSpan};
use super::grammar::{Rule, GrammarFile, MemberKind, Grammar, Item, IfGrammar, TokenItem};
use super::tokenizer::{Tokenizer, TokenKind, Token};
use super::src::Src;
use super::logger::{Message, Level};

impl Node {
    fn from(name: &str, token: Token, src: Arc<Src>) -> Node {
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
            .map(|v| Node::new_with_value(name, v, ArcSpan(src.clone(), token.span.1.clone())))
            .unwrap_or_else(|| Node::new_empty(name, ArcSpan(src, token.span.1)))
    }
}

#[derive(Default, Debug)]
enum Var {
    #[default]
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
            TokenItem::Keyword(s) => match tk.kind {
                TokenKind::Keyword => *s == tk.raw,
                TokenKind::Ident => *s == tk.raw && tokenizer.grammar().keywords.contextual.contains(s),
                _ => false,
            },
            TokenItem::Punct(s) => matches!(tk.kind, TokenKind::Punct) && *s == tk.raw,
            TokenItem::Op(s) => matches!(tk.kind, TokenKind::Op) && !s.is_some_and(|s| s != tk.raw),
            TokenItem::Eof(_) => false,
            TokenItem::OneOf(tokens) => tokens.iter().any(|t| Self::peek(t, tokenizer)),
        }
    }

    fn exec<I>(
        &self,
        rule_name: &str,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut HashMap<String, Var>
    ) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        match self {
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
                            ).assign(Node::from(rule_name, token, src));
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
            IfGrammar::Set { set } => {
                !matches!(vars.get(*set).expect(
                    "internal compiler error: unknown variable {r} for \"set\" \
                    - grammar file is invalid"
                ), Var::None)
            }
            IfGrammar::Not { not } => {
                !not.exec(rule_name, src, tokenizer, vars)
            }
            IfGrammar::Either { either } => {
                either.iter().any(|e| e.exec(rule_name, src.clone(), tokenizer, vars))
            }
        }
    }
}

struct InvalidToken;

impl<'s, 'g> Item<'g> {
    fn next<I>(
        rule_name: &str, 
        token: &TokenItem<'s>,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
    ) -> Result<(Node, Option<Vec<Token<'s>>>), InvalidToken>
        where I: Iterator<Item = Token<'s>>
    {
        // can't store EOF
        if matches!(token, TokenItem::Eof(_)) {
            return Err(InvalidToken);
        }
        let mut token = tokenizer.next().unwrap();
        if matches!(token.kind, TokenKind::Error(_)) {
            return Err(InvalidToken);
        }
        let inner = token.kind.take_inner();
        Ok((Node::from(rule_name, token, src.clone()), inner))
    } 

    fn parse<I>(
        &self,
        rule_name: &str, 
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        options: ParseOptions,
        with: Option<HashMap<String, Var>>,
    ) -> Result<(Node, Option<Vec<Token<'s>>>), InvalidToken>
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            Item::Token(token) => {
                if with.is_some() {
                    panic!(
                        "internal compiler error: attempted to use \"with\" \
                        on a token - grammar file is invalid"
                    );
                }
                if IfGrammar::peek(token, tokenizer) {
                    Self::next(rule_name, token, src, tokenizer)
                }
                else {
                    Grammar::expected_next_token(token, tokenizer);
                    Err(InvalidToken)
                }
            }
            Item::Rule(r) => Ok((
                tokenizer.grammar().rules.get(*r)
                    .unwrap_or_else(|| panic!(
                        "internal compiler error: unknown rule {r} \
                        - grammar file is invalid"
                    ))
                    .exec(src.clone(), tokenizer, options, with),
                None
            ))
        }
    }
}

impl<'s, 'g> Grammar<'g> {
    fn error_next_token<I, S>(error: S, tokenizer: &mut TokenIterator<'s, 'g, I>)
        where I: Iterator<Item = Token<'s>>, S: Into<String>
    {
        if let Some(token) = tokenizer.next() {
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Error, error, token.span
            ));
        }
        else {
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Error, error, tokenizer.eof_span()
            ));
        }
    }

    fn expected_next_token<I, F>(expected: F, tokenizer: &mut TokenIterator<'s, 'g, I>)
        where I: Iterator<Item = Token<'s>>, F: Display
    {
        Self::error_next_token(
            if let Some(token) = tokenizer.peek() {
                format!("Expected {expected}, got {token}")
            }
            else {
                format!("Expected {expected}, got end-of-file")
            },
            tokenizer
        )
    }

    #[must_use]
    fn exec<I>(
        &self,
        rule_name: &str, 
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut HashMap<String, Var>,
        options: ParseOptions,
    ) -> Option<Node>
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            Grammar::Match { match_, into, inner, with } => {
                if let Ok((node, inner_tokens)) = match_.parse(
                    rule_name, 
                    src.clone(),
                    tokenizer,
                    options.clone(),
                    with.as_ref().map(|w| w.iter().map(|s| (
                        s.0.to_string(),
                        std::mem::take(vars.get_mut(*s.1).expect(
                            "internal compiler error: unknown variable {r} \
                            - grammar file is invalid"
                        ))
                    )).collect())
                ) {
                    if let Some(into) = into {
                        vars.get_mut(*into).unwrap_or_else(|| panic!(
                            "internal compiler error: unknown variable {into} \
                            - grammar file is invalid"
                        )).assign(node);
                    }
                    if !inner.is_empty() {
                        if let Some(inner_tokens) = inner_tokens {
                            let mut iter = tokenizer.fork(inner_tokens.into_iter());
                            for g in inner {
                                if let Some(ret) = g.exec(
                                    rule_name, src.clone(),
                                    &mut iter, vars, options.clone()
                                ) {
                                    return Some(ret);
                                }
                            }
                            // let leftover = iter.count();
                            // if leftover > 0 {
                            //     panic!(
                            //         "internal compiler error: inner contents were not \
                            //         exhaustively matched, {leftover} tokens were unaccounted for"
                            //     );
                            // }
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
                if if_.exec(rule_name, src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(rule_name, src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                else {
                    for g in else_ {
                        if let Some(ret) = g.exec(rule_name, src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::While { while_, then } => {
                while while_.exec(rule_name, src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(rule_name, src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::Return { return_ } => {
                match return_ {
                    ItemOrMember::Item(i) => i.parse(rule_name, src, tokenizer, options, None).ok().map(|n| n.0),
                    ItemOrMember::Member(m) => match std::mem::take(vars.get_mut(*m).expect(
                        "internal compiler error: unknown variable {m} \
                        in \"return\" - grammar file is invalid"
                    )) {
                        Var::Node(n) => Some(n),
                        _ => panic!(
                            "internal compiler error: variable {m} was not a node \
                            in \"return\" - grammar file is invalid"
                        )
                    }
                }
            }
            Grammar::DebugLog { debug_log } => {
                println!("{debug_log}; peek: {:?}", tokenizer.peek());
                None
            }
            Grammar::Expected { expected } => {
                Self::expected_next_token(expected, tokenizer);
                None
            }
            Grammar::Error { error } => {
                Self::error_next_token(*error, tokenizer);
                None
            }
        }
    }
}

impl<'s, 'g> Rule<'g> {
    fn exec<I>(
        &self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        options: ParseOptions,
        with: Option<HashMap<String, Var>>,
    ) -> Node
        where I: Iterator<Item = Token<'s>>
    {
        if options.debug_log_matches {
            let o = tokenizer.start_offset();
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Info,
                format!("Matching rule {}", self.name),
                Span(src.as_ref(), o..o)
            ))
        }

        let mut vars = self.members.iter()
            .map(|kind| match kind {
                MemberKind::List(a)  => (a.to_string(), Var::List(vec![])),
                MemberKind::Maybe(a) => (a.to_string(), Var::Maybe(None)),
                MemberKind::Rule(a)  => (a.to_string(), Var::None),
            })
            .collect::<HashMap<_, _>>();

        if let Some(with) = with {
            for (name, var) in with {
                *vars.get_mut(&name).expect(
                    "internal compiler error: unknown variable {r} \
                    provided through \"with\" - grammar file is invalid"
                ) = var;
            }
        }
            
        let start = tokenizer.start_offset();
        for stmt in &self.grammar {
            if let Some(ret) = stmt.exec(self.name, src.clone(), tokenizer, &mut vars, options.clone()) {
                if options.debug_log_matches {
                    tokenizer.logger().lock().unwrap().log(Message::new(
                        Level::Info,
                        format!("Returned rule {}", ret.name()),
                        ret.span().as_ref()
                    ));
                }
                return ret;
            }
        }

        if options.debug_log_matches {
            tokenizer.logger().lock().unwrap().log(Message::new(
                Level::Info,
                format!("Matched rule {}", self.name),
                Span(src.as_ref(), start..tokenizer.end_offset())
            ));
        }
        Node::new(
            self.name,
            vars.into_iter()
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
    pub(super) fn exec(&self, src: Arc<Src>, logger: LoggerRef, options: ParseOptions) -> Node {
        let mut tokenizer = Tokenizer::new(src.as_ref(), self, logger).into();
        let ast = self.rules.get("main")
            .expect("internal compiler error: no 'main' rule provide - grammar file is invalid")
            .exec(src.clone(), &mut tokenizer, options, None);

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

#[derive(Default, Clone)]
pub struct ParseOptions {
    pub debug_log_matches: bool,
}
