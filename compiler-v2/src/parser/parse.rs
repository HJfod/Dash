
use std::{collections::HashMap, fmt::Display, sync::Arc};
use crate::checker::Ice;
use crate::ice;
use crate::parser::grammar::ItemOrMember;
use crate::shared::logger::LoggerRef;
use crate::shared::src::Span;
use crate::parser::tokenizer::TokenIterator;

use crate::checker::ast::{Node, Child, Value, ArcSpan, Children};
use crate::parser::grammar::{Rule, GrammarFile, MemberKind, Grammar, Item, IfGrammar, TokenItem};
use crate::parser::tokenizer::{Tokenizer, TokenKind, Token};
use crate::shared::src::Src;
use crate::shared::logger::{Message, Level};

use super::ParseOptions;

impl Node {
    fn from(token: Token, src: Arc<Src>) -> Node {
        let (n, v) = match token.kind {
            TokenKind::Keyword => ("kw", None),
            TokenKind::Op => ("op", None),
            TokenKind::Punct => ("punct", None),
            TokenKind::Ident => ("ident", None),
            TokenKind::Parentheses(_) => ("paren", None),
            TokenKind::Braces(_) => ("brace", None),
            TokenKind::Brackets(_) => ("bracket", None),
            TokenKind::Int(n) => ("int", Some(Value::Int(n))),
            TokenKind::Float(n) => ("float", Some(Value::Float(n))),
            TokenKind::String(s) => ("string", Some(Value::String(s))),
            TokenKind::Error(_) => unreachable!("errors should never be matched")
        };
        let n = format!("tk-{n}");
        v
            .map(|v| Node::new_with_value(&n, v, ArcSpan(src.clone(), token.span.1.clone())))
            .unwrap_or_else(|| Node::new_empty(n, ArcSpan(src, token.span.1)))
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
            Var::Node(_) => ice!("attempted to assign a child more than once"),
            Var::List(list) => list.push(node),
            Var::Maybe(opt) => *opt = Some(node),
        }
    }
}

struct Vars<'g> {
    vars: HashMap<&'g str, Var>,
}

impl<'g> Vars<'g> {
    fn get(&self, var: &'g str) -> &Var {
        self.vars.get(var).ice_due_to("unknown variable", var)
    }
    fn get_mut(&mut self, var: &'g str) -> &mut Var {
        self.vars.get_mut(var).ice_due_to("unknown variable", var)
    }
}

impl<'g> IntoIterator for Vars<'g> {
    type Item = <HashMap<&'g str, Var> as IntoIterator>::Item;
    type IntoIter = <HashMap<&'g str, Var> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.vars.into_iter()
    }
}

impl<'g> FromIterator<(&'g str, Var)> for Vars<'g> {
    fn from_iter<T: IntoIterator<Item = (&'g str, Var)>>(iter: T) -> Self {
        Self { vars: iter.into_iter().collect() }
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
            TokenItem::NamedTokenList(list) => tokenizer.grammar()
                .named_token_lists.get(list)
                    .ice_due_to("unknown named token list", list)
                    .iter().any(|t| Self::peek(t, tokenizer))
        }
    }

    fn exec<I>(
        &self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut Vars<'g>
    ) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            IfGrammar::Match { match_, into } => {
                if Self::peek(match_, tokenizer) {
                    if matches!(match_, TokenItem::Eof(_)) {
                        if into.is_some() {
                            ice!("can't use \"into\" with {{ \"match\": \"eof\" }}");
                        }
                        return true;
                    }
                    let token = tokenizer.next().unwrap();
                    if !matches!(token.kind, TokenKind::Error(_)) {
                        if let Some(into) = into {
                            vars.get_mut(into).assign(Node::from(token, src));
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
                !matches!(vars.get(set), Var::None)
            }
            IfGrammar::Not { not } => {
                !not.exec(src, tokenizer, vars)
            }
            IfGrammar::Either { either } => {
                either.iter().any(|e| e.exec(src.clone(), tokenizer, vars))
            }
        }
    }
}

struct InvalidToken;

impl<'n: 'g, 's, 'g: 's> Item<'g> {
    fn next<I>(
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
        Ok((Node::from(token, src.clone()), inner))
    } 

    fn parse<I>(
        &'g self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        options: ParseOptions,
        with: Option<Vars<'g>>,
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
                    Self::next(token, src, tokenizer)
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

impl<'n: 'g, 's, 'g: 's> Grammar<'g> {
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
        &'g self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        vars: &mut Vars<'g>,
        options: ParseOptions,
    ) -> Option<Node>
        where I: Iterator<Item = Token<'s>>
    {
        match self {
            Grammar::Match { match_, into, inner, with } => {
                if let Ok((node, inner_tokens)) = match_.parse(
                    src.clone(),
                    tokenizer,
                    options.clone(),
                    with.as_ref().map(|w| w.iter().map(|s| (
                        *s.0, std::mem::take(vars.get_mut(s.1))
                    )).collect())
                ) {
                    if let Some(into) = into {
                        vars.get_mut(into).assign(node);
                    }
                    if !inner.is_empty() {
                        if let Some(inner_tokens) = inner_tokens {
                            let mut iter = tokenizer.fork(inner_tokens.into_iter());
                            for g in inner {
                                if let Some(ret) = g.exec(
                                    src.clone(), &mut iter, vars, options.clone()
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
                if if_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                else {
                    for g in else_ {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::While { while_, then } => {
                while while_.exec(src.clone(), tokenizer, vars) {
                    for g in then {
                        if let Some(ret) = g.exec(src.clone(), tokenizer, vars, options.clone()) {
                            return Some(ret);
                        }
                    }
                }
                None
            }
            Grammar::Return { return_, with } => {
                match return_ {
                    ItemOrMember::Item(i) => i.parse(
                        src, tokenizer, options, 
                        with.as_ref().map(|w| w.iter().map(|s| (
                            *s.0,
                            std::mem::take(vars.get_mut(s.1))
                        )).collect())
                    ).ok().map(|n| n.0),
                    ItemOrMember::Member(m) => match std::mem::take(vars.get_mut(m)) {
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

impl<'n: 'g, 's, 'g: 's> Rule<'g> {
    fn exec<I>(
        &'g self,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s, 'g, I>,
        options: ParseOptions,
        with: Option<Vars<'g>>,
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
                MemberKind::List(a)  => (*a, Var::List(vec![])),
                MemberKind::Maybe(a) => (*a, Var::Maybe(None)),
                MemberKind::Rule(a)  => (*a, Var::None),
            })
            .collect::<Vars<'g>>();

        if let Some(with) = with {
            for (name, var) in with {
                *vars.get_mut(name) = var;
            }
        }
            
        let start = tokenizer.start_offset();
        for stmt in &self.grammar {
            let o: Option<Node> = stmt.exec(src.clone(), tokenizer, &mut vars, options.clone());
            if let Some(ret) = o {
                if options.debug_log_matches {
                    tokenizer.logger().lock().unwrap().log(Message::new(
                        Level::Info,
                        format!("Returned rule {}", ret.name()),
                        ret.span()
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
            Children::from(
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
                        Some((name.to_string(), value))
                    })
                    .collect::<Vec<_>>()
            ),
            ArcSpan(src.clone(), start..tokenizer.end_offset()),
            self.check.iter().map(|c| c.into()).collect()
        )
    }
}

impl<'g> GrammarFile<'g> {
    pub(crate) fn exec(&'g self, src: Arc<Src>, logger: LoggerRef, options: ParseOptions) -> Node {
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
