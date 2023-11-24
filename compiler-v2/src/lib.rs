
pub mod grammar;
pub mod parse;
pub mod char_iter;
pub mod src;
pub mod ast;

use std::collections::HashMap;
use ast::{Node, Child};
use grammar::{Rule, GrammarFile, MemberKind, Grammar, Item};
use parse::Tokenizer;
use src::{Logger, Message, Level, Span};

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

impl<'s, 'g> Grammar<'g> {
    fn exec(
        &self,
        tokenizer: &mut Tokenizer<'s, 'g>,
        vars: &mut HashMap<String, Var>
    ) -> bool {
        match self {
            Grammar::Match { match_, into, inner } => {
                match match_ {
                    Item::Rule(r) => {
                        // todo: rollback if no match
                        let node = tokenizer.grammar.rules.get(*r)
                            .expect(
                                "internal compiler error: unknown rule {r} \
                                - grammar file is invalid"
                            )
                            .exec(tokenizer);
                            
                        if let Some(into) = into {
                            vars.get_mut(*into).expect(
                                "internal compiler error: unknown variable {r} \
                                - grammar file is invalid"
                            ).assign(node);
                        }
                    }
                }
                true
            }
            Grammar::If { if_, then, else_ } => {
                if if_.exec(tokenizer, vars) {
                    for g in then {
                        g.exec(tokenizer, vars);
                    }
                }
                else {
                    for g in else_ {
                        g.exec(tokenizer, vars);
                    }
                }
                false
            }
            Grammar::While { while_, then } => {
                while while_.exec(tokenizer, vars) {
                    for g in then {
                        g.exec(tokenizer, vars);
                    }
                }
                false
            }
            Grammar::Return { return_ } => {
                todo!()
            }
            Grammar::Expected { expected } => {
                let span = tokenizer.next().map(|t| t.span).unwrap_or(tokenizer.last_non_ws_span());
                tokenizer.logger.lock().unwrap()(Message::new(Level::Error, *expected, span));
                false
            }
        }
    }
}

impl<'s, 'g> Rule<'g> {
    fn exec(&self, tokenizer: &mut Tokenizer<'s, 'g>) -> Node {
        let mut vars = self.layout.iter()
            .map(|(name, kind)| (name.to_owned(), match kind {
                MemberKind::List  => Var::List(vec![]),
                MemberKind::Maybe => Var::Maybe(None),
                MemberKind::Rule  => Var::None,
            }))
            .collect::<HashMap<_, _>>();
            
        for stmt in &self.grammar {
            stmt.exec(tokenizer, &mut vars);
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
            .collect()
        )
    }
}

impl<'s, 'g> GrammarFile<'g> {
    pub fn exec(&self, tokenizer: &mut Tokenizer<'s, 'g>) -> Node {
        let ast = self.rules.get("main")
            .expect("internal compiler error: no 'main' rule provide - grammar file is invalid")
            .exec(tokenizer);

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
