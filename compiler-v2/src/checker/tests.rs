
// These are separate from grammar's for a few reasons:
// - can't move out of grammar
// - need to store mutable state in tests (scope)

use std::ptr::NonNull;

use crate::{parser::grammar, shared::logger::{LoggerRef, Message, Level, Note}};

use super::{ty::Ty, coherency::{Scope, Checker}, ast::{Children, Child, Node}, path::IdentPath, entity::Entity};

pub enum TypeItem {
    Member(String),
    Type(Ty),
}

impl From<&grammar::TypeItem<'_>> for TypeItem {
    fn from(value: &grammar::TypeItem<'_>) -> Self {
        match value {
            grammar::TypeItem::Member(mem) => Self::Member(mem.to_string()),
            grammar::TypeItem::Type(ty) => Self::Type(Ty::new_builtin(ty)),
        }
    }
}

pub enum IdentItem {
    Member(String),
    Ident(String),
}

impl IdentItem {
    fn to_path(&self, children: &Children) -> Option<IdentPath> {
        match self {
            Self::Member(mem) => match children.get(mem).unwrap() {
                Child::Node(n) => Some(n.to_ident_path()),
                Child::Maybe(m) => m.as_ref().map(|n| n.to_ident_path()),
                Child::List(_) => panic!("can not use a list member as an ident path"),
            },
            Self::Ident(i) => Some(IdentPath::new([i.as_str().into()], true)),
        }
    }
}

impl From<&grammar::IdentItem<'_>> for IdentItem {
    fn from(value: &grammar::IdentItem<'_>) -> Self {
        match value {
            grammar::IdentItem::Member(mem) => Self::Member(mem.to_string()),
            grammar::IdentItem::Ident(ident) => Self::Ident(ident.to_string()),
        }
    }
}

pub(crate) enum Test {
    Equal {
        equal: (TypeItem, TypeItem),
    },
    Scope {
        scope: Option<Scope>,
        tests: Vec<Test>,
    },
    NewEntity {
        new_entity: IdentItem,
        ty: TypeItem,
    },
}

impl From<&grammar::Test<'_>> for Test {
    fn from(value: &grammar::Test<'_>) -> Self {
        match value {
            grammar::Test::Equal { equal } => Test::Equal {
                equal: ((&equal.0).into(), (&equal.1).into()),
            },
            grammar::Test::Scope { tests } => Test::Scope {
                scope: None,
                tests: tests.iter().map(|t| t.into()).collect(),
            },
            grammar::Test::NewEntity { new_entity, ty } => Test::NewEntity {
                new_entity: new_entity.into(),
                ty: ty.into(),
            } 
        }
    }
}

impl Test {
    pub(crate) fn exec(
        &mut self, children: &Children, node: NonNull<Node>, checker: &mut Checker, logger: LoggerRef
    ) {
        match self {
            Test::Equal { equal } => {
                let (a, b) = (equal.0.eval(children), equal.1.eval(children));
                if !a.convertible(&b) {
                    logger.lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Type {a} is not convertible to type {b}"),
                        a.decl().or(b.decl()).expect(
                            "neither type in \"test\".\"equal\" has a declaration"
                        ).span().as_ref()
                    ));
                }
            }
            Test::Scope { scope, tests } => {
                checker.enter_scope(NonNull::from(
                    scope.get_or_insert_with(|| Scope::new(checker.scope()))
                ));
                for test in tests {
                    test.exec(children, node, checker, logger.clone());
                }
                checker.leave_scope();
            }
            Test::NewEntity { new_entity, ty } => {
                println!("new entity test");
                let path = new_entity.to_path(children).unwrap();
                if let Err(e) = unsafe { checker.scope().as_mut() }.entities().try_push(
                    &path,
                    Entity::new(ty.eval(children), node),
                    checker.namespace_stack()
                ) {
                    logger.lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Name '{path}' has already been defined in this scope"),
                        unsafe { node.as_ref() }.span.as_ref()
                    ).note(Note::new_at(
                        "Previous definition here", e.decl().span.as_ref()
                    )));
                }
            }
        }
    }
}

pub(crate) struct Check {
    pub result: TypeItem,
    pub tests: Vec<Test>,
}

impl Default for Check {
    fn default() -> Self {
        Self {
            result: TypeItem::Type(Ty::Invalid),
            tests: vec![],
        }
    }
}

impl From<&grammar::Check<'_>> for Check {
    fn from(value: &grammar::Check<'_>) -> Self {
        Self {
            result: (&value.result).into(),
            tests: value.tests.iter().map(|t| t.into()).collect()
        }
    }
}
