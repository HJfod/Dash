
// These are separate from grammar's for a few reasons:
// - can't move out of grammar
// - need to store mutable state in tests (scope)

use std::ptr::NonNull;

use crate::{parser::grammar, shared::logger::{LoggerRef, Message, Level, Note}, ice};
use super::{ty::Ty, coherency::{Scope, Checker}, ast::{Children, Child, ArcSpan}, path::IdentPath, entity::Entity, Ice};

pub enum TypeItem {
    Member(String),
    Type(Ty),
    Find(IdentItem),
}

impl From<&grammar::TypeItem<'_>> for TypeItem {
    fn from(value: &grammar::TypeItem<'_>) -> Self {
        match value {
            grammar::TypeItem::Member(mem) => Self::Member(mem.to_string()),
            grammar::TypeItem::Type(ty) => Self::Type(Ty::new_builtin(ty)),
            grammar::TypeItem::Find(find) => Self::Find(find.into()),
        }
    }
}

pub enum IdentItem {
    Member(String),
    Ident(String),
}

impl IdentItem {
    pub(crate) fn to_path(&self, children: &Children) -> Option<IdentPath> {
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
        at: Option<String>,
    },
    Scope {
        scope: Option<Scope>,
        tests: Vec<Test>,
    },
    NewEntity {
        new_entity: IdentItem,
        ty: TypeItem,
    },
    Result {
        result: TypeItem,
    },
}

impl From<&grammar::Test<'_>> for Test {
    fn from(value: &grammar::Test<'_>) -> Self {
        match value {
            grammar::Test::Equal { equal, at } => Test::Equal {
                equal: ((&equal.0).into(), (&equal.1).into()),
                at: at.map(|s| s.to_string())
            },
            grammar::Test::Scope { tests } => Test::Scope {
                scope: None,
                tests: tests.iter().map(|t| t.into()).collect(),
            },
            grammar::Test::NewEntity { new_entity, ty } => Test::NewEntity {
                new_entity: new_entity.into(),
                ty: ty.into(),
            },
            grammar::Test::Result { result } => Test::Result {
                result: result.into()
            },
        }
    }
}

impl Test {
    #[must_use]
    pub(crate) fn exec(
        &mut self,
        children: &Children,
        node_span: ArcSpan,
        checker: &mut Checker,
        logger: LoggerRef
    ) -> Option<Ty> {
        match self {
            Test::Equal { equal, at } => {
                let (a, b) = (
                    equal.0.find_resulting_ty(node_span.clone(), children, checker),
                    equal.1.find_resulting_ty(node_span.clone(), children, checker)
                );
                if !a.convertible(&b) {
                    logger.lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Type {a} is not convertible to type {b}"),
                        at.as_ref()
                            .map(|at| children.get(at).ice_due_to("unknown child", at).span())
                            .unwrap_or(node_span.as_ref())
                    ));
                }
                None
            }
            Test::Scope { scope, tests } => {
                let scope = scope.get_or_insert_with(|| Scope::new(checker.scope()));
                checker.enter_scope(NonNull::from(scope));
                for test in tests {
                    if test.exec(children, node_span.clone(), checker, logger.clone()).is_some() {
                        ice!("\"result\" for check tests used inside a scope");
                    }
                }
                checker.leave_scope();
                None
            }
            Test::NewEntity { new_entity, ty } => {
                let path = new_entity.to_path(children).unwrap();
                if let Err(e) = checker.scope().entities().try_push(
                    &path,
                    Entity::new(ty.find_resulting_ty(node_span.clone(), children, checker), node_span.clone()),
                    checker.namespace_stack()
                ) {
                    logger.lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Name '{path}' has already been defined in this scope"),
                        node_span.as_ref()
                    ).note(Note::new_at(
                        "Previous definition here", e.span()
                    )));
                }
                None
            }
            Test::Result { result } => {
                Some(result.find_resulting_ty(node_span.clone(), children, checker))
            }
        }
    }
}
