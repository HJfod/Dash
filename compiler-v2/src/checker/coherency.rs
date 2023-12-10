
use std::ptr::NonNull;
use crate::{shared::logger::{LoggerRef, Message, Level}, parser::grammar};
use super::{ast::{Node, Child, Children}, ty::Ty};

// These are separate from grammar's for a few reasons:
// #1 Can't move out of grammar,

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

pub(crate) enum Test {
    Equal {
        equal: (TypeItem, TypeItem),
    },
    Scope {
        scope: Option<Scope>,
        tests: Vec<Test>,
    },
}

impl From<&grammar::Test<'_>> for Test {
    fn from(value: &grammar::Test<'_>) -> Self {
        match value {
            grammar::Test::Equal { equal } => Test::Equal {
                equal: ((&equal.0).into(), (&equal.1).into())
            },
            grammar::Test::Scope { tests } => Test::Scope {
                scope: None,
                tests: tests.iter().map(|t| t.into()).collect()
            }
        }
    }
}

impl Test {
    fn exec(&mut self, children: &Children, checker: &mut Checker, logger: LoggerRef) {
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
                    test.exec(children, checker, logger.clone());
                }
                checker.leave_scope();
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

pub(crate) struct Scope {
    parent: Option<NonNull<Scope>>,
}

impl Scope {
    pub fn new(parent: NonNull<Scope>) -> Self {
        Self {
            parent: Some(parent),
        }
    }
    
    pub fn root() -> Self {
        Self { parent: None }
    }
}

pub(crate) struct Checker {
    root_scope: Scope,
    current_scope: NonNull<Scope>,
}

impl Checker {
    pub fn new() -> Self {
        let mut ret = Self {
            root_scope: Scope::root(),
            current_scope: NonNull::dangling(),
        };
        ret.current_scope = NonNull::from(&ret.root_scope);
        ret
    }
    
    pub fn scope(&self) -> NonNull<Scope> {
        self.current_scope
    }

    pub fn enter_scope(&mut self, scope: NonNull<Scope>) {
        self.current_scope = scope;
    }

    pub fn leave_scope(&mut self) {
        if let Some(parent) = unsafe { self.current_scope.as_ref() }.parent {
            self.current_scope = parent;
        }
    }
}

impl TypeItem {
    fn eval(&self, children: &Children) -> Ty {
        match self {
            TypeItem::Type(ty) => ty.clone(),
            TypeItem::Member(member) => {
                match children.get(member).unwrap() {
                    Child::Node(n) => n.resolved_ty.clone().unwrap(),
                    Child::Maybe(maybe) => maybe.as_ref()
                        .and_then(|n| n.resolved_ty.clone())
                        .unwrap_or(Ty::Invalid),
                    Child::List(_) => panic!("can't eval list child"),
                }
            }
        }
    }
}

impl Node {
    fn for_all_children<F: FnMut(&mut Node)>(&mut self, mut fun: F) {
        (&mut self.children).into_iter().for_each(|child|
            match &mut child.1 {
                Child::Node(n) => fun(n),
                Child::Maybe(n) => if let Some(n) = n { fun(n) },
                Child::List(l) => l.iter_mut().for_each(&mut fun),
            }
        );
    }

    pub(crate) fn check_coherency(&mut self, checker: &mut Checker, logger: LoggerRef) {
        // If this AST node has already been resolved, that means that all of 
        // its children have also been resolved and no need to check them again
        if self.resolved_ty.is_some() {
            return;
        }

        // Check all children
        let mut some_unresolved = false;
        self.for_all_children(|n| {
            n.check_coherency(checker, logger.clone());
            if n.resolved_ty.is_some() {
                some_unresolved = true;
            }
        });
        if some_unresolved {
            return;
        }

        for test in &mut self.check.tests {
            test.exec(&self.children, checker, logger.clone());
        }
        self.resolved_ty = Some(self.check.result.eval(&self.children));
    }
}
