
use std::{ptr::NonNull, collections::HashMap};
use crate::shared::logger::LoggerRef;
use super::{ast::{Node, Child, Children}, ty::Ty, tests::TypeItem, path::{FullIdentPath, IdentPath}, entity::Entity};

pub(crate) struct ItemSpace<T> {
    items: HashMap<FullIdentPath, T>,
}

impl<T> ItemSpace<T> {
    fn new<H: Into<HashMap<FullIdentPath, T>>>(values: H) -> Self {
        Self { items: values.into() }
    }
    pub fn try_push(&mut self, name: IdentPath, item: T) -> Result<&T, &T> {
        self.items.insert(name.clone(), item);
        self.items.get(&name).unwrap()
    }
}

impl<T> Default for ItemSpace<T> {
    fn default() -> Self {
        Self {
            items: Default::default()
        }
    }
}

pub(crate) struct Scope {
    logger: LoggerRef,
    parent: Option<NonNull<Scope>>,
    types: ItemSpace<Ty>,
    entities: ItemSpace<Entity>,
}

impl Scope {
    pub fn new(parent: NonNull<Scope>) -> Self {
        Self {
            logger: unsafe { parent.as_ref() }.logger.clone(),
            parent: Some(parent),
            types: Default::default(),
            entities: Default::default(),
        }
    }
    pub fn root(logger: LoggerRef) -> Self {
        Self {
            logger,
            parent: None,
            types: ItemSpace::new(
                [Ty::Never, Ty::Void, Ty::Bool, Ty::Int, Ty::Float, Ty::String]
                    .map(|t| (FullIdentPath::new([t.to_string().into()]), t))
            ),
            entities: Default::default(),
        }
    }
    pub fn types(&mut self) -> &mut ItemSpace<Ty> {
        &mut self.types
    }
    pub fn entities(&mut self) -> &mut ItemSpace<Entity> {
        &mut self.entities
    }
}

pub(crate) struct Checker {
    logger: LoggerRef,
    root_scope: Scope,
    current_scope: NonNull<Scope>,
    namespace_stack: FullIdentPath,
}

impl Checker {
    pub fn new(logger: LoggerRef) -> Self {
        let mut ret = Self {
            logger: logger.clone(),
            root_scope: Scope::root(logger),
            current_scope: NonNull::dangling(),
            namespace_stack: FullIdentPath::default(),
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
    pub(crate) fn eval(&self, children: &Children) -> Ty {
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
