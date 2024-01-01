
use std::{pin::Pin, ptr::NonNull, fmt::Display, collections::HashMap};
use crate::shared::{logger::{LoggerRef, Message, Level, Note}, ptr_iter::PtrChainIter, src::ArcSpan};
use super::{ty::Ty, path::{FullIdentPath, IdentPath, Ident}, entity::Entity};

pub(crate) struct ItemSpace<T> {
    items: HashMap<FullIdentPath, T>,
}

impl<T> ItemSpace<T> {
    fn new<H: Into<HashMap<FullIdentPath, T>>>(values: H) -> Self {
        Self { items: values.into() }
    }
    pub fn try_push(&mut self, name: &IdentPath, item: T, namespace_stack: &FullIdentPath) -> Result<&T, &T> {
        // The full name for this item is the current topmost namespace name 
        // joined with the name of the item
        let full_name = namespace_stack.join(name);
        // Check if this name already exists in this scope
        // Can't just do `if let Some` because the borrow checker then complains 
        // that you can't mutate self.items in the `else` branch afterwards
        if self.items.contains_key(&full_name) {
            Err(self.items.get(&full_name).unwrap())
        }
        else {
            self.items.insert(full_name.clone(), item);
            Ok(self.items.get(&full_name).unwrap())
        }
    }
    /// Try to find an item in this scope with a fully resolved name
    pub fn get(&self, full_name: &FullIdentPath) -> Option<&T> {
        self.items.get(full_name)
    }
    /// Try to find an item in this scope with an unresolved name
    pub fn find(&self, name: &IdentPath, namespace_stack: &FullIdentPath) -> Option<&T> {
        // This is an optimization; the else branch would also do this since 
        // FullIdentPath::join would just return `name` every time
        if name.is_absolute() {
            self.get(&name.to_full())
        }
        else {
            // Try joining the path to the namespace stack. If not found, check 
            // that namespace's parent namespace, all the way down to root
            let mut temp = namespace_stack.clone();
            while !temp.is_empty() {
                if let Some(found) = self.get(&temp.join(name)) {
                    return Some(found);
                }
                temp.pop();
            }
            // Check root namespace
            self.get(&name.to_full())
        }
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
    parent: Option<NonNull<Scope>>,
    types: ItemSpace<Ty>,
    entities: ItemSpace<Entity>,
}

impl Scope {
    fn new(checker: &mut Checker) -> Self {
        Self {
            parent: Some(NonNull::from(checker.scope())),
            types: Default::default(),
            entities: Default::default(),
        }
    }
    fn root() -> Self {
        Self {
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
    fn drop_ephemeral(&mut self) {
        self.entities.items.retain(|_, v| !v.ephemeral())
    }
}

pub(crate) struct LeaveScope {
    checker: *mut Checker,
}

impl Drop for LeaveScope {
    fn drop(&mut self) {
        unsafe { self.checker.as_mut() }.unwrap().leave_scope();
    }
}

pub struct Checker {
    logger: LoggerRef,
    current_scope: NonNull<Scope>,
    scopes: Vec<Scope>,
    namespace_stack: FullIdentPath,
    unresolved_nodes: Vec<(String, ArcSpan)>,
}

impl Checker {
    pub fn new(logger: LoggerRef) -> Pin<Box<Self>> {
        let mut ret = Box::pin(Self {
            logger: logger.clone(),
            current_scope: NonNull::dangling(),
            scopes: Vec::from([Scope::root()]),
            namespace_stack: FullIdentPath::default(),
            unresolved_nodes: Vec::new(),
        });
        ret.current_scope = NonNull::from(ret.scopes.first_mut().unwrap());
        ret
    }

    pub(crate) fn scopes(&self) -> impl Iterator<Item = &mut Scope> {
        PtrChainIter::new(self.current_scope, |s| s.parent)
    }
    pub(crate) fn scope<'a>(&mut self) -> &'a mut Scope {
        unsafe { self.current_scope.as_mut() }
    }
    pub(crate) fn enter_scope(&mut self, scope: &mut Option<NonNull<Scope>>) -> LeaveScope {
        match scope {
            Some(scope) => self.current_scope = *scope,
            None => {
                let s = Scope::new(self);
                self.scopes.push(s);
                *scope = Some(NonNull::from(self.scopes.last_mut().unwrap()));
                self.current_scope = scope.unwrap();
            }
        }
        LeaveScope { checker: self }
    }
    fn leave_scope(&mut self) {
        if let Some(parent) = self.scope().parent {
            self.scope().drop_ephemeral();
            self.current_scope = parent;
        }
    }

    pub(crate) fn namespace_stack(&self) -> &FullIdentPath {
        &self.namespace_stack
    }
    pub(crate) fn enter_namespace(&mut self, name: Ident) {
        self.namespace_stack.push(name);
    }
    pub(crate) fn leave_namespace(&mut self) {
        self.namespace_stack.pop();
    }

    pub(crate) fn push_unresolved<S: Display>(&mut self, cause: S, span: Option<ArcSpan>) {
        self.unresolved_nodes.push((cause.to_string(), span.unwrap_or(ArcSpan::builtin())));
    }

    pub(crate) fn expect_ty_decided(&self, a: Ty, span: Option<ArcSpan>) -> bool {
        if let Ty::Undecided(name, a_span) = a {
            self.logger.lock().unwrap().log(Message::new(
                Level::Error,
                format!("The type of {name} needs to be known at this point"),
                span.into()
            ).note(Note::new_at(
                format!("Declaration of {name} here"),
                Some(a_span).into()
            )));
            return false;
        }
        true
    }
    pub(crate) fn expect_ty_eq(&self, a: Ty, b: Ty, span: Option<ArcSpan>) -> Ty {
        if self.expect_ty_decided(a.clone(), span.clone()) &&
            self.expect_ty_decided(b.clone(), span.clone()) {
            if !b.convertible(&a) {
                self.logger.lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Cannot convert from type {b} to {a}"),
                    span.into()
                ));
            }
            a.or(b)
        }
        else {
            Ty::Invalid
        }
    }
    pub(crate) fn logger(&self) -> LoggerRef {
        self.logger.clone()
    }
}
