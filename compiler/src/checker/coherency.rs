
use std::collections::HashMap;
use crate::shared::{logger::{LoggerRef, Message, Level, Note}, src::{ArcSpan, Span}};
use super::{ty::Ty, path::{FullIdentPath, IdentPath, Ident}, entity::Entity, pool::AST, resolve::Resolve};

pub(crate) struct ItemSpace<T> {
    items: HashMap<FullIdentPath, T>,
}

impl<T> ItemSpace<T> {
    fn new<H: Into<HashMap<FullIdentPath, T>>>(values: H) -> Self {
        Self { items: values.into() }
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

pub(crate) struct ItemSpaceWithStack<'s, T> {
    space: &'s mut ItemSpace<T>,
    stack: &'s FullIdentPath,
}

impl<'s, T> ItemSpaceWithStack<'s, T> {
    pub fn try_push(self, name: &IdentPath, item: T) -> Result<&'s T, &'s T> {
        // The full name for this item is the current topmost namespace name 
        // joined with the name of the item
        let full_name = self.stack.join(name);
        // Check if this name already exists in this scope
        // Can't just do `if let Some` because the borrow checker then complains 
        // that you can't mutate self.items in the `else` branch afterwards
        if self.space.items.contains_key(&full_name) {
            Err(self.space.items.get(&full_name).unwrap())
        }
        else {
            self.space.items.insert(full_name.clone(), item);
            Ok(self.space.items.get(&full_name).unwrap())
        }
    }
}

pub(crate) struct Scope {
    id: ScopeID,
    parent: Option<ScopeID>,
    types: ItemSpace<Ty>,
    entities: ItemSpace<Entity>,
}

impl Scope {
    fn new(id: ScopeID, parent: ScopeID) -> Self {
        Self {
            id,
            parent: Some(parent),
            types: Default::default(),
            entities: Default::default(),
        }
    }
    fn root() -> Self {
        Self {
            id: ScopeID(0),
            parent: None,
            types: ItemSpace::new(
                [Ty::Never, Ty::Void, Ty::Bool, Ty::Int, Ty::Float, Ty::String]
                    .map(|t| (FullIdentPath::new([t.to_string().into()]), t))
            ),
            entities: Default::default(),
        }
    }
    pub fn types(&self) -> &ItemSpace<Ty> {
        &self.types
    }
    pub fn entities(&self) -> &ItemSpace<Entity> {
        &self.entities
    }
    fn drop_ephemeral(&mut self) {
        self.entities.items.retain(|_, v| !v.ephemeral())
    }
}

pub(crate) struct ScopeWithStack<'s> {
    scope: &'s mut Scope,
    stack: &'s FullIdentPath,
}

impl<'s> ScopeWithStack<'s> {
    pub fn types(self) -> ItemSpaceWithStack<'s, Ty> {
        ItemSpaceWithStack { space: &mut self.scope.types, stack: self.stack }
    }
    pub fn entities(self) -> ItemSpaceWithStack<'s, Entity> {
        ItemSpaceWithStack { space: &mut self.scope.entities, stack: self.stack }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NodeID(usize);

impl NodeID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        use std::sync::atomic::AtomicUsize;
        static mut COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self(unsafe { &COUNTER }.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ScopeID(usize);

#[derive(PartialEq, Eq)]
struct UnresolvedData {
    msg: String,
    span: ArcSpan,
}

pub(crate) struct ScopeIter<'s> {
    current: Option<ScopeID>,
    scopes: &'s Vec<Scope>,
}

impl<'s> ScopeIter<'s> {
    fn new(first: ScopeID, scopes: &'s Vec<Scope>) -> Self {
        Self { current: Some(first), scopes }
    }
}

impl<'s> Iterator for ScopeIter<'s> {
    type Item = &'s Scope;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.scopes.get(self.current?.0).unwrap();
        self.current = ret.parent;
        Some(ret)
    }
}

pub(crate) struct Checker {
    logger: LoggerRef,
    current_scope: ScopeID,
    scopes: Vec<Scope>,
    namespace_stack: FullIdentPath,
    unresolved_nodes: HashMap<NodeID, UnresolvedData>,
}

impl Checker {
    fn new(logger: LoggerRef) -> Self {
        Self {
            logger: logger.clone(),
            current_scope: ScopeID(0),
            scopes: Vec::from([Scope::root()]),
            namespace_stack: FullIdentPath::default(),
            unresolved_nodes: HashMap::new(),
        }
    }
    pub fn try_resolve(ast: &mut AST, logger: LoggerRef) -> Ty {
        let mut checker = Checker::new(logger);
        let mut unresolved = checker.unresolved_nodes.keys().copied().collect::<Vec<_>>();
        for i in 0.. {
            // todo: allow customizing max loop count via a compiler option
            if i > 1000 {
                checker.logger.lock().unwrap().log(Message::new(
                    Level::Error,
                    "Internal error: maximum check loop count reached (1000)",
                    Span::builtin()
                ).note(Note::new(
                    "Try simplifying your codebase, moving definitions of types \
                    and functions before their uses", true
                )));
                return Ty::Invalid;
            }
            if let Some(r) = ast.try_resolve(&mut checker) {
                return r;
            }
            if unresolved.into_iter().eq(checker.unresolved_nodes.keys().copied()) {
                for err in checker.unresolved_nodes.values() {
                    checker.logger.lock().unwrap().log(Message::new(
                        Level::Error,
                        err.msg.clone(),
                        err.span.as_ref()
                    ));
                }
                return Ty::Invalid;
            }
            unresolved = checker.unresolved_nodes.keys().copied().collect::<Vec<_>>();
        }
        unreachable!()
    }

    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        ScopeIter::new(self.current_scope, &self.scopes)
    }
    pub fn scope(&mut self) -> ScopeWithStack {
        ScopeWithStack {
            scope: self.scopes.get_mut(self.current_scope.0).unwrap(),
            stack: &self.namespace_stack
        }
    }
    pub fn enter_scope(&mut self, scope: &mut Option<ScopeID>) -> LeaveScope {
        match scope {
            Some(scope) => self.current_scope = *scope,
            None => {
                let id = ScopeID(self.scopes.len());
                self.scopes.push(Scope::new(id, self.current_scope));
                *scope = Some(id);
                self.current_scope = scope.unwrap();
            }
        }
        LeaveScope { checker: self }
    }
    fn leave_scope(&mut self) {
        if let Some(parent) = self.scope().scope.parent {
            self.scope().scope.drop_ephemeral();
            self.current_scope = parent;
        }
    }

    pub fn namespace_stack(&self) -> &FullIdentPath {
        &self.namespace_stack
    }
    pub fn enter_namespace(&mut self, name: Ident) {
        self.namespace_stack.push(name);
    }
    pub fn leave_namespace(&mut self) {
        self.namespace_stack.pop();
    }

    pub(super) fn mark_unresolved(&mut self, id: NodeID, msg: String, span: ArcSpan) {
        self.unresolved_nodes.insert(id, UnresolvedData { msg, span });
    }
    pub(super) fn mark_resolved(&mut self, id: NodeID) {
        self.unresolved_nodes.remove(&id);
    }

    pub fn expect_ty_decided(&self, a: Ty, span: Option<ArcSpan>) -> bool {
        if let Ty::Undecided(name, a_span) = a {
            self.logger.lock().unwrap().log(Message::new(
                Level::Error,
                format!("The type of {name} needs to be known at this point"),
                span.unwrap_or(ArcSpan::builtin()).as_ref()
            ).note(Note::new_at(
                format!("Declaration of {name} here"),
                a_span.as_ref()
            )));
            return false;
        }
        true
    }
    pub fn expect_ty_eq(&self, a: Ty, b: Ty, span: Option<ArcSpan>) -> Ty {
        if self.expect_ty_decided(a.clone(), span.clone()) &&
            self.expect_ty_decided(b.clone(), span.clone()) {
            if !b.convertible(&a) {
                self.logger.lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Cannot convert from type {b} to {a}"),
                    span.unwrap_or(ArcSpan::builtin()).as_ref()
                ));
            }
            a.or(b)
        }
        else {
            Ty::Invalid
        }
    }
    pub fn logger(&self) -> LoggerRef {
        self.logger.clone()
    }
}
