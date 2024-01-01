
use std::{pin::Pin, ptr::NonNull, collections::HashMap};
use crate::shared::{logger::{LoggerRef, Message, Level, Note}, ptr_iter::PtrChainIter, src::{ArcSpan, Span}};
use super::{ty::Ty, path::{FullIdentPath, IdentPath, Ident}, entity::Entity, pool::AST, resolve::Resolve};

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

#[derive(PartialEq, Eq)]
struct UnresolvedData {
    msg: String,
    span: ArcSpan,
}

pub(crate) struct Checker {
    logger: LoggerRef,
    current_scope: NonNull<Scope>,
    // wait this isn't a stable memory location...
    scopes: Vec<Scope>,
    namespace_stack: FullIdentPath,
    unresolved_nodes: HashMap<NodeID, UnresolvedData>,
}

impl Checker {
    fn new(logger: LoggerRef) -> Pin<Box<Self>> {
        let mut ret = Box::pin(Self {
            logger: logger.clone(),
            current_scope: NonNull::dangling(),
            scopes: Vec::from([Scope::root()]),
            namespace_stack: FullIdentPath::default(),
            unresolved_nodes: HashMap::new(),
        });
        ret.current_scope = NonNull::from(ret.scopes.first_mut().unwrap());
        ret
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

    pub fn scopes(&self) -> impl Iterator<Item = &mut Scope> {
        PtrChainIter::new(self.current_scope, |s| s.parent)
    }
    pub fn scope<'a>(&mut self) -> &'a mut Scope {
        unsafe { self.current_scope.as_mut() }
    }
    pub fn enter_scope(&mut self, scope: &mut Option<NonNull<Scope>>) -> LeaveScope {
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
