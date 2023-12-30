
use std::{collections::HashMap, pin::Pin, ptr::NonNull};
use crate::{shared::{logger::{LoggerRef, Message, Level}, ptr_iter::PtrChainIter}, ice};
use super::{ty::Ty, path::{FullIdentPath, IdentPath, Ident}, entity::Entity, Ice};

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
    logger: LoggerRef,
    parent: Option<NonNull<Scope>>,
    types: ItemSpace<Ty>,
    entities: ItemSpace<Entity>,
}

impl Scope {
    pub fn new(parent: &mut Scope) -> Self {
        Self {
            logger: parent.logger.clone(),
            parent: Some(NonNull::from(parent)),
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

pub struct Checker {
    logger: LoggerRef,
    root_scope: Scope,
    current_scope: NonNull<Scope>,
    namespace_stack: FullIdentPath,
}

impl Checker {
    pub fn new(logger: LoggerRef) -> Pin<Box<Self>> {
        let mut ret = Box::pin(Self {
            logger: logger.clone(),
            root_scope: Scope::root(logger),
            current_scope: NonNull::dangling(),
            namespace_stack: FullIdentPath::default(),
        });
        ret.current_scope = NonNull::from(&mut ret.root_scope);
        ret
    }
    pub(crate) fn scopes(&self) -> impl Iterator<Item = &mut Scope> {
        PtrChainIter::new(self.current_scope, |s| s.parent)
    }
    pub fn scope<'a>(&mut self) -> &'a mut Scope {
        unsafe { self.current_scope.as_mut() }
    }
    pub fn enter_scope(&mut self, scope: NonNull<Scope>) {
        self.current_scope = scope;
    }
    pub fn leave_scope(&mut self) {
        if let Some(parent) = self.scope().parent {
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
}
