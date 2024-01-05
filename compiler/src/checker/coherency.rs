
use std::collections::HashMap;
use crate::{shared::{logger::{LoggerRef, Message, Level, Note}, src::{ArcSpan, Span}}, ast::token::op, parser::parse::{NodeID, NodePool}};
use super::{ty::Ty, path::{FullIdentPath, IdentPath, Ident}, entity::Entity, pool::AST, resolve::Resolve};

#[derive(Debug)]
struct ItemSpace<T> {
    items: HashMap<FullIdentPath, T>,
}

impl<T> ItemSpace<T> {
    fn new<H: Into<HashMap<FullIdentPath, T>>>(values: H) -> Self {
        Self { items: values.into() }
    }
    /// Try to find an item in this scope with a fully resolved name
    fn get(&self, full_name: &FullIdentPath) -> Option<&T> {
        self.items.get(full_name)
    }
    /// Try to find an item in this scope with an unresolved name
    fn find(&self, name: &IdentPath, stack: &FullIdentPath) -> Option<&T> {
        // This is an optimization; the else branch would also do this since 
        // FullIdentPath::join would just return `name` every time
        if name.is_absolute() {
            self.get(&name.to_full())
        }
        else {
            // Try joining the path to the namespace stack. If not found, check 
            // that namespace's parent namespace, all the way down to root
            let mut temp = stack.clone();
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
    fn try_push(&mut self, name: &IdentPath, item: T, stack: &FullIdentPath) -> Result<&T, &T> {
        // The full name for this item is the current topmost namespace name 
        // joined with the name of the item
        let full_name = stack.join(name);
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
}

impl<T> Default for ItemSpace<T> {
    fn default() -> Self {
        Self {
            items: Default::default()
        }
    }
}

#[derive(Debug)]
pub(crate) struct ItemSpaceWithStack<'s, T> {
    space: &'s ItemSpace<T>,
    stack: &'s FullIdentPath,
}

impl<'s, T> ItemSpaceWithStack<'s, T> {
    /// Try to find an item in this scope with an unresolved name
    pub fn find(self, name: &IdentPath) -> Option<&'s T> {
        self.space.find(name, self.stack)
    }
}

#[derive(Debug)]
pub(crate) struct ItemSpaceWithStackMut<'s, T> {
    space: &'s mut ItemSpace<T>,
    stack: &'s FullIdentPath,
}

#[allow(unused)]
impl<'s, T> ItemSpaceWithStackMut<'s, T> {
    /// Try to find an item in this scope with an unresolved name
    pub fn find(self, name: &IdentPath) -> Option<&'s T> {
        self.space.find(name, self.stack)
    }
    pub fn try_push(self, name: &IdentPath, item: T) -> Result<&'s T, &'s T> {
        println!("pushing {name}");
        self.space.try_push(name, item, self.stack)
    }
}

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeID>,
    types: ItemSpace<Ty>,
    entities: ItemSpace<Entity>,
}

impl Scope {
    fn new(parent: ScopeID) -> Self {
        Self {
            parent: Some(parent),
            types: Default::default(),
            entities: Default::default(),
        }
    }
    fn root() -> Self {
        macro_rules! decl_binop {
            ($a: ident $op: ident $b: ident => $r: ident) => {
                (Ty::$a, op::BinaryOp::$op, Ty::$b, Ty::$r)
            };
        }

        Self {
            parent: None,
            types: ItemSpace::new(
                [Ty::Never, Ty::Void, Ty::Bool, Ty::Int, Ty::Float, Ty::String]
                    .map(|t| (FullIdentPath::new([t.to_string().into()]), t))
            ),
            entities: ItemSpace::new(
                [
                    decl_binop!(Int Eq  Int => Bool),
                    decl_binop!(Int Neq Int => Bool),
                    decl_binop!(Int Less Int => Bool),
                    decl_binop!(Int Leq Int => Bool),
                    decl_binop!(Int Grt Int => Bool),
                    decl_binop!(Int Geq Int => Bool),
                    decl_binop!(Int Add Int => Int),
                    decl_binop!(Int Sub Int => Int),
                    decl_binop!(Int Mul Int => Int),
                    decl_binop!(Int Div Int => Int),
                    decl_binop!(Int Mod Int => Int),
                    
                    decl_binop!(Float Eq  Float => Bool),
                    decl_binop!(Float Neq Float => Bool),
                    decl_binop!(Float Less Float => Bool),
                    decl_binop!(Float Leq Float => Bool),
                    decl_binop!(Float Grt Float => Bool),
                    decl_binop!(Float Geq Float => Bool),
                    decl_binop!(Float Add Float => Float),
                    decl_binop!(Float Sub Float => Float),
                    decl_binop!(Float Mul Float => Float),
                    decl_binop!(Float Div Float => Float),
                    decl_binop!(Float Mod Float => Float),

                    decl_binop!(Int Add Float => Float),
                    decl_binop!(Int Sub Float => Float),
                    decl_binop!(Int Mul Float => Float),
                    decl_binop!(Int Div Float => Float),
                    decl_binop!(Int Mod Float => Int),
                    decl_binop!(Float Mod Int => Float),

                    decl_binop!(String Eq String => Bool),
                    decl_binop!(String Neq String => Bool),
                    decl_binop!(String Add String => String),
                    decl_binop!(String Mul Int => String),

                    decl_binop!(Bool And Bool => Bool),
                    decl_binop!(Bool Or Bool => Bool),
                ]
                .map(|(a, op, b, ret)| (
                    FullIdentPath::new([Ident::BinOp(a.clone(), op, b.clone())]),
                    Entity::new(
                        Ty::Function {
                            params: vec![(None, a), (None, b)],
                            ret_ty: Box::from(ret)
                        },
                        ArcSpan::builtin(),
                        false
                    )
                ))
            ),
        }
    }
    fn drop_ephemeral(&mut self) {
        // println!("dropping ephemeral: {}", self.entities.items.len());
        self.entities.items.retain(|_, v| !v.ephemeral());
        // println!("dropped ephemeral: {}", self.entities.items.len());
    }
}

#[derive(Debug)]
pub(crate) struct ScopeWithStack<'s> {
    scope: &'s Scope,
    stack: &'s FullIdentPath,
}

impl<'s> ScopeWithStack<'s> {
    pub fn types(&self) -> ItemSpaceWithStack<'s, Ty> {
        ItemSpaceWithStack { space: &self.scope.types, stack: self.stack }
    }
    pub fn entities(&self) -> ItemSpaceWithStack<'s, Entity> {
        ItemSpaceWithStack { space: &self.scope.entities, stack: self.stack }
    }
}

/// Used to pass the namespace stack from the checker to 
/// `ItemSpaceWithStack::try_push`, since it needs to know what the current 
/// namespace is to figure out the fully qualified name
#[derive(Debug)]
pub(crate) struct ScopeWithStackMut<'s> {
    scope: &'s mut Scope,
    stack: &'s FullIdentPath,
}

#[allow(unused)]
impl<'s> ScopeWithStackMut<'s> {
    pub fn types(self) -> ItemSpaceWithStack<'s, Ty> {
        ItemSpaceWithStack { space: &self.scope.types, stack: self.stack }
    }
    pub fn entities(self) -> ItemSpaceWithStack<'s, Entity> {
        ItemSpaceWithStack { space: &self.scope.entities, stack: self.stack }
    }
    pub fn types_mut(self) -> ItemSpaceWithStackMut<'s, Ty> {
        ItemSpaceWithStackMut { space: &mut self.scope.types, stack: self.stack }
    }
    pub fn entities_mut(self) -> ItemSpaceWithStackMut<'s, Entity> {
        ItemSpaceWithStackMut { space: &mut self.scope.entities, stack: self.stack }
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
pub(crate) struct ScopeID(usize);

#[derive(PartialEq, Eq)]
struct UnresolvedData {
    msg: String,
    span: ArcSpan,
}

pub(crate) struct ScopeIter<'s> {
    current: Option<ScopeID>,
    scopes: &'s Vec<Scope>,
    stack: &'s FullIdentPath,
}

impl<'s> ScopeIter<'s> {
    fn new(first: ScopeID, scopes: &'s Vec<Scope>, stack: &'s FullIdentPath) -> Self {
        Self { current: Some(first), scopes, stack }
    }
}

impl<'s> Iterator for ScopeIter<'s> {
    type Item = ScopeWithStack<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.scopes.get(self.current?.0).unwrap();
        self.current = ret.parent;
        Some(ScopeWithStack { scope: ret, stack: self.stack })
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
    pub fn try_resolve(ast: &mut AST, list: &mut NodePool, logger: LoggerRef) -> Ty {
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
            if let Some(r) = ast.try_resolve(list, &mut checker) {
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
            println!("going for another round");
        }
        unreachable!()
    }

    pub fn scopes(&self) -> ScopeIter {
        ScopeIter::new(self.current_scope, &self.scopes, &self.namespace_stack)
    }
    pub fn scope(&mut self) -> ScopeWithStackMut {
        ScopeWithStackMut {
            scope: self.scopes.get_mut(self.current_scope.0).unwrap(),
            stack: &self.namespace_stack
        }
    }
    pub fn enter_scope(&mut self, scope: &mut Option<ScopeID>) -> LeaveScope {
        match scope {
            Some(scope) => self.current_scope = *scope,
            None => {
                *scope = Some(ScopeID(self.scopes.len()));
                self.scopes.push(Scope::new(self.current_scope));
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
