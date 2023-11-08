
use std::collections::HashMap;
use crate::{parser::{node::ASTRef, ast::token::Op}, shared::{logging::{LoggerRef, Message, Level}, src::Span}};
use super::{ty::{FullPath, Path, Ty}, entity::{Entity, get_binop_fun_name}};
use crate::parser::ast::token;
use crate::shared::src::BUILTIN_SPAN;

macro_rules! parse_op {
    (+)  => { token::Add };
    (-)  => { token::Sub };
    (*)  => { token::Mul };
    (/)  => { token::Div };
    (%)  => { token::Mod };
    (==) => { token::Eq };
    (!=) => { token::Neq };
    (<)  => { token::Lss };
    (<=) => { token::Leq };
    (>)  => { token::Gtr };
    (>=) => { token::Geq };
    (&&) => { token::And };
    (||) => { token::Or };
}

macro_rules! define_ops {
    ($res:ident; $a:ident $op:tt $b:ident -> $r:ident; $($rest:tt)+) => {
        define_ops!($res; $a $op $b -> $r;);
        define_ops!($res; $($rest)+)
    };

    ($res:ident; $a:ident $op:tt $b:ident -> $r:ident;) => {
        let binop = Entity::new_builtin_binop(
            Ty::$a,
            <parse_op!($op)>::new(BUILTIN_SPAN.clone()).into(),
            Ty::$b,
            Ty::$r
        );
        $res.entities.push(binop.name(), binop);
    };
}

pub struct Space<T> {
    entities: HashMap<FullPath, T>,
}

impl<T> Space<T> {
    pub fn push(&mut self, path: FullPath, entity: T) -> &T {
        self.entities.insert(path.clone(), entity);
        self.entities.get(&path).unwrap()
    }

    pub fn find(&self, name: &FullPath) -> Option<&T> {
        self.entities.get(name)
    }

    pub fn resolve(&self, path: &Path) -> FullPath {
        self.entities.iter()
            .find(|(full, _)| full.ends_with(path))
            .map(|p| p.0.clone())
            .unwrap_or(path.clone().into_full())
    }
}

impl<T> Default for Space<T> {
    fn default() -> Self {
        Self {
            entities: Default::default(),
        }
    }
}

impl<'s, 'n> Space<Ty<'s, 'n>> {
    fn push_ty(&mut self, ty: Ty<'s, 'n>) -> &Ty<'s, 'n> {
        self.push(FullPath::new([ty.to_string()]), ty)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScopeLevel {
    /// Normal block scope. Variables from outer scopes may be used normally
    Block,
    /// Function scope. Only constants and types from outer scopes are available
    Function,
}

pub enum Return<'s, 'n> {
    Void,
    Explicit(Ty<'s, 'n>),
    Inferred(Ty<'s, 'n>, ASTRef<'s, 'n>),
}

pub struct Scope<'s, 'n> {
    parent: *mut Scope<'s, 'n>,
    children: Vec<Scope<'s, 'n>>,
    level: ScopeLevel,
    types: Space<Ty<'s, 'n>>,
    entities: Space<Entity<'s, 'n>>,
    return_type: Return<'s, 'n>,
    decl: ASTRef<'s, 'n>,
}

impl<'s, 'n> Scope<'s, 'n> {
    fn new(
        parent: *mut Scope<'s, 'n>,
        level: ScopeLevel,
        decl: ASTRef<'s, 'n>,
        return_type: Return<'s, 'n>
    ) -> Self {
        Self {
            parent,
            children: vec![],
            types: Space::default(),
            entities: Space::default(),
            level,
            decl,
            return_type,
        }
    }

    fn new_top() -> Self {
        let mut res = Self::new(
            std::ptr::null(),
            ScopeLevel::Block,
            ASTRef::Builtin,
            Return::Void
        );
        res.types.push_ty(Ty::Void);
        res.types.push_ty(Ty::Bool);
        res.types.push_ty(Ty::Int);
        res.types.push_ty(Ty::Float);
        res.types.push_ty(Ty::String);
        define_ops! {
            res;
            Void == Void -> Bool;

            Int + Int -> Int;
            Int - Int -> Int;
            Int / Int -> Int;
            Int * Int -> Int;
            Int % Int -> Int;
            Int == Int -> Bool;
            Int > Int -> Bool;
            Int < Int -> Bool;

            Float + Float -> Float;
            Float - Float -> Float;
            Float / Float -> Float;
            Float * Float -> Float;
            Float % Float -> Float;
            Float == Float -> Bool;
            Float > Float -> Bool;
            Float < Float -> Bool;

            String == String -> Bool;
            String + String -> String;
            String * Int    -> String;
            
            Bool == Bool -> Bool;

            Bool && Bool -> Bool;
            Bool || Bool -> Bool;
        }
        res
    }

    pub fn decl(&self) -> ASTRef<'s, 'n> {
        self.decl
    }
}

pub enum FindItem<T> {
    Some(T),
    NotAvailable(T),
    None,
}

impl<T> FindItem<T> {
    pub fn option(self) -> Option<T> {
        self.into()
    }
}

#[allow(clippy::from_over_into)]
impl<T> Into<Option<T>> for FindItem<T> {
    fn into(self) -> Option<T> {
        match self {
            FindItem::Some(t) => Some(t),
            _ => None,
        }
    }
}

pub enum FindScope<'s, 'n> {
    ByLevel(ScopeLevel),
    ByDecl(ASTRef<'s, 'n>),
    TopMost,
}

impl<'s, 'n> FindScope<'s, 'n> {
    pub fn matches(&self, scope: &Scope<'s, 'n>) -> bool {
        match self {
            Self::ByLevel(level) => scope.level >= *level,
            Self::ByDecl(decl) => scope.decl == *decl,
            Self::TopMost => true,
        }
    }
}

pub struct CoherencyVisitor<'s, 'n> {
    logger: LoggerRef<'s>,
    root_scope: Scope<'s, 'n>,
    current_scope: *mut Scope<'s, 'n>,
}

impl<'s, 'n> CoherencyVisitor<'s, 'n> {
    pub fn new(logger: LoggerRef<'s>) -> Self {
        let root_scope = Scope::new_top();
        Self {
            logger,
            current_scope: &root_scope,
            root_scope,
        }
    }

    pub fn find_entity(&self, name: Path) -> FindItem<Entity<'s, 'n>> {
        
    }

    /// Push a scope onto the top of the scope stack
    pub fn enter_scope<F>(&mut self, scope: &mut *const Scope<'s, 'n>, or_create: F)
        where F:
            FnMut() -> (ScopeLevel, ASTRef<'s, 'n>, Return<'s, 'n>)
    {
        if scope.is_null() {
            let (level, decl, return_type) = or_create();
            let new_scope = Scope::new(self.current_scope, level, decl, return_type);
            unsafe {
                (*self.current_scope).children.push(new_scope);
            }
            self.current_scope = &new_scope;
            *scope = &new_scope;
        }
        else {
            self.current_scope = scope;
        }
    }

    /// Pop the topmost scope from the scope stack with a default return type and 
    /// which expression resulted in that
    /// 
    /// If the scope contains an explicit return value (such as `yield value`) then 
    /// the default type does nothing (the default type is things like the final 
    /// expression in a block)
    pub fn leave_scope(&mut self) {
        unsafe { 
            self.current_scope = (*self.current_scope).parent;
        }
    }

    pub fn emit_msg(&self, msg: Message<'s>) {
        self.logger.lock().unwrap().log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: LoggerRef<'s>) {
        self.logger = logger;
    }

    /// Helper function for verifying that types are convertible to each other
    pub fn expect_eq(&self, a: Ty<'s, 'n>, b: Ty<'s, 'n>, span: &Span<'s>) -> Ty<'s, 'n> {
        if !a.convertible_to(&b) {
            self.emit_msg(Message::from_span(
                Level::Error,
                format!("Expected type {b}, got type {a}"),
                span,
            ));
        }
        b.or(a)
    }
}
