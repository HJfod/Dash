
use std::collections::HashMap;
use crate::{parser::{node::ASTRef, ast::token::Op}, shared::logging::LoggerRef};
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
        $res.entities.push(Entity::new_builtin_binop(
            Ty::$a,
            <parse_op!($op)>::new(BUILTIN_SPAN.clone()).into(),
            Ty::$b,
            Ty::$r
        ));
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
    Opaque,
    Function,
}

pub struct Scope<'s, 'n> {
    types: Space<Ty<'s, 'n>>,
    entities: Space<Entity<'s, 'n>>,
    level: ScopeLevel,
    decl: ASTRef<'s, 'n>,
    return_type: Option<Ty<'s, 'n>>,
    return_type_inferred_from: Option<ASTRef<'s, 'n>>,
    is_returned_to: bool,
    has_encountered_never: bool,
    /// Just stores if unreachable expressions have already been found in this scope
    /// Prevents producing six billion "unreachable expression" from a single let statement
    /// after a return
    unreachable_expression_logged: bool,
}

impl<'s, 'n> Scope<'s, 'n> {
    pub fn new(level: ScopeLevel, decl: ASTRef<'s, 'n>, return_type: Option<Ty<'s, 'n>>) -> Self {
        Self {
            types: Space::default(),
            entities: Space::default(),
            level,
            decl,
            return_type,
            return_type_inferred_from: None,
            is_returned_to: false,
            has_encountered_never: false,
            unreachable_expression_logged: false,
        }
    }

    pub fn new_top() -> Self {
        let mut res = Self::new(ScopeLevel::Opaque, ASTRef::Builtin, None);
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
    scopes: Vec<Scope<'s, 'n>>,
}

impl<'s, 'n> CoherencyVisitor<'s, 'n> {
    pub fn new(logger: LoggerRef<'s>) -> Self {
        Self {
            logger,
            scopes: vec![Scope::new_top()],
        }
    }

    fn try_find(&self, a: &Ty<'s, 'n>, op: &Op, b: &Ty<'s, 'n>) -> Option<Ty<'s, 'n>> {
        self.find::<Entity<'s, 'n>, _>(&get_binop_fun_name(a, op, b))
            .option()
            .map(move |e| e.ty().return_ty())
    }

    pub fn binop_ty(&self, a: &Ty<'s, 'n>, op: &Op, b: &Ty<'s, 'n>) -> Option<Ty<'s, 'n>> {
        if let Some(ty) = self.try_find(a, op, b) {
            return Some(ty);
        }
        // if op == Op::Neq {
        //     return self.try_find(a, Op::Eq, b);
        // }
        // if op == Op::Eq {
        //     return self.try_find(a, Op::Neq, b);
        // }
        None
    }

    /// Push a scope onto the top of the scope stack
    pub fn push_scope(&mut self, level: ScopeLevel, decl: ASTRef<'s, 'n>, return_type: Option<Ty<'s, 'n>>) {
        self.scopes.push(Scope::new(level, decl, return_type));
    }

    /// Pop the topmost scope from the scope stack with a default return type and 
    /// which expression resulted in that
    /// 
    /// If the scope contains an explicit return value (such as `yield value`) then 
    /// the default type does nothing (the default type is things like the final 
    /// expression in a block)
    pub fn pop_scope(&mut self, ty: Ty<'s, 'n>, yielding_expr: ASTRef<'s, 'n>) -> Ty<'s, 'n> {
        let scope = self.scopes.pop().expect("internal error: scope stack was empty");
        let mut ret_ty = if scope.is_returned_to {
            scope.return_type.unwrap()
        }
        else {
            if let Some(ref old) = scope.return_type {
                if !ty.convertible_to(old) {
                    self.logger.lock().unwrap().log_msg(Message::from_span(
                        Level::Error,
                        format!("Expected return type to be '{old}', got '{ty}'"),
                        yielding_expr.span(),
                    ));
                }
            }
            ty
        };

        // if any scope above this one has been returned to, this scope will never finish execution
        // if self.any_upper_scope_returns() {
            // ret_ty = Ty::Never;
        // }
        if scope.has_encountered_never {
            ret_ty = Ty::Never;
        }

        // if ret_ty.is_never() {
        //     self.encountered_never();
        // }
        ret_ty
    }

    fn encountered_never(&mut self) {
        self.scopes.last_mut().unwrap().has_encountered_never = true;
    }

    fn check_if_current_expression_is_unreachable<E: ASTNode<'s> + ?Sized>(&mut self, expr: &E) {
        // if any expression before whatever expression called this function 
        // has returned never, then this experssion is never going to be 
        // executed (by definition of never) 
        let scope = self.scopes.last_mut().unwrap();
        if scope.has_encountered_never && !scope.unreachable_expression_logged {
            scope.unreachable_expression_logged = true;
            self.logger.lock().unwrap().log_msg(Message::from_span(
                Level::Error,
                "Unreachable expression",
                expr.span(),
            ));
        }
    }

    pub fn emit_msg(&self, msg: Message<'s>) {
        self.logger.lock().unwrap().log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: LoggerRef<'s>) {
        self.logger = logger;
    }

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
