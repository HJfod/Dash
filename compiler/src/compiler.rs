use std::{collections::HashMap, fmt::Display, marker::PhantomData, sync::Arc};

use crate::{
    src::{Logger, Message, ConsoleLogger, Src, Level, Note},
    rules::ast::{Op, ASTRef}
};

macro_rules! define_ops {
    ($res:ident; $a:ident $op:tt $b:ident -> $r:ident; $($rest:tt)+) => {
        define_ops!($res; $a $op $b -> $r;);
        define_ops!($res; $($rest)+)
    };

    ($res:ident; $a:ident $op:tt $b:ident -> $r:ident;) => {
        $res.entities.push(Entity::new_builtin_binop(Ty::$a, crate::rules::parse_op!($op), Ty::$b, Ty::$r));
    };
}

pub trait PathLike<'s, 'n> {
    fn resolve<T: Item<'s, 'n>>(&self, space: &Space<'s, 'n, T>) -> FullPath;
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct FullPath {
    path: Vec<String>,
}

impl FullPath {
    pub fn new<T: Into<Vec<String>>>(path: T) -> Self {
        Self { path: path.into() }
    }

    pub fn ends_with(&self, path: &Path) -> bool {
        self.to_string().ends_with(&path.to_string())
    }
}

impl Display for FullPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("::{}", self.path.join("::")))
    }
}

impl<'s, 'n> PathLike<'s, 'n> for FullPath {
    fn resolve<T: Item<'s, 'n>>(&self, _: &Space<'s, 'n, T>) -> FullPath {
        self.clone()
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Path {
    absolute: bool,
    path: Vec<String>,
}

impl Path {
    pub fn new<T: Into<Vec<String>>>(path: T, absolute: bool) -> Self {
        Self { path: path.into(), absolute }
    }

    pub fn into_full(self) -> FullPath {
        FullPath::new(self.path)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}{}", 
            if self.absolute { "::" } else { "" },
            self.path.join("::")
        ))
    }
}

impl<'s, 'n> PathLike<'s, 'n> for Path {
    fn resolve<T: Item<'s, 'n>>(&self, space: &Space<'s, 'n, T>) -> FullPath {
        space.resolve(self)
    }
}

pub trait Item<'s, 'n>: Sized {
    fn full_path(&self) -> FullPath;
    fn space<'a>(scope: &'a Scope<'s, 'n>) -> &'a Space<'s, 'n, Self>;
    fn space_mut<'a>(scope: &'a mut Scope<'s, 'n>) -> &'a mut Space<'s, 'n, Self>;
    fn can_access_outside_function(&self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty<'s, 'n> {
    Invalid,
    Inferred,
    Void,
    Bool,
    Int,
    Float,
    String,
    Function {
        params: Vec<(String, Ty<'s, 'n>)>,
        ret_ty: Box<Ty<'s, 'n>>,
        decl: ASTRef<'s, 'n>,
    },
}

impl<'s, 'n> Ty<'s, 'n> {
    pub fn is_unreal(&self) -> bool {
        matches!(self, Ty::Inferred | Ty::Invalid)
    }

    pub fn convertible_to(&self, other: &Ty) -> bool {
        self.is_unreal() || other.is_unreal() || *self == *other
    }

    pub fn return_ty(&self) -> Ty<'s, 'n> {
        match self {
            Self::Function { params: _, ret_ty, decl: _ } => *ret_ty.clone(),
            _ => self.clone(),
        }
    }

    pub fn decl(&self) -> ASTRef<'s, 'n> {
        match self {
            Ty::Invalid => ASTRef::Builtin,
            Ty::Inferred => ASTRef::Builtin,
            Ty::Void => ASTRef::Builtin,
            Ty::Bool => ASTRef::Builtin,
            Ty::Int => ASTRef::Builtin,
            Ty::Float => ASTRef::Builtin,
            Ty::String => ASTRef::Builtin,
            Ty::Function { params: _, ret_ty: _, decl } => *decl,
        }
    }

    /// Returns `other` if this type is `invalid` or `unknown`
    pub fn or(self, other: Ty<'s, 'n>) -> Ty<'s, 'n> {
        if self.is_unreal() {
            other
        }
        else {
            self
        }
    }
}

impl Display for Ty<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => f.write_str("invalid"),
            Self::Inferred => f.write_str("unknown"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Function { params, ret_ty, decl: _ } => f.write_fmt(format_args!(
                "fun({}) -> {ret_ty}", params.iter()
                    .map(|(p, t)| format!("{p}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }
}

impl<'s, 'n> Item<'s, 'n> for Ty<'s, 'n> {
    fn full_path(&self) -> FullPath {
        match self {
            builtin => FullPath::new([format!("{builtin}")])
        }
    }

    fn space<'a>(scope: &'a Scope<'s, 'n>) -> &'a Space<'s, 'n, Self> {
        &scope.types
    }
    fn space_mut<'a>(scope: &'a mut Scope<'s, 'n>) -> &'a mut Space<'s, 'n, Self> {
        &mut scope.types
    }

    fn can_access_outside_function(&self) -> bool {
        true
    }
}

pub struct Entity<'s, 'n> {
    name: FullPath,
    decl: ASTRef<'s, 'n>,
    ty: Ty<'s, 'n>,
    mutable: bool,
}

impl<'s, 'n> Entity<'s, 'n> {
    pub fn new(name: FullPath, decl: ASTRef<'s, 'n>, ty: Ty<'s, 'n>, mutable: bool) -> Self {
        Self { name, decl, ty, mutable }
    }

    pub fn new_builtin_binop(a: Ty<'s, 'n>, op: Op, b: Ty<'s, 'n>, ret: Ty<'s, 'n>) -> Self {
        Self {
            name: get_binop_fun_name(&a, op, &b),
            decl: ASTRef::Builtin,
            ty: Ty::Function {
                params: vec![
                    ("a".into(), a),
                    ("b".into(), b),
                ],
                ret_ty: ret.into(),
                decl: ASTRef::Builtin,
            },
            mutable: false
        }
    }

    pub fn decl(&self) -> ASTRef<'s, 'n> {
        self.decl
    }

    pub fn ty(&self) -> Ty<'s, 'n> {
        self.ty.clone()
    }
}

impl<'s, 'n> Item<'s, 'n> for Entity<'s, 'n> {
    fn full_path(&self) -> FullPath {
        self.name.clone()
    }

    fn space<'a>(scope: &'a Scope<'s, 'n>) -> &'a Space<'s, 'n, Self> {
        &scope.entities
    }
    fn space_mut<'a>(scope: &'a mut Scope<'s, 'n>) -> &'a mut Space<'s, 'n, Self> {
        &mut scope.entities
    }

    fn can_access_outside_function(&self) -> bool {
        // only const entities defined outside the function scope can be accessed 
        !self.mutable
    }
}

pub struct Space<'s, 'n, T: Item<'s, 'n>> {
    entities: HashMap<FullPath, T>,
    _phantom1: PhantomData<&'s Src>,
    _phantom2: PhantomData<&'n i32>,
}

impl<'s, 'n, T: Item<'s, 'n>> Space<'s, 'n, T> {
    pub fn new() -> Self {
        Self {
            entities: Default::default(),
            _phantom1: Default::default(),
            _phantom2: Default::default(),
        }
    }

    pub fn push(&mut self, entity: T) -> &T {
        let name = entity.full_path().clone();
        self.entities.insert(entity.full_path().clone(), entity);
        self.entities.get(&name).unwrap()
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScopeLevel {
    Opaque,
    Function,
}

pub struct Scope<'s, 'n> {
    types: Space<'s, 'n, Ty<'s, 'n>>,
    entities: Space<'s, 'n, Entity<'s, 'n>>,
    level: ScopeLevel,
    decl: ASTRef<'s, 'n>,
}

impl<'s, 'n> Scope<'s, 'n> {
    pub fn new(level: ScopeLevel, decl: ASTRef<'s, 'n>) -> Self {
        Self {
            types: Space::new(),
            entities: Space::new(),
            level,
            decl,
        }
    }

    pub fn new_top() -> Self {
        let mut res = Self::new(ScopeLevel::Opaque, ASTRef::Builtin);
        res.types.push(Ty::Void);
        res.types.push(Ty::Bool);
        res.types.push(Ty::Int);
        res.types.push(Ty::Float);
        res.types.push(Ty::String);
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

impl<T> Into<Option<T>> for FindItem<T> {
    fn into(self) -> Option<T> {
        match self {
            FindItem::Some(t) => Some(t),
            _ => None,
        }
    }
}

pub struct TypeChecker<'s, 'n> {
    logger: Arc<dyn Logger<'s>>,
    scopes: Vec<Scope<'s, 'n>>,
    return_type: HashMap<ASTRef<'s, 'n>, Ty<'s, 'n>>,
}

impl<'s, 'n> TypeChecker<'s, 'n> {
    fn try_find(&self, a: &Ty<'s, 'n>, op: Op, b: &Ty<'s, 'n>) -> Option<Ty<'s, 'n>> {
        self.find::<Entity<'s, 'n>, _>(&get_binop_fun_name(a, op, b))
            .option()
            .map(move |e| e.ty.return_ty())
    }

    pub fn binop_ty(&self, a: &Ty<'s, 'n>, op: Op, b: &Ty<'s, 'n>) -> Option<Ty<'s, 'n>> {
        if let Some(ty) = self.try_find(a, op, b) {
            return Some(ty);
        }
        if op == Op::Neq {
            return self.try_find(a, Op::Eq, b);
        }
        if op == Op::Eq {
            return self.try_find(a, Op::Neq, b);
        }
        None
    }

    pub fn last_space<T: Item<'s, 'n>>(&self) -> &Space<'s, 'n, T> {
        T::space(self.scopes.last().unwrap())
    }

    pub fn push<T: Item<'s, 'n>>(&mut self, item: T) -> &T {
        T::space_mut(self.scopes.last_mut().unwrap()).push(item)
    }

    pub fn find<'a, T: Item<'s, 'n>, P: PathLike<'s, 'n>>(&'a self, path: &P) -> FindItem<&'a T> {
        let mut outside_function = false;
        for scope in self.scopes.iter().rev() {
            let space = T::space(&scope);
            if let Some(e) = space.find(&path.resolve(space)) {
                return if !outside_function || e.can_access_outside_function() {
                    FindItem::Some(e)
                }
                else {
                    FindItem::NotAvailable(e)
                }
            }
            // can't access mutable entities defined outside a function scope
            if scope.level >= ScopeLevel::Function {
                outside_function = true;
            }
        }
        FindItem::None
    }

    pub fn resolve_new(&self, path: Path) -> FullPath {
        // todo: namespaces
        path.into_full()
    }

    pub fn add_return_type(&mut self, level: ScopeLevel, ty: Ty<'s, 'n>, node: ASTRef<'s, 'n>) {
        match self.scopes.iter().rev().find(|s| s.level == level).map(|s| s.decl) {
            Some(decl) => {
                if let Some(old) = self.return_type.get(&decl) {
                    if !ty.convertible_to(&old) {
                        self.emit_msg(Message::from_meta(
                            Level::Error,
                            format!("Expected return type to be '{old}', got '{ty}'"),
                            node.meta(),
                        ).note(Note::new_at(
                            "Return type defined here",
                            decl.meta().src, decl.meta().range.clone()
                        )));
                    }
                }
                else {
                    self.return_type.insert(decl, ty);
                }
            }
            None => {
                self.emit_msg(Message::from_meta(
                    Level::Error,
                    format!("Can not return here"),
                    node.meta(),
                ));
            }
        }
    }
    
    pub fn push_scope(&mut self, level: ScopeLevel, decl: ASTRef<'s, 'n>) {
        self.scopes.push(Scope::new(level, decl));
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn emit_msg(&self, msg: Message<'s>) {
        self.logger.log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: Arc<dyn Logger<'s>>) {
        self.logger = logger;
    }

    pub fn new() -> Self {
        Self {
            logger: Arc::from(ConsoleLogger),
            scopes: vec![Scope::new_top()],
            return_type: Default::default(),
        }
    }
}

pub trait TypeCheck<'s, 'n> {
    fn typecheck(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n>;
}

fn get_unop_fun_name<'s, 'n>(a: &Ty<'s, 'n>, op: Op) -> FullPath {
    FullPath::new([format!("@unop`{a}{op}`")])
}

fn get_binop_fun_name<'s, 'n>(a: &Ty<'s, 'n>, op: Op, b: &Ty<'s, 'n>) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
