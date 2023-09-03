use std::{collections::HashMap, fmt::Display, marker::PhantomData, sync::Arc};

use crate::{
    src::{Logger, Message, ConsoleLogger, Src, Range},
    rules::ast::{Op, ASTNode}
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

pub trait PathLike<'s> {
    fn resolve<T: Item<'s>>(&self, space: &Space<'s, T>) -> FullPath;
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

impl<'s> PathLike<'s> for FullPath {
    fn resolve<T: Item<'s>>(&self, _: &Space<'s, T>) -> FullPath {
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

impl<'s> PathLike<'s> for Path {
    fn resolve<T: Item<'s>>(&self, space: &Space<'s, T>) -> FullPath {
        space.resolve(self)
    }
}

pub trait Item<'s>: Sized {
    fn full_path(&self) -> FullPath;
    fn space<'a>(scope: &'a Scope<'s>) -> &'a Space<'s, Self>;
    fn space_mut<'a>(scope: &'a mut Scope<'s>) -> &'a mut Space<'s, Self>;
    fn can_access_outside_function(&self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty<'s> {
    Invalid,
    Inferred,
    Void,
    Bool,
    Int,
    Float,
    String,
    Function {
        params: Vec<(String, Ty<'s>)>,
        ret_ty: Box<Ty<'s>>,
        decl: ASTNode<'s>,
    },
}

impl<'s> Ty<'s> {
    pub fn is_unreal(&self) -> bool {
        matches!(self, Ty::Inferred | Ty::Invalid)
    }

    pub fn convertible_to(&self, other: &Ty) -> bool {
        self.is_unreal() || other.is_unreal() || *self == *other
    }

    pub fn return_ty(&self) -> Ty<'s> {
        match self {
            Self::Function { params: _, ret_ty, decl: _ } => *ret_ty.clone(),
            _ => self.clone(),
        }
    }

    pub fn src(&self) -> &'s Src {
        match self {
            Ty::Invalid => &Src::Builtin,
            Ty::Inferred => &Src::Builtin,
            Ty::Void => &Src::Builtin,
            Ty::Bool => &Src::Builtin,
            Ty::Int => &Src::Builtin,
            Ty::Float => &Src::Builtin,
            Ty::String => &Src::Builtin,
            Ty::Function { params: _, ret_ty: _, decl } => decl.meta().src,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Ty::Invalid => Range::zero(),
            Ty::Inferred => Range::zero(),
            Ty::Void => Range::zero(),
            Ty::Bool => Range::zero(),
            Ty::Int => Range::zero(),
            Ty::Float => Range::zero(),
            Ty::String => Range::zero(),
            Ty::Function { params: _, ret_ty: _, decl } => decl.meta().range.clone(),
        }
    }

    /// Returns `other` if this type is `invalid` or `unknown`
    pub fn or(self, other: Ty<'s>) -> Ty<'s> {
        if self.is_unreal() {
            other
        }
        else {
            self
        }
    }
}

impl Display for Ty<'_> {
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

impl<'s> Item<'s> for Ty<'s> {
    fn full_path(&self) -> FullPath {
        match self {
            builtin => FullPath::new([format!("{builtin}")])
        }
    }

    fn space<'a>(scope: &'a Scope<'s>) -> &'a Space<'s, Self> {
        &scope.types
    }
    fn space_mut<'a>(scope: &'a mut Scope<'s>) -> &'a mut Space<'s, Self> {
        &mut scope.types
    }

    fn can_access_outside_function(&self) -> bool {
        true
    }
}

pub struct Entity<'s> {
    name: FullPath,
    src: &'s Src,
    range: Range,
    ty: Ty<'s>,
    mutable: bool,
}

impl<'s> Entity<'s> {
    pub fn new(name: FullPath, src: &'s Src, range: Range, ty: Ty<'s>, mutable: bool) -> Self {
        Self { name, src, range, ty, mutable }
    }

    pub fn new_builtin_binop(a: Ty<'s>, op: Op, b: Ty<'s>, ret: Ty<'s>) -> Self {
        Self {
            name: get_binop_fun_name(&a, op, &b),
            src: &Src::Builtin,
            range: Range::zero(),
            ty: Ty::Function {
                params: vec![
                    ("a".into(), a),
                    ("b".into(), b),
                ],
                ret_ty: ret.into(),
                decl: ASTNode::Builtin,
            },
            mutable: false
        }
    }

    pub fn src(&self) -> &'s Src {
        self.src
    }

    pub fn range(&self) -> Range {
        self.range.clone()
    }

    pub fn ty(&self) -> Ty<'s> {
        self.ty.clone()
    }
}

impl<'s> Item<'s> for Entity<'s> {
    fn full_path(&self) -> FullPath {
        self.name.clone()
    }

    fn space<'a>(scope: &'a Scope<'s>) -> &'a Space<'s, Self> {
        &scope.entities
    }
    fn space_mut<'a>(scope: &'a mut Scope<'s>) -> &'a mut Space<'s, Self> {
        &mut scope.entities
    }

    fn can_access_outside_function(&self) -> bool {
        // only const entities defined outside the function scope can be accessed 
        !self.mutable
    }
}

pub struct Space<'s, T: Item<'s>> {
    entities: HashMap<FullPath, T>,
    _phantom: PhantomData<&'s Src>,
}

impl<'s, T: Item<'s>> Space<'s, T> {
    pub fn new() -> Self {
        Self {
            entities: Default::default(),
            _phantom: Default::default(),
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

pub struct Scope<'s> {
    types: Space<'s, Ty<'s>>,
    entities: Space<'s, Entity<'s>>,
    level: ScopeLevel,
}

impl<'s> Scope<'s> {
    pub fn new(level: ScopeLevel) -> Self {
        Self {
            types: Space::new(),
            entities: Space::new(),
            level,
        }
    }

    pub fn new_top() -> Self {
        let mut res = Self::new(ScopeLevel::Opaque);
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

pub struct TypeChecker<'s> {
    logger: Arc<dyn Logger<'s>>,
    scopes: Vec<Scope<'s>>,
    return_type: HashMap<ASTNode<'s>, Ty<'s>>,
}

impl<'s> TypeChecker<'s> {
    fn try_find(&self, a: &Ty<'s>, op: Op, b: &Ty<'s>) -> Option<Ty<'s>> {
        self.find::<Entity<'s>, _>(&get_binop_fun_name(a, op, b))
            .option()
            .map(move |e| e.ty.return_ty())
    }

    pub fn binop_ty(&self, a: &Ty<'s>, op: Op, b: &Ty<'s>) -> Option<Ty<'s>> {
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

    pub fn last_space<T: Item<'s>>(&self) -> &Space<'s, T> {
        T::space(self.scopes.last().unwrap())
    }

    pub fn push<T: Item<'s>>(&mut self, item: T) -> &T {
        T::space_mut(self.scopes.last_mut().unwrap()).push(item)
    }

    pub fn find<'a, T: Item<'s>, P: PathLike<'s>>(&'a self, path: &P) -> FindItem<&'a T> {
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

    pub fn add_return_type(&mut self) {
    }
    
    pub fn push_scope(&mut self, level: ScopeLevel) {
        self.scopes.push(Scope::new(level));
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn emit_msg(&self, msg: &Message<'s>) {
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

pub trait TypeCheck<'s> {
    fn typecheck(&self, checker: &mut TypeChecker<'s>) -> Ty<'s>;
}

fn get_unop_fun_name<'s>(a: &Ty<'s>, op: Op) -> FullPath {
    FullPath::new([format!("@unop`{a}{op}`")])
}

fn get_binop_fun_name<'s>(a: &Ty<'s>, op: Op, b: &Ty<'s>) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
