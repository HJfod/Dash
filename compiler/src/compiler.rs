use std::{collections::HashMap, fmt::Display};

use crate::{src::{Logger, Message, ConsoleLogger}, rules::ast::Op};

pub trait PathLike {
    fn resolve<T: Item>(&self, space: &Space<T>) -> FullPath;
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

impl PathLike for FullPath {
    fn resolve<T: Item>(&self, _: &Space<T>) -> FullPath {
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

impl PathLike for Path {
    fn resolve<T: Item>(&self, space: &Space<T>) -> FullPath {
        space.resolve(self)
    }
}

pub trait Item: Sized {
    fn full_path(&self) -> FullPath;
    fn space<'s>(scope: &'s Scope) -> &'s Space<Self>;
    fn space_mut<'s>(scope: &'s mut Scope) -> &'s mut Space<Self>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Invalid,
    Inferred,
    Void,
    Bool,
    Int,
    Float,
    String,
    Function {
        params: Vec<(String, Ty)>,
        ret_ty: Box<Ty>,
    },
}

impl Ty {
    pub fn is_unreal(&self) -> bool {
        matches!(self, Ty::Inferred | Ty::Invalid)
    }

    pub fn convertible_to(&self, other: &Ty) -> bool {
        self.is_unreal() || other.is_unreal() || *self == *other
    }

    pub fn eval_ty(&self) -> Ty {
        match self {
            Self::Function { params: _, ret_ty } => *ret_ty.clone(),
            _ => self.clone(),
        }
    }

    /// Returns `other` if this type is `invalid` or `unknown`
    pub fn or(self, other: Ty) -> Ty {
        if self.is_unreal() {
            other
        }
        else {
            self
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => f.write_str("invalid"),
            Self::Inferred => f.write_str("unknown"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Function { params, ret_ty } => f.write_fmt(format_args!(
                "fun({}) -> {ret_ty}", params.iter()
                    .map(|(p, t)| format!("{p}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }
}

impl Item for Ty {
    fn full_path(&self) -> FullPath {
        match self {
            builtin => FullPath::new([format!("{builtin}")])
        }
    }

    fn space<'s>(scope: &'s Scope) -> &'s Space<Self> {
        &scope.types
    }
    fn space_mut<'s>(scope: &'s mut Scope) -> &'s mut Space<Self> {
        &mut scope.types
    }
}

pub enum EntityKind {
    Variable {},
    Function {},
}

pub struct Entity {
    name: FullPath,
    kind: EntityKind,
    ty: Ty,
}

impl Entity {
    pub fn new_var(name: FullPath, ty: Ty) -> Entity {
        Entity { name, kind: EntityKind::Variable {}, ty }
    }

    pub fn new_binop(a: Ty, op: Op, b: Ty, ret: Ty) -> Entity {
        Entity {
            name: get_binop_fun_name(&a, op, &b),
            kind: EntityKind::Function {},
            ty: Ty::Function {
                params: vec![
                    ("a".into(), a),
                    ("b".into(), b),
                ],
                ret_ty: ret.into()
            }
        }
    }

    pub fn ty(&self) -> Ty {
        self.ty.clone()
    }
}

impl Item for Entity {
    fn full_path(&self) -> FullPath {
        self.name.clone()
    }

    fn space<'s>(scope: &'s Scope) -> &'s Space<Self> {
        &scope.entities
    }
    fn space_mut<'s>(scope: &'s mut Scope) -> &'s mut Space<Self> {
        &mut scope.entities
    }
}

pub struct Space<T: Item> {
    entities: HashMap<FullPath, T>,
}

impl<T: Item> Space<T> {
    pub fn new() -> Self {
        Self {
            entities: Default::default()
        }
    }

    pub fn push(&mut self, entity: T) -> &T {
        let name = entity.full_path().clone();
        self.entities.insert(entity.full_path().clone(), entity);
        self.entities.get(&name).unwrap()
    }

    pub fn find(&self, name: FullPath) -> Option<&T> {
        self.entities.get(&name)
    }

    pub fn resolve(&self, path: &Path) -> FullPath {
        self.entities.iter()
            .find(|(full, _)| full.ends_with(path))
            .map(|p| p.0.clone())
            .unwrap_or(path.clone().into_full())
    }
}

pub struct Scope {
    types: Space<Ty>,
    entities: Space<Entity>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            types: Space::new(),
            entities: Space::new(),
        }
    }

    pub fn new_top() -> Self {
        let mut res = Self::new();
        res.types.push(Ty::Void);
        res.types.push(Ty::Bool);
        res.types.push(Ty::Int);
        res.types.push(Ty::Float);
        res.types.push(Ty::String);
        res.entities.push(Entity::new_binop(Ty::Int, Op::Add, Ty::Int, Ty::Int));
        res
    }
}

pub struct TypeChecker<'s, 'l> {
    logger: &'l dyn Logger<'s>,
    scopes: Vec<Scope>,
}

impl<'s, 'l> TypeChecker<'s, 'l> {
    pub fn push<T: Item>(&mut self, item: T) -> &T {
        T::space_mut(self.scopes.last_mut().unwrap()).push(item)
    }

    pub fn find<T: Item, P: PathLike>(&self, path: &P) -> Option<&T> {
        self.scopes.iter().rev().find_map(|p| {
            let space = T::space(&p);
            space.find(path.resolve(space))
        })
    }

    pub fn resolve_new(&self, path: Path) -> FullPath {
        // todo: namespaces
        path.into_full()
    }
    
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn emit_msg(&self, msg: &Message<'s>) {
        self.logger.log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: &'l dyn Logger<'s>) {
        self.logger = logger;
    }

    pub fn new() -> Self {
        Self {
            logger: &ConsoleLogger,
            scopes: vec![Scope::new_top()]
        }
    }
}

pub trait TypeCheck<'s, 'l> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty;
}

impl<'s, 'l> TypeCheck<'s, 'l> for String {
    fn typecheck(&self, _: &mut TypeChecker<'s, 'l>) -> Ty {
        Ty::String
    }
}

impl<'s, 'l> TypeCheck<'s, 'l> for i64 {
    fn typecheck(&self, _: &mut TypeChecker<'s, 'l>) -> Ty {
        Ty::Int
    }
}

impl<'s, 'l> TypeCheck<'s, 'l> for f64 {
    fn typecheck(&self, _: &mut TypeChecker<'s, 'l>) -> Ty {
        Ty::Float
    }
}

impl<'s, 'l, T: TypeCheck<'s, 'l>> TypeCheck<'s, 'l> for Box<T> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        self.as_ref().typecheck(checker)
    }
}

impl<'s, 'l, T: TypeCheck<'s, 'l>> TypeCheck<'s, 'l> for Option<T> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        if let Some(ref value) = self {
            value.typecheck(checker)
        }
        else {
            Ty::Invalid
        }
    }
}

impl<'s, 'l, T: TypeCheck<'s, 'l>> TypeCheck<'s, 'l> for Vec<T> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        self.iter().for_each(|v| drop(v.typecheck(checker)));
        Ty::Invalid
    }
}

pub fn get_unop_fun_name(a: &Ty, op: Op) -> FullPath {
    FullPath::new([format!("@unop`{a}{op}`")])
}

pub fn get_binop_fun_name(a: &Ty, op: Op, b: &Ty) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
