use std::{collections::HashMap, fmt::Display};

use crate::src::{Logger, Message};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct FullPath {
    path: Vec<String>,
}

impl FullPath {
    pub fn new<T: Into<Vec<String>>>(path: T) -> Self {
        Self { path: path.into() }
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

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Never,
    Unknown,
    Void,
    Int,
    Float,
    String,
}

impl Ty {
    pub fn convertible_to(&self, other: &Ty) -> bool {
        *self == *other
    }

    pub fn full_name(&self) -> FullPath {
        match self {
            builtin => FullPath::new([format!("{builtin}")])
        }
    }

    /// Returns `other` if this type is `never` or `unknown`
    pub fn or(self, other: Ty) -> Ty {
        if self == Ty::Never || self == Ty::Unknown {
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
            Self::Never => f.write_str("never"),
            Self::Unknown => f.write_str("unknown"),
            Self::Void => f.write_str("void"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
        }
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

    pub fn ty(&self) -> Ty {
        self.ty.clone()
    }
}

pub struct TypeChecker<'s, 'l> {
    logger: &'l dyn Logger<'s>,
    types: HashMap<FullPath, Ty>,
    entities: HashMap<FullPath, Entity>,
}

impl<'s, 'l> TypeChecker<'s, 'l> {
    pub fn push_entity(&mut self, entity: Entity) -> &Entity {
        let name = entity.name.clone();
        self.entities.insert(entity.name.clone(), entity);
        self.entities.get(&name).unwrap()
    }

    pub fn find_entity(&self, name: Path) -> Option<&Entity> {
        todo!()
    }

    pub fn push_type(&mut self, ty: Ty) {
        self.types.insert(ty.full_name(), ty);
    }

    pub fn find_type(&self, name: Path) -> Option<&Ty> {
        todo!()
    }

    pub fn emit_msg(&self, msg: &Message<'s>) {
        self.logger.log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: &'l dyn Logger<'s>) {
        self.logger = logger;
    }
}

pub trait TypeCheck<'s, 'l> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty;
}

impl<'s, 'l> TypeCheck<'s, 'l> for String {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        Ty::String
    }
}

impl<'s, 'l> TypeCheck<'s, 'l> for i64 {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        Ty::Int
    }
}

impl<'s, 'l> TypeCheck<'s, 'l> for f64 {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
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
            Ty::Unknown
        }
    }
}

impl<'s, 'l, T: TypeCheck<'s, 'l>> TypeCheck<'s, 'l> for Vec<T> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty {
        self.iter().for_each(|v| drop(v.typecheck(checker)));
        Ty::Unknown
    }
}
