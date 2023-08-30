use std::{collections::HashMap, fmt::Display};

use crate::{
    parser::Rule,
    src::{Level, Logger, Message},
};

#[derive(Hash, PartialEq, Eq)]
pub struct FullIdent {
    absolute: bool,
    path: Vec<String>,
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

    pub fn full_name(&self) -> FullIdent {

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
    name: FullIdent,
    kind: EntityKind,
    ty: Ty,
}

impl Entity {
    pub fn new_var(name: FullIdent, ty: Ty) -> Entity {
        Entity { name, kind: EntityKind::Variable {}, ty }
    }
}

pub struct TypeChecker<'s, 'l> {
    logger: &'l dyn Logger<'s>,
    types: HashMap<FullIdent, Ty>,
    entities: HashMap<FullIdent, Entity>,
}

impl<'s, 'l> TypeChecker<'s, 'l> {
    pub fn push_entity(&mut self, entity: Entity) {
        self.entities.insert(entity.name, entity);
    }

    pub fn push_type(&mut self, ty: Ty) {
        self.types.insert(ty.full_name(), ty);
    }

    pub fn emit_msg(&self, msg: &Message<'s>) {
        self.logger.log_msg(msg);
    }

    pub fn set_logger(&mut self, logger: &'l dyn Logger<'s>) {
        self.logger = logger;
    }
}

pub trait TypeCheck<'s, 'l>: Rule<'s> {
    fn typecheck(&self, checker: &mut TypeChecker<'s, 'l>) -> Ty;

    fn expect_ty(&self, checker: &mut TypeChecker<'s, 'l>, ty: Ty) -> Ty {
        let self_ty = self.typecheck(checker);
        if !ty.convertible_to(&self_ty) {
            checker.emit_msg(&Message::from_meta(
                Level::Error,
                format!("Expected type '{ty}', got '{self_ty}'"),
                self.meta(),
            ));
        }
        ty
    }

    fn expect_ty_convertible_to<O: TypeCheck<'s, 'l>>(
        &self,
        checker: &mut TypeChecker<'s, 'l>,
        into: &O,
    ) -> Ty {
        let self_ty = self.typecheck(checker);
        let into_ty = into.typecheck(checker);
        if !self_ty.convertible_to(&into_ty) {
            checker.emit_msg(&Message::from_meta(
                Level::Error,
                format!("Type '{self_ty}' is not convertible to '{into_ty}'"),
                self.meta(),
            ));
        }
        into_ty
    }
}
