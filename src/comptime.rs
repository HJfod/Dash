
use std::{collections::HashMap};
use derive_new::new;

use crate::{
    type_::{Type, FunType, BuiltInType},
    error::{CompileError},
    utils::File, ast::NodeKind,
    output::{Output}
};
use crate::ast::{PNode, OPNode, Node};
use colored::Colorize;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Void,
    None,
    I8(i8), I16(i16), I32(i32), I64(i64),
    U8(u8), U16(u16), U32(u32), U64(u64),
    F32(f32), F64(f64),
    String(String),
    Char(char),
    Bool(bool),
}

pub trait Entity {
    fn get_decl(&self) -> &PNode;
    fn get_type(&self) -> Type;
    fn to_query(&self) -> EntityQuery;
    fn into_overload(&self, old: Option<&Overloads>) -> Option<Overloads>;
}

#[derive(new, Clone, PartialEq, Debug)]
pub struct VarEntity {
    decl: PNode,
    type_: Type,
    value: Value,
}

impl Entity for VarEntity {
    fn get_decl(&self) -> &PNode {
        &self.decl
    }

    fn to_query(&self) -> EntityQuery {
        EntityQuery::Var
    }

    fn into_overload(&self, old: Option<&Overloads>) -> Option<Overloads> {
        if old.is_some() {
            None
        } else {
            Some(Overloads::Var(self.to_owned()))
        }
    }

    fn get_type(&self) -> Type {
        self.type_.to_owned()
    }
}

#[derive(new, Clone, PartialEq, Debug)]
pub struct FunEntity {
    pub decl: PNode,
    pub type_: FunType,
    pub body: OPNode,
}

impl Entity for FunEntity {
    fn get_decl(&self) -> &PNode {
        &self.decl
    }

    fn to_query(&self) -> EntityQuery {
        EntityQuery::Fun(self.type_.params.to_owned())
    }

    fn into_overload(&self, old: Option<&Overloads>) -> Option<Overloads> {
        if let Some(loads) = old {
            match loads {
                Overloads::Fun(funs) => {
                    let mut res_funs = funs.clone();
                    res_funs.push(self.to_owned());
                    Some(Overloads::Fun(res_funs))
                },
                _ => None
            }
        } else {
            Some(Overloads::Fun(vec!(self.to_owned())))
        }
    }

    fn get_type(&self) -> Type {
        Type::Fun(self.type_.to_owned())
    }
}

#[derive(new, Clone, PartialEq, Debug)]
pub struct TypeEntity {
    pub decl: PNode,
    pub type_: Type,
}

impl Entity for TypeEntity {
    fn get_decl(&self) -> &PNode {
        &self.decl
    }

    fn to_query(&self) -> EntityQuery {
        EntityQuery::Type
    }

    fn into_overload(&self, old: Option<&Overloads>) -> Option<Overloads> {
        match old {
            Some(_) => None,
            None => Some(Overloads::Type(self.to_owned()))
        }
    }

    fn get_type(&self) -> Type {
        self.type_.to_owned()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct UnresolvedEntity {
    pub name: String,
}

impl UnresolvedEntity {
    pub fn new(name: &String) -> UnresolvedEntity {
        UnresolvedEntity {
            name: name.to_owned()
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum EntityQuery {
    Var,
    Type,
    Fun(Vec<Type>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Overloads {
    Var(VarEntity),
    Type(TypeEntity),
    Fun(Vec<FunEntity>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    pub entities: HashMap<String, Overloads>,
}

impl Scope {
    pub fn insert_entity<T: Entity>(&mut self, name: &String, entity: &T) -> Option<()> {
        self.entities.insert(
            name.to_owned(),
            entity.into_overload(self.entities.get(name))?
        );
        Some(())
    }

    pub fn get_any_entity(&self, name: &String) -> Option<UnresolvedEntity> {
        if self.entities.contains_key(name) {
            Some(UnresolvedEntity::new(name))
        } else {
            None
        }
    }

    pub fn has_overload(&self, name: &String, query: &EntityQuery) -> Option<&dyn Entity> {
        match self.entities.get(name)? {
            Overloads::Var(v) => if matches!(query, EntityQuery::Var) {
                Some(v)
            } else {
                None
            },
            Overloads::Type(t) => if matches!(query, EntityQuery::Type) {
                Some(t)
            } else {
                None
            },
            Overloads::Fun(funs) => {
                if let EntityQuery::Fun(params) = query {
                    funs.iter().find(|f| f.type_.same_params(params)).map(|e| e as &dyn Entity)
                } else {
                    None
                }
            }
        }
    }

    pub fn get_overloads(&self, name: &String) -> Option<&Overloads> {
        self.entities.get(name)
    }

    pub fn get_overload_mut(&mut self, name: &String) -> Option<&mut Overloads> {
        self.entities.get_mut(name)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FileComptime {
    shared: *mut SharedComptime,
    file: Box<File>,
    scope: Vec<Scope>,
    channel: Output,
}

impl FileComptime {
    pub fn new(shared: *mut SharedComptime, file: &Box<File>, channel: &Output) -> FileComptime {
        FileComptime {
            shared,
            file: file.to_owned(),
            scope: vec!(Scope {
                entities: FileComptime::builtin_entities(file, channel)
            }),
            channel: channel.to_owned(),
        }
    }

    fn builtin_entities(file: &Box<File>, channel: &Output) -> HashMap<String, Overloads> {
        HashMap::from([

            ("void",    BuiltInType::Void),

            ("string",  BuiltInType::String),
            ("char",    BuiltInType::Char),

            ("bool",    BuiltInType::Bool),

            ("i8",      BuiltInType::I8),
            ("i16",     BuiltInType::I16),
            ("i32",     BuiltInType::I32),
            ("i64",     BuiltInType::I64),

            ("u8",      BuiltInType::U8),
            ("u16",     BuiltInType::U16),
            ("u32",     BuiltInType::U32),
            ("u64",     BuiltInType::U64),

            ("f32",     BuiltInType::F32),
            ("f64",     BuiltInType::U64),

        ].map(|(k, v)| (
            k.into(),
            Overloads::Type(TypeEntity::new(
                Node::new(file, 0, 0, NodeKind::Iden(k.into()), channel),
                Type::BuiltIn(v)
            ))
        )))
    }

    pub fn push_scope(&mut self) {
        self.scope.push(Scope {
            entities: HashMap::new()
        });
    }

    pub fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn insert_entity<T: Entity>(
        &mut self,
        name: &String,
        entity: &T
    ) -> Option<()> {
        self.scope.last_mut()?.insert_entity(name, entity)
    }

    pub fn new_entity<T: Entity>(
        &mut self,
        name: &String,
        entity: &T
    ) -> Result<(), CompileError> {
        if let Some(overload) = self.has_overload(name, &entity.to_query()) {
            Err(CompileError {
                file: entity.get_decl().get_file().to_owned(),
                info: format!(
                    "Variable, type or function overload \"{}\" \
                    already exists in this context", name
                ),
                note: Some(format!(
                    "Previous declaration at {}-{}:\n{}",
                    overload.get_decl().from().to_string().yellow(),
                    overload.get_decl().to().to_string().yellow(),
                    overload.get_decl().get_file().span_at(
                        &overload.get_decl().from(),
                        &overload.get_decl().to(),
                        Some(colored::Color::Black)
                    )
                )),
                hint: None,
            })
        } else {
            match self.insert_entity(name, entity) {
                Some(_) => Ok(()),
                None => Err(CompileError {
                    file: entity.get_decl().get_file().to_owned(),
                    info: "Internal compiler error: Unable to insert entity".into(),
                    note: None,
                    hint: None
                })
            }
        }
    }

    pub fn has_overload(&self, name: &String, query: &EntityQuery) -> Option<&dyn Entity> {
        self.scope.last()?.has_overload(name, query)
    }

    pub fn get_all_overloads(&self, name: &String) -> Vec<&Overloads> {
        self.scope.iter().rev()
            .filter_map(|s| s.get_overloads(name))
            .collect::<Vec<&Overloads>>()
    }

    pub fn get_any_entity(&self, name: &String) -> Option<UnresolvedEntity> {
        for scope in self.scope.iter().rev() {
            if let Some(ent) = scope.get_any_entity(name) {
                return Some(ent)
            }
        }
        None
    }

    pub fn get_entity(&self, name: &String, query: &EntityQuery) -> Option<&dyn Entity> {
        self.scope.iter().rev().find_map(|s| s.has_overload(name, query))
    }
    
    pub fn get_file(&self) -> &Box<File> {
        &self.file
    }
}

pub struct SharedComptime {
    files: Vec<FileComptime>,
    channel: Output,
}

impl SharedComptime {
    pub fn new(channel: &Output) -> SharedComptime {
        SharedComptime {
            files: vec!(),
            channel: channel.to_owned()
        }
    }

    pub fn new_file(&mut self, file: &Box<File>) -> &mut FileComptime {
        let f = FileComptime::new(self, file, &self.channel);
        self.files.push(f);
        self.files.last_mut().unwrap()
    }
}
