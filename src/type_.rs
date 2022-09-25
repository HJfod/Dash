use std::fmt::{Display};
use crate::{
    comptime::{Value, FileComptime, EntityQuery, Overloads},
    error::{CompileError, ErrorCode, ToLineResult},
    output::{FmtVec, ErrorToCode}, ast::Node
};
use colored::Colorize;

#[derive(Clone, PartialEq, Debug)]
pub enum MaybeType {
    Type(Type),
    Unresolved(String),
    None,
}

impl MaybeType {
    pub fn resolve(&self, comptime: &mut FileComptime) -> Result<Type, CompileError> {
        match self {
            MaybeType::Unresolved(name) => {
                if let Some(t) = comptime.get_entity(name, &EntityQuery::Type) {
                    Ok(t.get_type())
                } else {
                    Err(CompileError {
                        file: comptime.get_file().to_owned(),
                        info: format!("Unknown type \"{}\"", name),
                        note: None,
                        hint: None
                    })
                }
            },
            MaybeType::Type(t) => Ok(t.to_owned()),
            MaybeType::None => Err(CompileError {
                file: comptime.get_file().to_owned(),
                info: "Expression does not result in a type".into(),
                note: None,
                hint: None
            })
        }
    }

    pub fn full_resolve(
        &self,
        comptime: &mut FileComptime,
        node: &Node
    ) -> Result<Type, ErrorCode> {
        self.resolve(comptime)
            .to_line_as_type(node.from(), node.to())
            .print(node.get_channel())
    }

    pub fn resolve_fun(
        &self,
        comptime: &mut FileComptime,
        params: &Vec<Type>
    ) -> Result<FunType, CompileError> {
        match self {
            MaybeType::Unresolved(name) => {
                let mut tried: Vec<FunType> = vec!();
                for overload in comptime.get_all_overloads(name) {
                    match overload {
                        Overloads::Fun(funs) => {
                            for fun in funs {
                                if fun.type_.same_params(params) {
                                    return Ok(fun.type_.to_owned());
                                }
                                tried.push(fun.type_.to_owned());
                            }
                        },
                        _ => continue
                    }
                }
                Err(CompileError {
                    file: comptime.get_file().to_owned(),
                    info: format!(
                        "No matching overload ({}) found",
                        FmtVec::new(params, ", ")
                    ),
                    note: Some(format!(
                        "Possible overloads:\n{}", 
                        FmtVec::new(&tried, "\n")
                    )),
                    hint: None
                })
            },
            MaybeType::Type(t) => {
                match t {
                    Type::Fun(ft) => Ok(ft.to_owned()),
                    _ => Err(CompileError {
                        file: comptime.get_file().to_owned(),
                        info: "Type is not a function".into(),
                        note: None,
                        hint: None
                    })
                }
            },
            MaybeType::None => Err(CompileError {
                file: comptime.get_file().to_owned(),
                info: "Expression does not result in a type".into(),
                note: None,
                hint: None
            })
        }
    }

    pub fn full_resolve_fun(
        &self,
        comptime: &mut FileComptime,
        node: &Node,
        params: &Vec<Type>
    ) -> Result<FunType, ErrorCode> {
        self.resolve_fun(comptime, params)
            .to_line_as_type(node.from(), node.to())
            .print(node.get_channel())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    BuiltIn(BuiltInType),
    Fun(FunType),
}

impl Display for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::BuiltIn(b) => fmt.write_fmt(format_args!("{}", match b {
                BuiltInType::Void => "void",
                BuiltInType::Char => "char",
                BuiltInType::String => "string",
                BuiltInType::Bool => "bool",
                BuiltInType::I8  => "i8",
                BuiltInType::I16 => "i16",
                BuiltInType::I32 => "i32",
                BuiltInType::I64 => "i64",
                BuiltInType::U8  => "u8",
                BuiltInType::U16 => "u16",
                BuiltInType::U32 => "u32",
                BuiltInType::U64 => "u64",
                BuiltInType::F32 => "f32",
                BuiltInType::F64 => "f64",
            }.yellow())),
            Type::Fun(f) => f.fmt(fmt),
        }
    }
}

impl Type {
    pub fn to_maybe(&self) -> MaybeType {
        MaybeType::Type(self.to_owned())
    }

    pub fn is_void(&self) -> bool {
        match self {
            Type::BuiltIn(b) => {
                return *b == BuiltInType::Void;
            },
            _ => return false
        }
    }

    pub fn make_default(&self) -> Value {
        match self {
            Type::BuiltIn(b) => {
                match *b {
                    BuiltInType::Void   => Value::Void,
                    BuiltInType::Bool   => Value::Bool(true),
                    BuiltInType::Char   => Value::Char('\0'),
                    BuiltInType::String => Value::String(String::new()),
                    BuiltInType::I8     => Value::I8(0),
                    BuiltInType::I16    => Value::I16(0),
                    BuiltInType::I32    => Value::I32(0),
                    BuiltInType::I64    => Value::I64(0),
                    BuiltInType::U8     => Value::U8(0),
                    BuiltInType::U16    => Value::U16(0),
                    BuiltInType::U32    => Value::U32(0),
                    BuiltInType::U64    => Value::U64(0),
                    BuiltInType::F32    => Value::F32(0.0),
                    BuiltInType::F64    => Value::F64(0.0),
                }
            },
            _ => todo!()
        }
    }

    pub fn conv_to(&self, into: &Type) -> bool {
        match self {
            Type::BuiltIn(b1) => {
                if let Type::BuiltIn(b2) = into {
                    b1 == b2
                } else {
                    false
                }
            },

            Type::Fun(f1) => {
                if let Type::Fun(f2) = into {
                    f1.same_overload(f2) && f1.ret_type.conv_to(&*f2.ret_type)
                } else {
                    false
                }
            },
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum BuiltInType {
    Void,
    Bool,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Char,
    String,
}

impl BuiltInType {
    pub fn to_maybe(&self) -> MaybeType {
        MaybeType::Type(Type::BuiltIn(self.to_owned()))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunType {
    pub ret_type: Box<Type>,
    pub params: Vec<Type>,
}

impl FunType {
    pub fn new(ret_type: &Type, params: &Vec<Type>) -> FunType {
        FunType {
            ret_type: Box::from(ret_type.to_owned()),
            params: params.to_owned()
        }
    }

    pub fn same_params(&self, params: &Vec<Type>) -> bool {
        if self.params.len() != params.len() {
            false
        } else {
            for (p1, p2) in self.params.iter().zip(params.iter()) {
                if !p1.conv_to(p2) {
                    return false;
                }
            }
            true
        }
    }

    pub fn same_overload(&self, other: &FunType) -> bool {
        self.same_params(&other.params)
    }
}

impl Display for FunType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.write_fmt(format_args!(
            "{} ({}) -> {}",
            "fun".purple(),
            FmtVec::new(&self.params, ", "), self.ret_type
        ))
    }
}

