
use std::{ptr::{null_mut, NonNull}};
use peg::str::LineCol;

use crate::{
    comptime::{FileComptime, Value, VarEntity, FunEntity},
    error::{LineError, ErrorCode, Severity},
    utils::File,
    type_::{FunType}, output::{FmtVec}
};
use crate::output::Output;
use crate::type_::{MaybeType, Type, BuiltInType};

macro_rules! throw_type_err {
    ($node: expr, $msg: literal $(, $fmts:expr)*) => {
        return Err($node.channel.print(&LineError {
            file: $node.file.to_owned(),
            code: ErrorCode::TypeError,
            info: format!($msg, $($fmts),*),
            hint: None,
            note: None,
            severity: Severity::Error,
            from: $node.file.linecol_at($node.start),
            to: $node.file.linecol_at($node.end),
        }))
    };
}

#[derive(Clone, PartialEq, Debug)]
pub struct Node {
    parent: *mut Node,
    file: Box<File>,
    start: usize,
    end: usize,
    kind: NodeKind,
    channel: Output,
}

pub type NodePtr<T> = Box<T>;
pub type PNode = NodePtr<Node>;
pub type OPNode = Option<NodePtr<Node>>;

impl Node {
    pub fn new_raw(
        file: &Box<File>,
        start: usize,
        end: usize,
        kind: NodeKind,
        channel: &Output
    ) -> Node {
        Node {
            parent: null_mut(),
            start, end, kind,
            file: file.to_owned(),
            channel: channel.to_owned(),
        }
    }

    pub fn new(
        file: &Box<File>,
        start: usize,
        end: usize,
        kind: NodeKind,
        channel: &Output,
    ) -> PNode {
        NodePtr::from(Node::new_raw(file, start, end, kind, channel))
    }

    pub fn get_parent(&self) -> Option<NonNull<Node>> {
        NonNull::new(self.parent)
    }

    pub fn get_file(&self) -> &Box<File> {
        &self.file
    }

    pub fn from(&self) -> LineCol {
        self.file.linecol_at(self.start)
    }

    pub fn to(&self) -> LineCol {
        self.file.linecol_at(self.end)
    }

    pub fn get_channel(&self) -> &Output {
        &self.channel
    }

    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }

    #[allow(unused_variables)]
    pub fn eval(&self, comptime: &mut FileComptime) -> Result<Value, ErrorCode> {
        match &self.kind {
            NodeKind::List(list) => todo!(),
            NodeKind::DeclVar(name, type_, value) => todo!(),
            NodeKind::DeclFun(name, params, ret_type, body) => todo!(),
            NodeKind::Return(value) => todo!(),

            NodeKind::Iden(name) => todo!(),
            NodeKind::Binary(op, lhs, rhs) => todo!(),
            NodeKind::Unary(op, expr) => todo!(),
            NodeKind::Call(target, args) => todo!(),

            NodeKind::String(val) => todo!(),
            NodeKind::Int(val) => todo!(),
            NodeKind::Float(val) => todo!(),
        }
    }

    fn walk_sub_t(&self, value: &PNode, comptime: &mut FileComptime) -> Result<Type, ErrorCode> {
        value.walk(comptime)?.full_resolve(comptime, self)
    }

    fn walk_opt(&self, value: &OPNode, comptime: &mut FileComptime) -> Result<Option<MaybeType>, ErrorCode> {
        if let Some(v) = value {
            return Ok(Some(v.walk(comptime)?));
        }
        Ok(None)
    }

    fn walk_opt_t(&self, value: &OPNode, comptime: &mut FileComptime) -> Result<Option<Type>, ErrorCode> {
        if let Some(n) = self.walk_opt(value, comptime)? {
            Ok(Some(n.full_resolve(comptime, self)?))
        } else {
            Ok(None)
        }
    }

    fn walk_vec(&self, value: &Vec<PNode>, comptime: &mut FileComptime) -> Result<Vec<MaybeType>, ErrorCode> {
        let mut res: Vec<MaybeType> = vec!();
        for node in value {
            res.push(node.walk(comptime)?);
        }
        Ok(res)
    }

    fn walk_vec_t(&self, value: &Vec<PNode>, comptime: &mut FileComptime) -> Result<Vec<Type>, ErrorCode> {
        self.walk_vec(value, comptime)?.iter()
            .map(|n| n.full_resolve(comptime, self))
            .collect()
    }

    pub fn walk(self: &Box<Node>, comptime: &mut FileComptime) -> Result<MaybeType, ErrorCode> {
        match &self.kind {
            NodeKind::List(list) => {
                self.walk_vec(list, comptime)?;
                Ok(MaybeType::None)
            },

            NodeKind::DeclVar(name, type_, value) => {
                let tt = self.walk_opt_t(type_, comptime)?;
                let vt = self.walk_opt_t(value, comptime)?;

                let true_type = match tt {
                    Some(t) => t,
                    None => match vt {
                        Some(t) => t,
                        None => throw_type_err!(
                            &self,
                            "No type provided to variable \
                            or value to infer type from"
                        )
                    }
                };

                // todo: assign value
                comptime.new_entity(
                    name, &VarEntity::new(
                        self.to_owned(),
                        true_type.clone(),
                        true_type.make_default()
                    )
                ).map_err(|e| self.channel.print(
                    &e.to_line_err_in(self.start, self.end)
                ))?;

                Ok(true_type.to_maybe())
            },

            NodeKind::DeclFun(name, params, ret_type, body) => {
                comptime.push_scope();

                let pt = self.walk_vec_t(params, comptime)?;
                let rt = self.walk_opt_t(ret_type, comptime)?;
                self.walk_opt(body, comptime)?;

                let real_ret_type = match rt {
                    Some(t) => t,
                    None => todo!("infer type")
                };

                comptime.pop_scope();

                comptime.new_entity(
                    name, &FunEntity::new(
                        self.to_owned(),
                        FunType::new(&real_ret_type, &pt),
                        body.to_owned()
                    )
                ).map_err(|e| self.channel.print(
                    &e.to_line_err_in(self.start, self.end)
                ))?;

                Ok(MaybeType::None)
            },

            NodeKind::Return(value) => {
                self.walk_opt(value, comptime)?;
                Ok(MaybeType::None)
            },

            NodeKind::Iden(name) => {
                if let Some(s) = comptime.get_any_entity(name) {
                    Ok(MaybeType::Unresolved(s.name))
                } else {
                    throw_type_err!(
                        &self,
                        "Unknown identifier \"{}\"", name
                    )
                }
            },

            NodeKind::Binary(op, lhs, rhs) => {
                let t = lhs.walk(comptime)?;
                rhs.walk(comptime)?;
                Ok(t)
            },
            
            NodeKind::Unary(op, expr) => {
                expr.walk(comptime)
            },

            NodeKind::Call(target, args) => {
                let args = self.walk_vec_t(args, comptime)?;
                Ok(target.walk(comptime)?
                    .full_resolve_fun(comptime, self, &args)?
                    .ret_type.to_maybe()
                )
            },

            NodeKind::String(_) => Ok(BuiltInType::String.to_maybe()),
            NodeKind::Int(_)    => Ok(BuiltInType::I32.to_maybe()),
            NodeKind::Float(_)  => Ok(BuiltInType::F32.to_maybe()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum NodeKind {
    /// List
    List(Vec<PNode>),
    /// Name, Type, Value
    DeclVar(String, OPNode, OPNode),
    /// Name, Params, Return, Body
    DeclFun(String, Vec<PNode>, OPNode, OPNode),
    /// Value
    Return(OPNode),

    /// Name
    Iden(String),
    /// Op, LHS, RHS
    Binary(BinOp, PNode, PNode),
    /// Op, Expr
    Unary(UnaryOp, PNode),
    /// Expr, Args
    Call(PNode, Vec<PNode>),

    /// Value
    String(String),
    /// Value
    Int(i64),
    /// Value
    Float(f64),
}

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,

    And,
    Or,
}
