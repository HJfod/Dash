
use crate::parser::{node::ASTRef, ast::token::Op};
use super::ty::{FullPath, Ty};

pub struct Entity {
    name: FullPath,
    decl: ASTRef,
    ty: Ty,
    mutable: bool,
}

impl Entity {
    pub fn new(name: FullPath, decl: ASTRef, ty: Ty, mutable: bool) -> Self {
        Self { name, decl, ty, mutable }
    }

    pub fn new_builtin_binop(a: Ty, op: Op, b: Ty, ret: Ty) -> Self {
        Self {
            name: get_binop_fun_name(&a, &op, &b),
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

    pub fn name(&self) -> &FullPath {
        &self.name
    }

    pub fn decl(&self) -> ASTRef {
        self.decl
    }

    pub fn ty(&self) -> Ty {
        self.ty.clone()
    }

    pub fn can_access_outside_function(&self) -> bool {
        // only const entities defined outside the function scope can be accessed 
        !self.mutable
    }
}

pub fn get_unop_fun_name(a: &Ty, op: &Op) -> FullPath {
    FullPath::new([format!("@unop`{a}{op}`")])
}

pub fn get_binop_fun_name(a: &Ty, op: &Op, b: &Ty) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
