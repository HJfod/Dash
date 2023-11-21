
use crate::parser::{node::ASTRef, ast::token::Op};
use super::ty::{FullPath, Ty};

pub struct Entity<'s> {
    name: FullPath,
    decl: ASTRef<'s>,
    ty: Ty<'s>,
    mutable: bool,
}

impl<'s> Entity<'s> {
    pub fn new(name: FullPath, decl: ASTRef<'s>, ty: Ty<'s>, mutable: bool) -> Self {
        Self { name, decl, ty, mutable }
    }

    pub fn new_builtin_binop(a: Ty<'s>, op: Op, b: Ty<'s>, ret: Ty<'s>) -> Self {
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

    pub fn decl(&self) -> ASTRef<'s> {
        self.decl
    }

    pub fn ty(&self) -> Ty<'s> {
        self.ty.clone()
    }

    pub fn can_access_outside_function(&self) -> bool {
        // only const entities defined outside the function scope can be accessed 
        !self.mutable
    }
}

pub fn get_unop_fun_name(a: &Ty<'_, '_>, op: &Op) -> FullPath {
    FullPath::new([format!("@unop`{a}{op}`")])
}

pub fn get_binop_fun_name<'s>(a: &Ty<'s>, op: &Op, b: &Ty<'s>) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
