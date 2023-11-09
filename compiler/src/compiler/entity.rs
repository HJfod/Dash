
use crate::parser::{node::ASTRef, ast::token::Op};
use super::ty::{FullPath, Ty};

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

    pub fn decl(&self) -> ASTRef<'s, 'n> {
        self.decl
    }

    pub fn ty(&self) -> Ty<'s, 'n> {
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

pub fn get_binop_fun_name<'s, 'n>(a: &Ty<'s, 'n>, op: &Op, b: &Ty<'s, 'n>) -> FullPath {
    FullPath::new([format!("@binop`{a}{op}{b}`")])
}
