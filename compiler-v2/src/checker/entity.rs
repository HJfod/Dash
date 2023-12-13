
use std::ptr::NonNull;
use super::{ast::Node, ty::Ty};

pub(crate) struct Entity {
    ty: Ty,
    decl: NonNull<Node>,
}

impl Entity {
    pub(crate) fn new(ty: Ty, decl: NonNull<Node>) -> Self {
        Self { ty, decl }
    }
    pub(crate) fn decl(&self) -> &Node {
        unsafe { self.decl.as_ref() }
    }
}
