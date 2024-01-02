
use crate::shared::src::ArcSpan;

use super::ty::Ty;

#[derive(Debug)]
pub(crate) struct Entity {
    /// The type of the entity
    ty: Ty,
    decl_span: ArcSpan,
    /// Whether this entity only exists after declaration, i.e. variables
    ephemeral: bool,
}

impl Entity {
    pub fn new(ty: Ty, decl_span: ArcSpan, ephemeral: bool) -> Self {
        Self { ty, decl_span, ephemeral }
    }
    pub fn span(&self) -> ArcSpan {
        self.decl_span.clone()
    }
    pub fn ty(&self) -> Ty {
        self.ty.clone()
    }
    pub fn ephemeral(&self) -> bool {
        self.ephemeral
    }
}
