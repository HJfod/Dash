
use super::ty::Ty;
use crate::shared::src::ArcSpan;

pub(crate) struct Entity {
    ty: Ty,
    decl_span: ArcSpan,
}

impl Entity {
    pub(crate) fn new(ty: Ty, decl_span: ArcSpan) -> Self {
        Self { ty, decl_span }
    }
    pub(crate) fn span(&self) -> ArcSpan {
        self.decl_span.clone()
    }
    pub(crate) fn ty(&self) -> Ty {
        self.ty.clone()
    }
}
