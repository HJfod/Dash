
use crate::shared::src::Span;
use super::{ast::ArcSpan, ty::Ty};

pub(crate) struct Entity {
    ty: Ty,
    decl_span: ArcSpan,
}

impl Entity {
    pub(crate) fn new(ty: Ty, decl_span: ArcSpan) -> Self {
        Self { ty, decl_span }
    }
    pub(crate) fn span(&self) -> Span {
        self.decl_span.as_ref()
    }
}
