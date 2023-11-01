
pub struct CoherencyVisitor<'s, 'n> {
    logger: LoggerRef<'s>,
    // todo: found items, unresolved types, unresolved macros
}

pub trait VisitCoherency<'s, 'n> {
    #[must_use]
    fn visit_coherency(&'n mut self, )
}
