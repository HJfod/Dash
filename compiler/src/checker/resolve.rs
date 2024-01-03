
use crate::parser::parse::{DontExpect, CompileMessage, Parse, Node, NodeList};
use super::{ty::Ty, coherency::Checker};

// #[derive(Debug)]
// pub struct ResolveCache {
//     id: NodeID,
//     resolved: Option<Ty>,
//     unresolved_msg: Option<(String, ArcSpan)>,
// }

// impl Default for ResolveCache {
//     fn default() -> Self {
//         Self {
//             id: NodeID::new(),
//             resolved: None,
//             unresolved_msg: None,
//         }
//     }
// }

// impl ResolveCache {
//     pub fn set_unresolved<S: ToString>(&mut self, msg: S, span: Option<ArcSpan>) {
//         self.unresolved_msg = Some((msg.to_string(), span.unwrap_or(ArcSpan::builtin())))
//     }
// }

pub(crate) trait Resolve: Node {
    /// Try to resolve this AST node. This function should never be called 
    /// by any other code - rather, other code should call `try_resolve`
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty>;

    // Attempts to figure out the evaluation type for this AST node
    // Todo: better docs
    // fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
    //     if let Some(cache) = self.cache() {
    //         if let Some(cached) = cache.resolved.clone() {
    //             Some(cached)
    //         }
    //         else {
    //             let resolved = self.try_resolve(list, checker);
    //             let cache = self.cache().unwrap();
    //             cache.resolved = resolved;
    //             if cache.resolved.is_some() {
    //                 checker.mark_resolved(cache.id);
    //                 cache.resolved.clone()
    //             }
    //             else {
    //                 if let Some((msg, span)) = std::mem::take(&mut cache.unresolved_msg) {
    //                     checker.mark_unresolved(cache.id, msg, span);
    //                 }
    //                 None
    //             }
    //         }
    //     }
    //     else {
    //         Some(self.try_resolve(list, checker).ice("try_resolve shouldn't error without a cache"))
    //     }
    // }
}

impl<T: Resolve> Resolve for Box<T> {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        self.as_mut().try_resolve(list, checker)
    }
}

impl<T: Parse, M: CompileMessage> Resolve for DontExpect<T, M> {
    fn try_resolve(&mut self, _: &mut NodeList, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}
