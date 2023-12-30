
use crate::{shared::logger::Message, parser::parse::{DontExpect, CompileMessage, Parse}};
use super::{ty::Ty, coherency::Checker};

pub trait Resolve {
    /// Try to resolve this AST node
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty>;
}

impl<T: Resolve> Resolve for Box<T> {
    fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
        self.as_mut().try_resolve(checker)
    }
}

impl<T: Parse, M: CompileMessage> Resolve for DontExpect<T, M> {
    fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

pub trait Unresolved {
    /// If resolving everything wasn't succesful and this node caused it, 
    /// return a Message describing why it wasn't resolved
    fn unresolved_due_to(&self) -> Message;
}
