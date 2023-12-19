
use std::fmt::Display;
pub mod ast;
pub mod ty;
pub(crate) mod entity;
pub(crate) mod tests;
pub(crate) mod path;
pub(crate) mod coherency;

pub(crate) trait Ice: Sized {
    type R;
    fn ice(self, msg: &str) -> Self::R;
    fn ice_due_to<D: Display>(self, msg: &str, cause: D) -> Self::R {
        self.ice(&format!("{msg}; cause: {cause}"))
    }
}

impl<T> Ice for Option<T> {
    type R = T;
    fn ice(self, msg: &str) -> Self::R {
        self.unwrap_or_else(|| panic!("Internal compiler error: {msg}"))
    }
}

#[macro_export]
macro_rules! ice {
    ($msg:literal $($rest:tt)*) => {
        panic!("Internal compiler error: {}", format!($msg, $($rest)*))
    };
}
