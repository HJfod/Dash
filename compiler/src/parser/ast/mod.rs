
#[macro_export]
macro_rules! if_then_some {
    ($cond: expr => $body: expr) => {
        if $cond {
            Some($body)
        }
        else {
            None
        }
    };
}

pub mod token;
pub mod expr;
pub mod ty;
pub mod decls;
pub mod unop;
pub mod binop;
pub mod flow;
