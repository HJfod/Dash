
pub mod decl;
pub mod token;
pub mod ty;
pub mod expr;
pub mod ops;
pub mod atom;
pub mod flow;

#[macro_export]
macro_rules! try_resolve {
    ($t: expr, $checker: expr, $a: pat => $($r: ident),+) => {
        if let $a = &mut $t {
            $($r.try_resolve($checker)?);+
        }
        else {
            Ty::Invalid
        }
    };
}
