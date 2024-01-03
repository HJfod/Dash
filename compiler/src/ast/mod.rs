
pub mod decl;
pub mod token;
pub mod ty;
pub mod expr;
pub mod ops;
pub mod atom;
pub mod flow;

#[macro_export]
macro_rules! try_resolve {
    ($t: expr, $list: expr, $checker: expr, $a: pat => $($r: ident),+ else None) => {
        if let $a = $t {
            Some($($r.try_resolve($list, $checker)?);+)
        }
        else {
            None
        }
    };
    ($t: expr, $list: expr, $checker: expr, $a: pat => $($r: ident),+ else $else: expr) => {
        if let $a = $t {
            $($r.try_resolve($list, $checker)?);+
        }
        else {
            $else
        }
    };
    ($t: expr, $list: expr, $checker: expr, $a: pat => $($r: ident),+) => {
        try_resolve!($t, $list, $checker, $a => $($r),+ else Ty::Invalid)
    };
}
