
pub mod decl;
pub mod token;
pub mod ty;
pub mod expr;
pub mod ops;
pub mod atom;
pub mod flow;

#[macro_export]
macro_rules! try_resolve_ref {
    ($t: expr, ($pool: expr, $checker: expr), $a: pat => $($r: ident),+ else None) => {
        if let $a = $t {
            Some($($r.try_resolve_ref($pool, $checker)?);+)
        }
        else {
            None
        }
    };
    ($t: expr, ($pool: expr, $checker: expr), $a: pat => $($r: ident),+ else $else: expr) => {
        if let $a = $t {
            $($r.try_resolve_ref($pool, $checker)?);+
        }
        else {
            $else
        }
    };
    ($t: expr, ($pool: expr, $checker: expr), $a: pat => $($r: ident),+) => {
        try_resolve_ref!($t, ($pool, $checker), $a => $($r),+ else Ty::Invalid)
    };
}

#[macro_export]
macro_rules! try_resolve_list {
    ($t: expr, ($pool: expr, $checker: expr), $a: pat => $i: ident => $r: expr) => {
        {
            let mut res = vec![];
            let mut some_unresolved = false;
            for $a in $t {
                match $i.try_resolve_ref($pool, $checker) {
                    Some($i) => if !some_unresolved {
                        res.push($r)
                    }
                    None => {
                        some_unresolved = true;
                    }
                }
            }
            if some_unresolved {
                return None;
            }
            res
        }
    };
}
