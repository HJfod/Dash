
use std::fmt::Display;
use crate::shared::wrappers::RefWrapper;

use super::ast::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty<'n, 'g> {
    /// Represents that an error occurred during typechecking, or the 
    /// checked statement results in no type
    Invalid,
    /// This expression's containing branch will never finish execution
    Never,
    /// Unit type, can only have the value `void`
    Void,
    /// A boolean type, either true or false
    Bool,
    /// 64-bit integer
    Int,
    /// 64-bit float
    Float,
    /// UTF-8 string type
    String,
    /// Function type
    Function {
        params: Vec<(String, Ty<'n, 'g>)>,
        ret_ty: Box<Ty<'n, 'g>>,
        decl: RefWrapper<'n, Node<'n, 'g>>,
    },
    /// Alias for another type
    Alias {
        name: String,
        ty: Box<Ty<'n, 'g>>,
        decl: RefWrapper<'n, Node<'n, 'g>>,
    },
}

impl<'n, 'g> Ty<'n, 'g> {
    /// Whether this type is one that can't exist as a value (`invalid` or `never`)
    pub fn unreal(&self) -> bool {
        matches!(self, Ty::Invalid | Ty::Never)
    }

    /// Reduce type into its canonical representation, for example remove aliases
    pub fn reduce(&self) -> &Ty<'n, 'g> {
        match self {
            Self::Alias { name: _, ty, decl: _ } => ty,
            other => other,
        }
    }

    /// Test whether this type is implicitly convertible to another type or 
    /// not
    /// 
    /// In most cases this means equality
    pub fn convertible(&self, other: &Ty<'n, 'g>) -> bool {
        self.unreal() || other.unreal() || *self.reduce() == *other.reduce()
    }

    pub fn decl(&self) -> Option<&'n Node<'n, 'g>> {
        match self {
            Ty::Invalid => None,
            Ty::Never => None,
            Ty::Void => None,
            Ty::Bool => None,
            Ty::Int => None,
            Ty::Float => None,
            Ty::String => None,
            Ty::Function { params: _, ret_ty: _, decl } => Some(decl.to_inner()),
            Ty::Alias { name: _, ty: _, decl } => Some(decl.to_inner()),
        }
    }
}

impl Display for Ty<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => f.write_str("invalid"),
            Self::Never => f.write_str("never"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Function { params, ret_ty, decl: _ } => f.write_fmt(format_args!(
                "fun({}) -> {ret_ty}", params.iter()
                    .map(|(p, t)| format!("{p}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Self::Alias { name, ty: _, decl: _ } => f.write_str(name),
        }
    }
}
