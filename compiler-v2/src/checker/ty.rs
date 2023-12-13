
use std::fmt::Display;
use crate::{shared::src::Span, ice};
use super::ast::ArcSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
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
        params: Vec<(String, Ty)>,
        ret_ty: Box<Ty>,
    },
    /// Alias for another type. Can be implicitly converted to the other type
    Alias {
        name: String,
        ty: Box<Ty>,
        decl_span: ArcSpan,
    },
    /// A "new type" alias for another type; in other words, can *not* be 
    /// implicitly converted to the other type
    Named {
        name: String,
        ty: Box<Ty>,
        decl_span: ArcSpan,
    },
}

impl Ty {
    pub fn new_builtin(name: &str) -> Self {
        match name {
            "never" => Self::Never,
            "void" => Self::Void,
            "bool" => Self::Bool,
            "int" => Self::Int,
            "float" => Self::Float,
            "string" => Self::String,
            _ => ice!("invalid builtin type '{name}'")
        }
    }

    /// Whether this type is one that can't exist as a value (`invalid` or `never`)
    pub fn unreal(&self) -> bool {
        matches!(self, Ty::Invalid | Ty::Never)
    }

    /// Reduce type into its canonical representation, for example remove aliases
    pub fn reduce(&self) -> &Ty {
        match self {
            Self::Alias { name: _, ty, decl_span: _ } => ty,
            other => other,
        }
    }

    /// Test whether this type is implicitly convertible to another type or 
    /// not
    /// 
    /// In most cases this means equality
    pub fn convertible(&self, other: &Ty) -> bool {
        self.unreal() || other.unreal() || *self.reduce() == *other.reduce()
    }

    pub fn span(&self) -> Span {
        match self {
            Ty::Invalid => Span::builtin(),
            Ty::Never => Span::builtin(),
            Ty::Void => Span::builtin(),
            Ty::Bool => Span::builtin(),
            Ty::Int => Span::builtin(),
            Ty::Float => Span::builtin(),
            Ty::String => Span::builtin(),
            Ty::Function { params: _, ret_ty: _ } => Span::builtin(),
            Ty::Alias { name: _, ty: _, decl_span } |
            Ty::Named { name: _, ty: _, decl_span } => decl_span.as_ref(),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => f.write_str("invalid"),
            Self::Never => f.write_str("never"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Function { params, ret_ty } => f.write_fmt(format_args!(
                "fun({}) -> {ret_ty}", params.iter()
                    .map(|(p, t)| format!("{p}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Self::Alias { name, ty: _, decl_span: _ } => f.write_str(name),
            Self::Named { name, ty: _, decl_span: _ } => f.write_str(name),
        }
    }
}
