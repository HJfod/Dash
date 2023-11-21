
use std::fmt::Display;
use crate::parser::node::ASTRef;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct FullPath {
    path: Vec<String>,
}

impl FullPath {
    pub fn new<T: Into<Vec<String>>>(path: T) -> Self {
        Self { path: path.into() }
    }

    pub fn ends_with(&self, path: &IdentPath) -> bool {
        self.to_string().ends_with(&path.to_string())
    }
}

impl Display for FullPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("::{}", self.path.join("::")))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IdentPath {
    absolute: bool,
    path: Vec<String>,
}

impl IdentPath {
    pub fn new<T: Into<Vec<String>>>(path: T, absolute: bool) -> Self {
        Self { path: path.into(), absolute }
    }

    pub fn into_full(self) -> FullPath {
        FullPath::new(self.path)
    }
}

impl Display for IdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}{}", 
            if self.absolute { "::" } else { "" },
            self.path.join("::")
        ))
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Ty {
    /// Represents that an error occurred during typechecking. Only produced on 
    /// errors - should **never** appear on a succesful compilation!
    /// Always convertible to all other types in all contexts to avoid further 
    /// typechecking errors
    Invalid,
    /// Represents a type whose value is not known yet. Compilation should never 
    /// finish until all unknowns have been resolved
    #[default]
    Unresolved,
    /// Represents that the branch which produced this value will never 
    /// finish execution (for example because it returned to an outer scope)
    /// Always convertible to all other types
    /// No value of type `never` can ever exist
    Never,
    /// An unit type, can only have the value `void`
    Void,
    /// Boolean type
    Bool,
    /// 64-bit integer type
    Int,
    /// 64-bit floating point type
    Float,
    /// UTF-8 string type
    String,
    /// Function type (used for named functions that can't capture mutable 
    /// variables from the outer scope)
    Function {
        params: Vec<(String, Ty)>,
        ret_ty: Box<Ty>,
        decl: ASTRef,
    },
    /// Alias for another type
    Alias {
        name: String,
        ty: Box<Ty>,
        decl: ASTRef,
    },
}

impl Ty {
    /// Whether this type is one that can't exist as a value (`unknown`, `invalid`, or `never`)
    pub fn is_unreal(&self) -> bool {
        matches!(self, Ty::Unresolved | Ty::Invalid | Ty::Never)
    }

    /// Whether this type is  `never` 
    pub fn is_never(&self) -> bool {
        matches!(self, Ty::Never)
    }

    /// Reduce type into its canonical representation, for example remove aliases
    pub fn reduce(&self) -> &Ty {
        match self {
            Self::Alias { name: _, ty, decl: _ } => ty,
            other => other,
        }
    }

    /// Test whether this type is implicitly convertible to another type or 
    /// not
    /// 
    /// In most cases this means equality
    pub fn convertible_to(&self, other: &Ty) -> bool {
        self.is_unreal() || other.is_unreal() || *self.reduce() == *other.reduce()
    }

    /// Get the return type of this type
    /// 
    /// Returns the return type if the type is a function type, or itself 
    /// if it's something else
    pub fn return_ty(&self) -> Ty {
        match self {
            Self::Function { params: _, ret_ty, decl: _ } => *ret_ty.clone(),
            _ => self.clone(),
        }
    }

    /// Get the corresponding AST declaration that produced this type
    pub fn decl(&self) -> ASTRef {
        match self {
            Ty::Invalid => ASTRef::Builtin,
            Ty::Unresolved => ASTRef::Builtin,
            Ty::Never => ASTRef::Builtin,
            Ty::Void => ASTRef::Builtin,
            Ty::Bool => ASTRef::Builtin,
            Ty::Int => ASTRef::Builtin,
            Ty::Float => ASTRef::Builtin,
            Ty::String => ASTRef::Builtin,
            Ty::Function { params: _, ret_ty: _, decl } => *decl,
            Ty::Alias { name: _, ty: _, decl } => *decl,
        }
    }

    /// Returns `other` if this type is `invalid` or `unknown`
    pub fn or(self, other: Ty) -> Ty {
        if self.is_unreal() {
            other
        }
        else {
            self
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => f.write_str("invalid"),
            Self::Unresolved => f.write_str("unknown"),
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
