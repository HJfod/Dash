
use std::fmt::Display;
use crate::ice;
use crate::shared::src::ArcSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// The type of a variable whose real type has not yet been inferred
    Undecided(String, ArcSpan),
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
        params: Vec<(Option<String>, Ty)>,
        ret_ty: Box<Ty>,
    },
    /// Optional type
    Option {
        ty: Box<Ty>,
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

    pub fn is_undecided(&self) -> bool {
        matches!(self, Ty::Undecided(_, _))
    }

    /// Whether this type is one that can't exist as a value (`invalid` or `never`)
    pub fn is_unreal(&self) -> bool {
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
        self.is_unreal() || other.is_unreal() || *self.reduce() == *other.reduce()
    }

    pub fn span(&self) -> ArcSpan {
        match self {
            Ty::Undecided(_, span) => span.clone(),
            Ty::Invalid => ArcSpan::builtin(),
            Ty::Never => ArcSpan::builtin(),
            Ty::Void => ArcSpan::builtin(),
            Ty::Bool => ArcSpan::builtin(),
            Ty::Int => ArcSpan::builtin(),
            Ty::Float => ArcSpan::builtin(),
            Ty::String => ArcSpan::builtin(),
            Ty::Function { params: _, ret_ty: _ } => ArcSpan::builtin(),
            Ty::Option { ty: _ } => ArcSpan::builtin(),
            Ty::Alias { name: _, ty: _, decl_span } |
            Ty::Named { name: _, ty: _, decl_span } => decl_span.clone(),
        }
    }

    /// Returns this if this type is not unreal, or the other if it is
    pub fn or(self, other: Ty) -> Ty {
        if self.is_unreal() { other } else { self }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undecided(name, _) => write!(f, "unknown ({name})"),
            Self::Invalid => f.write_str("unknown"),
            Self::Never => f.write_str("never"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Function { params, ret_ty } => write!(
                f,
                "fun({}) -> {ret_ty}", params.iter()
                    .map(|(p, t)| if let Some(p) = p {
                        format!("{p}: {t}")
                    }
                    else {
                        t.to_string()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Option { ty } => write!(f, "{ty}?"),
            Self::Alias { name, ty: _, decl_span: _ } => write!(f, "{name}"),
            Self::Named { name, ty: _, decl_span: _ } => write!(f, "{name}"),
        }
    }
}
