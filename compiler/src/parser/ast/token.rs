
use derive_new::new;
use std::fmt::{Display, Debug};
use strum::EnumIter;
use dash_macros::snake_case_ident;
use crate::{
    parser::{
        node::{Parse, ASTNode, ASTRef},
        stream::{TokenStream, Token}
    },
    shared::{logging::{Message, Level}, src::{Span, Spanful}}, compiler::ty::{Ty, self}
};

pub trait Tokenize: ASTNode + TryFrom<Token, Error = Message> {
    fn name() -> &'static str;
    fn peek<I: Iterator<Item = Token>>(stream: &TokenStream<I>) -> Option<Self> {
        Self::try_from(stream.peek()).ok()
    }
    fn peek_and_parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Option<Self> {
        Self::peek(stream).and_then(|_| Self::parse(stream).ok())
    }
}

impl<T: Tokenize> Parse for T {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::try_from(stream.next())
    }
}

pub fn is_op_char(ch: char) -> bool {
    "=+-/%&|^*~@!?<>#".contains(ch)
}

pub fn closing_paren(ch: char) -> char {
    match ch {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _   => unreachable!(),
    }
}

macro_rules! impl_tokenize {
    ($class: ident::$item: ident -> $name: expr) => {
        impl TryFrom<Token> for $item {
            type Error = Message;
            fn try_from(token: Token) -> Result<Self, Self::Error> {
                match token {
                    Token::$class($class::$item(v)) => Ok(v),
                    t => Err(Message::from_span(
                        Level::Error, 
                        format!("Expected {}, got {t}", Self::name()),
                        t.span()
                    )),
                }
            }
        }

        impl Tokenize for $item {
            fn name() -> &'static str {
                $name
            }
        }

        impl ASTNode for $item {
            fn span(&self) -> &Span {
                &self.span
            }
            fn children(&mut self) -> Vec<ASTRef> {
                std::iter::empty()
            }
            fn eval_ty(&self) -> Ty {
                Ty::Unresolved
            }
        }
    };

    ($item: ident -> $name: expr) => {
        impl TryFrom<Token> for $item {
            type Error = Message;
            fn try_from(token: Token) -> Result<Self, Self::Error> {
                match token {
                    Token::$item(v) => Ok(v),
                    t => Err(Message::from_span(
                        Level::Error, 
                        format!("Expected {}, got {t}", Self::name()),
                        t.span()
                    )),
                }
            }
        }

        impl Tokenize for $item {
            fn name() -> &'static str {
                $name
            }
        }

        impl ASTNode for $item {
            fn span(&self) -> &Span {
                &self.span
            }
            fn children(&mut self) -> Vec<ASTRef> {
                std::iter::empty()
            }
            fn eval_ty(&self) -> Ty {
                Ty::Unresolved
            }
        }
    };
}

macro_rules! declare_token {
    (
        [enum $class:ident $class_as_str:literal]
        $(
            $kind:ident {
                $(
                    $item:ident $(= $as_str:literal)?,
                )*
            }
        )*
        $(
            @trait $trait_fn:ident -> $trait_fn_ty:ty {
                $(
                    $($var:ident)|+ => $value:expr,
                )*
            }
        )*
    ) => {
        $($(
            #[derive(new, Clone, Debug)]
            pub struct $item {
                span: Span,
            }

            impl_tokenize!($class::$item -> declare_token!(#get_display_name $($as_str)? $class_as_str $item));

            impl Display for $item {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str(Self::name())
                }
            }
        )*)*

        $($($(
            impl $var {
                pub fn $trait_fn(&self) -> $trait_fn_ty {
                    $value
                }
            }

            impl std::convert::From<$var> for $class {
                fn from(value: $var) -> Self {
                    Self::$var(value)
                }
            }
        )+)*)*

        #[derive(Clone, Debug)]
        pub enum $class {
            $($($item($item),)*)*
        }

        impl $class {
            $(
                pub fn $kind(&self) -> bool {
                    match self {
                        $(Self::$item(_) => true,)*
                        _ => false
                    }
                }
            )*

            $(
                pub fn $trait_fn(&self) -> $trait_fn_ty {
                    match self {
                        $($(Self::$var(t) => t.$trait_fn(),)+)*
                    }
                }
            )*

            pub fn try_new(value: &str, span: Span) -> Option<$class> {
                match value {
                    $(
                        $(
                            declare_token!(#get_bare_name $($as_str)? $class_as_str $item)
                            => Some($class::$item($item::new(span))),
                        )*
                    )*
                    _ => None,
                }
            }
        }

        impl TryFrom<Token> for $class {
            type Error = Message;
            fn try_from(token: Token) -> Result<Self, Self::Error> {
                match token {
                    Token::$class(v) => Ok(v),
                    t => Err(Message::from_span(
                        Level::Error, 
                        format!("Expected {}, got {t}", Self::name()),
                        t.span()
                    )),
                }
            }
        }

        impl Tokenize for $class {
            fn name() -> &'static str {
                $class_as_str
            }
        }

        impl ASTNode for $class {
            fn span(&self) -> &Span {
                match self {
                    $($(Self::$item(t) => t.span(),)*)*
                }
            }
            fn children(&mut self) -> Vec<ASTRef> {
                match self {
                    $($(Self::$item(t) => t.children(),)*)*
                }
            }
            fn eval_ty(&self) -> Ty {
                match self {
                    $($(Self::$item(t) => t.eval_ty(),)*)*
                }
            }
        }

        impl Display for $class {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        $(
                            Self::$item(t) => std::fmt::Display::fmt(t, f),
                        )*
                    )*
                }
            }
        }
    };

    (
        [struct $item:ident $item_as_str:literal]
        $(
            +$field:ident: $field_ty:ty;
        )*
        $(
            display {
                $display:item
            }
        )?
        ---
        $(
            $fun:item
        )*
    ) => {
        #[derive(new, Clone, Debug)]
        pub struct $item {
            $($field: $field_ty,)*
            span: Span,
        }

        impl $item {
            $($fun)*
        }

        declare_token!(#impl_display $item $($display)?);

        impl_tokenize!($item -> $item_as_str);
    };

    (#impl_display $item:ident $display:item) => {
        impl Display for $item {
            $display
        }
    };

    (#impl_display $item:ident) => {
        impl Display for $item {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(Self::name())
            }
        }
    };

    (#get_display_name $as_str:literal $class_as_str:literal $_1:ident) => {
        concat!($class_as_str, " '", $as_str, "'")
    };

    (#get_display_name $class_as_str:literal $name:ident) => {
        concat!($class_as_str, " '", snake_case_ident!($name), "'")
    };

    (#get_bare_name $as_str:literal $_0:literal $_1:ident) => {
        $as_str
    };

    (#get_bare_name $class_as_str:literal $name:ident) => {
        snake_case_ident!($name)
    };
}

declare_token! {
    [enum Kw "keyword"]
    is_strict {
        // Literals
        Void, True, False, None,
        // Constants & special variables
        This, Super,
        // Declarations
        Var, Let, Fun, Struct, Enum, Using,
        Macro, Extends, Module, Type,
        // Prepositions
        In, Is, As, Where, From,
        // Loops & conditional
        If, Else, For, While,
        // Control flow
        Unwrap, Try, Return, Break, Continue,
        // Visibility
        Extern, Public, Private,
        // Types
        Typeof, Const,
        // Other
        Codegen, CompilerIntrinsic,
    }
    is_contextual {
        Get, Set, Assert, Default,
    }
    is_reserved {
        // Declarations
        Trait, Class, Interface,
        // Control flow
        Yield, Match, Switch,
        // Visibility
        Export, Import,
        // Reactivity
        Depends, Required,
        // Other
        Mut, New, Null,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter)]
pub enum Prec {
    Unary,
    Mul,
    Add,
    Ord,
    Eq,
    And,
    Or,
    Seq,
}

impl Prec {
    pub fn peek_op_of_this_prec<I>(&self, stream: &TokenStream<I>) -> bool
        where I: Iterator<Item = Token>
    {
        if let Some(op) = Op::peek(stream) {
            op.prec() == *self
        }
        else {
            false
        }
    }
}

declare_token! {
    [enum Op "operator"]
    is_unary {
        Not = "!",
    }
    is_binary {
        Mul    = "*",
        Div    = "/",
        Mod    = "%",
        Add    = "+",
        Sub    = "-",
        Lss    = "<" ,
        Gtr    = ">" ,
        Leq    = "<=",
        Geq    = ">=",
        Eq     = "==",
        Neq    = "!=",
        And    = "&&",
        Or     = "||",
        Seq    = "=",
        AddSeq = "+=",
        SubSeq = "-=",
        MulSeq = "*=",
        DivSeq = "/=",
        ModSeq = "%=",
    }
    @trait prec -> Prec {
        Not => Prec::Unary,
        Mul | Div | Mod => Prec::Mul,
        Add | Sub => Prec::Add,
        Lss | Gtr | Leq | Geq => Prec::Ord,
        Eq | Neq => Prec::Eq,
        And => Prec::And,
        Or => Prec::Or,
        Seq | AddSeq | SubSeq | MulSeq | DivSeq | ModSeq => Prec::Seq,
    }
}

declare_token! {
    [enum Punct "punctuation"]
    is_chained {
        Dot     = ".",
        Didot   = "..",
        Tridot  = "...",
        Colon   = ":",
        Dicolon = "::",
    }
    is_single {
        Comma     = ",",
        Semicolon = ";",
    }
    is_symbol {
        Arrow    = "->",
        FatArrow = "=>",
    }
}

declare_token! {
    [struct Ident "identifier"]
    +value: String;
    +is_decorator: bool;

    display {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("{}{}",
                if self.is_decorator { "@" } else { "" },
                &self.value
            ))
        }
    }

    ---

    pub fn is_keyword(&self) -> bool {
        Kw::try_new(self.value.as_str(), self.span.clone()).is_some()
    }

    pub fn is_decorator(&self) -> bool {
        self.is_decorator
    }

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn path(&self) -> ty::IdentPath {
        ty::IdentPath::new([self.to_string()], false)
    }
}

declare_token! {
    [struct Parenthesized "parenthesized expression"]
    +content: Vec<Token>;
    ---
    pub fn into_stream(self) -> TokenStream<std::vec::IntoIter<Token>> {
        TokenStream::new(self.span.src(), self.content.into_iter())
    }
}

declare_token! {
    [struct Bracketed "bracketed expression"]
    +content: Vec<Token>;
    ---
    pub fn into_stream(self) -> TokenStream<std::vec::IntoIter<Token>> {
        TokenStream::new(self.span.src(), self.content.into_iter())
    }
}

declare_token! {
    [struct Braced "braced expression"]
    +content: Vec<Token>;
    ---
    pub fn into_stream(self) -> TokenStream<std::vec::IntoIter<Token>> {
        TokenStream::new(self.span.src(), self.content.into_iter())
    }
}

declare_token! {
    [struct VoidLit "void"]
    ---
}

declare_token! {
    [struct BoolLit "boolean"]
    +value: bool;
    ---
}

declare_token! {
    [struct IntLit "integer"]
    +value: i64;
    ---
}

declare_token! {
    [struct FloatLit "float"]
    +value: f64;
    ---
}

declare_token! {
    [struct StringLit "string"]
    +value: String;
    ---
}
