
use derive_new::new;
use std::fmt::{Display, Debug};
use strum::EnumIter;
use geo_macros::snake_case_ident;
use crate::{
    parser::{
        node::{Parse, ASTNode},
        stream::{TokenStream, Token}
    },
    shared::{logging::{Message, Level}, src::Span}, compiler::typecheck
};

pub trait Tokenize<'s>: ASTNode<'s> + TryFrom<Token<'s>, Error = Message<'s>> {
    fn name() -> &'static str;
    fn peek<I: Iterator<Item = Token<'s>>>(stream: &TokenStream<'s, I>) -> Option<Self> {
        Self::try_from(stream.peek()).ok()
    }
    fn peek_and_parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Option<Self> {
        Self::peek(stream).and_then(|_| Self::parse(stream).ok())
    }
}

impl<'s, T: Tokenize<'s>> Parse<'s> for T {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
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
        impl<'s> TryFrom<Token<'s>> for $item<'s> {
            type Error = Message<'s>;
            fn try_from(token: Token<'s>) -> Result<Self, Self::Error> {
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

        impl<'s> Tokenize<'s> for $item<'s> {
            fn name() -> &'static str {
                $name
            }
        }

        impl<'s> ASTNode<'s> for $item<'s> {
            fn span(&self) -> &Span<'s> {
                &self.span
            }
        }
    };

    ($item: ident -> $name: expr) => {
        impl<'s> TryFrom<Token<'s>> for $item<'s> {
            type Error = Message<'s>;
            fn try_from(token: Token<'s>) -> Result<Self, Self::Error> {
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

        impl<'s> Tokenize<'s> for $item<'s> {
            fn name() -> &'static str {
                $name
            }
        }

        impl<'s> ASTNode<'s> for $item<'s> {
            fn span(&self) -> &Span<'s> {
                &self.span
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
            pub struct $item<'s> {
                span: Span<'s>,
            }

            impl_tokenize!($class::$item -> declare_token!(#get_display_name $($as_str)? $class_as_str $item));

            impl<'s> Display for $item<'s> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str(Self::name())
                }
            }
        )*)*

        $($($(
            impl<'s> $var<'s> {
                pub fn $trait_fn(&self) -> $trait_fn_ty {
                    $value
                }
            }

            impl<'s> std::convert::From<$var<'s>> for $class<'s> {
                fn from(value: $var<'s>) -> Self {
                    Self::$var(value)
                }
            }
        )+)*)*

        #[derive(Clone, Debug)]
        pub enum $class<'s> {
            $($($item($item<'s>),)*)*
        }

        impl<'s> $class<'s> {
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

            pub fn try_new(value: &str, span: Span<'s>) -> Option<$class<'s>> {
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

        impl<'s> TryFrom<Token<'s>> for $class<'s> {
            type Error = Message<'s>;
            fn try_from(token: Token<'s>) -> Result<Self, Self::Error> {
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

        impl<'s> Tokenize<'s> for $class<'s> {
            fn name() -> &'static str {
                $class_as_str
            }
        }

        impl<'s> ASTNode<'s> for $class<'s> {
            fn span(&self) -> &Span<'s> {
                match self {
                    $(
                        $(
                            Self::$item(t) => t.span(),
                        )*
                    )*
                }
            }
        }

        impl<'s> Display for $class<'s> {
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
        pub struct $item<'s> {
            $($field: $field_ty,)*
            span: Span<'s>,
        }

        impl<'s> $item<'s> {
            $($fun)*
        }

        declare_token!(#impl_display $item $($display)?);

        impl_tokenize!($item -> $item_as_str);
    };

    (#impl_display $item:ident $display:item) => {
        impl<'s> Display for $item<'s> {
            $display
        }
    };

    (#impl_display $item:ident) => {
        impl<'s> Display for $item<'s> {
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
        Void, True, False,
        Var, Let, Fun, Decl, Struct, Const,
        Is, As, From,
        If, Else, For, While,
        Return, Break,
        Extern, Export, Import,
    }
    is_contextual {}
    is_reserved {
        Yield, Match, Switch, Mut,
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
    pub fn peek_op_of_this_prec<'s, I>(&self, stream: &TokenStream<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
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

    display {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.value)
        }
    }

    ---

    pub fn is_keyword(&self) -> bool {
        Kw::try_new(self.value.as_str(), self.span.clone()).is_some()
    }

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new([self.value.clone()], false)
    }
}

declare_token! {
    [struct Parenthesized "parenthesized expression"]
    +content: Vec<Token<'s>>;
    ---
    pub fn into_stream(self) -> TokenStream<'s, std::vec::IntoIter<Token<'s>>> {
        TokenStream::new(self.span.src(), self.content.into_iter())
    }
}

declare_token! {
    [struct Bracketed "bracketed expression"]
    +content: Vec<Token<'s>>;
    ---
    pub fn into_stream(self) -> TokenStream<'s, std::vec::IntoIter<Token<'s>>> {
        TokenStream::new(self.span.src(), self.content.into_iter())
    }
}

declare_token! {
    [struct Braced "braced expression"]
    +content: Vec<Token<'s>>;
    ---
    pub fn into_stream(self) -> TokenStream<'s, std::vec::IntoIter<Token<'s>>> {
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
