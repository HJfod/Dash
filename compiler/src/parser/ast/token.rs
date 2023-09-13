
use std::{fmt::{Display, Debug}, marker::PhantomData};
use strum::{EnumString, Display, EnumIter};
use gdml_macros::snake_case_ident;
use crate::{
    parser::{
        node::{Parse, Span, ASTNode},
        stream::{TokenStream, Token}
    },
    shared::logging::{Message, Level}, compiler::typecheck
};

pub trait Tokenize<'s>: ASTNode<'s> {
    fn name() -> &'static str;
}

impl<'s, T: Tokenize<'s>> Display for T {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(Self::name())
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

macro_rules! declare_token {
    (
        [$class:ident $class_as_str:literal]
        $(
            $kind:ident {
                $(
                    $item:ident $(= $as_str:literal)?
                    $(($($trait:ident $(
                        ($($arg:ident: $arg_type:ty),*)
                    )? -> $type:ty = $value:expr),+))?,
                )*
            }
        )*
    ) => {
        $($(
            #[derive(Debug)]
            pub struct $item<'s> {
                span: Span<'s>,
            }

            impl<'s> $item<'s> {
                $(
                    $(
                        fn $trait($($($arg: $arg_type),*)?) -> $type {
                            $value
                        }
                    )+
                )?
            }

            impl<'s> Tokenize<'s> for $item<'s> {
                fn name() -> &'static str {
                    declare_token!(#get_name $($as_str)? $class_as_str $item) 
                }
            }

            impl<'s> ASTNode<'s> for $item<'s> {
                fn span(&self) -> &Span<'s> {
                    &self.span
                }
            }

            impl<'s> Parse<'s> for $item<'s> {
                fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
                    match stream.next() {
                        Some(Token::$class($class::$item(kw))) => Ok(kw),
                        Some(t) => Err(Message::from_span(
                            Level::Error, 
                            format!("Expected {}, got {t}", Self::name()),
                            t.span()
                        )),
                        None => Err(Message::from_eof(
                            Level::Error,
                            format!("Expected {}, got EOF", Self::name()),
                            stream.src()
                        ))
                    }
                }
            }
        )*)*

        #[derive(Debug)]
        pub enum $class<'s> {
            $($($item($item<'s>),)*)*
        }

        impl<'s> $class<'s> {
            $(
                pub fn $kind(&self) -> bool {
                    match self {
                        $(Self::$item(t) => true,)*
                        _ => false
                    }
                }
            )*
        }
    };

    (#get_name $as_str:literal $_0:literal $_1:ident) => {
        $as_str
    };

    (#get_name $class_as_str:literal $name:ident) => {
        concat!($class_as_str, " '", snake_case_ident!($name), "'")
    };
}

declare_token! {
    [Kw "keyword"]
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

declare_token! {
    [Op "operator"]
    is_unary {
        Not = "!" (prec -> Prec = Prec::Unary),
    }
    is_binary {
        Mul = "*" (prec -> Prec = Prec::Mul),
        Div = "/" (prec -> Prec = Prec::Mul),
        Mod = "%" (prec -> Prec = Prec::Mul),

        Add = "+" (prec -> Prec = Prec::Add),
        Sub = "-" (prec -> Prec = Prec::Add),

        Lss = "<"  (prec -> Prec = Prec::Ord),
        Gtr = ">"  (prec -> Prec = Prec::Ord),
        Leq = "<=" (prec -> Prec = Prec::Ord),
        Geq = ">=" (prec -> Prec = Prec::Ord),

        Eq  = "==" (prec -> Prec = Prec::Eq),
        Neq = "!=" (prec -> Prec = Prec::Eq),

        And = "&&" (prec -> Prec = Prec::And),

        Or  = "||" (prec -> Prec = Prec::Or),

        Seq    = "="  (prec -> Prec = Prec::Seq),
        AddSeq = "+=" (prec -> Prec = Prec::Seq),
        SubSeq = "-=" (prec -> Prec = Prec::Seq),
        MulSeq = "*=" (prec -> Prec = Prec::Seq),
        DivSeq = "/=" (prec -> Prec = Prec::Seq),
        ModSeq = "%=" (prec -> Prec = Prec::Seq),
    }
}

declare_token! {
    [Punct "punctuation"]
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

#[derive(PartialEq, Debug)]
pub struct Ident<'s>(String, Span<'s>);

impl<'s> Ident<'s> {
    pub fn new(value: String, span: Span<'s>) -> Self {
        Self(value, span)
    }

    pub fn is_keyword(&self) -> bool {
        Kw::try_from(self.0.as_str()).is_ok()
    }

    pub fn value(&self) -> &String {
        &self.0
    }

    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new([self.0.clone()], false)
    }
}

impl Display for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'s> ASTNode<'s> for Ident<'s> {
    fn span(&self) -> &Span<'s> {
        &self.1
    }
}

#[derive(Debug)]
pub struct Path<'s> {
    components: Vec<Ident<'s>>,
    absolute: bool,
    span: Span<'s>,
}

impl<'s> Path<'s> {
    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new(
            self.components.iter().map(|i| i.value().clone()).collect::<Vec<_>>(),
            self.absolute
        )
    }
}

impl<'s> Parse<'s> for Path<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let absolute = "::".parse_value(stream).is_ok();
        let mut components = vec![];
        loop {
            components.push(stream.parse()?);
            if "::".parse_value(stream).is_err() {
                break;
            }
        }
        Ok(Self { components, absolute, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for Path<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

#[derive(Debug)]
pub struct Lit<'s, T: Debug> {
    value: T,
    span: Span<'s>,
}

impl<'s, T: Debug> Lit<'s, T> {
    pub fn new(value: T, span: Span<'s>) -> Self {
        Self { value, span }
    }
}

impl<'s, T: Debug> ASTNode<'s> for Lit<'s, T> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

pub type VoidLit<'s> = Lit<'s, ()>;
pub type BoolLit<'s> = Lit<'s, bool>;
pub type IntLit<'s> = Lit<'s, i64>;
pub type FloatLit<'s> = Lit<'s, f64>;
pub type StringLit<'s> = Lit<'s, String>;

pub trait SurroundFromToken<'s> {
    type Result;
    fn surround_from_token(tk: Token<'s>) -> Option<Self::Result>;
    fn kind_name() -> &'static str;
}

/// A list of tokens surrounded by punctuation, such as parentheses or braces
#[derive(Debug)]
pub struct Surrounded<'s, T: SurroundFromToken<'s>> {
    content: Vec<Token<'s>>,
    span: Span<'s>,
    _phantom: PhantomData<T>,
}

impl<'s, T: SurroundFromToken<'s>> Surrounded<'s, T> {
    pub fn new(content: Vec<Token<'s>>, span: Span<'s>) -> Self {
        Self { content, span, _phantom: PhantomData }
    }

    pub fn into_stream(self) -> Vec<Token<'s>> {
        self.content
    }
}

impl<'s, T: SurroundFromToken<'s>> Parse<'s> for Surrounded<'s, T> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        match stream.next() {
            Some(tk) => {
                if let Some(p) = T::surround_from_token(tk) {
                    Ok(p)
                }
                else {
                    Err(Message::from_span(
                        Level::Error,
                        format!("Expected {} expression, got {tk}", T::kind_name()),
                        tk.span()
                    ))
                }
            }
            None => Err(stream.error(format!("Expected {} expression, got EOF", T::kind_name()), start))
        }
    }
}

impl<'s, T: SurroundFromToken<'s>> ASTNode<'s> for Surrounded<'s, T> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

struct Paren;
impl<'s> SurroundFromToken<'s> for Paren {
    type Result = Parenthesized<'s>;
    fn surround_from_token(tk: Token<'s>) -> Option<Self::Result> {
        match tk {
            Token::Parenthesized(p) => Some(p),
            _ => None,
        }
    }
    fn kind_name() -> &'static str {
        "parenthesized"
    }
}
pub type Parenthesized<'s> = Surrounded<'s, Paren>;

struct Bracket;
impl<'s> SurroundFromToken<'s> for Bracket {
    type Result = Bracketed<'s>;
    fn surround_from_token(tk: Token<'s>) -> Option<Self::Result> {
        match tk {
            Token::Bracketed(p) => Some(p),
            _ => None,
        }
    }
    fn kind_name() -> &'static str {
        "bracketed"
    }
}
pub type Bracketed<'s> = Surrounded<'s, Bracket>;

struct Brace;
impl<'s> SurroundFromToken<'s> for Brace {
    type Result = Braced<'s>;
    fn surround_from_token(tk: Token<'s>) -> Option<Self::Result> {
        match tk {
            Token::Braced(p) => Some(p),
            _ => None,
        }
    }
    fn kind_name() -> &'static str {
        "braced"
    }
}
pub type Braced<'s> = Surrounded<'s, Brace>;
