
use std::fmt::Display;

use dash_macros::token;

#[token(kind = "Ident", include_raw)]
pub struct Ident {}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.raw)
    }
}

pub(crate) mod kw {
    use dash_macros::token;

    #[token(kind = "Keyword", raw = "let")]
    pub struct Let {}
    #[token(kind = "Keyword", raw = "fun")]
    pub struct Fun {}
    #[token(kind = "Keyword", raw = "if")]
    pub struct If {}
    #[token(kind = "Keyword", raw = "else")]
    pub struct Else {}
    #[token(kind = "Keyword", raw = "this")]
    pub struct This {}
    #[token(kind = "Keyword", raw = "return")]
    pub struct Return {}
    #[token(kind = "Keyword", raw = "using")]
    pub struct Using {}

    #[token(kind = "Ident", raw = "get")]
    pub struct Get {}
    #[token(kind = "Ident", raw = "set")]
    pub struct Set {}
}

pub(crate) mod lit {
    use dash_macros::{token, Parse};

    use crate::checker::{resolve::Resolve, coherency::Checker, ty::Ty};

    #[token(kind = "Keyword", raw = "void")]
    pub struct Void {}

    impl Resolve for Void {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Void)
        }
    }

    #[token(kind = "Keyword", raw = "true")]
    pub struct True {}

    #[token(kind = "Keyword", raw = "false")]
    pub struct False {}

    #[derive(Debug, Parse)]
    #[parse(expected = "boolean")]
    pub enum Bool {
        True(True),
        False(False),
    }

    impl Resolve for Bool {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Bool)
        }
    }

    #[token(kind = "Int(_)")]
    pub struct Int {
        value: i64,
    }

    impl Resolve for Int {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Int)
        }
    }

    #[token(kind = "Float(_)")]
    pub struct Float {
        value: f64,
    }

    impl Resolve for Float {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Float)
        }
    }

    #[token(kind = "String(_)")]
    pub struct String {
        value: std::string::String,
    }

    impl Resolve for String {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::String)
        }
    }
}

pub(crate) mod punct {
    use std::sync::Arc;

    use dash_macros::token;

    use crate::{
        shared::{src::{ArcSpan, Src}, logger::{Message, Level}},
        parser::{
            parse::{Parse, FatalParseError, calculate_span},
            tokenizer::{TokenIterator, Token}
        }
    };

    #[token(kind = "Punct", raw = ",")]
    pub struct Comma {}

    #[token(kind = "Punct", raw = ";")]
    pub struct Semicolon {}

    #[derive(Debug)]
    pub struct TerminatingSemicolon {
        semicolons: Vec<Semicolon>,
    }

    impl TerminatingSemicolon {
        pub fn has_semicolon(&self) -> bool {
            !self.semicolons.is_empty()
        }
    }

    impl Parse for TerminatingSemicolon {
        fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
            where I: Iterator<Item = Token<'s>>
        {
            let last_was_braced = tokenizer.last_was_braced();
            let mut found = vec![];
            while let Some(s) = Semicolon::peek_and_parse(src.clone(), tokenizer)? {
                found.push(s);
            }
            // If the last token was a Braced or we're at EOF of this tree 
            // then allow omitting semicolon
            if found.is_empty() && !last_was_braced && tokenizer.peek(0).is_some() {
                tokenizer.expected("semicolon");
            }
            // Warn if there were multiple semicolons
            if !found.is_empty() && last_was_braced {
                tokenizer.logger().lock().unwrap().log(Message::new(
                    Level::Warning,
                    if found.len() > 1 {
                        "Unnecessary semicolons"
                    }
                    else {
                        "Unnecessary semicolon"
                    },
                    calculate_span(found.iter().map(|s| s.span())).unwrap().as_ref()
                ));
            }
            else if found.len() > 1 {
                tokenizer.logger().lock().unwrap().log(Message::new(
                    Level::Warning,
                    "Unnecessary semicolons",
                    calculate_span(found.iter().skip(1).map(|s| s.span())).unwrap().as_ref()
                ));
            }
            // Missing semicolon is not a fatal parsing error
            Ok(Self { semicolons: found })
        }

        fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
            where I: Iterator<Item = Token<'s>>
        {
            Semicolon::peek(pos, tokenizer)
        }

        fn span(&self) -> Option<ArcSpan> {
            calculate_span(self.semicolons.iter().map(|s| s.span()))
        }
    }

    #[token(kind = "Punct", raw = ":")]
    pub struct Colon {}

    #[token(kind = "Punct", raw = "::")]
    pub struct Namespace {}

    #[token(kind = "Punct", raw = "->")]
    pub struct Arrow {}

    #[token(kind = "Punct", raw = "=>")]
    pub struct FatArrow {}

    #[token(kind = "Punct", raw = "@")]
    pub struct At {}
}

pub(crate) mod op {
    use std::fmt::Display;

    use dash_macros::{token, Parse};
    use crate::parser::parse::Parse;
    use crate::parser::tokenizer::{TokenIterator, Token};

    #[token(kind = "Punct", raw = "!", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Not {}
    #[token(kind = "Punct", raw = "?", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Question {}
    
    #[token(kind = "Punct", raw = "==", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Eq {}
    #[token(kind = "Punct", raw = "!=", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Neq {}
    
    #[token(kind = "Punct", raw = "&&", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct And {}
    #[token(kind = "Punct", raw = "||", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Or {}
    
    #[token(kind = "Punct", raw = "=", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Seq {}

    #[token(kind = "Punct", raw = "+", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Add {}
    #[token(kind = "Punct", raw = "-", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Sub {}

    #[token(kind = "Punct", raw = "*", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Mul {}
    #[token(kind = "Punct", raw = "/", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Div {}
    #[token(kind = "Punct", raw = "%", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Mod {}

    #[token(kind = "Punct", raw = ">", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Grt {}
    #[token(kind = "Punct", raw = "<", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Less {}
    #[token(kind = "Punct", raw = ">=", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Geq {}
    #[token(kind = "Punct", raw = "<=", include_raw)]
    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Leq {}

    #[derive(Clone, Debug, Parse, PartialEq, Eq, Hash)]
    #[parse(expected = "operator")]
    pub enum Binary {
        Eq(Eq), Neq(Neq),
        Seq(Seq),
        Add(Add), Sub(Sub),
        Mul(Mul), Div(Div), Mod(Mod),
        Grt(Grt), Less(Less), Geq(Geq), Leq(Leq),
        And(And), Or(Or),
    }

    impl Display for Binary {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(match self {
                Self::Eq(e) => &e.raw,
                Self::Neq(e) => &e.raw,
                Self::Seq(e) => &e.raw,
                Self::Add(e) => &e.raw,
                Self::Sub(e) => &e.raw,
                Self::Mul(e) => &e.raw,
                Self::Div(e) => &e.raw,
                Self::Mod(e) => &e.raw,
                Self::Grt(e) => &e.raw,
                Self::Less(e) => &e.raw,
                Self::Geq(e) => &e.raw,
                Self::Leq(e) => &e.raw,
                Self::And(e) => &e.raw,
                Self::Or(e) => &e.raw,
            })
        }
    }

    #[derive(Clone, Debug, Parse, PartialEq, Eq, Hash)]
    #[parse(expected = "operator")]
    pub enum Unary {
        Plus(Add),
        Neg(Sub),
        Not(Not),
    }

    impl Display for Unary {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(match self {
                Self::Plus(e) => &e.raw,
                Self::Neg(e) => &e.raw,
                Self::Not(e) => &e.raw,
            })
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Prec {
        Mul,
        Add,
        Ord,
        Eq,
        And,
        Or,
        Seq,
    }

    impl Prec {
        pub(crate) const fn order() -> [Prec; 7] {
            [Prec::Mul, Prec::Add, Prec::Ord, Prec::Eq, Prec::And, Prec::Or, Prec::Seq]
        }
        pub fn peek<'s, I>(&self, tokenizer: &TokenIterator<'s, I>) -> bool
            where I: Iterator<Item = Token<'s>>
        {
            match self {
                Prec::Mul => Mul::peek(0, tokenizer) || Div::peek(0, tokenizer) ||
                             Mod::peek(0, tokenizer),
                Prec::Ord => Grt::peek(0, tokenizer) || Less::peek(0, tokenizer) ||
                             Geq::peek(0, tokenizer) || Leq::peek(0, tokenizer),
                Prec::Add => Add::peek(0, tokenizer) || Sub::peek(0, tokenizer),
                Prec::Eq  => Eq::peek(0, tokenizer) || Neq::peek(0, tokenizer),
                Prec::And => And::peek(0, tokenizer),
                Prec::Or  => Or::peek(0, tokenizer),
                Prec::Seq => Seq::peek(0, tokenizer),
            }
        }
    }
}

pub(crate) mod delim {
    use dash_macros::{token, Parse};

    use crate::{
        parser::parse::Parse,
        checker::{resolve::Resolve, coherency::Checker, ty::Ty}
    };

    #[token(kind = "Parentheses(_)", value_is_token_tree)]
    pub struct Parenthesized<T: Parse> {
        pub value: T,
    }

    impl<T: Parse + Resolve> Resolve for Parenthesized<T> {
        fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
            self.value.try_resolve(checker)
        }
    }
     
    #[token(kind = "Brackets(_)", value_is_token_tree)]
    pub struct Bracketed<T: Parse> {
        pub value: T,
    }

    impl<T: Parse + Resolve> Resolve for Bracketed<T> {
        fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
            self.value.try_resolve(checker)
        }
    }

    #[token(kind = "Braces(_)", value_is_token_tree)]
    pub struct Braced<T: Parse> {
        pub value: T,
    }

    impl<T: Parse + Resolve> Resolve for Braced<T> {
        fn try_resolve(&mut self, checker: &mut Checker) -> Option<Ty> {
            self.value.try_resolve(checker)
        }
    }

    /// Placeholder used for peeking delimiters
    #[derive(Debug, Parse)]
    pub struct P;

    impl Resolve for P {
        fn try_resolve(&mut self, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Invalid)
        }
    }
}
