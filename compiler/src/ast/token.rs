
use dash_macros::token;

#[token(kind = "Ident")]
pub struct Ident {}

pub(crate) mod kw {
    use dash_macros::token;

    #[token(kind = "Keyword", raw = "let")]
    pub struct Let {}

    #[token(kind = "Keyword", raw = "fun")]
    pub struct Fun {}

    #[token(kind = "Keyword", raw = "this")]
    pub struct This {}

    #[token(kind = "Ident", raw = "get")]
    pub struct Get {}

    #[token(kind = "Ident", raw = "set")]
    pub struct Set {}
}

pub(crate) mod lit {
    use dash_macros::{token, Parse};

    #[token(kind = "Keyword", raw = "void")]
    pub struct Void {}

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

    #[token(kind = "Int(_)")]
    pub struct Int {
        value: i64,
    }

    #[token(kind = "Float(_)")]
    pub struct Float {
        value: f64,
    }

    #[token(kind = "String(_)")]
    pub struct String {
        value: std::string::String,
    }
}

pub(crate) mod punct {
    use std::sync::Arc;

    use dash_macros::token;

    use crate::{shared::{src::{ArcSpan, Src}, logger::{Message, Level}}, parser::{parse::{Parse, FatalParseError, calculate_span}, tokenizer::{TokenIterator, Token}}};

    #[token(kind = "Punct", raw = ",")]
    pub struct Comma {}

    #[token(kind = "Punct", raw = ";")]
    pub struct Semicolon {}

    #[derive(Debug)]
    pub struct TerminatingSemicolon;

    impl Parse for TerminatingSemicolon {
        fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, FatalParseError>
            where I: Iterator<Item = Token<'s>>
        {
            let last_was_braced = tokenizer.last_was_braced();
            let mut found = vec![];
            while let Some(s) = Semicolon::peek_and_parse(src.clone(), tokenizer)? {
                found.push(s.span());
            }
            // If the last token was a Braced then allow omitting semicolon
            if found.is_empty() && !last_was_braced {
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
                    calculate_span(found).unwrap().as_ref()
                ));
            }
            else if found.len() > 1 {
                tokenizer.logger().lock().unwrap().log(Message::new(
                    Level::Warning,
                    "Unnecessary semicolons",
                    calculate_span(found.iter().skip(1).cloned()).unwrap().as_ref()
                ));
            }
            // Missing semicolon is not a fatal parsing error
            Ok(Self)
        }

        fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
            where I: Iterator<Item = Token<'s>>
        {
            Semicolon::peek(pos, tokenizer)
        }

        fn span(&self) -> Option<ArcSpan> {
            None
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
}

pub(crate) mod op {
    use dash_macros::{token, Parse};

    #[token(kind = "Punct", raw = "=")]
    pub struct Seq {}

    #[token(kind = "Punct", raw = "+")]
    pub struct Add {}

    #[token(kind = "Punct", raw = "-")]
    pub struct Sub {}

    #[derive(Debug, Parse)]
    #[parse(expected = "operator")]
    pub enum BinOp {
        Seq(Box<Seq>),
        Add(Box<Add>),
        Sub(Box<Sub>),
    }
}

pub(crate) mod delim {
    use dash_macros::token;

    use crate::parser::parse::Parse;

    #[token(kind = "Parentheses(_)", value_is_token_tree)]
    pub struct Parenthesized<T: Parse> {
        value: T,
    }
    
    #[token(kind = "Brackets(_)", value_is_token_tree)]
    pub struct Bracketed<T: Parse> {
        value: T,
    }

    #[token(kind = "Braces(_)", value_is_token_tree)]
    pub struct Braced<T: Parse> {
        value: T,
    }
}
