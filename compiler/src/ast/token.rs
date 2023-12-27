
use dash_macros::token;

#[token(kind = "Ident")]
pub struct Ident {}

pub(crate) mod kw {
    use dash_macros::token;

    #[token(kind = "Keyword", raw = "let")]
    pub struct Let {}

    #[token(kind = "Ident", raw = "get")]
    pub struct Get {}
}

pub(crate) mod lit {
    use dash_macros::token;

    #[token(kind = "Int(_)")]
    pub struct Int {
        value: i64,
    }
}

pub(crate) mod punct {
    use dash_macros::token;

    #[token(kind = "Punct", raw = ";")]
    pub struct Semicolon {}

    #[token(kind = "Punct", raw = ":")]
    pub struct Colon {}

    #[token(kind = "Punct", raw = "::")]
    pub struct Namespace {}
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
