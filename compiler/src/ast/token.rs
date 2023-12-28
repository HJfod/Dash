
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
    use dash_macros::token;

    #[token(kind = "Punct", raw = ",")]
    pub struct Comma {}

    #[token(kind = "Punct", raw = ";")]
    pub struct Semicolon {}

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
