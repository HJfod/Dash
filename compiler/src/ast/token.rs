
use dash_macros::token;

#[token(kind = "Ident")]
pub struct Ident {}

pub(crate) mod kw {
    use dash_macros::token;

    #[token(kind = "Keyword", raw = "let")]
    pub struct Let {}
}

pub(crate) mod lit {
    use dash_macros::token;

    #[token(kind = "Int(_)")]
    pub struct Int {}
}

pub(crate) mod op {
    use dash_macros::token;

    #[token(kind = "Punct", raw = "=")]
    pub struct Seq {}

    #[token(kind = "Punct", raw = ":")]
    pub struct Colon {}

    #[token(kind = "Punct", raw = "::")]
    pub struct Namespace {}
}
