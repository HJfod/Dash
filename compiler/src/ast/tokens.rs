
use dash_macros::token;

#[token(kind = Ident)]
pub struct Ident {}

#[token(kind = Punct, raw = "::")]
pub struct Colon {}
