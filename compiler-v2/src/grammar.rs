
use std::{collections::{HashSet, HashMap}, fmt::Display};
use serde::{Deserialize, Deserializer};

#[derive(Default, Debug, Deserialize)]
pub struct Keywords<'g> {
    #[serde(borrow, default)]
    pub strict: HashSet<&'g str>,
    #[serde(default)]
    pub reserved: HashSet<&'g str>,
    #[serde(default)]
    pub contextual: HashSet<&'g str>,
}

#[derive(Debug)]
pub enum MemberKind<'g> {
    Rule(&'g str),
    Maybe(&'g str),
    List(&'g str),
}

impl<'de: 'g, 'g> Deserialize<'de> for MemberKind<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &'g str = Deserialize::deserialize(deserializer)?;
        if let Some(maybe) = s.strip_prefix('?') {
            Ok(MemberKind::Maybe(maybe))
        }
        else if let Some(list) = s.strip_prefix('+') {
            Ok(MemberKind::List(list))
        }
        else {
            Ok(MemberKind::Rule(s))
        }
    }
}

#[derive(Debug)]
pub enum TokenItem<'g> {
    Token(&'g str),
    Ident,
    Int,
    Float,
    String,
    Parentheses,
    Brackets,
    Braces,
    Eof(Option<&'g str>),
}

impl Display for TokenItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenItem::Token(s) => write!(f, "'{}'", s),
            TokenItem::Ident => write!(f, "identifier"),
            TokenItem::Int => write!(f, "integer"),
            TokenItem::Float => write!(f, "float"),
            TokenItem::String => write!(f, "string"),
            TokenItem::Parentheses => write!(f, "parenthesized expression"),
            TokenItem::Brackets => write!(f, "bracketed expression"),
            TokenItem::Braces => write!(f, "braced expression"),
            TokenItem::Eof(s) => write!(f, "{}", s.unwrap_or("end-of-file")),
        }
    }
}

impl<'g> From<&'g str> for TokenItem<'g> {
    fn from(s: &'g str) -> Self {
        match s {
            "ident" => TokenItem::Ident,
            "int" => TokenItem::Int,
            "float" => TokenItem::Float,
            "string" => TokenItem::String,
            "(...)" => TokenItem::Parentheses,
            "[...]" => TokenItem::Brackets,
            "{...}" => TokenItem::Braces,
            s if s.starts_with("eof") => TokenItem::Eof(s.strip_prefix("eof:")),
            _ => TokenItem::Token(s),
        }
    }
}

impl<'de: 'g, 'g> Deserialize<'de> for TokenItem<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        Ok(<Self as From<&'g str>>::from(Deserialize::deserialize(deserializer)?))
    }
}

#[derive(Debug)]
pub enum Item<'g> {
    Token(TokenItem<'g>),
    Rule(&'g str),
}

impl<'de: 'g, 'g> Deserialize<'de> for Item<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &'g str = Deserialize::deserialize(deserializer)?;
        if let Some(rule) = s.strip_prefix('#') {
            Ok(Item::Rule(rule))
        }
        else {
            Ok(Item::Token(s.into()))
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum IfGrammar<'g> {
    #[serde(rename = "not-eof")]
    NotEOF,
    #[serde(untagged)]
    Match {
        #[serde(rename = "match")]
        match_: TokenItem<'g>,
        into: Option<&'g str>,
    },
    #[serde(untagged)]
    Peek {
        peek: TokenItem<'g>,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum Grammar<'g> {
    Match {
        #[serde(rename = "match", borrow)]
        match_: Item<'g>,
        into: Option<&'g str>,
        #[serde(default)]
        inner: Vec<Grammar<'g>>,
    },
    If {
        #[serde(rename = "if")]
        if_: IfGrammar<'g>,
        then: Vec<Grammar<'g>>,
        #[serde(default)]
        #[serde(rename = "else")]
        else_: Vec<Grammar<'g>>,
    },
    While {
        #[serde(rename = "while")]
        while_: IfGrammar<'g>,
        then: Vec<Grammar<'g>>,
    },
    Return {
        #[serde(rename = "return", borrow)]
        return_: Item<'g>,
    },
    Expected {
        expected: &'g str,
    },
}

#[derive(Debug)]
pub enum TypeItem<'g> {
    Member(&'g str),
    Type(&'g str),
}

impl<'de: 'g, 'g> Deserialize<'de> for TypeItem<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &'g str = Deserialize::deserialize(deserializer)?;
        if let Some(rule) = s.strip_prefix('@') {
            Ok(TypeItem::Type(rule))
        }
        else {
            Ok(TypeItem::Member(s))
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum Check<'g> {
    Equal {
        #[serde(borrow)]
        equal: Vec<TypeItem<'g>>,
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Rule<'g> {
    #[serde(default)]
    pub members: Vec<MemberKind<'g>>,
    #[serde(borrow)]
    pub grammar: Vec<Grammar<'g>>,
    #[serde(default)]
    pub check: Vec<Check<'g>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct GrammarFile<'g> {
    #[serde(default)]
    pub keywords: Keywords<'g>,
    #[serde(borrow)]
    pub rules: HashMap<String, Rule<'g>>,
}
