
use std::{collections::{HashSet, HashMap}, fmt::Display};
use serde::{Deserialize, Deserializer};

#[derive(Deserialize)]
pub struct Keywords<'g> {
    #[serde(borrow)]
    pub strict: HashSet<&'g str>,
    pub reserved: HashSet<&'g str>,
    pub contextual: HashSet<&'g str>,
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MemberKind {
    Rule,
    Maybe,
    List,
}

pub enum TokenItem<'g> {
    Token(&'g str),
    Ident,
    Int,
    Float,
    String,
    Parentheses,
    Brackets,
    Braces,
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

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum IfGrammar<'g> {
    Match {
        #[serde(rename = "match")]
        match_: TokenItem<'g>,
        into: Option<&'g str>,
    },
    Peek {
        peek: TokenItem<'g>,
    },
}

#[derive(Deserialize)]
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

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum Check<'g> {
    Equal {
        #[serde(borrow)]
        equal: Vec<TypeItem<'g>>,
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Rule<'g> {
    #[serde(default)]
    pub layout: HashMap<String, MemberKind>,
    #[serde(borrow)]
    pub grammar: Vec<Grammar<'g>>,
    #[serde(default)]
    pub check: Vec<Check<'g>>,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct GrammarFile<'g> {
    pub keywords: Keywords<'g>,
    #[serde(borrow)]
    pub rules: HashMap<String, Rule<'g>>,
}
