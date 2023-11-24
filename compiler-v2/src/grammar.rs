
use std::collections::{HashSet, HashMap};
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

pub enum Item<'g> {
    Token(&'g str),
    Rule(&'g str),
    Braces,
    Parentheses,
    Brackets,
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
            Ok(Item::Token(s))
        }
    }
}

#[derive(Deserialize)]
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
        if_: Box<Grammar<'g>>,
        then: Vec<Grammar<'g>>,
        #[serde(default)]
        #[serde(rename = "else")]
        else_: Vec<Grammar<'g>>,
    },
    While {
        #[serde(rename = "while")]
        while_: Box<Grammar<'g>>,
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
pub enum Check<'g> {
    Equal {
        #[serde(borrow)]
        equal: Vec<TypeItem<'g>>,
    }
}

#[derive(Deserialize)]
pub struct Rule<'g> {
    #[serde(default)]
    pub layout: HashMap<String, MemberKind>,
    #[serde(borrow)]
    pub grammar: Vec<Grammar<'g>>,
    #[serde(default)]
    pub check: Vec<Check<'g>>,
}

#[derive(Deserialize)]
pub struct GrammarFile<'g> {
    pub keywords: Keywords<'g>,
    #[serde(borrow)]
    pub rules: HashMap<String, Rule<'g>>,
}
