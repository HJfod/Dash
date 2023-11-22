
use std::collections::{HashSet, HashMap};
use serde::{Deserialize, Deserializer};

#[derive(Deserialize)]
struct Keywords {
    strict: HashSet<String>,
    reserved: HashSet<String>,
    contextual: HashSet<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum MemberKind {
    Rule,
    Maybe,
    List,
}

enum Item {
    Token(String),
    Rule(String),
}

impl<'de> Deserialize<'de> for Item {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        if let Some(rule) = s.strip_prefix('#') {
            Ok(Item::Rule(rule.to_string()))
        }
        else {
            Ok(Item::Token(s.to_string()))
        }
    }
}

#[derive(Deserialize)]
enum Grammar {
    Match {
        #[serde(rename = "match")]
        match_: Item,
        into: Option<String>,
    },
    If {
        #[serde(rename = "if")]
        if_: Box<Grammar>,
        then: Vec<Grammar>,
        #[serde(default)]
        #[serde(rename = "else")]
        else_: Vec<Grammar>,
    },
    While {
        #[serde(rename = "while")]
        while_: Box<Grammar>,
        then: Vec<Grammar>,
    },
    Return {
        #[serde(rename = "return")]
        return_: Item,
    },
    Error {
        error: String,
    },
}

enum TypeItem {
    Member(String),
    Type(String),
}

impl<'de> Deserialize<'de> for TypeItem {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        if let Some(rule) = s.strip_prefix('@') {
            Ok(TypeItem::Type(rule.to_string()))
        }
        else {
            Ok(TypeItem::Member(s.to_string()))
        }
    }
}

#[derive(Deserialize)]
enum Check {
    Equal {
        equal: Vec<TypeItem>,
    }
}

#[derive(Deserialize)]
struct Rule {
    #[serde(default)]
    layout: HashMap<String, MemberKind>,
    grammar: Vec<Grammar>,
    #[serde(default)]
    check: Vec<Check>,
}

#[derive(Deserialize)]
struct GrammarFile {
    keywords: Keywords,
    rules: HashMap<String, Rule>,
}

struct Runner {}
