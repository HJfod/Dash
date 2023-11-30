
use std::{collections::{HashSet, HashMap}, fmt::Display};
use serde::{Deserialize, Deserializer, de::Visitor};

use crate::tokenizer::IsToken;

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
        let verify_str = |s: &'g str| -> Result<&'g str, D::Error> {
            if s.chars().all(|c| c.is_alphabetic() || c == '-') {
                Ok(s)
            }
            else {
                Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(s),
                    &"kebab-case identifier"
                ))
            }
        };
        if let Some(maybe) = s.strip_prefix('?') {
            Ok(MemberKind::Maybe(verify_str(maybe)?))
        }
        else if let Some(list) = s.strip_prefix('+') {
            Ok(MemberKind::List(verify_str(list)?))
        }
        else {
            Ok(MemberKind::Rule(verify_str(s)?))
        }
    }
}

#[derive(Debug)]
pub enum TokenItem<'g> {
    Keyword(&'g str),
    Op(&'g str),
    Punct(&'g str),
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
            TokenItem::Keyword(s) => write!(f, "keyword {s}"),
            TokenItem::Op(s) => write!(f, "operator '{s}'"),
            TokenItem::Punct(s) => write!(f, "'{s}'"),
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
            s if s.chars().all(|s| s.is_alphabetic() || s == '_') => TokenItem::Keyword(s),
            s@("->" | "=>") => TokenItem::Punct(s),
            s if s.chars().all(|s| s.is_punct_char()) => TokenItem::Punct(s),
            s if s.chars().all(|s| s.is_op_char()) => TokenItem::Op(s),
            s => panic!("invalid thing to match '{}'", s),
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
    OneOf(Vec<TokenItem<'g>>),
}

pub struct ItemVisitor;
impl<'g> Visitor<'g> for ItemVisitor {
    type Value = Item<'g>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("token, rule, or list of tokens to match")
    }

    fn visit_borrowed_str<E>(self, v: &'g str) -> Result<Self::Value, E>
        where
            E: serde::de::Error
    {
        if let Some(rule) = v.strip_prefix('#') {
            Ok(Item::Rule(rule))
        }
        else {
            Ok(Item::Token(v.into()))
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'g>
    {
        let mut items = Vec::new();
        while let Some(item) = seq.next_element()? {
            items.push(item);
        }
        Ok(Item::OneOf(items))
    }
}

impl<'de: 'g, 'g> Deserialize<'de> for Item<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        deserializer.deserialize_any(ItemVisitor)
    }
}

#[derive(Debug)]
pub enum ItemOrMember<'g> {
    Item(Item<'g>),
    Member(&'g str),
}

impl<'de: 'g, 'g> Deserialize<'de> for ItemOrMember<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let s: &'g str = Deserialize::deserialize(deserializer)?;
        if let Some(s) = s.strip_prefix(':') {
            Ok(ItemOrMember::Member(s))
        }
        else {
            ItemVisitor.visit_borrowed_str(s).map(ItemOrMember::Item)
        }
    }
}

#[derive(Debug, Deserialize)]
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
    Set {
        member: &'g str,
    },
    Not {
        cond: Box<IfGrammar<'g>>,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum Grammar<'g> {
    Match {
        /// Item to match
        #[serde(rename = "match", borrow)]
        match_: Item<'g>,
        /// Member to store the matched result into
        into: Option<&'g str>,
        /// If the matched token has inner contents (parentheses), this specifies 
        /// how to match that
        #[serde(default)]
        inner: Vec<Grammar<'g>>,
        /// A list of members to initialize with values. The values are moved 
        /// to the members and left to unassigned
        with: Option<HashMap<&'g str, &'g str>>,
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
        #[serde(rename = "return")]
        return_: ItemOrMember<'g>,
    },
    DebugLog {
        #[serde(rename = "debug-log")]
        debug_log: &'g str,
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
