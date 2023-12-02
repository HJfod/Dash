
use std::{collections::{HashSet, HashMap}, fmt::Display};
use serde::{Deserialize, Deserializer, de::Visitor};
use crate::parser::tokenizer::IsToken;

fn verify_str<'de: 'g, 'g, D: Deserializer<'de>>(s: &'g str) -> Result<&'g str, D::Error> {
    if s.chars().all(|c| c.is_alphabetic() || c == '-') {
        Ok(s)
    }
    else {
        Err(serde::de::Error::invalid_value(
            serde::de::Unexpected::Str(s),
            &"kebab-case identifier"
        ))
    }
}

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
            Ok(MemberKind::Maybe(verify_str::<D>(maybe)?))
        }
        else if let Some(list) = s.strip_prefix('+') {
            Ok(MemberKind::List(verify_str::<D>(list)?))
        }
        else {
            Ok(MemberKind::Rule(verify_str::<D>(s)?))
        }
    }
}

#[derive(Debug)]
pub enum TokenItem<'g> {
    Keyword(&'g str),
    Op(Option<&'g str>),
    Punct(&'g str),
    Ident,
    Int,
    Float,
    String,
    Parentheses,
    Brackets,
    Braces,
    Eof(Option<&'g str>),
    OneOf(Vec<TokenItem<'g>>),
    NamedTokenList(&'g str),
}

impl Display for TokenItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenItem::Keyword(s) => write!(f, "keyword {s}"),
            TokenItem::Op(s) => write!(f, "operator{}", s.map(|s| format!(" '{s}'")).unwrap_or(String::new())),
            TokenItem::Punct(s) => write!(f, "'{s}'"),
            TokenItem::Ident => write!(f, "identifier"),
            TokenItem::Int => write!(f, "integer"),
            TokenItem::Float => write!(f, "float"),
            TokenItem::String => write!(f, "string"),
            TokenItem::Parentheses => write!(f, "parenthesized expression"),
            TokenItem::Brackets => write!(f, "bracketed expression"),
            TokenItem::Braces => write!(f, "braced expression"),
            TokenItem::Eof(s) => write!(f, "{}", s.unwrap_or("end-of-file")),
            TokenItem::OneOf(s) => write!(
                f, "one of {}",
                s.iter()
                    .map(|s| format!("'{s}'"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TokenItem::NamedTokenList(s) => write!(f, "[{s}]"),
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
            "op" => TokenItem::Op(None),
            "(...)" => TokenItem::Parentheses,
            "[...]" => TokenItem::Brackets,
            "{...}" => TokenItem::Braces,
            s if s.starts_with('$') => TokenItem::NamedTokenList(s.strip_prefix('$').unwrap()),
            s if s.starts_with("eof") => TokenItem::Eof(s.strip_prefix("eof:")),
            s@("->" | "=>") => TokenItem::Punct(s),
            s if s.chars().all(|s| s.is_punct_char()) => TokenItem::Punct(s),
            s if s.chars().all(|s| s.is_op_char()) => TokenItem::Op(Some(s)),
            s if s.chars().all(|s| s.is_alphabetic() || s == '_') => TokenItem::Keyword(s),
            s => panic!("invalid thing to match '{}'", s),
        }
    }
}

pub struct TokenItemVisitor;
impl<'g> Visitor<'g> for TokenItemVisitor {
    type Value = TokenItem<'g>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("token or list of tokens to match")
    }

    fn visit_borrowed_str<E>(self, v: &'g str) -> Result<Self::Value, E>
        where
            E: serde::de::Error
    {
        Ok(TokenItem::from(v))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'g>
    {
        let mut items = Vec::new();
        while let Some(item) = seq.next_element()? {
            items.push(item);
        }
        Ok(TokenItem::OneOf(items))
    }
}

impl<'de: 'g, 'g> Deserialize<'de> for TokenItem<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        deserializer.deserialize_any(TokenItemVisitor)
    }
}

#[derive(Debug)]
pub enum Item<'g> {
    Token(TokenItem<'g>),
    Rule(&'g str),
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

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'g>
    {
        Ok(Item::Token(TokenItemVisitor.visit_seq(seq)?))
    }
}

impl<'de: 'g, 'g> Deserialize<'de> for Item<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: Deserializer<'de>
    {
        deserializer.deserialize_any(ItemVisitor)
    }
}

#[derive(Debug)]
pub enum ItemOrMember<'g> {
    Item(Item<'g>),
    Member(&'g str),
}

pub struct ItemOrMemberVisitor;
impl<'g> Visitor<'g> for ItemOrMemberVisitor {
    type Value = ItemOrMember<'g>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("token, rule, member, or list of tokens to match")
    }

    fn visit_borrowed_str<E>(self, v: &'g str) -> Result<Self::Value, E>
        where
            E: serde::de::Error
    {
        if let Some(s) = v.strip_prefix(':') {
            Ok(ItemOrMember::Member(s))
        }
        else {
            Ok(ItemOrMember::Item(ItemVisitor.visit_borrowed_str(v)?))
        }
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'g>
    {
        Ok(ItemOrMember::Item(ItemVisitor.visit_seq(seq)?))
    }
}

impl<'de: 'g, 'g> Deserialize<'de> for ItemOrMember<'g> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        deserializer.deserialize_any(ItemOrMemberVisitor)
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
        set: &'g str,
    },
    Not {
        not: Box<IfGrammar<'g>>,
    },
    Either {
        either: Vec<IfGrammar<'g>>,
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
        /// A list of members to initialize with values. The values are moved 
        /// to the members and left to unassigned
        with: Option<HashMap<&'g str, &'g str>>,
    },
    DebugLog {
        #[serde(rename = "debug-log")]
        debug_log: &'g str,
    },
    Expected {
        expected: &'g str,
    },
    Error {
        error: &'g str,
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
        if let Some(s) = s.strip_prefix(':') {
            Ok(TypeItem::Member(s))
        }
        else {
            Ok(TypeItem::Type(s))
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(untagged)]
pub enum Test<'g> {
    Equal {
        #[serde(borrow)]
        equal: (TypeItem<'g>, TypeItem<'g>),
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Check<'g> {
    #[serde(borrow)]
    pub result: TypeItem<'g>,
    pub tests: Vec<Test<'g>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Rule<'g> {
    #[serde(default)]
    pub name: &'g str,
    #[serde(default)]
    pub members: Vec<MemberKind<'g>>,
    #[serde(borrow)]
    pub grammar: Vec<Grammar<'g>>,
    pub check: Option<Check<'g>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct GrammarFile<'g> {
    #[serde(borrow, default)]
    pub keywords: Keywords<'g>,
    #[serde(deserialize_with = "deserialize_rules")]
    pub rules: HashMap<&'g str, Rule<'g>>,
    pub named_token_lists: HashMap<&'g str, Vec<TokenItem<'g>>>,
}

fn deserialize_rules<'g, D>(deserializer: D) -> Result<HashMap<&'g str, Rule<'g>>, D::Error>
    where D: Deserializer<'g>
{
    let mut res = HashMap::<&'g str, Rule<'g>>::deserialize(deserializer)?;
    for (name, rule) in &mut res {
        rule.name = verify_str::<D>(name)?;
    }
    Ok(res)
}
