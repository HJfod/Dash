#![feature(proc_macro_span)]

use std::{ffi::OsStr, path::PathBuf, collections::{HashSet, HashMap}, hash::Hash, fmt::Display};
use proc_macro::{TokenStream, Span};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use syn::{parse_macro_input, LitStr};
use quote::quote;

extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate serde;
extern crate serde_json;
extern crate json_comments;

#[derive(Deserialize, Serialize)]
struct Keywords {
    pub strict: HashSet<String>,
    pub reserved: HashSet<String>,
    pub contextual: HashSet<String>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
struct GrammarFile {
    pub keywords: Keywords,
    pub rules: HashMap<String, Value>,
}

fn merge_hashset<T: Eq + Hash + Display>(into: &mut HashSet<T>, from: HashSet<T>, what: &str) {
    for kw in from {
        if into.contains(&kw) {
            panic!("{} '{kw}' defined in multiple grammar files", what);
        }
        into.insert(kw);
    }
}

fn merge_grammar_files(into: &mut GrammarFile, from: GrammarFile) {
    merge_hashset(&mut into.keywords.strict, from.keywords.strict, "strict keyword");
    merge_hashset(&mut into.keywords.reserved, from.keywords.reserved, "reserved keyword");
    merge_hashset(&mut into.keywords.contextual, from.keywords.contextual, "contextual keyword");
    for (rule, value) in from.rules {
        if into.rules.contains_key(&rule) {
            panic!("rule '{}' defined in multiple grammar files", rule);
        }
        into.rules.insert(rule, value);
    }
}

fn find_json_files(dir: PathBuf, res: &mut Vec<GrammarFile>) {
    for file in std::fs::read_dir(&dir).unwrap() {
        let file = file.unwrap();
        if file.file_type().unwrap().is_dir() {
            find_json_files(dir.clone(), res);
        }
        else {
            if file.path().extension() == Some(OsStr::new("jsonc")) {
                res.push(serde_json::from_reader(json_comments::StripComments::new(
                    std::fs::read(file.path()).unwrap().as_slice()
                )).unwrap());
            }
            if file.path().extension() == Some(OsStr::new("json")) {
                res.push(serde_json::from_str(
                    &std::fs::read_to_string(file.path()).unwrap()
                ).unwrap());
            }
        }
    }
}

#[proc_macro]
pub fn include_grammar_jsons(stream: TokenStream) -> TokenStream {
    let path = parse_macro_input!(stream as LitStr);
    let mut jsons = vec![];
    find_json_files(Span::call_site().source_file().path().parent().unwrap().join(path.value()), &mut jsons);
    if jsons.is_empty() {
        return quote!{ "{}" }.into();
    }
    let mut iter = jsons.into_iter();
    let mut res = iter.next().unwrap();
    for json in iter {
        merge_grammar_files(&mut res, json);
    }
    let str = serde_json::to_string(&res).unwrap();
    quote!{ #str }.into()
}
