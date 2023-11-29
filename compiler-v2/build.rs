
use std::{collections::{HashSet, HashMap}, fmt::Display, path::PathBuf, ffi::OsStr, fs};
use std::hash::Hash;
use json_comments::StripComments;
use serde::{Serialize, Deserialize};
use serde_json::Value;

#[derive(Default, Deserialize, Serialize)]
struct Keywords {
    #[serde(default)]
    pub strict: HashSet<String>,
    #[serde(default)]
    pub reserved: HashSet<String>,
    #[serde(default)]
    pub contextual: HashSet<String>,
}

#[derive(Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
struct Rules(HashMap<String, Value>);

enum InputFile {
    Keywords(Keywords),
    Rules(Rules),
}

#[derive(Default, Serialize)]
#[serde(rename_all = "kebab-case")]
struct OutputGrammarFile {
    #[serde(default)]
    pub keywords: Keywords,
    pub rules: Rules,
}

fn merge_hashset<T: Eq + Hash + Display>(into: &mut HashSet<T>, from: HashSet<T>, what: &str) {
    for kw in from {
        if into.contains(&kw) {
            panic!("{} '{kw}' defined in multiple grammar files", what);
        }
        into.insert(kw);
    }
}

fn merge_keyword_file(into: &mut OutputGrammarFile, from: Keywords) {
    merge_hashset(&mut into.keywords.strict, from.strict, "strict keyword");
    merge_hashset(&mut into.keywords.reserved, from.reserved, "reserved keyword");
    merge_hashset(&mut into.keywords.contextual, from.contextual, "contextual keyword");
}

fn merge_grammar_file(into: &mut OutputGrammarFile, from: Rules) {
    for (rule, value) in from.0 {
        if into.rules.0.contains_key(&rule) {
            panic!("rule '{}' defined in multiple grammar files", rule);
        }
        into.rules.0.insert(rule, value);
    }
}

fn merge_input_file(into: &mut OutputGrammarFile, from: InputFile) {
    match from {
        InputFile::Keywords(from) => merge_keyword_file(into, from),
        InputFile::Rules(from) => merge_grammar_file(into, from),
    }
}

fn find_json_files(dir: PathBuf, res: &mut Vec<InputFile>) {
    for file in std::fs::read_dir(&dir).unwrap() {
        let file = file.unwrap();
        if file.file_type().unwrap().is_dir() {
            find_json_files(dir.clone(), res);
        }
        else {
            let file = file.path();
            if file.file_stem() == Some(OsStr::new("output.combined")) {
                continue;
            }
            if matches!(file.extension().map(|s| s.to_str().unwrap()), Some("jsonc" | "json")) {
                let read = fs::read(&file).unwrap();
                let uncommented = StripComments::new(read.as_slice());
                println!("cargo:rerun-if-changed={}", file.display());
                if file.file_stem() == Some(OsStr::new("input.keywords")) {
                    res.push(InputFile::Keywords(serde_json::from_reader(uncommented).unwrap()));
                }
                else {
                    res.push(InputFile::Rules(serde_json::from_reader(uncommented).unwrap()));
                }
            }
        }
    }
}

fn combine_grammar_jsons(dir: PathBuf) -> String {
    let mut jsons = vec![];
    find_json_files(dir, &mut jsons);
    if jsons.is_empty() {
        return String::new();
    }
    let mut res = OutputGrammarFile::default();
    for json in jsons.into_iter() {
        merge_input_file(&mut res, json);
    }
    serde_json::to_string_pretty(&res).unwrap()
}

fn main() {
    let src = combine_grammar_jsons("./grammar".into());
    std::fs::write("./grammar/output.combined.json", src).unwrap();
}
