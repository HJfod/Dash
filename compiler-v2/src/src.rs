
use std::{path::{PathBuf, Path}, sync::{Arc, Mutex}, fs, fmt::{Debug, Display, Write}, ops::RangeInclusive};
use line_col::LineColLookup;

use crate::char_iter::CharIter;

#[derive(Debug)]
pub struct Span<'s>(pub &'s Src, pub RangeInclusive<usize>);

impl Display for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1.is_empty() {
            write!(f, "{}:{}", self.0.name(), self.1.start())
        }
        else {
            write!(f, "{}:{}:{}", self.0.name(), self.1.start(), self.1.end())
        }
    }
}

pub enum Src {
    Builtin,
    File {
        path: PathBuf,
        data: String,
    }
}

impl Src {
    pub fn builtin() -> Arc<Self> {
        Arc::from(Self::Builtin)
    }

    pub fn from_file(path: &Path) -> Result<Arc<Self>, String> {
        Ok(Arc::from(Src::File {
            path: path.to_path_buf(),
            data: fs::read_to_string(path)
                .map_err(|e| format!("Can't read file: {}", e))?,
        }))
    }

    pub fn name(&self) -> String {
        match self {
            Src::Builtin => String::from("<compiler built-in>"),
            Src::File { path, data: _ } => path.to_string_lossy().to_string(),
        }
    }

    pub fn data(&self) -> &str {
        match self {
            Src::Builtin => "",
            Src::File { path: _, data } => data.as_str(),
        }
    }

    pub fn iter(&self) -> CharIter {
        CharIter::new(self.data())
    }

    pub fn underlined(&self, range: RangeInclusive<usize>) -> String {
        let lookup = LineColLookup::new(self.data());
        let start = lookup.get(*range.start());
        let end = lookup.get(*range.end());
        let mut lines = self
            .data().lines()
            .skip(start.0 - 1).take(end.0 - start.0);
        
        if end.0 == start.0 {
            format!(
                "{}\n{}{}\n",
                lines.next().unwrap(),
                " ".repeat(start.1 - 1),
                "~".repeat(std::cmp::max(1, end.1 - start.1))
            )
        } else {
            let mut res = String::new();
            let mut i = 1;
            let len = end.0 - start.0;
            for line in lines {
                res += &if i == len {
                    format!("{}\n{}\n", line, "~".repeat(std::cmp::max(1, end.1 - 1)))
                }
                else if i == 1 {
                    format!(
                        "{}\n{}{}\n",
                        line,
                        " ".repeat(start.1 - 1),
                        "~".repeat(std::cmp::max(1, line.len() - start.1 + 1))
                    )
                }
                else {
                    format!("{}\n{}\n", line, "~".repeat(std::cmp::max(1, line.len())))
                };
                i += 1;
            }
            res
        }
    }
}

impl Debug for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin => f.write_str("Builtin"),
            Self::File { path, data: _ } => f.write_fmt(format_args!("File({path:?})")),
        }
    }
}

impl Display for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Info,
    Warning,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Note<'s> {
    info: String,
    at: Option<Span<'s>>,
}

impl<'s> Note<'s> {
    pub fn new<S: Into<String>>(info: S) -> Self {
        Self { info: info.into(), at: None }
    }
    pub fn new_at<S: Into<String>>(info: S, span: Span<'s>) -> Self {
        Self { info: info.into(), at: Some(span) }
    }
}

impl Display for Note<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref span) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {})",
                self.info, span.0.underlined(span.1.clone()), span
            ))
        } else {
            f.write_fmt(format_args!("Note: {}", self.info))
        }
    }
}

#[derive(Debug)]
pub struct Message<'s> {
    level: Level,
    info: String,
    notes: Vec<Note<'s>>,
    span: Span<'s>,
}

impl<'s> Message<'s> {
    pub fn new<S: Into<String>>(level: Level, info: S, span: Span<'s>) -> Self {
        Self { level, info: info.into(), notes: vec![], span }
    }
    pub fn note(mut self, note: Note<'s>) -> Self {
        self.notes.push(note);
        self
    }
}

impl Display for Message<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} at {}:\n{}{}{}",
            self.level,
            self.span,
            self.span.0.underlined(self.span.1.clone()),
            self.info,
            self.notes
                .iter()
                .fold(String::new(), |mut acc, note| {
                    write!(&mut acc, " * {note}").unwrap();
                    acc
                })
        ))
    }
}

pub type Logger = Arc<Mutex<dyn FnMut(&Message)>>;
