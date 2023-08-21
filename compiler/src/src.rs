
use std::{fmt::{Display, Debug}, path::{PathBuf, Path}, fs, cmp::max};

use crate::{parser::{ExprMeta, Parser, Rule}, rules::ast::ExprList};

#[derive(Debug, Clone)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Loc {
    pub fn zero() -> Self {
        Self { line: 0, column: 0, offset: 0 }
    }
}

impl PartialEq for Loc {
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset
    }
}

impl PartialOrd for Loc {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    pub start: Loc,
    pub end: Loc,
}

impl Range {
    pub fn zero() -> Self {
        Self { start: Loc::zero(), end: Loc::zero() }
    }
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        let (ss, se) = if self.start <= self.end {
            (&self.start, &self.end)
        } else {
            (&self.end, &self.start)
        };
        let (os, oe) = if other.start <= other.end {
            (&other.start, &other.end)
        } else {
            (&other.end, &other.start)
        };
        ss == os && se == oe
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            f.write_fmt(format_args!("{}", self.start))
        }
        else {
            f.write_fmt(format_args!("{}-{}", self.start, self.end))
        }
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
        f.write_str(match self {
            Level::Info    => "Info",
            Level::Warning => "Warning",
            Level::Error   => "Error",
        })
    }
}

pub struct Note<'s> {
    info: String,
    at: Option<(&'s Src, Range)>,
}

impl<'s> Note<'s> {
    pub fn new(info: &str) -> Self {
        Self { info: info.into(), at: None }
    }

    pub fn new_at<S: Into<String>>(info: S, src: &'s Src, range: Range) -> Self {
        Self { info: info.into(), at: Some((src, range)) }
    }
}

impl Display for Note<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((ref src, ref range)) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {} at {})",
                self.info,
                src.underlined(&range),
                src.name(), range
            ))
        }
        else {
            f.write_fmt(format_args!("Note: {}", self.info))
        }
    }
}

pub struct Message<'s> {
    pub level: Level,
    pub info: String,
    pub notes: Vec<Note<'s>>,
    pub src: &'s Src,
    pub range: Range,
}

impl<'s> Message<'s> {
    pub fn from_meta(level: Level, info: String, meta: &ExprMeta<'s>) -> Self {
        Self {
            level, info,
            notes: vec![],
            src: meta.src,
            range: meta.range.clone()
        }
    }

    pub fn note(mut self, note: Note<'s>) -> Self {
        self.notes.push(note);
        self
    }

    pub fn augment<F: Fn(String) -> String>(mut self, info: F) -> Self {
        self.info = info(self.info);
        self
    }
}

impl Display for Message<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} in {} at {}:\n{}{}{}",
            self.level, self.src.name(), self.range,
            self.src.underlined(&self.range),
            self.info,
            self.notes.iter().map(|note| format!("\n * {}", note)).collect::<String>()
        ))
    }
}

#[derive(PartialEq)]
pub enum Src {
    Builtin,
    File {
        path: PathBuf,
        chars: Vec<char>,
    },
}

impl Debug for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin => f.write_str("Builtin"),
            Self::File { path, chars: _ } => f.write_fmt(format_args!("File({path:?})")),
        }
    }
}

static BUILTIN_SRC: Src = Src::Builtin;

impl Src {
    pub fn builtin() -> &'static Self {
        &BUILTIN_SRC
    }

    pub fn from_file(path: &Path) -> Result<Self, String> {
        Ok(Src::File {
            path: path.to_path_buf(),
            chars: fs::read_to_string(path)
                .map_err(|e| format!("Can't read file: {}", e))?
                .chars()
                .collect(),
        })
    }

    pub fn name(&self) -> String {
        match self {
            Src::Builtin => String::from("<compiler built-in>"),
            Src::File { path, chars: _ } => path.file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or("<anonymous file>".to_string()),
        }
    }

    pub fn get(&self, pos: usize) -> Option<char> {
        match self {
            Src::Builtin => None,
            Src::File { path: _, chars } => chars.get(pos).map(|c| *c),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Src::Builtin => 0,
            Src::File { path: _, chars } => chars.len(),
        }
    }

    pub fn loc(&self, offset: usize) -> Loc {
        let mut o = 0usize;
        let len = self.len();
        let mut line = 0;
        let mut column = 0;
        while o != offset {
            let c = self.get(o).expect("Internal Compiler Error: Src::get failed at offset despite offset being within 0..Src::len");
            if c == '\n' {
                line += 1;
                column = 0;
            }
            else {
                column += 1;
            }
            o += 1;
            if o >= len {
                break;
            }
        }
        Loc { line, column, offset, }
    }

    pub fn range(&self, mut start: usize, mut end: usize) -> Range {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }
        Range {
            start: self.loc(start),
            end: self.loc(end),
        }
    }

    fn lines(&self) -> Vec<String> {
        match self {
            Src::Builtin => Vec::new(),
            Src::File { path: _, chars } => chars.iter()
                .collect::<String>()
                .split('\n')
                .map(|s| s.into())
                .collect(),
        }
    }

    pub fn underlined(&self, range: &Range) -> String {
        let lines = self.lines()
            .get(range.start.line..=range.end.line)
            .and_then(|p| (!p.is_empty()).then_some(Vec::from(p)))
            .unwrap_or(vec![String::from("/* Invalid source code range */")]);
        if lines.len() == 1 {
            format!(
                "{}\n{}{}\n",
                lines[0],
                " ".repeat(range.start.column),
                "~".repeat(max(1, range.end.column - range.start.column))
            )
        }
        else {
            let mut res = String::new();
            let mut i = 1;
            let len = lines.len();
            for line in lines {
                res += &if i == len {
                    format!("{}\n{}\n", line, "~".repeat(max(1, range.end.column)))
                }
                else if i == 1 {
                    format!("{}\n{}{}\n", line, 
                        " ".repeat(range.start.column),
                        "~".repeat(max(1, line.len() - range.start.column))
                    )
                }
                else {
                    format!("{}\n{}\n", line, "~".repeat(max(1, line.len())))
                };
                i += 1;
            }
            res
        }
    }

    pub fn parse<'s>(&'s self) -> Result<ExprList<'s>, Message<'s>> {
        let mut parser = Parser::new(self);
        let ast = ExprList::expect(&mut parser)?;
        if !parser.is_eof() {
            Err(parser.error(parser.pos(), format!(
                "Unexpected character '{}'",
                parser.peek().unwrap()
            )))
        }
        else {
            Ok(ast)
        }
    }
}
