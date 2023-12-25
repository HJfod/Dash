
use std::{path::PathBuf, sync::Arc, fs, fmt::{Debug, Display}, ops::Range, ffi::OsStr, cmp::max};
use line_col::LineColLookup;
use colored::{Color, Colorize};

use crate::shared::char_iter::CharIter;

pub enum Underline {
    /// Error squiggle
    Squiggle,
    /// Highlight
    Highlight,
    /// Gray underline
    Normal,
}

impl Underline {
    fn line(&self, range: Range<usize>) -> String {
        let (symbol, color) = match self {
            Self::Squiggle => ("~", Color::Red),
            Self::Highlight => ("^", Color::Cyan),
            Self::Normal => ("-", Color::Black),
        };
        format!("{}{}",
            " ".repeat(range.start),
            symbol.repeat(max(1, range.end - range.start)).color(color)
        )
    }
}

#[derive(Debug)]
pub struct Span<'s>(pub &'s Src, pub Range<usize>);

impl<'s> Span<'s> {
    pub fn builtin() -> Self {
        Self(&Src::Builtin, 0..0)
    }
    pub fn underlined(&self, style: Underline) -> String {
        // Get the starting and ending linecols as 0-based indices
        let sub_tuple = |a: (usize, usize)| { (a.0 - 1, a.1 - 1) };
        let lookup = LineColLookup::new(self.0.data());
        let start = sub_tuple(lookup.get(self.1.start));
        let end = sub_tuple(lookup.get(self.1.end));

        let mut lines = self.0
            .data().lines()
            .skip(start.0).take(end.0 - start.0 + 1);

        let padding = (end.0 + 1).to_string().len();
        let output_line = |line: usize, content, range| {
            format!(
                "{:pad1$}{}{}\n{:pad2$}{}\n",
                line.to_string().yellow(), " | ".black(), content,
                "", style.line(range),
                pad1 = padding - line.to_string().len(),
                pad2 = padding + 3
            )
        };
        
        let underlined = if end.0 == start.0 {
            output_line(start.0 + 1, lines.next().unwrap(), start.1..end.1)
        }
        else {
            let mut res = String::new();
            let mut i = 1;
            let len = end.0 - start.0;
            for line in lines {
                res.push_str(&output_line(start.0 + i, line, match i {
                    _ if i == len => 0..end.1,
                    1 => start.1..line.len(),
                    _ => 0..line.len(),
                }));
                i += 1;
            }
            res
        };
        format!(
            "{}{}{}\n{}",
            " ".repeat(padding), "--> ".black(), self.to_string().black(),
            underlined
        )
    }
}

impl<'s> Clone for Span<'s> {
    fn clone(&self) -> Self {
        Self(self.0, self.1.clone())
    }
}

impl Display for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lookup = LineColLookup::new(self.0.data());
        let start = lookup.get(self.1.start);
        if self.1.is_empty() {
            write!(f, "{}:{}:{}", self.0.name(), start.0, start.1)
        }
        else {
            let end = lookup.get(self.1.end);
            write!(f, "{}:{}:{}-{}:{}", self.0.name(), start.0, start.1, end.0, end.1)
        }
    }
}
#[derive(Clone)]
pub struct ArcSpan(pub Arc<Src>, pub Range<usize>);

impl ArcSpan {
    pub fn as_ref(&self) -> Span {
        Span(self.0.as_ref(), self.1.clone())
    }
}

impl Debug for ArcSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl PartialEq for ArcSpan {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
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

    pub fn from_file<P: Into<PathBuf>>(path: P) -> Result<Arc<Self>, String> {
        let path = path.into();
        Ok(Arc::from(Src::File {
            data: fs::read_to_string(&path).map_err(|e| format!("Can't read file: {}", e))?,
            path,
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

impl PartialEq for Src {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Src::Builtin, Src::Builtin) => true,
            (Src::File { path: a, data: _ }, Self::File { path: b, data: _ }) => a == b,
            (_, _) => false
        }
    }
}

#[derive(Debug)]
pub struct SrcPool {
    srcs: Vec<Arc<Src>>,
}

impl SrcPool {
    pub fn new(files: Vec<PathBuf>) -> Result<Self, String> {
        Ok(Self {
            srcs: files.into_iter().map(Src::from_file).collect::<Result<_, _>>()?
        })
    }
    pub fn new_from_dir(dir: PathBuf) -> Result<Self, String> {
        if dir.is_file() {
            return Self::new(vec![dir]);
        }
        if !dir.exists() {
            Err("Directory does not exist".to_string())?;
        }
        let srcs = Self::find_src_files(dir);
        if srcs.is_empty() {
            Err("Directory is empty".to_string())
        }
        else {
            Self::new(srcs)
        }
    }
    fn find_src_files(dir: PathBuf) -> Vec<PathBuf> {
        let mut res = vec![];
        if let Ok(entries) = std::fs::read_dir(dir) { 
            for entry in entries {
                let file = entry.unwrap();
                if let Ok(ty) = file.file_type() {
                    if ty.is_dir() {
                        res.extend(Self::find_src_files(file.path()));
                    }
                    else if file.path().extension() == Some(OsStr::new("dash")) {
                        res.push(file.path());
                    }
                }
            }
        }
        res
    }
    pub fn iter(&self) -> impl Iterator<Item = Arc<Src>> + '_ {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a SrcPool {
    type IntoIter = std::iter::Cloned<<&'a Vec<Arc<Src>> as IntoIterator>::IntoIter>;
    type Item = Arc<Src>;
    fn into_iter(self) -> Self::IntoIter {
        self.srcs.iter().cloned()
    }
}
