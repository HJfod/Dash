
use std::{
    cmp::max,
    fmt::{Debug, Display},
    fs,
    path::{Path, PathBuf}, ffi::OsStr, hash::Hash,
};
use crate::parser::stream::{SrcReader, TokenStream};

use super::logging::LoggerRef;

#[derive(Debug, Copy, Clone)]
pub struct Loc<'s> {
    pub src: &'s Src,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl<'s> Loc<'s> {
    pub const fn builtin() -> Self {
        Self {
            src: &Src::Builtin,
            line: 0,
            column: 0,
            offset: 0,
        }
    }
}

impl PartialEq for Loc<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset
    }
}

impl PartialOrd for Loc<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Display for Loc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line + 1, self.column + 1))
    }
}

pub type Span<'s> = std::ops::Range<Loc<'s>>;

pub static BUILTIN_SPAN: Span<'static> = Span {
    start: Loc::builtin(),
    end: Loc::builtin(),
};

pub trait Spanful<'s> {
    fn start(&self) -> Loc<'s>;
    fn end(&self) -> Loc<'s>;
    fn src(&self) -> &'s Src {
        self.start().src
    }
    fn display(&self) -> String {
        if self.start() == self.end() {
            format!("{}", self.start())
        } else {
            format!("{}-{}", self.start(), self.end())
        }
    }
    fn full_display(&self) -> String {
        format!("{}:{}", self.src(), self.display())
    }
    fn underlined(&self) -> String {
        let start = self.start();
        let end = self.end();
        let lines = self.src()
            .lines()
            .get(start.line..=end.line)
            .and_then(|p| (!p.is_empty()).then_some(Vec::from(p)))
            .unwrap_or(vec![String::from("/* Invalid source code range */")]);
        if lines.len() == 1 {
            format!(
                "{}\n{}{}\n",
                lines[0],
                " ".repeat(start.column),
                "~".repeat(max(1, end.column - start.column))
            )
        } else {
            let mut res = String::new();
            let mut i = 1;
            let len = lines.len();
            for line in lines {
                res += &if i == len {
                    format!("{}\n{}\n", line, "~".repeat(max(1, end.column)))
                } else if i == 1 {
                    format!(
                        "{}\n{}{}\n",
                        line,
                        " ".repeat(start.column),
                        "~".repeat(max(1, line.len() - start.column))
                    )
                } else {
                    format!("{}\n{}\n", line, "~".repeat(max(1, line.len())))
                };
                i += 1;
            }
            res
        }
    }
}

impl<'s> Spanful<'s> for Span<'s> {
    fn start(&self) -> Loc<'s> {
        self.start
    }

    fn end(&self) -> Loc<'s> {
        self.end
    }
}

#[derive(Eq)]
pub enum Src {
    Builtin,
    File { path: PathBuf, chars: Vec<char> },
}

impl Src {
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
            Src::File { path, chars: _ } => path.to_string_lossy().to_string(),
        }
    }

    pub fn get(&self, pos: usize) -> Option<char> {
        match self {
            Src::Builtin => None,
            Src::File { path: _, chars } => chars.get(pos).copied(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Src::Builtin => true,
            Src::File { path: _, chars } => chars.is_empty(),
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
            } else {
                column += 1;
            }
            o += 1;
            if o >= len {
                break;
            }
        }
        Loc {
            src: self,
            line,
            column,
            offset,
        }
    }

    pub fn span(&self, mut start: usize, mut end: usize) -> Span {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }
        Span {
            start: self.loc(start),
            end: self.loc(end),
        }
    }

    pub fn lines(&self) -> Vec<String> {
        match self {
            Src::Builtin => Vec::new(),
            Src::File { path: _, chars } => chars
                .iter()
                .collect::<String>()
                .split('\n')
                .map(|s| s.into())
                .collect(),
        }
    }

    pub fn tokenize<'s>(&'s self, logger: LoggerRef<'s>) -> TokenStream<'s, SrcReader<'s>> {
        TokenStream::new(self, SrcReader::new(self, logger))
    }
}

impl Hash for Src {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin => 0.hash(state),
            Self::File { path, chars: _ } => path.hash(state),
        }
    }
}

impl PartialEq for Src {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Src::Builtin, Src::Builtin) => true,
            (Src::File { path: a, chars: _ }, Src::File { path: b, chars: _ }) => a == b,
            _ => false,
        }
    }
}

impl Debug for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin => f.write_str("Builtin"),
            Self::File { path, chars: _ } => f.write_fmt(format_args!("File({path:?})")),
        }
    }
}

impl Display for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

#[derive(Debug)]
pub struct SrcPool {
    srcs: Vec<Src>,
}

impl SrcPool {
    pub fn new(files: Vec<PathBuf>) -> Result<Self, String> {
        Ok(Self {
            srcs: files
                .into_iter()
                .map(|f| Src::from_file(&f))
                .collect::<Result<_, _>>()?
        })
    }

    pub fn new_from_dir(dir: PathBuf) -> Result<Self, String> {
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
                    else if file.path().extension() == Some(OsStr::new("gs")) {
                        res.push(file.path());
                    }
                }
            }
        }
        res
    }

    pub fn iter(&self) -> <&Vec<Src> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'s> IntoIterator for &'s SrcPool {
    type Item = &'s Src;
    type IntoIter = <&'s Vec<Src> as IntoIterator>::IntoIter;
    
    fn into_iter(self) -> Self::IntoIter {
        self.srcs.iter()
    }
}
