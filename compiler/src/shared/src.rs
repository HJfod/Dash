
use std::{
    cmp::max,
    fmt::{Debug, Display},
    fs,
    path::{Path, PathBuf}, ffi::OsStr, hash::Hash,
};
use crate::parser::stream::{SrcReader, TokenStream};

use super::logging::LoggerRef;

#[derive(Debug, Clone)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Loc {
    pub const fn zero() -> Self {
        Self {
            line: 0,
            column: 0,
            offset: 0,
        }
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

#[derive(Clone)]
pub struct Span<'s> {
    src: &'s Src,
    start: Loc,
    end: Loc,
}

static BUILTIN_SPAN: Span<'static> = Span {
    src: &Src::Builtin,
    start: Loc::zero(),
    end: Loc::zero(),
};

impl<'s> Span<'s> {
    pub fn builtin() -> &'static Self {
        &BUILTIN_SPAN
    }

    pub fn new(src: &'s Src, start: Loc, end: Loc) -> Self {
        Self { src, start, end }
    }

    pub fn src(&self) -> &'s Src {
        self.src
    }

    pub fn start(&self) -> Loc {
        self.start.clone()
    }

    pub fn end(&self) -> Loc {
        self.end.clone()
    }

    pub fn underlined(&self) -> String {
        let lines = self.src
            .lines()
            .get(self.start.line..=self.end.line)
            .and_then(|p| (!p.is_empty()).then_some(Vec::from(p)))
            .unwrap_or(vec![String::from("/* Invalid source code range */")]);
        if lines.len() == 1 {
            format!(
                "{}\n{}{}\n",
                lines[0],
                " ".repeat(self.start.column),
                "~".repeat(max(1, self.end.column - self.start.column))
            )
        } else {
            let mut res = String::new();
            let mut i = 1;
            let len = lines.len();
            for line in lines {
                res += &if i == len {
                    format!("{}\n{}\n", line, "~".repeat(max(1, self.end.column)))
                } else if i == 1 {
                    format!(
                        "{}\n{}{}\n",
                        line,
                        " ".repeat(self.start.column),
                        "~".repeat(max(1, line.len() - self.start.column))
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

impl PartialEq for Span<'_> {
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

impl Display for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            f.write_fmt(format_args!("{}", self.start))
        } else {
            f.write_fmt(format_args!("{}-{}", self.start, self.end))
        }
    }
}

impl Debug for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
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
            Src::File { path, chars: _ } => path
                .file_name()
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
            } else {
                column += 1;
            }
            o += 1;
            if o >= len {
                break;
            }
        }
        Loc {
            line,
            column,
            offset,
        }
    }

    pub fn span<'s>(&'s self, mut start: usize, mut end: usize) -> Span<'s> {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }
        Span {
            src: self,
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
        let srcs = Self::find_src_files(dir);
        if srcs.is_empty() {
            Err(format!("directory is empty"))
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
                    else if file.path().extension() == Some(&OsStr::new("gs")) {
                        res.push(file.path());
                    }
                }
            }
        }
        res
    }

    pub fn iter<'s>(&'s self) -> <&'s Vec<Src> as IntoIterator>::IntoIter {
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
