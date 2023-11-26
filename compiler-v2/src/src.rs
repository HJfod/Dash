
use std::{path::PathBuf, sync::Arc, fs, fmt::{Debug, Display}, ops::Range, ffi::OsStr};
use line_col::LineColLookup;

use crate::char_iter::CharIter;

#[derive(Debug)]
pub struct Span<'s>(pub &'s Src, pub Range<usize>);

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

    pub fn underlined(&self, range: Range<usize>) -> String {
        let lookup = LineColLookup::new(self.data());
        let start = lookup.get(range.start);
        let end = lookup.get(range.end);
        let mut lines = self
            .data().lines()
            .skip(start.0 - 1).take(end.0 - start.0 + 1);
        
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
