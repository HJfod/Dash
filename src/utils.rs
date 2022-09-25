use std::{fmt::{Debug, Display}, path::PathBuf};
use peg::{str::LineCol, Parse};
use std::fs;
use crate::error::{Severity, ErrorCode, SimpleError};
use crate::output::Output;
use colored::{Colorize, Color};

#[derive(Clone, PartialEq)]
pub struct File {
    pub path: PathBuf,
    pub data: String,
}

impl Debug for File {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("File")
            .field("path", &self.path)
            .field("data", if self.data.len() > 40 {
                &"..."
            } else {
                &self.data
            })
            .finish()
    }
}

pub struct Span {
    pub from: LineCol,
    pub to: LineCol,
    pub lines: Vec<String>,
    pub squiggle: Option<Color>,
}

impl Span {
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }
}

impl Display for Span {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let side_size = self.to.line.to_string().len() + 4;
        let mut line_num = self.from.line;
        for line in &self.lines {
            fmt.write_fmt(format_args!(
                " {}{} ",
                " ".repeat(side_size - 4 - line_num.to_string().len()),
                line_num.to_string().bright_yellow()
            ))?;
            fmt.write_fmt(format_args!("{} ", "|".bright_black()))?;
            fmt.write_str(line.as_str())?;
            fmt.write_str("\n")?;
            if let Some(color) = self.squiggle {
                if self.from.line == self.to.line {
                    fmt.write_fmt(format_args!(
                        "{}{}",
                        " ".repeat(self.from.column - 1 + side_size),
                        "~".repeat(if self.to.column == self.from.column { 1 } else {
                            self.to.column - self.from.column
                        }).color(color)
                    ))?;
                } else {
                    if line_num == self.from.line {
                        fmt.write_fmt(format_args!(
                            "{}{}",
                            " ".repeat(self.from.column - 1 + side_size),
                            "~".repeat(line.len() - self.from.column + 1).color(color)
                        ))?;
                        fmt.write_str("\n")?;
                    } else if line_num == self.to.line {
                        fmt.write_fmt(format_args!(
                            "{}{}",
                            " ".repeat(side_size),
                            "~".repeat(self.to.column - 1).color(color)
                        ))?;
                    } else {
                        fmt.write_fmt(format_args!(
                            "{}{}",
                            " ".repeat(side_size),
                            "~".repeat(line.len()).color(color)
                        ))?;
                        fmt.write_str("\n")?;
                    }
                }
            }
            line_num += 1;
        }
        Ok(())
    }
}

impl File {
    pub fn new(path: &PathBuf, channel: &Output) -> Result<Box<File>, ErrorCode> {
        Ok(Box::new(File {
            path: path.clone(),
            data: fs::read_to_string(path)
                .map_err(|e| channel.print(
                    &SimpleError {
                        info: e.to_string(),
                        severity: Severity::Fatal,
                        code: ErrorCode::CantReadFile
                    }
                ))?
        }))
    }

    pub fn lines_at(&self, from: &LineCol, to: &LineCol) -> Vec<String> {
        self.data.split('\n')
            .map(|i| String::from(i))
            .collect::<Vec<_>>()[from.line - 1..to.line]
            .to_vec()
    }

    pub fn span_at(
        &self,
        from: &LineCol,
        to: &LineCol,
        squiggle: Option<Color>
    ) -> Span {
        Span {
            from: from.to_owned(),
            to: to.to_owned(),
            lines: self.lines_at(from, to),
            squiggle
        }
    }

    pub fn lines_joined(&self, from: &LineCol, to: &LineCol) -> String {
        self.span_at(from, to, None).to_string()
    }

    pub fn lines_squiggled(&self, from: &LineCol, to: &LineCol) -> String {
        self.span_at(from, to, Some(Color::BrightRed)).to_string()
    }

    pub fn linecol_at(&self, pos: usize) -> LineCol {
        self.data.as_str().position_repr(pos)
    }
}
