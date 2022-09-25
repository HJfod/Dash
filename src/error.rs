use enum_display_derive::Display;
use std::{fmt::{Display, Debug}};
use peg::{error::ParseError, str::LineCol};
use crate::utils::File;
use colored::Colorize;

#[derive(Display, Clone, PartialEq, Debug)]
pub enum Severity {
    Debug,
    Warning,
    Error,
    Fatal,
}

#[derive(Display, Clone, PartialEq, Debug)]
pub enum ErrorCode {
    Unknown,
    CantReadFile,
    SyntaxError,
    TypeError,
    CompileError,
}

pub trait Error : Display {
    fn severity(&self) -> Severity;
    fn code(&self) -> ErrorCode;
    fn file(&self) -> Option<Box<File>>;
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleError {
    pub code: ErrorCode,
    pub info: String,
    pub severity: Severity,
}

impl Error for SimpleError {
    fn severity(&self) -> Severity {
        self.severity.to_owned()
    }

    fn code(&self) -> ErrorCode {
        self.code.to_owned()
    }

    fn file(&self) -> Option<Box<File>> {
        None
    }
}

impl Display for SimpleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.info)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CompileError {
    pub file: Box<File>,
    pub info: String,
    pub note: Option<String>,
    pub hint: Option<String>,
}

impl CompileError {
    pub fn to_line_err(&self, from: LineCol, to: LineCol) -> LineError {
        LineError {
            file: self.file.to_owned(),
            code: ErrorCode::CompileError,
            info: self.info.to_owned(),
            note: self.note.to_owned(),
            hint: self.hint.to_owned(),
            severity: Severity::Error,
            from, to
        }
    }

    pub fn to_line_type_err(&self, from: LineCol, to: LineCol) -> LineError {
        LineError {
            file: self.file.to_owned(),
            code: ErrorCode::TypeError,
            info: self.info.to_owned(),
            note: self.note.to_owned(),
            hint: self.hint.to_owned(),
            severity: Severity::Error,
            from, to
        }
    }

    pub fn to_line_err_in(&self, from: usize, to: usize) -> LineError {
        LineError {
            file: self.file.to_owned(),
            code: ErrorCode::CompileError,
            info: self.info.to_owned(),
            note: self.note.to_owned(),
            hint: self.hint.to_owned(),
            severity: Severity::Error,
            from: self.file.linecol_at(from),
            to: self.file.linecol_at(to)
        }
    }
}

impl Error for CompileError {
    fn code(&self) -> ErrorCode {
        ErrorCode::CompileError
    }

    fn file(&self) -> Option<Box<File>> {
        Some(self.file.to_owned())
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.info.as_str())?;
        if let Some(note) = &self.note {
            f.write_str(format!("\nNote: {}", note).as_str())?;
        }
        if let Some(hint) = &self.hint {
            f.write_str(format!("\nHint: {}", hint).as_str())?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LineError {
    pub file: Box<File>,
    pub code: ErrorCode,
    pub info: String,
    pub note: Option<String>,
    pub hint: Option<String>,
    pub severity: Severity,
    pub from: LineCol,
    pub to: LineCol,
}

impl LineError {
    pub fn from(file: &Box<File>, error: &ParseError<LineCol>) -> LineError {
        LineError {
            file: file.to_owned(),
            code: ErrorCode::SyntaxError,
            info: format!("Expected {}", error.expected.to_string()),
            note: None,
            hint: None,
            severity: Severity::Error,
            from: error.location.clone(),
            to: error.location.clone()
        }
    }
}

impl Error for LineError {
    fn severity(&self) -> Severity {
        self.severity.to_owned()
    }

    fn code(&self) -> ErrorCode {
        self.code.to_owned()
    }

    fn file(&self) -> Option<Box<File>> {
        Some(self.file.to_owned())
    }
}

impl Display for LineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.info.as_str())?;
        f.write_str("\n")?;
        f.write_fmt(format_args!("{}", self.file.lines_squiggled(&self.from, &self.to)))?;
        if let Some(note) = &self.note {
            f.write_str(format!("\n{}: {}", "Note".cyan().bold(), note).as_str())?;
        }
        if let Some(hint) = &self.hint {
            f.write_str(format!("\n{}: {}", "Hint".green().bold(), hint).as_str())?;
        }
        Ok(())
    }
}

pub trait ToLineResult<T> {
    fn to_line(self, from: LineCol, to: LineCol) -> Result<T, LineError>;
    fn to_line_as_type(self, from: LineCol, to: LineCol) -> Result<T, LineError>;
}

impl<T> ToLineResult<T> for Result<T, CompileError> {
    fn to_line(self, from: LineCol, to: LineCol) -> Result<T, LineError> {
        self.map_err(|e| e.to_line_err(from, to))
    }

    fn to_line_as_type(self, from: LineCol, to: LineCol) -> Result<T, LineError> {
        self.map_err(|e| e.to_line_type_err(from, to))
    }
}
