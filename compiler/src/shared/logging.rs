
use std::fmt::Display;
use crate::parser::node::Span;
use super::src::{Src, Range};

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
            Level::Info => "Info",
            Level::Warning => "Warning",
            Level::Error => "Error",
        })
    }
}

#[derive(Debug)]
pub struct Note<'s> {
    info: String,
    at: Option<(&'s Src, Range)>,
}

impl<'s> Note<'s> {
    pub fn new(info: &str) -> Self {
        Self {
            info: info.into(),
            at: None,
        }
    }

    pub fn new_at<S: Into<String>>(info: S, src: &'s Src, range: Range) -> Self {
        Self {
            info: info.into(),
            at: Some((src, range)),
        }
    }

    pub fn from_span<S: Into<String>>(info: S, span: &Span<'s>) -> Self {
        Self {
            info: info.into(),
            at: Some((span.src, span.range.clone())),
        }
    }
}

impl Display for Note<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((ref src, ref range)) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {} at {})",
                self.info,
                src.underlined(&range),
                src.name(),
                range
            ))
        } else {
            f.write_fmt(format_args!("Note: {}", self.info))
        }
    }
}

#[derive(Debug)]
pub struct Message<'s> {
    pub level: Level,
    pub info: String,
    pub notes: Vec<Note<'s>>,
    pub src: &'s Src,
    pub range: Range,
}

impl<'s> Message<'s> {
    pub fn from_span(level: Level, info: String, meta: &Span<'s>) -> Self {
        Self {
            level,
            info,
            notes: vec![],
            src: meta.src,
            range: meta.range.clone(),
        }
    }

    pub fn note(mut self, note: Note<'s>) -> Self {
        self.notes.push(note);
        self
    }

    pub fn note_if(mut self, note: Option<Note<'s>>) -> Self {
        if let Some(note) = note {
            self.notes.push(note);
        }
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
            self.level,
            self.src.name(),
            self.range,
            self.src.underlined(&self.range),
            self.info,
            self.notes
                .iter()
                .map(|note| format!("\n * {}", note))
                .collect::<String>()
        ))
    }
}

pub trait Logger<'s> {
    fn log_msg(&self, msg: Message<'s>);
}

pub struct ConsoleLogger;

impl<'s> Logger<'s> for ConsoleLogger {
    fn log_msg(&self, msg: Message<'s>) {
        println!("{msg}");
    }
}
