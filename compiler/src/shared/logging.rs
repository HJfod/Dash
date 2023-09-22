
use std::{fmt::{Display, Debug, Write}, sync::{Arc, Mutex}};
use super::src::Span;

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
    at: Option<Span<'s>>,
}

impl<'s> Note<'s> {
    pub fn new(info: &str) -> Self {
        Self {
            info: info.into(),
            at: None,
        }
    }

    pub fn from_span<S: Into<String>>(info: S, span: &Span<'s>) -> Self {
        Self {
            info: info.into(),
            at: Some(span.clone()),
        }
    }
}

impl Display for Note<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref span) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {} at {})",
                self.info,
                span.underlined(),
                span.src().name(),
                span
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
    pub span: Span<'s>,
}

impl<'s> Message<'s> {
    pub fn from_span<S: Into<String>>(level: Level, info: S, span: &Span<'s>) -> Self {
        Self {
            level,
            info: info.into(),
            notes: vec![],
            span: span.clone(),
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
            self.span.src().name(),
            self.span,
            self.span.underlined(),
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

pub trait Logger<'s>: Debug {
    /// Log a message in this logger
    fn log_msg(&mut self, msg: Message<'s>);
    /// Number of warnings sent to this logger
    fn warn_count(&self) -> usize;
    /// Number of errors sent to this logger
    fn error_count(&self) -> usize;
}

pub type LoggerRef<'s> = Arc<Mutex<dyn Logger<'s>>>;

#[derive(Debug)]
pub struct ConsoleLogger {
    warn_count: usize,
    error_count: usize,
}

impl ConsoleLogger {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<'s>() -> LoggerRef<'s> {
        Arc::from(Mutex::new(Self {
            warn_count: 0,
            error_count: 0,
        }))
    }
}

impl<'s> Logger<'s> for ConsoleLogger {
    fn log_msg(&mut self, msg: Message<'s>) {
        println!("{msg}");
    }

    fn warn_count(&self) -> usize {
        self.warn_count
    }

    fn error_count(&self) -> usize {
        self.error_count
    }
}
