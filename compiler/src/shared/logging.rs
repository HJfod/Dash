
use std::{fmt::{Display, Debug, Write}, sync::{Arc, Mutex}};
use super::src::{Span, Spanful};

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
pub struct Note {
    info: String,
    at: Option<Span>,
}

impl Note {
    pub fn new(info: &str) -> Self {
        Self {
            info: info.into(),
            at: None,
        }
    }

    pub fn from_span<S: Into<String>>(info: S, span: &Span) -> Self {
        Self {
            info: info.into(),
            at: Some(span.clone()),
        }
    }
}

impl Display for Note {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref span) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {})",
                self.info,
                span.underlined(),
                span.full_display()
            ))
        } else {
            f.write_fmt(format_args!("Note: {}", self.info))
        }
    }
}

#[derive(Debug)]
pub struct Message {
    pub level: Level,
    pub info: String,
    pub notes: Vec<Note>,
    pub span: Span,
}

impl Message {
    pub fn from_span<S: Into<String>>(level: Level, info: S, span: &Span) -> Self {
        Self {
            level,
            info: info.into(),
            notes: vec![],
            span: span.clone(),
        }
    }

    pub fn note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }

    pub fn note_if(mut self, note: Option<Note>) -> Self {
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

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} at {}:\n{}{}{}",
            self.level,
            self.span.full_display(),
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

pub trait Logger: Debug {
    /// Log a message in this logger
    fn log_msg(&mut self, msg: Message);
    /// Number of warnings sent to this logger
    fn warn_count(&self) -> usize;
    /// Number of errors sent to this logger
    fn error_count(&self) -> usize;
}

pub type LoggerRef = Arc<Mutex<dyn Logger>>;

#[derive(Debug)]
pub struct ConsoleLogger {
    warn_count: usize,
    error_count: usize,
}

impl ConsoleLogger {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> LoggerRef {
        Arc::from(Mutex::new(Self {
            warn_count: 0,
            error_count: 0,
        }))
    }
}

impl Logger for ConsoleLogger {
    fn log_msg(&mut self, msg: Message) {
        println!("{msg}");
    }

    fn warn_count(&self) -> usize {
        self.warn_count
    }

    fn error_count(&self) -> usize {
        self.error_count
    }
}
