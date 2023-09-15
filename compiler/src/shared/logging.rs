
use std::fmt::Display;
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
    pub fn from_span(level: Level, info: String, span: &Span<'s>) -> Self {
        Self {
            level,
            info,
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
