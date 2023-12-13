
use std::{sync::{Arc, Mutex}, fmt::{Display, Write}};
use crate::shared::src::Span;

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Info,
    Warning,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Note<'s> {
    info: String,
    at: Option<Span<'s>>,
}

impl<'s> Note<'s> {
    pub fn new<S: Into<String>>(info: S) -> Self {
        Self { info: info.into(), at: None }
    }
    pub fn new_at<S: Into<String>>(info: S, span: Span<'s>) -> Self {
        Self { info: info.into(), at: Some(span) }
    }
}

impl Display for Note<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref span) = self.at {
            f.write_fmt(format_args!(
                "Note: {}\n{}\n(In {})",
                self.info, span.0.underlined(span.1.clone()), span
            ))
        } else {
            f.write_fmt(format_args!("Note: {}", self.info))
        }
    }
}

#[derive(Debug)]
pub struct Message<'s> {
    pub(crate) level: Level,
    info: String,
    notes: Vec<Note<'s>>,
    span: Span<'s>,
}

impl<'s> Message<'s> {
    pub fn new<S: Into<String>>(level: Level, info: S, span: Span<'s>) -> Self {
        Self { level, info: info.into(), notes: vec![], span }
    }
    pub fn note(mut self, note: Note<'s>) -> Self {
        self.notes.push(note);
        self
    }
}

impl Display for Message<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} at {}:\n{}{}{}",
            self.level,
            self.span,
            self.span.0.underlined(self.span.1.clone()),
            self.info,
            self.notes
                .iter()
                .fold(String::new(), |mut acc, note| {
                    write!(&mut acc, "\n * {note}").unwrap();
                    acc
                })
        ))
    }
}

pub struct Logger {
    logger: Box<dyn FnMut(Message)>,
    error_count: usize,
    warn_count: usize,
}

impl std::fmt::Debug for Logger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Logger")
    }
}

impl Logger {
    pub fn new<F: FnMut(Message) + 'static>(logger: F) -> LoggerRef {
        Arc::from(Mutex::from(Self {
            logger: Box::from(logger),
            error_count: 0,
            warn_count: 0,
        }))
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> LoggerRef {
        Self::new(default_console_logger)
    }
    pub fn log(&mut self, msg: Message) {
        match msg.level {
            Level::Info => {}
            Level::Warning => self.warn_count += 1,
            Level::Error => self.error_count += 1,
        }
        (self.logger)(msg);
    }
    pub fn errors(&self) -> usize {
        self.error_count
    }
    pub fn warnings(&self) -> usize {
        self.warn_count
    }
}

pub(crate) type LoggerRef = Arc<Mutex<Logger>>;

pub fn default_console_logger(msg: Message) {
    println!("{msg}");
}
