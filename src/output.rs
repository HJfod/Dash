
use crate::error::{Error, ErrorCode, Severity};
use std::fmt::{Display, Debug};
use colored::Colorize;

pub struct FmtVec<'a, 'b, T: Display> {
    vec: &'a Vec<T>,
    separator: &'b str,
}

impl<T: Display> FmtVec<'_, '_, T> {
    pub fn new<'a, 'b>(vec: &'a Vec<T>, separator: &'b str) -> FmtVec<'a, 'b, T> {
        FmtVec { vec, separator }
    }
}

impl<T: Display> Display for FmtVec<'_, '_, T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for v in self.vec {
            fmt.write_fmt(format_args!(
                "{}{}",
                if first {
                    first = false;
                    ""
                } else {
                    self.separator
                },
                v
            ))?;
        }
        Ok(())
    }
}

pub trait OutputChannel {
    fn print(&self, error: &dyn Error) -> ErrorCode;
}

#[derive(Clone, PartialEq, Debug)]
pub struct DefaultChannel {}

impl OutputChannel for DefaultChannel {
    fn print(&self, error: &dyn Error) -> ErrorCode {
        println!(
            "---\n{} {}:\n{}",
            error.severity().to_string().color(match error.severity() {
                Severity::Warning => colored::Color::BrightYellow,
                Severity::Error   => colored::Color::BrightRed,
                Severity::Fatal   => colored::Color::Red,
                Severity::Debug   => colored::Color::Cyan,
            }).bold(),
            error.code().to_string().yellow().bold(),
            error
        );
        error.code()
    }
}

pub struct Output {
    channel: &'static dyn OutputChannel
}

impl Clone for Output {
    fn clone(&self) -> Self {
        Output {
            channel: self.channel
        }
    }
}

impl PartialEq for Output {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Debug for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[Output Channel]")
    }
}

impl Default for Output {
    fn default() -> Self {
        Output { channel: &DefaultChannel {} }
    }
}

impl Output {
    pub fn new(channel: &'static dyn OutputChannel) -> Output {
        Output { channel }
    }

    pub fn print<T: Error>(&self, error: &T) -> ErrorCode {
        self.channel.print(error)
    }
}

pub trait ErrorToCode<T> {
    fn print(self, channel: &Output) -> Result<T, ErrorCode>;
}

impl<T, E: Error> ErrorToCode<T> for Result<T, E> {
    fn print(self, channel: &Output) -> Result<T, ErrorCode> {
        self.map_err(|e| channel.print(&e))
    }
}
