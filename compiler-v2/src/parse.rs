
use crate::{grammar, char_iter::CharIter, src::{Src, Logger, Span, Message, Level}};
use unicode_xid::UnicodeXID;

fn closing_paren(ch: char) -> char {
    match ch {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _   => unreachable!(),
    }
}

pub enum TokenKind<'s> {
    Keyword,
    Ident,
    Punct,
    Op,
    Int(i64),
    Float(f64),
    String(String),
    Parentheses(Vec<Token<'s>>),
    Brackets(Vec<Token<'s>>),
    Braces(Vec<Token<'s>>),
    Error(String),
}

pub struct Token<'s> {
    kind: TokenKind<'s>,
    raw: &'s str,
    span: Span<'s>,
}

pub struct Parser<'s, 'g> {
    src: &'s Src,
    iter: CharIter<'s>,
    grammar: &'g grammar::GrammarFile<'g>,
    logger: Logger,
}

impl<'s, 'g> Parser<'s, 'g> {
    pub fn new(src: &'s Src, grammar: &'g grammar::GrammarFile<'g>, logger: Logger) -> Self {
        Self { src, iter: src.iter(), grammar, logger, }
    }
}

impl<'s, 'g> Iterator for Parser<'s, 'g> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! parse {
            (next $cond: ident $(, $second: ident)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| self.iter.nth(2)).is_some()
            };
            (next $cond: ident $(, $second: pat)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| self.iter.nth(2)).is_some()
            };
            (next $cond: pat $(, $second: ident)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| self.iter.nth(2)).is_some()
            };
            (next $cond: pat $(, $second: pat)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| self.iter.nth(2)).is_some()
            };
            (next_while $cond: ident) => { {
                let mut some = false;
                while parse!(peek $cond) {
                    self.iter.next();
                    some = true;
                }
                some
            } };
            (next_while $cond: pat) => { {
                let mut some = false;
                while parse!(peek $cond) {
                    self.iter.next();
                    some = true;
                }
                some
            } };
            (peek $first: ident $(, $second: ident)? $(,)?) => {
                self.iter.peek().is_some_and(|c| c.$first()) $(&& self.iter.peek1().is_some_and(|c| c.$second()))?
            };
            (peek $first: ident $(, $second: pat)? $(,)?) => {
                self.iter.peek().is_some_and(|c| c.$first()) $(&& self.iter.peek1().is_some_and(|c| matches!(c, $second)))?
            };
            (peek $first: pat $(, $second: ident)? $(,)?) => {
                self.iter.peek().is_some_and(|c| matches!(c, $first)) $(&& self.iter.peek1().is_some_and(|c| c.$second()))?
            };
            (peek $first: pat $(, $second: pat)? $(,)?) => {
                self.iter.peek().is_some_and(|c| matches!(c, $first)) $(&& self.iter.peek1().is_some_and(|c| matches!(c, $second)))?
            };
        }

        macro_rules! skip_ws {
            () => {
                loop {
                    // Ignore comments
                    if parse!(next '/', '/') {
                        for c in &mut self.iter {
                            if c == '\n' {
                                break;
                            }
                        }
                        continue;
                    }
                    // Continue skipping until we encounter a non-whitespace character
                    if parse!(next is_whitespace) {
                        continue;
                    }
                    break;
                }
            };
        }

        // Skip whitespace & check for EOF
        skip_ws!();
        self.iter.peek()?;

        // Store first non-WS position for range of token
        let start = self.iter.offset();

        macro_rules! raw {
            () => {
                &self.iter.src_str()[start..=self.iter.offset()]
            };
        }

        macro_rules! make_token {
            ($kind: expr) => { {
                let end = self.iter.offset();
                let raw = &self.iter.src_str()[start..=end];
                Some(Token { kind: $kind, raw, span: Span(self.src, start..=end) })
            } };
        }

        // Identifier or keyword
        if parse!(next is_xid_start) {
            parse!(next_while is_xid_continue);
            let raw = raw!();
            if self.grammar.keywords.strict.contains(raw) {
                return make_token!(TokenKind::Keyword);
            }
            if self.grammar.keywords.reserved.contains(raw) {
                return make_token!(TokenKind::Error(format!("reserved keyword '{raw}'")));
            }
            return make_token!(TokenKind::Ident);
        }

        // Number
        if parse!(next is_ascii_digit) {
            // Eat all digits
            parse!(next_while is_ascii_digit);

            // If there's a .[0-9]+, then it's a float, 
            // otherwise it should be parsed as a member access like 0.abc
            if parse!(next '.', is_ascii_digit) {
                parse!(next_while is_ascii_digit);
                return match raw!().parse::<f64>() {
                    Ok(num) => make_token!(TokenKind::Float(num)),
                    Err(e) => make_token!(TokenKind::Error(format!("invalid float ({e}"))),
                };
            }
            else {
                return match raw!().parse::<i64>() {
                    Ok(num) => make_token!(TokenKind::Int(num)),
                    Err(e) => make_token!(TokenKind::Error(format!("invalid integer ({e}"))),
                };
            }
        }

        // String
        if parse!(next '"') {
            let mut escaped = String::new();
            while match self.iter.next() {
                Some('"') => {
                    false
                }
                Some(c) => {
                    escaped.push(match c {
                        '\\' => match self.iter.next() {
                            Some('n')  => '\n',
                            Some('t')  => '\t',
                            Some('0')  => '\0',
                            Some('r')  => '\r',
                            Some('\\') => '\\',
                            Some('\"') => '\"',
                            Some('\'') => '\'',
                            Some(c) => {
                                self.logger.lock().unwrap()(&Message::new(
                                    Level::Warning,
                                    format!("Invalid escape sequence '\\{c}'"),
                                    Span(self.src, self.iter.offset() - 1..=self.iter.offset())
                                ));
                                c
                            }
                            None => {
                                self.logger.lock().unwrap()(&Message::new(
                                    Level::Warning,
                                    "Expected escape sequence",
                                    Span(self.src, self.iter.offset() - 1..=self.iter.offset())
                                ));
                                '\\'
                            }
                        },
                        o => o
                    });
                    true
                }
                None => {
                    return make_token!(TokenKind::Error("unclosed string literal".to_string()));
                }
            } {}
            return make_token!(TokenKind::String(escaped))
        }

        // Punctuation
        if
            // Chained
            parse!(next_while '.') || parse!(next_while ':') ||
            // Single
            parse!(next ',' | ';' | '@') ||
            // Arrows
            parse!(next '-' | '=', '>')
        {
            return make_token!(TokenKind::Punct);
        }

        // Operators
        if parse!(next_while '=' | '+' | '-' | '/' | '%' | '&' | '|' | '^' | '*' | '~' | '!' | '?' | '<' | '>' | '#') {
            return make_token!(TokenKind::Op);
        }

        // Parentheses
        let opening = self.iter.peek().unwrap();
        if parse!(next '(' | '[' | '{') {
            let mut tree = vec![];
            'find_closing: loop {
                skip_ws!();
                match self.iter.peek() {
                    Some(c @ (')' | ']' | '}')) if c == closing_paren(opening) => {
                        self.iter.next();
                        break 'find_closing;
                    },
                    Some(_) => {}
                    None => return make_token!(TokenKind::Error("unclosed parenthesis".to_string())),
                }
                tree.push(self.next().unwrap());
            }
            return make_token!(match opening {
                '(' => TokenKind::Parentheses(tree),
                '[' => TokenKind::Brackets(tree),
                '{' => TokenKind::Braces(tree),
                _ => unreachable!(),
            });
        }

        let c = self.iter.next().unwrap();
        make_token!(TokenKind::Error(format!("invalid character '{c}'")))
    }
}
