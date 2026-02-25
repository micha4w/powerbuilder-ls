use std::{backtrace::Backtrace, sync::Arc};

use super::{token_iter::TokenIter, types::*};
use crate::{
    tokenizer::{Token, TokenType, Tokenizer},
    types::*,
};

pub struct Parser<I>
where
    I: Iterator<Item = char>,
{
    pub(super) tokens: TokenIter<Tokenizer<I>>,

    syntax_errors: Vec<Diagnostic>,
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(iter: I, uri: Arc<Url>) -> Self {
        Parser {
            tokens: TokenIter::new(Tokenizer::new(iter, uri)),
            syntax_errors: Vec::new(),
        }
    }

    pub fn new_file(iter: I, uri: Arc<Url>) -> Self {
        let mut tokenizer = Tokenizer::new(iter, uri);
        tokenizer.skip_headers();
        Parser {
            tokens: TokenIter::new(tokenizer),
            syntax_errors: Vec::new(),
        }
    }

    pub(super) fn uri(&self) -> Arc<Url> {
        self.tokens.underlying().uri.clone()
    }

    pub(super) fn consume_line(&mut self) -> EOFOr<()> {
        loop {
            if self.tokens.next()?.token_type == TokenType::NEWLINE {
                break Some(());
            }
        }
    }

    pub(super) fn hint(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Hint,
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
    }

    pub(super) fn error(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Error,
            // message: format!("[Parser] {}", error),
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
        // panic!("[Parser] {} {:?}\n", error, range);
    }

    pub(super) fn fatal<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
    ) -> EOFOr<Result<T, ParseError>> {
        self.error(error, range);
        if consume_line {
            self.consume_line()?;
        }
        Some(Err(ParseError::UnexpectedToken))
    }

    pub(super) fn fatal_res<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
        value: Option<T>,
    ) -> EOFOrParserResult<T> {
        self.fatal::<T>(error, range, consume_line)?.ok();
        Some(Err((ParseError::UnexpectedToken, value)))
    }

    pub(super) fn optional(&mut self, token_type: TokenType) -> EOFOr<Option<Token>> {
        if self.tokens.peek()?.token_type == token_type {
            Some(Some(self.tokens.next()?))
        } else {
            Some(None)
        }
    }

    pub(super) fn expect(&mut self, token_type: TokenType) -> EOFOr<Result<Token, ParseError>> {
        let token = self.tokens.next()?;
        if token.token_type == token_type {
            Some(Ok(token))
        } else {
            self.fatal(
                &format!("Expected {:?}", token_type),
                token.range,
                token.token_type != TokenType::NEWLINE,
            )
        }
    }

    pub(super) fn optional_newline(&mut self) -> EOFOr<Option<Token>> {
        if self.tokens.peek()?.token_type == TokenType::NEWLINE {
            Some(Some(self.tokens.next()?))
        } else {
            Some(None)
        }
    }

    pub(super) fn expect_newline(&mut self) -> EOFOr<Result<Token, ParseError>> {
        assert!(!self.tokens.ignore_literal_newlines);

        let token = self.tokens.next()?;
        // let Some(token) = self.tokens.next() else {
        //     return Some(Ok(Token {
        //         token_type: TokenType::NEWLINE,
        //         content: "".into(),
        //         range: Range::new_point(
        //             self.tokens
        //                 .prev()
        //                 .expect("no newlines at start of file")
        //                 .range
        //                 .end,
        //             self.uri(),
        //         ),
        //         error: None,
        //     }));
        // };

        if token.token_type == TokenType::NEWLINE {
            while self.optional_newline().is_some_and(|t| t.is_some()) {}
            Some(Ok(token))
        } else {
            self.fatal(
                &format!("Expected NEWLINE"),
                token.range,
                token.token_type != TokenType::NEWLINE,
            )
        }
    }

    pub(super) fn previous_comments(&self) -> EOFOr<Option<String>> {
        let mut help = None;
        for comment in self.tokens.comments() {
            if comment.content.starts_with("/*") {
                let mut content = comment.content.clone();
                content.pop();
                content.pop();
                let lines = content[2..].split("\n").collect::<Vec<_>>();

                let mut all_asterisk = true;
                let mut space_after_asterisk = true;
                let mut min_length = None;
                lines.iter().for_each(|line| {
                    if let Some((i, c)) = line.chars().enumerate().find(|(_, c)| c != &' ') {
                        if min_length.is_none_or(|l| i < l) {
                            min_length = Some(i)
                        }
                        all_asterisk &= c == '*';
                        if let Some(c) = lines.get(i + 1) {
                            space_after_asterisk &= c == &" ";
                        }
                    } else {
                        all_asterisk = false;
                    }
                });

                let Some(mut n) = min_length else { continue };
                if all_asterisk {
                    n += 1;
                    if space_after_asterisk {
                        n += 1;
                    }
                }

                let new_lines = lines
                    .iter()
                    .map(|line| line.get(n..).unwrap_or("").to_string())
                    .collect::<Vec<_>>()
                    .join("\n");

                match &mut help {
                    None => help = Some(new_lines),
                    Some(h) => {
                        h.push('\n');
                        h.push_str(new_lines.as_str());
                    }
                }
            } else {
                assert!(comment.content.starts_with("//"));

                let new_line = comment.content[2..].trim();

                match &mut help {
                    None => help = Some(new_line.to_string()),
                    Some(h) => {
                        h.push('\n');
                        h.push_str(new_line);
                    }
                }
            }
        }

        Some(help)
    }

    pub(super) fn id_or_invalid(
        &mut self,
        err: &Option<ParseError>,
        range: &mut Range,
    ) -> EOFOr<Token> {
        if err.is_none() {
            match self.optional(TokenType::ID)? {
                Some(token) => {
                    range.end = token.range.end;
                    return Some(token);
                }
                None => {
                    let token = self.tokens.peek()?;
                    self.error(&"Expected an ID".into(), token.range.clone());
                }
            }
        }

        Some(Token {
            range: self.tokens.peek()?.range.expanded(
                self.tokens
                    .prev()
                    .map_or(&Position::default(), |token| &token.range.end),
            ),
            token_type: TokenType::INVALID,
            content: String::new(),
            error: None,
        })
    }

    pub fn parse_tokens(&mut self) -> Vec<TopLevel> {
        let mut top_levels = Vec::new();
        loop {
            match self.parse_top_level() {
                None => break,
                Some(Some(top_level)) => {
                    // println!("{:?}", top_level);
                    top_levels.push(top_level);

                    // for error in &self.syntax_errors {
                    //     println!("{} - {}", error.range, error.message)
                    // }
                    // self.syntax_errors = Vec::new();
                }
                Some(None) => {
                    // println!("Unexpected Token");
                }
            }
        }

        // for error in &self.syntax_errors {
        //     println!("{} - {}", error.range, error.message)
        // }

        top_levels
    }

    pub fn get_syntax_errors(self) -> Vec<Diagnostic> {
        self.syntax_errors
    }
}
