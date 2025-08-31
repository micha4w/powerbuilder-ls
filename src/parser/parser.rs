use std::{backtrace::Backtrace, sync::Arc};

use super::{token_iter::TokenIter, types::*};
use crate::{
    tokenizer::{self, Token, TokenType, Tokenizer},
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
    pub fn new(iter: I, uri: Url) -> Self {
        Parser {
            tokens: TokenIter::new(Tokenizer::new(iter, uri)),
            syntax_errors: Vec::new(),
        }
    }

    pub fn new_file(iter: I, uri: Url) -> Self {
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
            match self.tokens.next()?.token_type {
                TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                    break Some(())
                }
                _ => {}
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
                !matches!(
                    token.token_type,
                    TokenType::NEWLINE | TokenType::Symbol(crate::tokenizer::Symbol::SEMICOLON)
                ),
            )
        }
    }

    pub(super) fn optional_newline(&mut self) -> EOFOr<Option<Token>> {
        if let TokenType::NEWLINE | TokenType::Symbol(crate::tokenizer::Symbol::SEMICOLON) =
            self.tokens.peek()?.token_type
        {
            Some(Some(self.tokens.next()?))
        } else {
            Some(None)
        }
    }

    pub(super) fn expect_newline(&mut self) -> EOFOr<Result<Token, ParseError>> {
        let token = self.tokens.next()?;
        if let TokenType::NEWLINE | TokenType::Symbol(crate::tokenizer::Symbol::SEMICOLON) =
            token.token_type
        {
            Some(Ok(token))
        } else {
            self.fatal(
                &format!("Expected NEWLINE"),
                token.range,
                token.token_type != TokenType::NEWLINE,
            )
        }
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
                    .as_ref()
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
