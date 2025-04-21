use std::{
    backtrace::Backtrace,
    cell::RefCell,
    path::{Path, PathBuf}, sync::Arc,
};

use super::{token_iter::TokenIter, types::*};
use crate::{
    tokenizer::{self, tokenize, tokenize_file, FileTokenizer, Token, TokenType},
    types::*,
};

macro_rules! quick_exit {
    ( $func:expr, $err:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err((err, Some(ret))) => {
                $err = Some(err);
                ret
            }
            Err((err, None)) => return Some(Err((err, None))),
        }
    };
    ( $func:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err((err, _)) => return Some(Err((err, None))),
        }
    };
}
macro_rules! quick_exit_opt {
    ( $func:expr, $err:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err((err, Some(ret))) => {
                $err = Some(err);
                ret
            }
            Err(_) => return Some(None),
        }
    };
    ( $func:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err((_, _)) => return Some(None),
        }
    };
}

macro_rules! quick_exit_simple {
    ( $func:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err(err) => return Some(Err((err, None))),
        }
    };
}

macro_rules! quick_exit_simple_opt {
    ( $func:expr ) => {
        match $func {
            Ok(ret) => ret,
            Err(_) => return Some(None),
        }
    };
}

pub(crate) use quick_exit;
pub(crate) use quick_exit_opt;
pub(crate) use quick_exit_simple;
pub(crate) use quick_exit_simple_opt;

pub struct Parser {
    pub(crate) tokens: TokenIter<FileTokenizer>,

    syntax_errors: Vec<Diagnostic>,
}

impl Parser {
    pub fn new(tokenizer: FileTokenizer) -> Parser {
        Parser {
            tokens: TokenIter::new(tokenizer),
            syntax_errors: Vec::new(),
        }
    }
    pub fn new_from_file(file: &Path) -> anyhow::Result<Parser> {
        Ok(Parser::new(tokenize_file(file)?))
    }
    pub fn new_from_string(buf: &String, uri: PathBuf) -> anyhow::Result<Parser> {
        Ok(Parser::new(tokenize(buf, uri)))
    }

    pub(crate) fn uri(&self) -> Arc<PathBuf> {
        self.tokens.underlying().uri.clone()
    }

    pub(crate) fn consume_line(&mut self) -> EOFOr<()> {
        loop {
            match self.tokens.next()?.token_type {
                TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                    break Some(())
                }
                _ => {}
            }
        }
    }

    pub(crate) fn hint(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Hint,
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
    }

    pub(crate) fn error(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Error,
            // message: format!("[Parser] {}", error),
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
        // panic!("[Parser] {} {:?}\n", error, range);
    }

    pub(crate) fn fatal<T>(
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

    pub(crate) fn fatal_res<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
        value: Option<T>,
    ) -> EOFOrParserResult<T> {
        self.fatal::<T>(error, range, consume_line)?.ok();
        Some(Err((ParseError::UnexpectedToken, value)))
    }

    pub(crate) fn optional(&mut self, token_type: TokenType) -> EOFOr<Option<Token>> {
        if self.tokens.peek()?.token_type == token_type {
            Some(Some(self.tokens.next()?))
        } else {
            Some(None)
        }
    }

    pub(crate) fn expect(&mut self, token_type: TokenType) -> EOFOr<Result<Token, ParseError>> {
        let token = self.tokens.next()?;
        if token.token_type == token_type {
            Some(Ok(token))
        } else {
            self.fatal(
                &format!("Expected {:?}", token_type),
                token.range,
                matches!(
                    token.token_type,
                    TokenType::NEWLINE | TokenType::Symbol(crate::tokenizer::Symbol::SEMICOLON)
                ),
            )
        }
    }

    pub(crate) fn optional_newline(&mut self) -> EOFOr<Option<Token>> {
        if let TokenType::NEWLINE | TokenType::Symbol(crate::tokenizer::Symbol::SEMICOLON) =
            self.tokens.peek()?.token_type
        {
            Some(Some(self.tokens.next()?))
        } else {
            Some(None)
        }
    }

    pub(crate) fn expect_newline(&mut self) -> EOFOr<Result<Token, ParseError>> {
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
