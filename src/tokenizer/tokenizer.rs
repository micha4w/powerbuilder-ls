use std::{str::FromStr, sync::Arc};

use multipeek::{multipeek, MultiPeek};
use strum::Display;

use super::types::*;
use crate::types::*;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum TokenType {
    ScopeModif(ScopeModif),
    AccessType(AccessType),
    Keyword(Keyword),

    Literal(Literal),
    Symbol(Symbol),
    Operator(Operator),
    IncrDecrOperator(IncrDecrOperator),
    SpecialAssignment(SpecialAssignment),

    ID,
    COMMENT,
    NEWLINE,
    INVALID,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub content: String,
    pub range: Range,
    // TODO: change this to a Option<Diagnostic>?
    pub error: Option<String>,
}

pub struct Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    chars: MultiPeek<I>,

    buildup: String,
    pub uri: Arc<Url>,

    pos: Position,
    next: Option<Token>,
    previous: Token,
}

impl<I: Iterator<Item = char>> Tokenizer<I> {
    pub fn new(iter: I, uri: Url) -> Self {
        let uri = Arc::new(uri);
        Self {
            chars: multipeek(iter),
            buildup: String::new(),
            pos: Position { line: 0, column: 0 },
            next: None,
            uri: uri.clone(),

            previous: Token {
                token_type: TokenType::INVALID,
                content: String::new(),
                range: Range::empty(uri),
                error: None,
            },
        }
    }

    pub fn skip_headers(&mut self) -> Option<()> {
        if self.chars.peek_nth(0)? == &'H' && self.chars.peek_nth(1)? == &'A' {
            self.next();
            self.next();
        }

        while self.chars.peek()? == &'$' {
            while self.next()? != '\n' {}
        }

        Some(())
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        self.pos.column += 1;
        self.buildup.push(c);

        if c == '\n' {
            self.pos.line += 1;
            self.pos.column = 0;
        }

        Some(c)
    }

    fn skip_white_spaces(&mut self) -> Option<()> {
        loop {
            let c = self.chars.peek()?;
            match c {
                ' ' | '\t' | '\r' => {
                    self.next();
                }
                '&' => {
                    self.next();
                    loop {
                        match self.chars.peek()? {
                            ' ' | '\t' | '\r' => {
                                self.next();
                            }
                            '\n' => {
                                self.next();
                                break;
                            }
                            _ => return Some(()),
                        };
                    }
                }
                _ => return Some(()),
            }
        }
    }

    fn keyword_to_token(&self, token: &String) -> TokenType {
        if let Ok(token_type) = Keyword::from_str(token) {
            return TokenType::Keyword(token_type);
        }
        if let Ok(token_type) = ScopeModif::from_str(token) {
            return TokenType::ScopeModif(token_type);
        }
        if let Ok(token_type) = AccessType::from_str(token) {
            return TokenType::AccessType(token_type);
        }

        match token.as_str() {
            "CONST" => TokenType::Keyword(Keyword::CONSTANT),

            "AND" => TokenType::Operator(Operator::AND),
            "OR" => TokenType::Operator(Operator::OR),
            "FALSE" | "TRUE" => TokenType::Literal(Literal::BOOLEAN),

            _ => TokenType::ID,
        }
    }

    fn get_token(&mut self) -> Option<Token> {
        self.buildup.clear();

        let start_pos = self.pos;
        let mut error = None;
        let token_type = match self.next()? {
            '\n' => TokenType::NEWLINE,
            ';' => TokenType::Symbol(Symbol::SEMICOLON),
            c if c.is_alphabetic() || c == '_' => loop {
                match self.chars.peek() {
                    Some('_' | '$' | '#' | '%' | '-') => {
                        self.next();
                    }
                    Some(c) if c.is_alphanumeric() => {
                        self.next();
                    }
                    Some('!') => {
                        self.next();
                        break TokenType::Literal(Literal::ENUM);
                    }
                    _ => break self.keyword_to_token(&self.buildup.to_uppercase()),
                };
            },
            c @ ('.' | '0'..='9') => {
                enum Stage {
                    Number,
                    Fraction,
                    Exponent,
                }
                let mut stage = Stage::Number;
                let token_type = if c == '.' {
                    match self.chars.peek() {
                        Some('0'..='9') => {
                            stage = Stage::Fraction;
                            None
                        }
                        Some('.') => {
                            if self.chars.peek_nth(1) == Some(&'.') {
                                self.next();
                                self.next();
                                Some(TokenType::Symbol(Symbol::DOTDOTDOT))
                            } else {
                                None
                            }
                        }
                        _ => Some(TokenType::Symbol(Symbol::DOT)),
                    }
                } else {
                    let next = |this: &mut Self, n: usize| *this.chars.peek_nth(n).unwrap_or(&'\0');
                    if ('0'..='9').contains(&next(self, 0))
                        && ('0'..='9').contains(&next(self, 1))
                        && ('0'..='9').contains(&next(self, 2))
                        && '-' == next(self, 3)
                        && ('0'..='9').contains(&next(self, 4))
                        && ('0'..='9').contains(&next(self, 5))
                        && '-' == next(self, 6)
                        && ('0'..='9').contains(&next(self, 7))
                        && ('0'..='9').contains(&next(self, 8))
                    {
                        for _ in 0..=8 {
                            self.next();
                        }
                        Some(TokenType::Literal(Literal::DATE))
                    } else if ('0'..='9').contains(&next(self, 0))
                        && ':' == next(self, 1)
                        && ('0'..='9').contains(&next(self, 2))
                        && ('0'..='9').contains(&next(self, 3))
                        && ':' == next(self, 4)
                        && ('0'..='9').contains(&next(self, 5))
                        && ('0'..='9').contains(&next(self, 6))
                    {
                        for _ in 0..=6 {
                            self.next();
                        }

                        if self.chars.peek() == Some(&'.') {
                            self.next();
                            loop {
                                match self.chars.peek() {
                                    Some('0'..='9') => {
                                        self.next();
                                    }
                                    _ => break,
                                }
                            }
                        }

                        Some(TokenType::Literal(Literal::TIME))
                    } else {
                        None
                    }
                };

                match token_type {
                    Some(token_type) => token_type,
                    None => loop {
                        match self.chars.peek() {
                            Some('0'..='9') => {
                                self.next();
                            }
                            Some('.') => {
                                self.next();
                                match stage {
                                    Stage::Number => stage = Stage::Fraction,
                                    Stage::Fraction => error = Some("Multiple Dots inside Number"),
                                    Stage::Exponent => {
                                        error = Some("Not allowed to have fractional Exponent")
                                    }
                                }
                            }
                            Some('e' | 'E') => {
                                match stage {
                                    Stage::Number | Stage::Fraction => stage = Stage::Exponent,
                                    Stage::Exponent => {
                                        error = Some("Multiple exponents inside Number")
                                    }
                                }
                                match self.chars.peek() {
                                    Some('-' | '+') => {
                                        self.next();
                                        match self.chars.peek() {
                                            Some('0'..='9') => {
                                                self.next();
                                            }
                                            _ => error = Some("Exponent Missing"),
                                        }
                                    }
                                    _ => (),
                                }
                            }
                            // Ok('d' | 'D' | 'f' | 'F') => {
                            //     self.seek()?;
                            //     break TokenType::Literal(Literal::NUMBER);
                            // }
                            _ => break TokenType::Literal(Literal::NUMBER),
                        }
                    },
                }
            }
            del @ ('\'' | '"') => loop {
                match self.chars.peek() {
                    Some(&c @ ('\'' | '"')) => {
                        self.next();
                        if del == c {
                            break TokenType::Literal(Literal::STRING);
                        }
                    }
                    Some('~') => match self.chars.peek() {
                        Some('\n') => {}
                        _ => {
                            self.next();
                        }
                    },
                    Some('&') => {
                        self.next();
                        loop {
                            match self.chars.peek()? {
                                ' ' | '\t' | '\r' => {
                                    self.next();
                                }
                                '\n' => {
                                    self.next();
                                    break;
                                }
                                _ => break,
                            };
                        }
                    }
                    Some('\n') => {
                        error = Some("String not closed");
                        break TokenType::Literal(Literal::STRING);
                    }
                    _ => {
                        self.next();
                    }
                }
            },
            '/' => match self.chars.peek() {
                Some('/') => {
                    self.next();
                    loop {
                        match self.chars.peek() {
                            Some('\n') | None => break,
                            _ => {
                                self.next();
                            }
                        }
                    }
                    TokenType::COMMENT
                }
                Some('*') => {
                    self.next();
                    loop {
                        match self.next() {
                            Some('*') => {
                                if self.next() == Some('/') {
                                    break;
                                }
                            }
                            None => error = Some("Multiline comment not closed"),
                            _ => {}
                        }
                    }
                    TokenType::COMMENT
                }
                Some('=') => {
                    self.next();
                    TokenType::SpecialAssignment(SpecialAssignment::DIVEQ)
                }
                _ => TokenType::Operator(Operator::DIV),
            },
            '*' => match self.chars.peek() {
                Some('=') => {
                    self.next();
                    TokenType::SpecialAssignment(SpecialAssignment::MULTEQ)
                }
                _ => TokenType::Operator(Operator::MULT),
            },
            '+' => match self.chars.peek() {
                Some('=') => {
                    self.next();
                    TokenType::SpecialAssignment(SpecialAssignment::PLUSEQ)
                }
                Some('+') => {
                    self.next();
                    TokenType::IncrDecrOperator(IncrDecrOperator::PLUSPLUS)
                }
                _ => TokenType::Operator(Operator::PLUS),
            },
            '-' => match self.chars.peek() {
                Some('=') => {
                    self.next();
                    TokenType::SpecialAssignment(SpecialAssignment::MINUSEQ)
                }
                Some('-') => {
                    self.next();
                    TokenType::IncrDecrOperator(IncrDecrOperator::MINUSMINUS)
                }
                _ => TokenType::Operator(Operator::MINUS),
            },
            '<' => match self.chars.peek() {
                Some('=') => {
                    self.next();
                    TokenType::Operator(Operator::LTE)
                }
                Some('>') => {
                    self.next();
                    TokenType::Operator(Operator::GTLT)
                }
                _ => TokenType::Operator(Operator::LT),
            },
            '>' => match self.chars.peek() {
                Some('=') => {
                    self.next();
                    TokenType::Operator(Operator::GTE)
                }
                _ => TokenType::Operator(Operator::GT),
            },
            ':' => match self.chars.peek() {
                Some(':') => {
                    self.next();
                    TokenType::Symbol(Symbol::COLONCOLON)
                }
                _ => TokenType::Symbol(Symbol::COLON),
            },
            '^' => TokenType::Operator(Operator::CARAT),
            '=' => TokenType::Operator(Operator::EQ),

            '{' => TokenType::Symbol(Symbol::LCURLY),
            '}' => TokenType::Symbol(Symbol::RCURLY),
            '(' => TokenType::Symbol(Symbol::LPAREN),
            ')' => TokenType::Symbol(Symbol::RPAREN),
            '[' => TokenType::Symbol(Symbol::LBRACE),
            ']' => TokenType::Symbol(Symbol::RBRACE),
            ',' => TokenType::Symbol(Symbol::COMMA),
            '`' => TokenType::Symbol(Symbol::TICK),
            '@' => TokenType::Symbol(Symbol::AT),
            _ => TokenType::INVALID,
        };

        Some(Token {
            token_type,
            content: self.buildup.clone(),
            range: Range {
                start: start_pos,
                end: self.pos,
                uri: self.uri.clone(),
            },
            error: error.map(str::to_owned),
        })
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.next.take() {
            self.previous = token.clone();
            return Some(token);
        }

        let ret = loop {
            self.skip_white_spaces()?;
            let mut token = self.get_token()?;
            // println!("got: {token:?}");
            // println!("had: {:?}", self.previous.token_type);

            // Because some SQL Statements can also be used as function names
            // e.g. The SQL OPEN Cursor;  keyword can appear in the open(aw_window) function
            // But if an '(' appears after the keyword, we know its a function, otherwise its a the keyword.
            macro_rules! non_reserved_sql_keyword {
                () => {
                    TokenType::Keyword(Keyword::OPEN | Keyword::CLOSE | Keyword::UPDATE)
                };
            }

            match (self.previous.token_type, token.token_type) {
                (_, TokenType::COMMENT) => continue,
                (TokenType::NEWLINE, TokenType::NEWLINE) => continue,

                // Because nicer
                (
                    TokenType::Symbol(Symbol::COLONCOLON),
                    TokenType::Keyword(Keyword::CREATE | Keyword::DESTROY),
                ) => {
                    token.token_type = TokenType::ID;
                }

                (TokenType::NEWLINE, non_reserved_sql_keyword!()) => {
                    self.previous = token;
                    // println!("condition newline");
                    continue;
                }
                (_, non_reserved_sql_keyword!()) => {
                    token.token_type = TokenType::ID;
                }
                (non_reserved_sql_keyword!(), cur) => {
                    self.next = Some(token);
                    // println!("double open/close");

                    if let TokenType::Symbol(Symbol::LPAREN) = cur {
                        // println!("condition lbrace");
                        self.previous.token_type = TokenType::ID;
                    }
                    break Some(self.previous.clone());
                }
                _ => {}
            };

            self.previous = token.clone();
            break Some(token);
        };

        // println!("ret: {ret:?}");
        return ret;
    }
}
