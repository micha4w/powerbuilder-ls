use std::{collections::VecDeque, fs::File, io::Read, str::FromStr};

use anyhow::Result;
use encoding_rs_io::DecodeReaderBytesBuilder;

use super::types::*;

pub struct FileTokenizer {
    chars: std::vec::IntoIter<char>,
    peek_chars: VecDeque<char>,

    buildup: String,

    pos: Position,
}

impl FileTokenizer {
    pub fn open(path: &str) -> Result<Self> {
        let file = File::open(path)?;
        let mut reader = DecodeReaderBytesBuilder::new().build(file);
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;

        let out = Self {
            chars: buf.chars().collect::<Vec<_>>().into_iter(),
            peek_chars: VecDeque::new(),
            buildup: String::new(),
            pos: Position { line: 0, column: 0 },
        };
        Ok(out)
    }

    fn __get_char(&mut self) -> Option<char> {
        loop {
            match self.chars.next()? {
                '\r' => {}
                c => {
                    break Some(c);
                }
            };
        }
    }

    fn _next(&mut self) -> Option<char> {
        self.peek_chars.pop_front().or_else(|| self.__get_char())
    }

    fn _peek(&mut self, n: usize) -> Option<char> {
        for _ in self.peek_chars.len()..=n {
            let c = self.__get_char()?;
            self.peek_chars.push_back(c);
        }
        Some(self.peek_chars[n])
    }

    fn seek(&mut self) -> char {
        let c = self._next();
        c.map(|c| {
            self.pos.column += 1;
            self.buildup.push(c)
        });
        match c {
            Some('\n') => {
                loop {
                    match self._peek(0) {
                        Some('\n') => {
                            self.pos.line += 1;
                            self.pos.column = 0;
                            self._next();
                        }
                        _ => break,
                    };
                }
                '\n'
            }
            Some(';') | None => '\n',
            Some(' ' | '\t') => loop {
                match self._peek(0) {
                    Some(' ' | '\t') => self._next(),
                    _ => break ' ',
                };
            },
            Some('&') => loop {
                match self._peek(0) {
                    Some(' ' | '\t') => match self._peek(1) {
                        Some(' ' | '\t') => self._next(),
                        _ => break '&',
                    },
                    Some('\n') => {
                        self.chars.next();
                        break ' ';
                    }
                    _ => break '&',
                };
            },
            Some(c) => c.to_owned(),
        }
    }

    fn peek(&mut self) -> char {
        match self._peek(0) {
            Some(';' | '\n') | None => '\n',
            Some(' ' | '\t' | '&') => ' ',
            Some(c) => c.to_owned(),
        }
    }

    fn keyword_to_token(&self, token: &String) -> TokenType {
        if let Ok(token_type) = Type::from_str(token) {
            return TokenType::Type(token_type);
        }
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

            "CHARACTER" => TokenType::Type(Type::CHAR),
            "DEC" => TokenType::Type(Type::DECIMAL),
            "INTEGER" => TokenType::Type(Type::INT),
            "UINT" => TokenType::Type(Type::UINT),
            "ULONG" => TokenType::Type(Type::ULONG),

            "AND" => TokenType::Operator(Operator::AND),
            "OR" => TokenType::Operator(Operator::OR),
            "FALSE" | "TRUE" => TokenType::Literal(Literal::BOOLEAN),

            _ => TokenType::Literal(Literal::VARIABLE),
        }
    }

    fn get_token(&mut self) -> Option<Token> {
        self._peek(0)?;

        self.buildup.clear();

        let start_pos = self.pos;
        let mut error = None;
        let token_type = match self.seek() {
            '\n' | ';' => TokenType::NEWLINE,
            ' ' => TokenType::SPACE,
            c if c.is_alphabetic() || c == '_' => loop {
                match self.peek() {
                    '_' | '$' | '#' | '%' | '-' => {
                        self.seek();
                    }
                    c if c.is_alphanumeric() => {
                        self.seek();
                    }
                    '!' => {
                        self.seek();
                        break TokenType::Literal(Literal::ENUM);
                    }
                    _ => break self.keyword_to_token(&self.buildup.to_uppercase()),
                };
            },
            char @ ('.' | '0'..='9') => {
                enum Stage {
                    Number,
                    Fraction,
                    Exponent,
                }
                let mut stage = Stage::Number;

                let mut token_type = None;
                if char == '.' {
                    token_type = match self.peek() {
                        '0'..='9' => {
                            stage = Stage::Fraction;
                            None
                        }
                        // '.' => {
                        //     if self.peek_nth(1) == Ok('.') {
                        //         // These will never return EOF
                        //         self.seek()?;
                        //         self.seek()?;
                        //         Some(TokenType::Symbol(Symbol::DOTDOTDOT))
                        //     } else {
                        //         None
                        //     }
                        // }
                        _ => Some(TokenType::Symbol(Symbol::DOT)),
                    }
                }

                match token_type {
                    // TODO date/time
                    Some(token_type) => token_type,
                    None => loop {
                        match self.peek() {
                            '0'..='9' => {
                                self.seek();
                            }
                            '.' => {
                                self.seek();
                                match stage {
                                    Stage::Number => stage = Stage::Fraction,
                                    Stage::Fraction => error = Some("Multiple Dots inside Number"),
                                    Stage::Exponent => {
                                        error = Some("Not allowed to have fractional Exponent")
                                    }
                                }
                            }
                            'e' | 'E' => {
                                match stage {
                                    Stage::Number | Stage::Fraction => stage = Stage::Exponent,
                                    Stage::Exponent => {
                                        error = Some("Multiple exponents inside Number")
                                    }
                                }
                                match self.peek() {
                                    '-' | '+' => {
                                        self.seek();
                                        match self.peek() {
                                            '0'..='9' => {
                                                self.seek();
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
                match self.peek() {
                    char @ ('\'' | '"') => {
                        self.seek();
                        if del == char {
                            break TokenType::Literal(Literal::STRING);
                        }
                    }
                    '~' => match self.peek() {
                        '\n' => {}
                        _ => {
                            self.seek();
                        }
                    },
                    '\n' => {
                        error = Some("String not closed");
                        break TokenType::Literal(Literal::STRING);
                    }
                    _ => {
                        self.seek();
                    }
                }
            },
            '/' => match self.peek() {
                '/' => {
                    self.seek();
                    loop {
                        match self.seek() {
                            '\n' => break,
                            _ => {}
                        }
                    }
                    TokenType::COMMENT
                }
                '*' => {
                    self.seek();
                    loop {
                        match self.seek() {
                            '*' => {
                                if self.seek() == '/' {
                                    break;
                                }
                            }
                            '\n' => {
                                if self._peek(0) == None {
                                    error = Some("Multiline comment not closed");
                                }
                            }
                            _ => {}
                        }
                    }
                    TokenType::COMMENT
                }
                '=' => {
                    self.seek();
                    TokenType::SpecialAssignment(SpecialAssignment::DIVEQ)
                }
                _ => TokenType::Operator(Operator::DIV),
            },
            '*' => match self.peek() {
                '=' => {
                    self.seek();
                    TokenType::SpecialAssignment(SpecialAssignment::MULTEQ)
                }
                _ => TokenType::Operator(Operator::MULT),
            },
            '+' => match self.peek() {
                '=' => {
                    self.seek();
                    TokenType::SpecialAssignment(SpecialAssignment::PLUSEQ)
                }
                _ => TokenType::Operator(Operator::PLUS),
            },
            '-' => match self.peek() {
                '=' => {
                    self.seek();
                    TokenType::SpecialAssignment(SpecialAssignment::MINUSEQ)
                }
                _ => TokenType::Operator(Operator::MINUS),
            },
            '<' => match self.peek() {
                '=' => {
                    self.seek();
                    TokenType::Operator(Operator::LTE)
                }
                '>' => {
                    self.seek();
                    TokenType::Operator(Operator::GTLT)
                }
                _ => TokenType::Operator(Operator::LT),
            },
            '>' => match self.peek() {
                '=' => {
                    self.seek();
                    TokenType::Operator(Operator::GTE)
                }
                _ => TokenType::Operator(Operator::GT),
            },
            ':' => match self.peek() {
                ':' => {
                    self.seek();
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
            },
            error: error.map(str::to_owned),
        })
    }
}

impl Iterator for FileTokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.get_token() {
                Some(Token {
                    token_type: TokenType::SPACE,
                    ..
                }) => {}
                token => break token,
            }
        }
    }
}
