use std::borrow::BorrowMut;
use std::{iter::Filter, rc::Rc};

use multipeek::{multipeek, MultiPeek};

use super::parser_types::*;
use super::tokenizer::*;
use super::tokenizer_types as tokens;
use super::tokenizer_types::Range;

pub struct Parser<'a> {
    tokens: MultiPeek<Filter<&'a mut dyn Iterator<Item = Token>, fn(&Token) -> bool>>,
    syntax_errors: Vec<Diagnostic>,
}

impl Parser<'_> {
    pub fn new<'a>(tokens: &'a mut dyn Iterator<Item = Token>) -> Parser<'a> {
        fn filter(token: &Token) -> bool {
            token.token_type != TokenType::COMMENT
        }

        Parser::<'a> {
            tokens: multipeek(tokens.filter::<fn(&Token) -> bool>(filter)),
            syntax_errors: Vec::new(),
        }
    }

    fn next(&mut self) -> ParseResult<Token> {
        self.tokens.next().ok_or(ParseError::EOF)
    }

    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens.peek().ok_or(ParseError::EOF)
    }

    fn peek_nth(&mut self, n: usize) -> ParseResult<&Token> {
        self.tokens.peek_nth(n).ok_or(ParseError::EOF)
    }

    fn consume_line(&mut self) -> ParseResult<()> {
        loop {
            match self.next()?.token_type {
                TokenType::NEWLINE => break Ok(()),
                _ => {}
            }
        }
    }

    fn error(&mut self, error: &String, range: &Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Error,
            message: format!("[Parser] {}", error),
            range: range.clone(),
        });
    }

    fn fatal<R>(&mut self, error: &String, range: &Range, consume_line: bool) -> ParseResult<R> {
        self.error(error, range);
        if consume_line {
            self.consume_line()?;
        }
        Err(ParseError::UnexpectedToken)
    }

    fn optional(&mut self, token_type: TokenType) -> ParseResult<Option<Token>> {
        let token = self.peek()?;
        if token.token_type == token_type {
            Ok(self.tokens.next())
        } else {
            Ok(None)
        }
    }

    fn expect(&mut self, token_type: TokenType) -> ParseResult<Token> {
        let token = self.next()?;
        if token.token_type == token_type {
            Ok(token)
        } else {
            return self.fatal(
                &format!("Expected {:?}", token_type),
                &token.range,
                token.token_type != TokenType::NEWLINE,
            );
        }
    }

    // fn find_variable(&self, name: &String) -> Option<Box<Variable>> {
    //     if let Some(function) = &self.function {
    //         let local = function
    //             .variables
    //             .iter()
    //             .find(|var| &var.borrow().name == name);
    //         if let Some(_) = local {
    //             return local.cloned();
    //         }
    //     }

    //     self.file
    //         .instance_variables
    //         .iter()
    //         .find(|var| &var.borrow().name == name)
    //         .cloned()
    // }

    // fn find_function(
    //     &self,
    //     name: &String,
    //     returns: &DataType,
    //     arguments: &Vec<DataType>,
    // ) -> Option<Box<Function>> {
    //     // TODO global functions
    //     let instance = self.file.functions.iter().find(|func| {
    //         &func.borrow().name == name
    //             && (returns == &DataType::Unknown || returns == &func.borrow().returns)
    //             && func.borrow().arguments.iter().zip(arguments.iter()).all(
    //                 |((existing_type, _), function_type)| {
    //                     function_type == &DataType::Unknown || function_type == existing_type
    //                 },
    //             )
    //     });
    //     if let Some(_) = instance {
    //         return instance.cloned();
    //     }
    //     None
    // }

    fn parse_type(&mut self) -> ParseResult<DataType> {
        let token = self.next()?;

        let data_type = match token.token_type {
            TokenType::Type(tokens::Type::DECIMAL) => {
                if self
                    .optional(TokenType::Symbol(tokens::Symbol::LCURLY))?
                    .is_some()
                {
                    self.expect(TokenType::Literal(tokens::Literal::NUMBER))?;
                    self.expect(TokenType::Symbol(tokens::Symbol::RCURLY))?;
                }
                DataType::Primitive(tokens::Type::DECIMAL)
            }
            TokenType::Type(data_type) => DataType::Primitive(data_type),
            TokenType::ID => match self.optional(TokenType::Symbol(tokens::Symbol::TICK))? {
                Some(_) => {
                    let token = self.expect(TokenType::ID)?;
                    DataType::Complex(Some(token.content), self.next()?.content)
                }
                None => DataType::Complex(None, token.content),
            },
            _ => self.fatal(&"Expected a type".to_owned(), &token.range, true)?,
        };

        Ok(data_type)
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        let start = self.peek()?.range.start;
        let end;

        let expression_type = match self.peek()?.token_type {
            TokenType::Symbol(tokens::Symbol::LPAREN) => {
                self.next()?;
                let expression = self.parse_expression()?;
                end = self
                    .expect(TokenType::Symbol(tokens::Symbol::RPAREN))?
                    .range
                    .end;

                ExpressionType::Parenthesized(Box::new(expression))
            }
            TokenType::Symbol(tokens::Symbol::LCURLY) => {
                let mut expressions = Vec::new();

                if self.peek()?.token_type == TokenType::Symbol(tokens::Symbol::RCURLY) {
                    loop {
                        expressions.push(self.parse_expression()?);

                        match self.peek()?.token_type {
                            TokenType::Symbol(tokens::Symbol::COMMA) => {
                                self.next()?;
                            }
                            TokenType::Symbol(tokens::Symbol::RCURLY) => {
                                break;
                            }
                            _ => todo!("Expected ',' or ')'"),
                        }
                    }
                }
                end = self
                    .expect(TokenType::Symbol(tokens::Symbol::RCURLY))?
                    .range
                    .end;

                ExpressionType::ArrayLiteral(expressions)
            }
            TokenType::Keyword(tokens::Keyword::NOT) => {
                self.next()?;
                let expression = self.parse_expression()?;
                end = expression.range.end;

                ExpressionType::BooleanNot(Box::new(expression))
            }
            TokenType::ID => {
                let lvalue = self.parse_lvalue()?;
                end = lvalue.range.end;

                ExpressionType::LValue(lvalue)
            }
            TokenType::Keyword(tokens::Keyword::CREATE) => {
                self.next()?;
                let class = self.next()?;
                end = class.range.end;
                ExpressionType::Create(class.content)
            }
            TokenType::Literal(literal) => {
                end = self.next()?.range.end;
                ExpressionType::Literal(literal)
            }
            stuff => {
                println!("INVALID {stuff:?} {:?}", self.next()?.range);
                todo!("invalid");
            }
        };

        let mut expression = Expression {
            expression_type,
            range: Range { start, end },
        };

        match self.peek()?.token_type {
            TokenType::Symbol(
                operator @ (tokens::Symbol::PLUSPLUS | tokens::Symbol::MINUSMINUS),
            ) => {
                let token = self.next()?;

                expression = Expression {
                    range: Range {
                        start: expression.range.start,
                        end: token.range.end,
                    },
                    expression_type: ExpressionType::IncrementDecrement(
                        Box::new(expression),
                        operator,
                    ),
                }
            }
            _ => {}
        }

        if let TokenType::Operator(operator) = self.peek()?.token_type {
            self.next()?;
            let right_side = self.parse_expression()?;

            expression = match right_side.expression_type {
                ExpressionType::Operation(sub_left, sub_operator, sub_right)
                    if operator.precedence() > sub_operator.precedence() =>
                {
                    Expression {
                        range: Range {
                            start: expression.range.start,
                            end: right_side.range.end,
                        },
                        expression_type: ExpressionType::Operation(
                            Box::new(Expression {
                                range: Range {
                                    start: expression.range.start,
                                    end: right_side.range.end,
                                },
                                expression_type: ExpressionType::Operation(
                                    Box::new(expression),
                                    operator,
                                    sub_left,
                                ),
                            }),
                            sub_operator,
                            sub_right,
                        ),
                    }
                }
                _ => Expression {
                    range: Range {
                        start: expression.range.start,
                        end: right_side.range.end,
                    },
                    expression_type: ExpressionType::Operation(
                        Box::new(expression),
                        operator,
                        Box::new(right_side),
                    ),
                },
            };
        }

        Ok(expression)
    }

    fn parse_lvalue(&mut self) -> ParseResult<LValue> {
        let mut previous = match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::SUPER) => {
                let sup = self.next()?;
                Some(LValue {
                    lvalue_type: LValueType::Super,
                    range: sup.range,
                })
            }
            _ => None,
        };

        loop {
            match &previous {
                Some(prev) => match self.peek()?.token_type {
                    TokenType::Symbol(tokens::Symbol::COLONCOLON) => {
                        let colon = self.next()?;

                        if let LValueType::Super = prev.lvalue_type {
                        } else {
                            self.error(
                                &"Only super methods can be indexed using '::'".to_owned(),
                                &colon.range,
                            );
                        }
                    }
                    TokenType::Symbol(tokens::Symbol::DOT) => {
                        let dot = self.next()?;

                        if let LValueType::Super = prev.lvalue_type {
                            self.error(
                                &"Super methods have to be indexed using '::'".to_owned(),
                                &dot.range,
                            );
                        }
                    }
                    TokenType::Symbol(tokens::Symbol::LBRACE) => {
                        let _lbrace = self.next()?;
                        let expression = self.parse_expression()?;
                        let rbrace = self.expect(TokenType::Symbol(tokens::Symbol::RBRACE))?;

                        previous = Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end: rbrace.range.end,
                            },
                            lvalue_type: LValueType::Index(
                                Box::new(previous.take().unwrap()),
                                Box::new(expression),
                            ),
                        });
                        continue;
                    }
                    _ => break,
                },
                None => {}
            }

            let name = self.expect(TokenType::ID)?;

            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::LPAREN) => {
                    self.next()?;

                    let mut arguments = Vec::new();
                    let end;
                    if self.peek()?.token_type != TokenType::Symbol(tokens::Symbol::RPAREN) {
                        loop {
                            let expression = self.parse_expression()?;

                            arguments.push(expression);

                            let token = self.next()?;
                            match token.token_type {
                                TokenType::Symbol(tokens::Symbol::COMMA) => {}
                                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                                    end = token.range.end;
                                    break;
                                }
                                _ => {
                                    return self.fatal(
                                        &"Expected ',' or ')'".to_string(),
                                        &token.range,
                                        token.token_type != TokenType::NEWLINE,
                                    );
                                }
                            }
                        }
                    } else {
                        end = self.next()?.range.end;
                    }

                    let func = Rc::new(Function {
                        returns: DataType::Unknown,
                        scope_modif: None,
                        access: None,
                        name: name.content,
                        arguments: Vec::new(),
                        range: Range::default(),
                    });

                    previous = match previous {
                        Some(prev) => Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end,
                            },
                            lvalue_type: LValueType::Method(Box::new(prev), func, arguments),
                        }),
                        None => Some(LValue {
                            range: Range {
                                start: name.range.start,
                                end,
                            },
                            lvalue_type: LValueType::Function(func, arguments),
                        }),
                    };
                }
                _ => {
                    let var = Rc::new(Variable {
                        access: Access {
                            read: None,
                            write: None,
                        },
                        data_type: DataType::Unknown,
                        constant: false,
                        name: name.content,
                        range: Range::default(),
                        initial_value: None,
                    });

                    previous = match previous {
                        Some(prev) => Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end: name.range.end,
                            },
                            lvalue_type: LValueType::Member(Box::new(prev), var),
                        }),
                        None => Some(LValue {
                            range: name.range,
                            lvalue_type: LValueType::Variable(var),
                        }),
                    };
                }
            }
        }

        previous.ok_or(ParseError::UnexpectedToken)
    }

    fn parse_function_header(&mut self) -> ParseResult<Function> {
        let mut arguments = Vec::new();
        let name;
        let start = self.peek()?.range.start;

        let scope_modif = if let TokenType::ScopeModif(scope_modif) = self.peek()?.token_type {
            self.next()?;
            Some(scope_modif)
        } else {
            None
        };

        let access = if let TokenType::AccessType(access) = self.peek()?.token_type {
            self.next()?;
            Some(access)
        } else {
            None
        };

        let returns = match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::FUNCTION) => {
                self.next()?;
                self.parse_type()?
            }
            TokenType::Keyword(tokens::Keyword::SUBROUTINE) => {
                self.next()?;
                DataType::Void
            }
            _ => {
                let range = self.peek()?.range;
                return self.fatal(&"Expected FUNCTION or SUBROUTINE".to_owned(), &range, true);
            }
        };

        name = self.expect(TokenType::ID)?.content;
        self.expect(TokenType::Symbol(tokens::Symbol::LPAREN))?;
        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                    end = self.next()?.range.end;
                    break;
                }
                TokenType::Symbol(tokens::Symbol::DOTDOTDOT) => {
                    self.next()?;
                    end = self
                        .expect(TokenType::Symbol(tokens::Symbol::RPAREN))?
                        .range
                        .end;
                    break;
                }
                _ => {}
            };

            // TODO           
            self.optional(TokenType::Keyword(tokens::Keyword::REF))?;
            self.optional(TokenType::Keyword(tokens::Keyword::READONLY))?;

            let data_type = self.parse_type()?;
            let name = self.expect(TokenType::ID)?.content;
            arguments.push((data_type, name));

            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::COMMA) => {
                    self.next()?;
                }
                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                    end = self.next()?.range.end;
                    break;
                }
                _ => {
                    let range = self.peek()?.range;
                    return self.fatal(&"Expected ',' or ')'".to_owned(), &range, true);
                }
            }
        }

        if self.optional(TokenType::Keyword(tokens::Keyword::THROWS))?.is_some() {
            self.expect(TokenType::ID)?;
        }

        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::RPCFUNC) => {
                self.next()?;
            }
            TokenType::Keyword(tokens::Keyword::LIBRARY) => {
                self.next()?;
                self.expect(TokenType::Literal(tokens::Literal::STRING))?;

                if self.optional(TokenType::Keyword(tokens::Keyword::ALIAS))?.is_some() {
                    self.expect(TokenType::Keyword(tokens::Keyword::FOR))?;
                    self.expect(TokenType::Literal(tokens::Literal::STRING))?;

                }
            }
            _ => {}
        }
        self.expect(TokenType::NEWLINE).split_eof()?;

        Ok(Function {
            returns,
            scope_modif,
            access,
            name,
            arguments,
            range: Range { start, end },
        })
    }

    fn parse_variable_declaration(&mut self) -> ParseResult<Statement> {
        let start = self.peek()?.range.start;

        let base = match self.peek()?.token_type {
            TokenType::AccessType(
                access_type @ (tokens::AccessType::PUBLIC
                | tokens::AccessType::PRIVATE
                | tokens::AccessType::PROTECTED),
            ) => {
                self.next()?;
                Some(access_type)
            }
            _ => None,
        };

        let mut access = Access {
            read: base,
            write: base,
        };

        loop {
            if let TokenType::AccessType(access_type) = self.peek()?.token_type {
                let token = self.next()?;
                match access_type {
                    tokens::AccessType::PUBLIC
                    | tokens::AccessType::PRIVATE
                    | tokens::AccessType::PROTECTED => {
                        self.error(&"Only one Access Right allowed".into(), &token.range);
                    }
                    tokens::AccessType::PRIVATEWRITE
                    | tokens::AccessType::PROTECTEDWRITE
                    | tokens::AccessType::SYSTEMWRITE => {
                        if let Some(write) = &mut access.write {
                            if write.is_general() {
                                if access_type.strictness() < write.strictness() {
                                    *write = access_type;
                                } else {
                                    self.error(
                                        &"Write Access needs to be less Strict than general Access"
                                            .into(),
                                        &token.range,
                                    );
                                }
                            } else {
                                self.error(
                                    &"Cannot supply more than one Write Access".into(),
                                    &token.range,
                                );
                            }
                        } else {
                            access.write = Some(access_type);
                        }
                    }
                    tokens::AccessType::PRIVATEREAD
                    | tokens::AccessType::PROTECTEDREAD
                    | tokens::AccessType::SYSTEMREAD => {
                        if let Some(read) = &mut access.read {
                            if read.is_general() {
                                if access_type.strictness() < read.strictness() {
                                    *read = access_type;
                                } else {
                                    self.error(
                                        &"Read Access needs to be less Strict than general Access"
                                            .into(),
                                        &token.range,
                                    );
                                }
                            } else {
                                self.error(
                                    &"Cannot supply more than one Read Access".into(),
                                    &token.range,
                                );
                            }
                        } else {
                            access.read = Some(access_type);
                        }
                    }
                }
            } else {
                break;
            }
        }

        let constant = self
            .optional(TokenType::Keyword(tokens::Keyword::CONSTANT))?
            .is_some();

        let mut data_type = self.parse_type()?;

        let name = self.expect(TokenType::ID)?;

        let mut end = name.range.end;
        if self
            .optional(TokenType::Symbol(tokens::Symbol::LBRACE))?
            .is_some()
        {
            let mut first = true;
            loop {
                match self.peek()?.token_type {
                    TokenType::Symbol(tokens::Symbol::RBRACE) => {
                        end = self.next()?.range.end;
                        break;
                    }
                    TokenType::Symbol(tokens::Symbol::COMMA) if !first => {
                        self.next()?;
                    }
                    TokenType::Literal(tokens::Literal::NUMBER) if first => {}
                    _ => todo!("Expected , or ]"),
                }
                first = false;

                self.expect(TokenType::Literal(tokens::Literal::NUMBER))?;
                if self
                    .optional(TokenType::Keyword(tokens::Keyword::TO))?
                    .is_some()
                {
                    self.expect(TokenType::Literal(tokens::Literal::NUMBER))?;
                }
            }

            data_type = DataType::Array(Box::new(data_type))
        }

        let expression;
        if self
            .optional(TokenType::Operator(tokens::Operator::EQ))?
            .is_some()
        {
            let ex = self.parse_expression()?;
            end = ex.range.end;
            expression = Some(ex);
        } else {
            expression = None;
        }

        self.expect(TokenType::NEWLINE)?;

        Ok(Statement {
            statement_type: StatementType::Declaration(Rc::new(Variable {
                access,
                data_type,
                name: name.content,
                constant,
                range: name.range,
                initial_value: expression,
            })),
            range: Range { start, end },
        })
    }

    // Consumes trailing newlines
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::CONSTANT) | TokenType::Type(_) => {
                self.parse_variable_declaration()
            }
            TokenType::ID => match self.peek_nth(1)?.token_type {
                TokenType::ID => self.parse_variable_declaration(),
                _ => {
                    let expression = self.parse_expression()?;
                    match (expression.expression_type.clone(), self.peek()?.token_type) {
                        (ExpressionType::Operation(left, tokens::Operator::EQ, right), _)
                            if matches!(left.expression_type, ExpressionType::LValue(_),) =>
                        {
                            if let ExpressionType::LValue(lvalue) = left.expression_type {
                                self.expect(TokenType::NEWLINE)?;
                                Ok(Statement {
                                    range: expression.range,
                                    statement_type: StatementType::Assignment(lvalue, *right),
                                })
                            } else {
                                unreachable!()
                            }
                        }
                        (_, TokenType::SpecialAssignment(_)) => {
                            let value = self.parse_expression()?;

                            if let ExpressionType::LValue(lvalue) = expression.expression_type {
                                self.expect(TokenType::NEWLINE)?;
                                Ok(Statement {
                                    range: expression.range,
                                    statement_type: StatementType::Assignment(lvalue, value),
                                })
                            } else {
                                todo!("ERROR")
                            }
                        }
                        _ => {
                            self.expect(TokenType::NEWLINE)?;
                            Ok(Statement {
                                range: expression.range,
                                statement_type: StatementType::Expression(expression),
                            })
                        }
                    }
                    // | TokenType::SpecialAssignment(_) => match expression.expression_type {
                    //     ExpressionType::LValue(lvalue) => {
                    //         let operator = self.next()?;
                    //         let value = self.parse_expression()?;
                    //         self.expect(TokenType::NEWLINE)?;

                    //         Ok(Statement {
                    //             range: Range {
                    //                 start: expression.range.start,
                    //                 end: value.range.end,
                    //             },
                    //             statement_type: StatementType::Assignment(lvalue, value),
                    //         })
                    //     }
                    //     _ => {
                    //         return self.fatal(
                    //             &"Trying to assign to a non LValue".to_string(),
                    //             &expression.range,
                    //             true,
                    //         )
                    //     }
                    // },
                    // }
                }
            },
            TokenType::Keyword(tokens::Keyword::THROW) => {
                let throw = self.next()?;
                let expression = self.parse_expression()?;
                self.expect(TokenType::NEWLINE)?;
                Ok(Statement {
                    range: Range {
                        start: throw.range.start,
                        end: expression.range.end,
                    },
                    statement_type: StatementType::Throw(expression),
                })
            }
            TokenType::Keyword(tokens::Keyword::IF) => {
                let if_token = self.next()?;
                let condition = self.parse_expression()?;
                self.expect(TokenType::Keyword(tokens::Keyword::THEN))?;

                match self.peek()?.token_type {
                    TokenType::NEWLINE => {
                        self.next()?;

                        let mut ifs = IfStatement {
                            condition,
                            statements: Vec::new(),
                            elseif_statements: Vec::new(),
                            else_statements: Vec::new(),
                        };

                        let mut part = tokens::Keyword::IF;
                        let end;
                        'outer: loop {
                            loop {
                                match self.peek()?.token_type.clone() {
                                    TokenType::Keyword(tokens::Keyword::END) => match self
                                        .peek_nth(1)?
                                        .token_type
                                    {
                                        TokenType::Keyword(tokens::Keyword::IF) => {
                                            self.next()?;
                                            end = self.next()?.range.end;
                                            self.expect(TokenType::NEWLINE).split_eof()?;
                                            break 'outer;
                                        }
                                        _ => {
                                            let range = self.next()?.range;
                                            self.error(&"Dangling end keyword".to_string(), &range);
                                            // TODO dont always consume line
                                            self.consume_line()?;
                                        }
                                    },

                                    TokenType::Keyword(
                                        token_type @ (tokens::Keyword::ELSEIF | tokens::Keyword::ELSE),
                                    ) => {
                                        match token_type {
                                            tokens::Keyword::ELSE => {
                                                self.next()?;
                                            }
                                            tokens::Keyword::ELSEIF => {
                                                self.next()?;

                                                let expression = self.parse_expression()?;
                                                ifs.elseif_statements
                                                    .push((expression, Vec::new()));
                                            }
                                            _ => unreachable!(),
                                        }

                                        self.expect(TokenType::NEWLINE).split_eof()?;
                                        part = token_type;
                                        break;
                                    }
                                    _ => {
                                        match part {
                                            tokens::Keyword::IF => &mut ifs.statements,
                                            tokens::Keyword::ELSEIF => {
                                                &mut ifs.elseif_statements.last_mut().unwrap().1
                                            }
                                            tokens::Keyword::ELSE => &mut ifs.else_statements,
                                            _ => unreachable!(),
                                        }
                                        .push(self.parse_statement()?);
                                    }
                                }
                            }
                        }

                        Ok(Statement {
                            range: Range {
                                start: if_token.range.start,
                                end,
                            },
                            statement_type: StatementType::If(ifs),
                        })
                    }
                    _ => {
                        let statement = self.parse_statement()?;
                        Ok(Statement {
                            range: Range {
                                start: if_token.range.start,
                                end: statement.range.end,
                            },
                            statement_type: StatementType::If(IfStatement {
                                condition,
                                statements: vec![statement],
                                elseif_statements: vec![],
                                else_statements: vec![],
                            }),
                        })
                    }
                }
            }
            TokenType::Keyword(tokens::Keyword::RETURN) => {
                let ret = self.next()?;
                // TODO can you return nothing in pb?
                let expression = self.parse_expression()?;
                self.expect(TokenType::NEWLINE).split_eof()?;
                Ok(Statement {
                    range: Range {
                        start: ret.range.start,
                        end: expression.range.end,
                    },
                    statement_type: StatementType::Return(expression),
                })
            }
            TokenType::Keyword(tokens::Keyword::FOR) => {
                let for_token = self.next()?;
                let name = self.expect(TokenType::ID)?;
                self.expect(TokenType::Operator(tokens::Operator::EQ))
                    .split_eof()?;
                let start = self.parse_expression()?;
                self.expect(TokenType::Keyword(tokens::Keyword::TO)).split_eof()?;
                let stop = self.parse_expression()?;

                let step = match self.peek()?.token_type {
                    TokenType::Keyword(tokens::Keyword::STEP) => {
                        self.next()?;
                        Some(self.parse_expression()?)
                    }
                    _ => None,
                };

                self.expect(TokenType::NEWLINE).split_eof()?;

                let mut statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::NEXT) => {
                            self.next()?;
                            break;
                        }
                        _ => {
                            statements.push(self.parse_statement()?);
                        }
                    }
                }

                let next_token = self.next()?;
                self.expect(TokenType::NEWLINE).split_eof()?;

                Ok(Statement {
                    range: Range {
                        start: for_token.range.start,
                        end: next_token.range.end,
                    },
                    statement_type: StatementType::ForLoop(ForLoopStatement {
                        statements,
                        start,
                        stop,
                        step,
                        variable: Rc::new(Variable {
                            constant: false,
                            access: Access {
                                read: None,
                                write: None,
                            },
                            data_type: DataType::Unknown,
                            name: name.content,
                            initial_value: None,
                            range: name.range,
                        }),
                    }),
                })
            }
            TokenType::Keyword(tokens::Keyword::DO) => {
                let do_token = self.next()?;
                let mut condition = Expression {
                    expression_type: ExpressionType::Literal(tokens::Literal::BOOLEAN),
                    range: Range::default(),
                };
                let is_inversed;
                let mut is_until = false;
                match self.peek()?.token_type {
                    TokenType::Keyword(keyword @ (tokens::Keyword::WHILE | tokens::Keyword::UNTIL)) => {
                        condition = self.parse_expression()?;

                        is_inversed = false;
                        is_until = keyword == tokens::Keyword::UNTIL;
                    }
                    _ => {
                        is_inversed = true;
                    }
                }

                self.expect(TokenType::NEWLINE).split_eof()?;

                let mut statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::LOOP) => {
                            self.next()?;
                            break;
                        }
                        _ => {
                            statements.push(self.parse_statement()?);
                        }
                    }
                }

                let loop_token = self.next()?;

                if is_inversed {
                    match self.peek()?.token_type {
                        TokenType::Keyword(keyword @ (tokens::Keyword::WHILE | tokens::Keyword::UNTIL)) => {
                            condition = self.parse_expression()?;
                            is_until = keyword == tokens::Keyword::UNTIL;
                            self.expect(TokenType::NEWLINE).split_eof()?;
                        }
                        _ => {
                            let range = self.peek()?.range;
                            self.error(&"Expected either WHILE or UNTIL".into(), &range);
                            self.consume_line()?;
                        }
                    }
                } else {
                    self.expect(TokenType::NEWLINE).split_eof()?;
                }

                Ok(Statement {
                    range: Range {
                        start: do_token.range.start,
                        end: if is_inversed {
                            loop_token.range.end
                        } else {
                            condition.range.end
                        },
                    },
                    statement_type: StatementType::WhileLoop(WhileLoopStatement {
                        condition,
                        is_inversed,
                        is_until,
                        statements,
                    }),
                })
            }
            TokenType::Keyword(tokens::Keyword::CHOOSE) => {
                let choose_token = self.next()?;
                self.expect(TokenType::Keyword(tokens::Keyword::CASE))
                    .split_eof()?;
                let choose = self.parse_expression()?;
                let mut cases = Vec::new();
                let end;

                'outer: loop {
                    self.expect(TokenType::Keyword(tokens::Keyword::CASE))?;
                    let mut specifiers = Vec::new();

                    loop {
                        let specifier = match self.peek()?.token_type {
                            TokenType::Keyword(tokens::Keyword::IS) => {
                                let is = self.next()?;
                                let operator = match self.peek()?.token_type {
                                    TokenType::Operator(operator) => operator,
                                    _ => todo!("Unexpeceed"),
                                };
                                self.next()?;

                                let literal = match self.peek()?.token_type {
                                    TokenType::Literal(literal) => literal,
                                    _ => todo!("Unexpeceed"),
                                };
                                let literal_token = self.next()?;

                                CaseSpecifier {
                                    specifier_type: CaseSpecifierType::Is(operator, literal),
                                    range: Range {
                                        start: is.range.start,
                                        end: literal_token.range.end,
                                    },
                                }
                            }
                            TokenType::Literal(literal) => {
                                let token = self.next()?;
                                if self.optional(TokenType::Keyword(tokens::Keyword::TO))?.is_some() {
                                    let up_to = match self.peek()?.token_type {
                                        TokenType::Literal(literal) => literal,
                                        _ => todo!("Unexpeceed"),
                                    };
                                    let up_to_token = self.next()?;

                                    CaseSpecifier {
                                        specifier_type: CaseSpecifierType::To(literal, up_to),
                                        range: Range {
                                            start: token.range.start,
                                            end: up_to_token.range.end,
                                        },
                                    }
                                } else {
                                    CaseSpecifier {
                                        specifier_type: CaseSpecifierType::Literals(literal),
                                        range: token.range,
                                    }
                                }
                            }
                            TokenType::Keyword(tokens::Keyword::ELSE) => {
                                let token = self.next()?;
                                CaseSpecifier {
                                    specifier_type: CaseSpecifierType::Else,
                                    range: token.range,
                                }
                            }
                            _ => todo!("Unexpected"),
                        };

                        specifiers.push(specifier);

                        if self
                            .optional(TokenType::Symbol(tokens::Symbol::COMMA))?
                            .is_none()
                        {
                            break;
                        }
                    }
                    self.expect(TokenType::NEWLINE).split_eof()?;

                    let mut statements = Vec::new();

                    loop {
                        match self.peek()?.token_type.clone() {
                            TokenType::Keyword(tokens::Keyword::CASE)
                            | TokenType::Keyword(tokens::Keyword::END) => {
                                if let TokenType::Keyword(tokens::Keyword::CASE) = self.peek()?.token_type {
                                    cases.push((specifiers, statements));

                                    break;
                                } else {
                                    match self.peek_nth(1)?.token_type {
                                        TokenType::Keyword(tokens::Keyword::CHOOSE) => {
                                            cases.push((specifiers, statements));

                                            self.next()?;
                                            end = self.next()?.range.end;
                                            self.expect(TokenType::NEWLINE).split_eof()?;
                                            break 'outer;
                                        }
                                        _ => {
                                            let range = self.next()?.range;
                                            self.error(&"Dangling end keyword".to_string(), &range);
                                            self.consume_line()?;
                                        }
                                    }
                                }
                            }
                            _ => {
                                statements.push(self.parse_statement()?);
                            }
                        }
                    }
                }

                Ok(Statement {
                    range: Range {
                        start: choose_token.range.start,
                        end,
                    },
                    statement_type: StatementType::Choose(ChooseCaseStatement { choose, cases }),
                })
            }
            _ => {
                let Token {
                    token_type, range, ..
                } = *self.peek()?;
                return self.fatal(&format!("Unexpected {:?}", token_type), &range, true);
            }
        }
    }

    fn parse_function(&mut self) -> ParseResult<TopLevel> {
        let function = self.parse_function_header()?;
        let mut statements = Vec::new();
        let end;

        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(
                        function_type @ (tokens::Keyword::FUNCTION | tokens::Keyword::SUBROUTINE),
                    ) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        if (function.returns == DataType::Void)
                            ^ (function_type == tokens::Keyword::SUBROUTINE)
                        {
                            self.error(&"Wrong function closing".to_string(), &close.range);
                        }

                        self.expect(TokenType::NEWLINE).split_eof()?;
                        break;
                    }
                    _ => {
                        let range = self.next()?.range;
                        self.error(&"Dangling end keyword".to_string(), &range);
                        self.consume_line()?;
                    }
                },
                _ => {
                    if let Ok(statement) = self.parse_statement().split_eof()? {
                        statements.push(statement)
                    }
                }
            };
        }

        Ok(TopLevel {
            range: Range {
                start: function.range.start,
                end,
            },
            top_level_type: TopLevelType::FunctionBody(Rc::new(function), statements),
        })
    }

    fn parse_on(&mut self) -> ParseResult<TopLevel> {
        let start = self
            .expect(TokenType::Keyword(tokens::Keyword::ON))?
            .range
            .start;

        self.expect(TokenType::ID)?;
        self.expect(TokenType::Symbol(tokens::Symbol::DOT))?;
        let token = self.next()?;
        match token.token_type {
            TokenType::Keyword(
                tokens::Keyword::OPEN
                | tokens::Keyword::CLOSE
                | tokens::Keyword::CREATE
                | tokens::Keyword::DESTROY,
            ) => {
                self.expect(TokenType::NEWLINE).split_eof()?;
            }
            _ => {
                self.error(
                    &"Expected one of [open, close, create, destroy]".to_string(),
                    &token.range,
                );
                if token.token_type != TokenType::NEWLINE {
                    self.consume_line()?;
                }
            }
        }

        let mut statements = Vec::new();
        let end;

        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::ON) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE).split_eof()?;
                        break;
                    }
                    _ => {
                        let range = self.next()?.range;
                        self.error(&"Dangling end keyword".to_string(), &range);
                        self.consume_line()?;
                    }
                },
                _ => {
                    if let Ok(statement) = self.parse_statement().split_eof()? {
                        statements.push(statement);
                    }
                }
            };
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::OnBody(token.content, statements),
            range: Range { start, end },
        })
    }

    fn parse_datatype_decl(&mut self) -> ParseResult<TopLevel> {
        let start = self.peek()?.range.start;
        let modif = if let TokenType::ScopeModif(modif) = self.peek()?.token_type {
            self.next()?;
            Some(modif)
        } else {
            None
        };
        self.expect(TokenType::Keyword(tokens::Keyword::TYPE))?;

        // TODO! do something with name, from, within
        let name = self.expect(TokenType::ID)?;
        self.expect(TokenType::Keyword(tokens::Keyword::FROM))?;
        let from = self.expect(TokenType::ID)?;

        let within = if self
            .optional(TokenType::Keyword(tokens::Keyword::WITHIN))?
            .is_some()
        {
            Some(self.expect(TokenType::ID)?)
        } else {
            None
        };

        self.expect(TokenType::NEWLINE).split_eof()?;

        let end;
        let mut declarations = Vec::new();
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::TYPE) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE);
                        break;
                    }
                    _ => {
                        let range = self.next()?.range;
                        self.error(&"Dangling end keyword".to_string(), &range);
                        self.consume_line()?;
                    }
                },
                _ => {
                    if let Ok(statement) = self.parse_statement().split_eof()? {
                        match statement.statement_type {
                            StatementType::Declaration(var) => {
                                declarations.push(var);
                            }
                            _ => {
                                self.error(
                                    &"Type Block can only contain Variable Declarations"
                                        .to_string(),
                                    &statement.range,
                                );
                            }
                        }
                    }
                }
            }
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::DatatypeDecl(modif, declarations),
            range: Range { start, end },
        })
    }

    fn parse_global_variables_decl(&mut self) -> ParseResult<TopLevel> {
        let start = self.peek()?.range.start;
        self.expect(TokenType::ScopeModif(tokens::ScopeModif::GLOBAL))?;
        let variable = self.parse_variable_declaration()?;

        if let StatementType::Declaration(var) = variable.statement_type {
            Ok(TopLevel {
                range: Range {
                    start,
                    end: variable.range.end,
                },
                top_level_type: TopLevelType::GlobalVariablesDecl(var),
            })
        } else {
            unreachable!();
        }
    }

    fn parse_type_variables_decl(&mut self) -> ParseResult<TopLevel> {
        let start = self.peek()?.range.start;
        self.expect(TokenType::Keyword(tokens::Keyword::TYPE))?;
        self.expect(TokenType::Keyword(tokens::Keyword::VARIABLES))?;
        self.expect(TokenType::NEWLINE).split_eof()?;

        let end;
        let mut declarations = Vec::new();
        let mut access = None;
        loop {
            match self.peek()?.token_type.clone() {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::VARIABLES) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE);
                        break;
                    }
                    _ => {
                        let range = self.next()?.range;
                        self.error(&"Dangling end keyword".to_string(), &range);
                        self.consume_line()?;
                    }
                },
                TokenType::AccessType(new_access)
                    if self.peek_nth(1)?.token_type == TokenType::Symbol(tokens::Symbol::COLON) =>
                {
                    let token = self.next()?;

                    if new_access.is_general() {
                        access = Some(new_access);
                    } else {
                        self.error(
                            &"Access Section must be a General Access Type".into(),
                            &token.range,
                        );
                    }

                    self.next()?;
                    self.expect(TokenType::NEWLINE)?;
                }
                _ => {
                    if let Ok(statement) = self.parse_variable_declaration().split_eof()? {
                        if let StatementType::Declaration(var) = statement.statement_type {
                            let var = (*var).clone();
                            declarations.push(Rc::new(Variable {
                                access: Access {
                                    read: var.access.read.clone().or(access),
                                    write: var.access.write.clone().or(access),
                                },
                                ..var
                            }));
                        } else {
                            unreachable!();
                        }
                    }
                }
            }
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::TypeVariablesDecl(declarations),
            range: Range { start, end },
        })
    }

    fn parse_functions_forward_decl(&mut self) -> ParseResult<TopLevel> {
        let start = self
            .expect(TokenType::Keyword(tokens::Keyword::FORWARD))?
            .range
            .start;

        self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))?;
        self.expect(TokenType::NEWLINE).split_eof()?;
        let mut functions = Vec::new();

        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => {
                    self.next()?;
                    end = self.peek()?.range.end;
                    self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))
                        .split_eof()?;
                    self.expect(TokenType::NEWLINE).split_eof()?;
                    break;
                }
                _ => {
                    if let Ok(function) = self.parse_function_header().split_eof()? {
                        functions.push(Rc::new(function));
                    }
                }
            };
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::FunctionsForwardDecl(functions),
            range: Range { start, end },
        })
    }

    fn parse_external_functions(&mut self) -> ParseResult<TopLevel> {
        let start = self
            .expect(TokenType::Keyword(tokens::Keyword::TYPE))?
            .range
            .start;

        self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))?;
        self.expect(TokenType::NEWLINE).split_eof()?;
        let mut functions = Vec::new();

        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => {
                    self.next()?;
                    end = self.peek()?.range.end;
                    self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))
                        .split_eof()?;
                    self.expect(TokenType::NEWLINE).split_eof()?;
                    break;
                }
                _ => {
                    if let Ok(function) = self.parse_function_header().split_eof()? {
                        functions.push(Rc::new(function));
                    }
                }
            };
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::ExternalFunctions(functions),
            range: Range { start, end },
        })
    }

    fn parse_forward_decl(&mut self) -> ParseResult<TopLevel> {
        let start = self
            .expect(TokenType::Keyword(tokens::Keyword::FORWARD))?
            .range
            .start;

        _ = self.expect(TokenType::NEWLINE).split_eof()?;

        let end;
        loop {
            match self.next()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek()?.token_type {
                    TokenType::Keyword(tokens::Keyword::FORWARD) => {
                        end = self.next()?.range.end;
                        self.expect(TokenType::NEWLINE)?;
                        break;
                    }
                    _ => _ = self.consume_line()?,
                },
                _ => _ = self.consume_line()?,
            };
        }

        Ok(TopLevel {
            top_level_type: TopLevelType::ForwardDecl,
            range: Range { start, end },
        })
    }

    fn parse_top_level(&mut self) -> ParseResult<TopLevel> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::FORWARD) => match self.peek_nth(1)?.token_type {
                TokenType::Keyword(tokens::Keyword::PROTOTYPES) => {
                    self.parse_functions_forward_decl()
                }
                TokenType::NEWLINE => self.parse_forward_decl(),
                _ => {
                    let range = self.peek()?.range;
                    return self.fatal(&"Unknown forward".to_string(), &range, true);
                }
            },
            TokenType::AccessType(_)
            | TokenType::Keyword(tokens::Keyword::FUNCTION | tokens::Keyword::SUBROUTINE) => self.parse_function(),
            TokenType::Keyword(tokens::Keyword::ON) => self.parse_on(),
            TokenType::Keyword(tokens::Keyword::TYPE) => match self.peek_nth(1)?.token_type {
                TokenType::Keyword(tokens::Keyword::VARIABLES) => self.parse_type_variables_decl(),
                TokenType::Keyword(tokens::Keyword::PROTOTYPES) => self.parse_external_functions(),
                _ => self.parse_datatype_decl(),
            },
            TokenType::ScopeModif(_) => {
                if let TokenType::Keyword(tokens::Keyword::TYPE) = self.peek_nth(1)?.token_type {
                    self.parse_datatype_decl()
                } else {
                    self.parse_global_variables_decl()
                }
            }
            stuff => {
                println!("INVALID {stuff:?} {:?}", self.next()?.range);
                todo!("invalid");
            }
        }
    }

    pub fn parse_tokens(&mut self) -> Vec<TopLevel> {
        let mut top_levels = Vec::new();
        loop {
            match self.parse_top_level() {
                Ok(top_level) => {
                    // println!("{:?}", top_level);
                    top_levels.push(top_level);

                    for error in &self.syntax_errors {
                        println!("{} - {}", error.range, error.message)
                    }
                    self.syntax_errors = Vec::new();
                }
                Err(ParseError::EOF) => break,
                _ => {}
            }
        }

        for error in &self.syntax_errors {
            println!("{} - {}", error.range, error.message)
        }

        top_levels
    }

    pub fn get_syntax_errors(&self) -> &Vec<Diagnostic> {
        &self.syntax_errors
    }
}
