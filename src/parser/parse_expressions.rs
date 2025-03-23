use super::*;
use crate::{
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl Parser {
    // TODO why is this function included inside parse_type
    pub fn parse_class_id(&mut self) -> EOFOrParserResult<(Option<Token>, Token)> {
        let name = self.next()?;
        Some(
            if self
                .optional(TokenType::Symbol(tokenizer::Symbol::TICK))?
                .is_some()
            {
                match self.expect(TokenType::ID)? {
                    Ok(class_name) => Ok((Some(name), class_name)),
                    Err(err) => Err((err, Some((None, name)))),
                }
            } else {
                Ok((None, name))
            },
        )
    }

    pub fn parse_type(&mut self) -> EOFOrParserResult<DataType> {
        let mut err = None;
        let (group, name) = quick_exit!(self.parse_class_id()?, err);
        if let Some(group) = group {
            return Some(ParseResult::new(
                DataType {
                    data_type_type: DataTypeType::Complex(GroupedName::new(
                        Some(group.content),
                        name.content,
                    )),
                    range: Range {
                        start: group.range.start,
                        end: name.range.end,
                    },
                },
                err,
            ));
        }

        if name.content.eq_ignore_ascii_case("decimal") || name.content.eq_ignore_ascii_case("dec")
        {
            let mut precision = None;
            let end = match self.optional(TokenType::Symbol(tokenizer::Symbol::LCURLY))? {
                Some(lcurly) => {
                    match self.optional(TokenType::Literal(tokenizer::Literal::NUMBER))? {
                        Some(prec) => {
                            precision = match prec.content.parse() {
                                Ok(prec) => Some(prec),
                                Err(err) => {
                                    self.error(
                                        &format!("Integer Parse Error: {}", err),
                                        prec.range,
                                    );
                                    None
                                }
                            };

                            match self.optional(TokenType::Symbol(tokenizer::Symbol::RCURLY))? {
                                Some(rcurly) => rcurly.range.end,
                                None => {
                                    let range = self.peek()?.range;
                                    self.error(&"Expected '{'".into(), range);
                                    prec.range.end
                                }
                            }
                        }
                        None => {
                            let range = self.peek()?.range;
                            self.error(&"Expected a Number Literal".into(), range);
                            lcurly.range.end
                        }
                    }
                }
                None => name.range.end,
            };

            return Some(Ok(DataType {
                data_type_type: DataTypeType::Decimal(precision),
                range: Range {
                    start: name.range.start,
                    end,
                },
            }));
        }

        let data_type = match name.content.to_lowercase().as_str() {
            "any" => DataTypeType::Any,
            "blob" => DataTypeType::Blob,
            "boolean" => DataTypeType::Boolean,
            "byte" => DataTypeType::Byte,
            "char" => DataTypeType::Char,
            "date" => DataTypeType::Date,
            "datetime" => DataTypeType::Datetime,
            "double" => DataTypeType::Double,
            "integer" | "int" => DataTypeType::Int,
            "long" => DataTypeType::Long,
            "longlong" => DataTypeType::Longlong,
            "longptr" => DataTypeType::Longptr,
            "real" => DataTypeType::Real,
            "string" => DataTypeType::String,
            "time" => DataTypeType::Time,
            "unsignedinteger" | "unsignedint" | "uint" => DataTypeType::Uint,
            "unsignedlong" | "ulong" => DataTypeType::Ulong,
            _ => DataTypeType::Complex(GroupedName::simple(name.content.clone())),
        };

        Some(Ok(DataType {
            data_type_type: data_type,
            range: name.range,
        }))
    }

    pub fn parse_expression_list(
        &mut self,
        is_sql: bool,
        starting_token: TokenType,
        ending_token: TokenType,
    ) -> EOFOrParserResult<(Vec<Expression>, Range)> {
        let mut range = quick_exit_simple!(self.expect(starting_token)?).range;

        let mut expressions = Vec::new();
        let mut err = None;

        loop {
            match self.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                    while self.peek()?.token_type == TokenType::Symbol(tokenizer::Symbol::COMMA) {
                        range.end = self.next()?.range.end;
                        expressions.push(Expression {
                            expression_type: ExpressionType::Error,
                            range: Range {
                                start: range.end,
                                end: range.end,
                            },
                        })
                    }
                }
                ending if ending == ending_token => {
                    range.end = self.next()?.range.end;
                    break;
                }
                _ => {}
            }

            let expression;
            (expression, err) = self.parse_expression(is_sql)?.split();
            if let Some(expression) = expression {
                range.end = expression.range.end;
                expressions.push(expression);
            }
            if err.is_some() {
                break;
            }

            match self.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                    range.end = self.next()?.range.end;
                }
                token_type => {
                    if token_type != ending_token {
                        let range = self.peek()?.range;
                        err = self
                            .fatal::<()>(
                                &format!("Expected either ',' or {:#}", ending_token),
                                range,
                                matches!(
                                    token_type,
                                    TokenType::NEWLINE
                                        | TokenType::Symbol(tokenizer::Symbol::SEMICOLON)
                                ),
                            )?
                            .err();
                        break;
                    }
                }
            }
        }

        Some(ParseResult::new((expressions, range), err))
    }

    pub fn parse_array_declaration(&mut self) -> EOFOrParserResult<Expression> {
        let mut err = None;
        let (expressions, range) = quick_exit!(
            self.parse_expression_list(
                false,
                TokenType::Symbol(tokenizer::Symbol::LCURLY),
                TokenType::Symbol(tokenizer::Symbol::RCURLY),
            )?,
            err
        );

        Some(ParseResult::new(
            Expression {
                expression_type: ExpressionType::ArrayLiteral(expressions),
                range,
            },
            err,
        ))
    }

    pub fn parse_expression(&mut self, is_sql: bool) -> EOFOrParserResult<Expression> {
        let start = self.peek()?.range.start;
        let mut end;
        let mut err = None;

        let expression_type = match self.peek()?.token_type {
            TokenType::Symbol(tokenizer::Symbol::LPAREN) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression(is_sql)?, err);
                end = expression.range.end;
                if err.is_none() {
                    match self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => err = Some(res_err),
                    }
                }
                ExpressionType::Parenthesized(Box::new(expression))
            }
            TokenType::Symbol(tokenizer::Symbol::LCURLY) if !is_sql => {
                let array = quick_exit!(self.parse_array_declaration()?, err);
                end = array.range.end;
                array.expression_type
            }
            TokenType::Keyword(tokenizer::Keyword::NOT) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression(is_sql)?, err);
                end = expression.range.end;
                ExpressionType::BooleanNot(Box::new(expression))
            }
            TokenType::Operator(
                operator @ (tokenizer::Operator::PLUS | tokenizer::Operator::MINUS),
            ) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression(is_sql)?, err);
                end = expression.range.end;
                ExpressionType::UnaryOperation(operator, Box::new(expression))
            }
            TokenType::Keyword(tokenizer::Keyword::THIS)
            | TokenType::Keyword(tokenizer::Keyword::SUPER)
            | TokenType::Keyword(tokenizer::Keyword::PARENT)
            | TokenType::ID => {
                let lvalue = quick_exit!(self.parse_lvalue(is_sql)?, err);
                end = lvalue.range.end;
                ExpressionType::LValue(lvalue)
            }
            TokenType::Keyword(tokenizer::Keyword::CREATE) if !is_sql => {
                self.next()?;

                match self.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::USING) => {
                        let class = quick_exit!(self.parse_expression(is_sql)?, err);
                        end = class.range.end;
                        ExpressionType::CreateUsing(Box::new(class))
                    }
                    TokenType::ID => {
                        let class = self.next()?;

                        end = class.range.end;
                        ExpressionType::Create(DataType {
                            data_type_type: DataTypeType::Complex(GroupedName::new(
                                None,
                                class.content,
                            )),
                            range: class.range,
                        })
                    }
                    _ => {
                        let range = self.peek()?.range;
                        return self.fatal_res(
                            &"Expected 'USING' or a Class Name".into(),
                            range,
                            true,
                            None,
                        );
                    }
                }
            }
            TokenType::Literal(literal) => {
                let token = self.next()?;
                end = token.range.end;
                ExpressionType::Literal(Literal {
                    literal_type: literal,
                    content: token.content,
                    range: token.range,
                })
            }
            TokenType::Symbol(
                tokenizer::Symbol::RBRACE | tokenizer::Symbol::COMMA | tokenizer::Symbol::DOT,
            )
            | TokenType::IncrDecrOperator(_) => {
                let range = self.peek()?.range;
                self.error(&"Unexpected Token for Expression".into(), range);
                end = range.end;
                ExpressionType::Error
            }
            _ => {
                let range = self.peek()?.range;
                return self.fatal_res(
                    &"Unexpected Token for Expression".into(),
                    range,
                    true,
                    None,
                );
            }
        };

        let mut expression = Expression {
            expression_type,
            range: Range { start, end },
        };
        if let Some(err) = err {
            return Some(Err((err, Some(expression))));
        }

        match self.peek()?.token_type {
            TokenType::IncrDecrOperator(operator) => {
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
            let right_side = quick_exit!(self.parse_expression(is_sql)?, err);

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

        Some(ParseResult::new(expression, err))
    }

    pub fn parse_lvalue(&mut self, is_sql: bool) -> EOFOrParserResult<LValue> {
        let colon = if is_sql {
            self.optional(TokenType::Symbol(tokenizer::Symbol::COLON))?
        } else {
            None
        };

        let keyword = match self.peek()?.token_type {
            TokenType::Keyword(kw @ (tokenizer::Keyword::THIS | tokenizer::Keyword::PARENT)) => {
                Some(kw)
            }
            TokenType::Keyword(kw @ tokenizer::Keyword::SUPER) if colon.is_none() || !is_sql => {
                Some(kw)
            }
            _ => None,
        };

        let mut previous = match keyword {
            Some(kw) => Some({
                let token = self.next()?;
                let range = token.range;

                LValue {
                    lvalue_type: if is_sql && colon.is_none() {
                        LValueType::Variable(VariableAccess {
                            name: token,
                            is_write: false,
                        })
                    } else {
                        match kw {
                            tokenizer::Keyword::SUPER => LValueType::Super,
                            tokenizer::Keyword::THIS => LValueType::This,
                            tokenizer::Keyword::PARENT => LValueType::Parent,
                            _ => unreachable!(),
                        }
                    },
                    range,
                }
            }),
            None => None,
        };

        loop {
            match &mut previous {
                Some(prev) => match self.peek()?.token_type {
                    TokenType::Symbol(tokenizer::Symbol::COLONCOLON) if !is_sql => {
                        let colon = self.next()?;

                        match prev.lvalue_type {
                            LValueType::Super => {}
                            LValueType::Variable(_) => prev.lvalue_type = LValueType::Super, // TODO ?
                            _ => {
                                self.error(
                                    &"Only super methods can be indexed using '::'".into(),
                                    colon.range,
                                );
                            }
                        }
                    }
                    TokenType::Symbol(tokenizer::Symbol::DOT) => {
                        let dot = self.next()?;

                        if let LValueType::Super = prev.lvalue_type {
                            self.error(
                                &"Super methods have to be indexed using '::'".into(),
                                dot.range,
                            );
                        }
                    }
                    TokenType::Symbol(tokenizer::Symbol::LBRACE) if !is_sql => {
                        let lbrace = self.next()?;
                        let res = match self.parse_expression(is_sql)? {
                            Ok(exp) => {
                                match self.expect(TokenType::Symbol(tokenizer::Symbol::RBRACE))? {
                                    Ok(rbrace) => Ok((rbrace.range.end, exp)),
                                    Err(err) => Err((err, Some((exp.range.end, exp)))),
                                }
                            }
                            Err((err, exp)) => Err((err, exp.map(|exp| (exp.range.end, exp)))),
                        };
                        let expression = match res {
                            Ok((_, exp)) => exp,
                            Err((err, res)) => {
                                let (end, exp) = res.unzip();
                                let lvalue = LValue {
                                    range: Range {
                                        start: prev.range.start,
                                        end: end.unwrap_or(lbrace.range.end),
                                    },
                                    lvalue_type: LValueType::Index(
                                        Box::new(previous.take().unwrap()),
                                        Box::new(exp.unwrap_or(Expression {
                                            expression_type: ExpressionType::Error,
                                            range: Range {
                                                start: lbrace.range.end,
                                                end: lbrace.range.end,
                                            },
                                        })),
                                    ),
                                };

                                return Some(Err((err, Some(lvalue))));
                            }
                        };

                        previous = Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end: expression.range.end,
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

            match (self.peek()?.token_type, self.peek_nth(1)?.token_type) {
                (
                    TokenType::Keyword(tokenizer::Keyword::STATIC)
                    | TokenType::Keyword(tokenizer::Keyword::DYNAMIC)
                    | TokenType::Keyword(tokenizer::Keyword::EVENT)
                    | TokenType::Keyword(tokenizer::Keyword::POST),
                    _,
                )
                | (_, TokenType::Symbol(tokenizer::Symbol::LPAREN)) => {
                    let mut dynamics = Vec::new();
                    let mut events = Vec::new();
                    let mut posts = Vec::new();
                    let mut statics = Vec::new();
                    if !is_sql
                    /* TODO: || colon.is_some()*/
                    {
                        loop {
                            match self.peek()?.token_type {
                                TokenType::Keyword(tokenizer::Keyword::STATIC) => {
                                    statics.push(self.next()?)
                                }
                                TokenType::Keyword(tokenizer::Keyword::DYNAMIC) => {
                                    dynamics.push(self.next()?)
                                }
                                TokenType::Keyword(tokenizer::Keyword::EVENT) => {
                                    events.push(self.next()?)
                                }
                                TokenType::Keyword(tokenizer::Keyword::POST) => {
                                    posts.push(self.next()?)
                                }
                                _ => break,
                            }
                        }
                    }

                    let name = quick_exit_simple!(self.expect(TokenType::ID)?);

                    if !dynamics.is_empty() && !statics.is_empty() {
                        self.error(
                            &"Mixing DYNAMIC and STATIC is not allowed".into(),
                            name.range,
                        );
                    }
                    for dynamic in dynamics.iter().skip(1) {
                        self.error(
                            &"Cannot have multiple DYNAMIC keywords".into(),
                            dynamic.range,
                        );
                    }
                    for r#static in statics.iter().skip(1) {
                        self.error(
                            &"Cannot have multiple STATIC keywords".into(),
                            r#static.range,
                        );
                    }
                    for post in posts.iter().skip(1) {
                        self.error(&"Cannot have multiple POST keywords".into(), post.range);
                    }
                    for event in events.iter().skip(1) {
                        self.error(&"Cannot have multiple EVENT keywords".into(), event.range);
                    }

                    if let Some(LValueType::Super) =
                        previous.as_ref().map(|lvalue| &lvalue.lvalue_type)
                    {
                        if let Some(dynamic) = dynamics.get(0) {
                            self.error(
                                &"'SUPER::' calls do not support DYNAMIC".into(),
                                dynamic.range,
                            );
                        }
                    }

                    let (res, err) = self
                        .parse_expression_list(
                            is_sql,
                            TokenType::Symbol(tokenizer::Symbol::LPAREN),
                            TokenType::Symbol(tokenizer::Symbol::RPAREN),
                        )?
                        .split();
                    let (arguments, range) = res.unwrap_or((Vec::new(), name.range.end.into()));

                    let func = FunctionCall {
                        range: Range {
                            start: name.range.start,
                            end: range.end,
                        },
                        name,
                        arguments,
                        dynamic: dynamics.into_iter().next(),
                        event: events.into_iter().next(),
                        post: posts.into_iter().next(),
                    };

                    previous = match previous {
                        Some(prev) => Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end: range.end,
                            },
                            lvalue_type: LValueType::Method(Box::new(prev), func),
                        }),
                        None => Some(LValue {
                            range: func.range,
                            lvalue_type: LValueType::Function(func),
                        }),
                    };

                    if let Some(err) = err {
                        return Some(Err((err, previous)));
                    }
                }
                (TokenType::ID, _) => {
                    let var = VariableAccess {
                        name: self.next()?,
                        is_write: false,
                    };

                    previous = match previous {
                        Some(prev) => Some(LValue {
                            range: Range::merge(&prev.range, &var.name.range),
                            lvalue_type: LValueType::Member(Box::new(prev), var),
                        }),
                        None => Some(LValue {
                            range: var.name.range,
                            lvalue_type: LValueType::Variable(var),
                        }),
                    };
                }
                _ => {
                    let range = self.peek()?.range;
                    return self.fatal_res(
                        &"Unexpected Token for LValue".into(),
                        range,
                        true,
                        previous,
                    );
                }
            }
        }

        Some(match previous {
            Some(prev) => Ok(match colon {
                Some(colon) => LValue {
                    range: Range::merge(&colon.range, &prev.range),
                    lvalue_type: LValueType::SQLAccess(colon, Box::new(prev)),
                },
                None => prev,
            }),
            None => {
                let range = self.peek()?.range.start.into();
                Err((
                    ParseError::UnexpectedToken,
                    Some(LValue {
                        lvalue_type: LValueType::Variable(VariableAccess {
                            name: Token {
                                token_type: TokenType::INVALID,
                                content: "".into(),
                                range,
                                error: None,
                            },
                            is_write: false,
                        }),
                        range,
                    }),
                ))
            }
        })
    }
}
