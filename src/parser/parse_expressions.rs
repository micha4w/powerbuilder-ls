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
        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

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

        if let Some(tick) = self.optional(TokenType::Symbol(tokenizer::Symbol::TICK))? {
            let start = name.range.start;
            let group = name.content;

            let (name, end, err) = match self.expect(TokenType::ID)? {
                Ok(id) => (id.content, id.range.end, None),
                Err(err) => ("".into(), tick.range.end, Some(err)),
            };

            return Some(ParseResult::new(
                DataType {
                    data_type_type: DataTypeType::Complex(GroupedName::new(Some(group), name)),
                    range: Range { start, end },
                },
                err,
            ));
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
            _ => DataTypeType::Complex(GroupedName::new(None, name.content.clone())),
        };

        Some(Ok(DataType {
            data_type_type: data_type,
            range: name.range,
        }))
    }

    pub fn parse_array_declaration(&mut self) -> EOFOrParserResult<Expression> {
        let Range { start, mut end } = self.next()?.range;
        let mut expressions = Vec::new();
        let mut err = None;

        loop {
            match self.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                    while self.peek()?.token_type == TokenType::Symbol(tokenizer::Symbol::COMMA) {
                        end = self.next()?.range.end;
                        expressions.push(Expression {
                            expression_type: ExpressionType::Error,
                            range: Range { start: end, end },
                        })
                    }
                }
                TokenType::Symbol(tokenizer::Symbol::RCURLY) => {
                    end = self.next()?.range.end;
                    break;
                }
                _ => {}
            }

            let expression;
            (expression, err) = self.parse_expression()?.split();
            if let Some(expression) = expression {
                end = expression.range.end;
                expressions.push(expression);
            }
            if err.is_some() {
                break;
            }

            match self.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                    end = self.next()?.range.end;
                }
                TokenType::Symbol(tokenizer::Symbol::RCURLY) => {}
                _ => {
                    let range = self.peek()?.range;
                    err = self
                        .fatal::<()>(&"Expected either , or }".into(), range, true)?
                        .err();
                    break;
                }
            }
        }

        Some(ParseResult::new(
            Expression {
                expression_type: ExpressionType::ArrayLiteral(expressions),
                range: Range::new(start, end),
            },
            err,
        ))
    }

    pub fn parse_expression(&mut self) -> EOFOrParserResult<Expression> {
        let start = self.peek()?.range.start;
        let mut end;
        let mut err = None;

        let expression_type = match self.peek()?.token_type {
            TokenType::Symbol(tokenizer::Symbol::LPAREN) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                if err.is_none() {
                    match self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => err = Some(res_err),
                    }
                }
                ExpressionType::Parenthesized(Box::new(expression))
            }
            TokenType::Symbol(tokenizer::Symbol::LCURLY) => {
                let array = quick_exit!(self.parse_array_declaration()?, err);
                end = array.range.end;
                array.expression_type
            }
            TokenType::Keyword(tokenizer::Keyword::NOT) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                ExpressionType::BooleanNot(Box::new(expression))
            }
            TokenType::Operator(
                operator @ (tokenizer::Operator::PLUS | tokenizer::Operator::MINUS),
            ) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                ExpressionType::UnaryOperation(operator, Box::new(expression))
            }
            TokenType::Keyword(tokenizer::Keyword::THIS)
            | TokenType::Keyword(tokenizer::Keyword::SUPER)
            | TokenType::Keyword(tokenizer::Keyword::PARENT)
            | TokenType::ID => {
                let lvalue = quick_exit!(self.parse_lvalue()?, err);
                end = lvalue.range.end;
                ExpressionType::LValue(lvalue)
            }
            TokenType::Keyword(tokenizer::Keyword::CREATE) => {
                self.next()?;

                match self.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::USING) => {
                        let class = quick_exit!(self.parse_expression()?, err);
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
                tokenizer::Symbol::RBRACE
                | tokenizer::Symbol::COMMA
                | tokenizer::Symbol::DOT
                | tokenizer::Symbol::PLUSPLUS
                | tokenizer::Symbol::MINUSMINUS,
            ) => {
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
            TokenType::Symbol(
                operator @ (tokenizer::Symbol::PLUSPLUS | tokenizer::Symbol::MINUSMINUS),
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
            let right_side = quick_exit!(self.parse_expression()?, err);

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

    pub fn parse_lvalue(&mut self) -> EOFOrParserResult<LValue> {
        let mut previous = match self.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::SUPER) => Some(LValue {
                lvalue_type: LValueType::Super,
                range: self.next()?.range,
            }),
            TokenType::Keyword(tokenizer::Keyword::THIS) => Some(LValue {
                lvalue_type: LValueType::This,
                range: self.next()?.range,
            }),
            TokenType::Keyword(tokenizer::Keyword::PARENT) => Some(LValue {
                lvalue_type: LValueType::Parent,
                range: self.next()?.range,
            }),
            _ => None,
        };

        loop {
            match &mut previous {
                Some(prev) => match self.peek()?.token_type {
                    TokenType::Symbol(tokenizer::Symbol::COLONCOLON) => {
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
                    TokenType::Symbol(tokenizer::Symbol::LBRACE) => {
                        let lbrace = self.next()?;
                        let res = match self.parse_expression()? {
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

                    let mut arguments = Vec::new();

                    let mut err = None;
                    let mut end;
                    match self.expect(TokenType::Symbol(tokenizer::Symbol::LPAREN))? {
                        Ok(lparen) => {
                            end = lparen.range.end;
                            if self.peek()?.token_type
                                != TokenType::Symbol(tokenizer::Symbol::RPAREN)
                            {
                                loop {
                                    let argument;
                                    (argument, err) = self.parse_expression()?.split();
                                    if let Some(arg) = argument {
                                        end = arg.range.end;
                                        arguments.push(arg);
                                    }
                                    if err.is_some() {
                                        break;
                                    }

                                    let token = self.next()?;
                                    match token.token_type {
                                        TokenType::Symbol(tokenizer::Symbol::COMMA) => {}
                                        TokenType::Symbol(tokenizer::Symbol::RPAREN) => {
                                            end = token.range.end;
                                            break;
                                        }
                                        _ => {
                                            err = self
                                                .fatal::<()>(
                                                    &"Expected ',' or ')'".into(),
                                                    token.range,
                                                    token.token_type != TokenType::NEWLINE,
                                                )?
                                                .err();
                                            break;
                                        }
                                    }
                                }
                            } else {
                                end = self.next()?.range.end;
                            }
                        }
                        Err(res_err) => {
                            err = Some(res_err);
                            end = name.range.end
                        }
                    }

                    let func = FunctionCall {
                        range: Range {
                            start: name.range.start,
                            end,
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
                                end,
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
                            range: Range {
                                start: prev.range.start,
                                end: var.name.range.end,
                            },
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

        match previous {
            Some(prev) => Some(Ok(prev)),
            None => Some(Err((ParseError::UnexpectedToken, None))),
        }
    }
}
