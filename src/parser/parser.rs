use std::backtrace::Backtrace;
use std::iter::Filter;

use multipeek::{multipeek, MultiPeek};

use super::parser_types::*;
use super::tokenizer::*;
use super::tokenizer_types as tokens;
use super::tokenizer_types::Range;
use super::tokenizer_types::ScopeModif;

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
            Err(_) => return Some(None),
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

// macro_rules! optional {
//     ( $self:expr, $token_type:expr ) => {
//         if let $token_type(token) = $self.peek()? {
//             Some($self.next()?)
//         } else {
//             None
//         }
//     };
// }

fn ret<T>(t: T, err: Option<ParseError>) -> EOFOrParserResult<T> {
    Some(match err {
        Some(err) => Err((err, Some(t))),
        None => Ok(t),
    })
}

pub struct Parser {
    last_token: Option<Token>,
    tokens: MultiPeek<Filter<FileTokenizer, fn(&Token) -> bool>>,
    syntax_errors: Vec<Diagnostic>,
}

impl Parser {
    pub fn new<'a>(tokens: FileTokenizer) -> Parser {
        fn filter(token: &Token) -> bool {
            token.token_type != TokenType::COMMENT
        }

        Parser {
            last_token: None,
            tokens: multipeek(tokens.filter::<fn(&Token) -> bool>(filter)),
            syntax_errors: Vec::new(),
        }
    }

    fn next(&mut self) -> EOFOr<Token> {
        self.last_token = self.tokens.next();
        self.last_token.clone()
    }

    fn peek(&mut self) -> EOFOr<&Token> {
        self.tokens.peek()
    }

    fn peek_nth(&mut self, n: usize) -> EOFOr<&Token> {
        self.tokens.peek_nth(n)
    }

    fn consume_line(&mut self) -> EOFOr<()> {
        loop {
            match self.next()?.token_type {
                TokenType::NEWLINE => break Some(()),
                _ => {}
            }
        }
    }

    fn hint(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Hint,
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
    }

    fn error(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Error,
            // message: format!("[Parser] {}", error),
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range,
        });
        // panic!("[Parser] {} {:?}\n", error, range);
    }

    fn fatal<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
    ) -> EOFOr<ParseResult<T>> {
        self.error(error, range);
        if consume_line {
            self.consume_line()?;
        }
        Some(Err(ParseError::UnexpectedToken))
    }

    fn fatal_res<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
        value: Option<T>,
    ) -> EOFOrParserResult<T> {
        self.fatal::<T>(error, range, consume_line)?.ok();
        Some(Err((ParseError::UnexpectedToken, value)))
    }

    fn optional(&mut self, token_type: TokenType) -> EOFOr<Option<Token>> {
        if self.peek()?.token_type == token_type {
            Some(Some(self.next()?))
        } else {
            Some(None)
        }
    }

    fn expect(&mut self, token_type: TokenType) -> EOFOr<ParseResult<Token>> {
        let token = self.next()?;
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

    pub fn parse_type(&mut self) -> EOFOrParserResult<DataType> {
        let token = quick_exit_simple!(self.expect(TokenType::ID)?);

        Some(
            if token.content.eq_ignore_ascii_case("decimal")
                || token.content.eq_ignore_ascii_case("dec")
            {
                let mut precission = None;
                let end = match self.optional(TokenType::Symbol(tokens::Symbol::LCURLY))? {
                    Some(lcurly) => {
                        match self.optional(TokenType::Literal(tokens::Literal::NUMBER))? {
                            Some(prec) => {
                                precission = Some(prec.content);

                                match self.optional(TokenType::Symbol(tokens::Symbol::RCURLY))? {
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
                    None => token.range.end,
                };

                Ok(DataType {
                    data_type_type: DataTypeType::Decimal(precission),
                    range: Range {
                        start: token.range.start,
                        end,
                    },
                })
            } else {
                if let Some(_) = self.optional(TokenType::Symbol(tokens::Symbol::TICK))? {
                    match self.expect(TokenType::ID)? {
                        Ok(group) => Ok(DataType {
                            data_type_type: DataTypeType::Complex(group.content, token.content),
                            range: Range {
                                start: token.range.start,
                                end: group.range.end,
                            },
                        }),
                        Err(err) => Err((
                            err,
                            Some(DataType {
                                data_type_type: DataTypeType::ID(token.content),
                                range: token.range,
                            }),
                        )),
                    }
                } else {
                    Ok(DataType {
                        data_type_type: DataTypeType::ID(token.content),
                        range: token.range,
                    })
                }
            },
        )
    }

    fn parse_expression(&mut self) -> EOFOrParserResult<Expression> {
        let start = self.peek()?.range.start;
        let mut end;
        let mut err = None;

        let expression_type = match self.peek()?.token_type {
            TokenType::Symbol(tokens::Symbol::LPAREN) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                if err.is_none() {
                    match self.expect(TokenType::Symbol(tokens::Symbol::RPAREN))? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => err = Some(res_err),
                    }
                }
                ExpressionType::Parenthesized(Box::new(expression))
            }
            TokenType::Symbol(tokens::Symbol::LCURLY) => {
                end = self.next()?.range.end;
                let mut expressions = Vec::new();

                loop {
                    match self.peek()?.token_type {
                        TokenType::Symbol(tokens::Symbol::COMMA) => {
                            while self.peek()?.token_type
                                == TokenType::Symbol(tokens::Symbol::COMMA)
                            {
                                end = self.next()?.range.end;
                                expressions.push(Expression {
                                    expression_type: ExpressionType::Error,
                                    range: Range { start: end, end },
                                })
                            }
                        }
                        TokenType::Symbol(tokens::Symbol::RCURLY) => {
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
                        TokenType::Symbol(tokens::Symbol::COMMA) => {
                            end = self.next()?.range.end;
                        }
                        TokenType::Symbol(tokens::Symbol::RCURLY) => {}
                        _ => {
                            let range = self.peek()?.range;
                            err = self
                                .fatal::<()>(&"Expected either , or }".into(), range, true)?
                                .err();
                            break;
                        }
                    }
                }
                ExpressionType::ArrayLiteral(expressions)
            }
            TokenType::Keyword(tokens::Keyword::NOT) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                ExpressionType::BooleanNot(Box::new(expression))
            }
            TokenType::Operator(operator @ (tokens::Operator::PLUS | tokens::Operator::MINUS)) => {
                self.next()?;
                let expression = quick_exit!(self.parse_expression()?, err);
                end = expression.range.end;
                ExpressionType::UnaryOperation(operator, Box::new(expression))
            }
            TokenType::Keyword(tokens::Keyword::THIS)
            | TokenType::Keyword(tokens::Keyword::SUPER)
            | TokenType::Keyword(tokens::Keyword::PARENT)
            | TokenType::ID => {
                let lvalue = quick_exit!(self.parse_lvalue()?, err);
                end = lvalue.range.end;
                ExpressionType::LValue(lvalue)
            }
            TokenType::Keyword(tokens::Keyword::CREATE) => {
                self.next()?;

                match self.peek()?.token_type {
                    TokenType::Keyword(tokens::Keyword::USING) => {
                        let class = quick_exit!(self.parse_expression()?, err);
                        end = class.range.end;
                        ExpressionType::CreateUsing(Box::new(class))
                    }
                    TokenType::ID => {
                        let class = self.next()?;

                        end = class.range.end;
                        ExpressionType::Create(DataType {
                            data_type_type: DataTypeType::ID(class.content),
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
                tokens::Symbol::RBRACE
                | tokens::Symbol::COMMA
                | tokens::Symbol::DOT
                | tokens::Symbol::PLUSPLUS
                | tokens::Symbol::MINUSMINUS,
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

        ret(expression, err)
    }

    fn parse_lvalue(&mut self) -> EOFOrParserResult<LValue> {
        let mut previous = match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::SUPER) => Some(LValue {
                lvalue_type: LValueType::Super,
                range: self.next()?.range,
            }),
            TokenType::Keyword(tokens::Keyword::THIS) => Some(LValue {
                lvalue_type: LValueType::This,
                range: self.next()?.range,
            }),
            TokenType::Keyword(tokens::Keyword::PARENT) => Some(LValue {
                lvalue_type: LValueType::Parent,
                range: self.next()?.range,
            }),
            _ => None,
        };

        loop {
            match &mut previous {
                Some(prev) => match self.peek()?.token_type {
                    TokenType::Symbol(tokens::Symbol::COLONCOLON) => {
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
                    TokenType::Symbol(tokens::Symbol::DOT) => {
                        let dot = self.next()?;

                        if let LValueType::Super = prev.lvalue_type {
                            self.error(
                                &"Super methods have to be indexed using '::'".into(),
                                dot.range,
                            );
                        }
                    }
                    TokenType::Symbol(tokens::Symbol::LBRACE) => {
                        let lbrace = self.next()?;
                        let res = match self.parse_expression()? {
                            Ok(exp) => {
                                match self.expect(TokenType::Symbol(tokens::Symbol::RBRACE))? {
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
                    TokenType::Keyword(tokens::Keyword::STATIC)
                    | TokenType::Keyword(tokens::Keyword::DYNAMIC)
                    | TokenType::Keyword(tokens::Keyword::EVENT)
                    | TokenType::Keyword(tokens::Keyword::POST),
                    _,
                )
                | (_, TokenType::Symbol(tokens::Symbol::LPAREN)) => {
                    let mut dynamics = Vec::new();
                    let mut events = Vec::new();
                    let mut posts = Vec::new();
                    let mut statics = Vec::new();
                    loop {
                        match self.peek()?.token_type {
                            TokenType::Keyword(tokens::Keyword::STATIC) => {
                                statics.push(self.next()?)
                            }
                            TokenType::Keyword(tokens::Keyword::DYNAMIC) => {
                                dynamics.push(self.next()?)
                            }
                            TokenType::Keyword(tokens::Keyword::EVENT) => events.push(self.next()?),
                            TokenType::Keyword(tokens::Keyword::POST) => posts.push(self.next()?),
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
                    match self.expect(TokenType::Symbol(tokens::Symbol::LPAREN))? {
                        Ok(lparen) => {
                            end = lparen.range.end;
                            if self.peek()?.token_type != TokenType::Symbol(tokens::Symbol::RPAREN)
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
                                        TokenType::Symbol(tokens::Symbol::COMMA) => {}
                                        TokenType::Symbol(tokens::Symbol::RPAREN) => {
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
                    let var = VariableAccess { name: self.next()? };

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

    fn parse_argument_list_declaration(
        &mut self,
    ) -> EOFOrParserResult<(Range, Vec<Argument>, Option<Token>)> {
        let start = quick_exit_simple!(self.expect(TokenType::Symbol(tokens::Symbol::LPAREN))?)
            .range
            .start;

        let mut end = tokens::Position {
            line: start.line,
            column: start.column + 1,
        };
        let mut vararg = None;
        let mut err = None;

        let mut arguments = Vec::new();
        loop {
            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                    end = self.next()?.range.end;
                    break;
                }
                TokenType::Symbol(tokens::Symbol::DOTDOTDOT) => {
                    let token = self.next()?;
                    end = self.next()?.range.end;
                    vararg = Some(token);

                    match self.expect(TokenType::Symbol(tokens::Symbol::RPAREN))? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => err = Some(res_err),
                    }
                    break;
                }
                _ => {}
            };

            let var_start = self.peek()?.range.end;
            let is_ref = self
                .optional(TokenType::Keyword(tokens::Keyword::REF))?
                .is_some();
            let is_readonly = self
                .optional(TokenType::Keyword(tokens::Keyword::READONLY))?
                .is_some();

            let data_type = match self.parse_type()? {
                Ok(data_type) => data_type,
                Err((res_err, _)) => {
                    err = Some(res_err);
                    break;
                }
            };
            end = data_type.range.end;

            let name = match self.expect(TokenType::ID)? {
                Ok(token) => {
                    end = token.range.end;
                    token
                }
                Err(res_err) => {
                    err = Some(res_err);
                    break;
                }
            };

            arguments.push(Argument {
                is_ref,
                variable: Variable {
                    constant: is_readonly,
                    data_type,
                    name,
                    initial_value: None,
                    range: Range {
                        start: var_start,
                        end,
                    },
                },
            });

            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::COMMA) => {
                    end = self.next()?.range.end;
                }
                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                    end = self.next()?.range.end;
                    break;
                }
                _ => {
                    let range = self.peek()?.range;
                    err = self
                        .fatal::<()>(&"Expected ',' or ')'".into(), range, true)?
                        .err();
                }
            }
        }

        let list = (Range { start, end }, arguments, vararg);
        ret(list, err)
    }

    fn parse_event_header(&mut self) -> EOFOrParserResult<Event> {
        let start = quick_exit_simple!(self.expect(TokenType::Symbol(tokens::Symbol::LPAREN))?)
            .range
            .start;

        let returns = if self
            .optional(TokenType::Keyword(tokens::Keyword::TYPE))?
            .is_some()
        {
            Some(quick_exit!(self.parse_type()?))
        } else {
            None
        };

        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

        let mut end;
        let mut err = None;
        let event_type = match self.peek()?.token_type {
            TokenType::ID => {
                let system = self.next()?;
                end = system.range.end;

                if returns.is_some() {
                    self.error(
                        &"Can't specify a return Type when declaring a System Event".into(),
                        system.range,
                    );
                }

                EventType::System(system.content)
            }
            TokenType::Symbol(tokens::Symbol::LPAREN) => {
                end = self.peek()?.range.end;
                let list;
                (list, err) = self.parse_argument_list_declaration()?.split();
                EventType::User(
                    returns,
                    match list {
                        Some((range, arguments, vararg)) => {
                            if let Some(vararg) = vararg {
                                self.error(
                                    &"Events cannot have variadic Arguments".into(),
                                    vararg.range,
                                );
                            }

                            end = range.end;
                            arguments
                        }
                        None => Vec::new(),
                    },
                )
            }
            _ => {
                // TODO what event type is this?
                end = name.range.end;

                if returns.is_some() {
                    self.error(
                        &"Can't specify a return Type when declaring a Predefined Event".into(),
                        Range { start, end },
                    );
                }

                EventType::Predefined
            }
        };

        if err.is_none() {
            err = self.expect(TokenType::NEWLINE)?.err();
        }

        let event = Event {
            name,
            event_type,
            range: Range { start, end },
        };

        ret(event, err)
    }

    fn parse_function_header(&mut self) -> EOFOrParserResult<Function> {
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
                Some(quick_exit!(self.parse_type()?))
            }
            TokenType::Keyword(tokens::Keyword::SUBROUTINE) => {
                self.next()?;
                None
            }
            _ => {
                let range = self.peek()?.range;
                return self.fatal_res(
                    &"Expected FUNCTION or SUBROUTINE".into(),
                    range,
                    true,
                    None,
                );
            }
        };

        let name = quick_exit_simple!(self.expect(TokenType::ID)?);
        let mut end = name.range.end;

        let (list, mut err) = self.parse_argument_list_declaration()?.split();
        let (arguments, vararg) = match list {
            Some((range, arguments, vararg)) => {
                end = range.end;
                (arguments, vararg)
            }
            None => (Vec::new(), None),
        };

        if err.is_none() {
            if let Some(token) = self.optional(TokenType::Keyword(tokens::Keyword::THROWS))? {
                end = token.range.end;

                loop {
                    match self.expect(TokenType::ID)? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => {
                            err = Some(res_err);
                            break;
                        }
                    }

                    match self.optional(TokenType::Symbol(tokens::Symbol::COMMA))? {
                        Some(token) => end = token.range.end,
                        None => break,
                    }
                }
            }
        }

        if err.is_none() {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::RPCFUNC) => {
                    end = self.next()?.range.end;
                }
                TokenType::Keyword(tokens::Keyword::LIBRARY) => {
                    end = self.next()?.range.end;
                    err = self
                        .expect(TokenType::Literal(tokens::Literal::STRING))?
                        .err();

                    if err.is_none()
                        && self
                            .optional(TokenType::Keyword(tokens::Keyword::ALIAS))?
                            .is_some()
                    {
                        err = self.expect(TokenType::Keyword(tokens::Keyword::FOR))?.err();
                        if err.is_none() {
                            err = self
                                .expect(TokenType::Literal(tokens::Literal::STRING))?
                                .err();
                        }
                    }
                }
                _ => {}
            }
        }

        if err.is_none() {
            err = self.expect(TokenType::NEWLINE)?.err();
        }

        ret(
            Function {
                returns,
                scope_modif,
                access,
                name,
                arguments,
                vararg,

                range: Range { start, end },
            },
            err,
        )
    }

    fn parse_variable_declaration(&mut self) -> EOFOrParserResult<Statement> {
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
                        self.error(&"Only one Access Right allowed".into(), token.range);
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
                                        token.range,
                                    );
                                }
                            } else {
                                self.error(
                                    &"Cannot supply more than one Write Access".into(),
                                    token.range,
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
                                        token.range,
                                    );
                                }
                            } else {
                                self.error(
                                    &"Cannot supply more than one Read Access".into(),
                                    token.range,
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

        let mut data_type = quick_exit!(self.parse_type()?);
        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

        let mut end = name.range.end;
        let mut err = None;
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
                        end = self.next()?.range.end;
                    }
                    TokenType::Literal(tokens::Literal::NUMBER) if first => {}
                    _ => {
                        let range = self.peek()?.range;
                        err = self
                            .fatal::<()>(
                                &if first {
                                    "Expected ']' or a Number Literal"
                                } else {
                                    "Expected ']' or ','"
                                }
                                .into(),
                                range,
                                true,
                            )?
                            .err();
                        break;
                    }
                }
                first = false;

                err = self
                    .expect(TokenType::Literal(tokens::Literal::NUMBER))?
                    .err();
                if err.is_some() {
                    break;
                }

                if self
                    .optional(TokenType::Keyword(tokens::Keyword::TO))?
                    .is_some()
                {
                    err = self
                        .expect(TokenType::Literal(tokens::Literal::NUMBER))?
                        .err();
                    if err.is_some() {
                        break;
                    }
                }
            }

            data_type = DataType {
                range: Range {
                    start: data_type.range.start,
                    end,
                },
                data_type_type: DataTypeType::Array(Box::new(data_type)),
            }
        }

        let expression = if err.is_none()
            && self
                .optional(TokenType::Operator(tokens::Operator::EQ))?
                .is_some()
        {
            let expression;
            (expression, err) = self.parse_expression()?.split();
            if let Some(ex) = &expression {
                end = ex.range.end;
            }
            expression
        } else {
            None
        };

        if err.is_none() {
            err = self.expect(TokenType::NEWLINE)?.err();
        }

        ret(
            Statement {
                statement_type: StatementType::Declaration(InstanceVariable {
                    access,
                    variable: Variable {
                        range: name.range,
                        data_type,
                        name,
                        constant,
                        initial_value: expression,
                    },
                }),
                range: Range { start, end },
            },
            err,
        )
    }

    fn parse_class_id(&mut self) -> EOFOrParserResult<(Option<Token>, Token)> {
        let name = self.next()?;
        Some(
            if self
                .optional(TokenType::Symbol(tokens::Symbol::TICK))?
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

    // Consumes trailing newlines
    fn parse_statement(&mut self) -> EOFOr<Option<Statement>> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::CONSTANT) => {
                Some(self.parse_variable_declaration()?.value())
            }
            TokenType::ID
            | TokenType::Keyword(
                tokens::Keyword::THIS | tokens::Keyword::SUPER | tokens::Keyword::PARENT,
            ) => match self.peek_nth(1)?.token_type {
                TokenType::ID => Some(self.parse_variable_declaration()?.value()),
                _ => {
                    match self.parse_expression()?.split() {
                        (Some(expression), mut err) => {
                            match (expression, self.peek()?.token_type) {
                                (
                                    Expression {
                                        expression_type:
                                            ExpressionType::Operation(left, tokens::Operator::EQ, right),
                                        ..
                                    },
                                    _,
                                ) if matches!(left.expression_type, ExpressionType::LValue(_),) => {
                                    if let ExpressionType::LValue(lvalue) = left.expression_type {
                                        if err.is_none() {
                                            self.expect(TokenType::NEWLINE)?.ok();
                                        }
                                        Some(Some(Statement {
                                            range: Range {
                                                start: left.range.start,
                                                end: right.range.end,
                                            },
                                            statement_type: StatementType::Assignment(
                                                lvalue, *right,
                                            ),
                                        }))
                                    } else {
                                        unreachable!()
                                    }
                                }
                                (expression, TokenType::SpecialAssignment(_)) => {
                                    self.next()?;
                                    if err.is_none() {
                                        let exp;
                                        (exp, err) = self.parse_expression()?.split();

                                        if let Some(value) = exp {
                                            if let ExpressionType::LValue(lvalue) =
                                                value.expression_type
                                            {
                                                if err.is_none() {
                                                    self.expect(TokenType::NEWLINE)?.ok();
                                                }
                                                Some(Some(Statement {
                                                    range: expression.range,
                                                    statement_type: StatementType::Assignment(
                                                        lvalue, expression,
                                                    ),
                                                }))
                                            } else {
                                                Some(
                                                    self.fatal(
                                                        &"Cannot assign to non-LValue".into(),
                                                        value.range,
                                                        true,
                                                    )?
                                                    .ok(),
                                                )
                                            }
                                        } else {
                                            Some(None)
                                        }
                                    } else {
                                        Some(None)
                                    }
                                }
                                (expression, _) => {
                                    self.expect(TokenType::NEWLINE)?.ok();
                                    Some(Some(Statement {
                                        range: expression.range,
                                        statement_type: StatementType::Expression(expression),
                                    }))
                                }
                            }
                        }
                        _ => Some(None),
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
                    //             &"Trying to assign to a non LValue".into(),
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
                let mut err = None;
                let expression = quick_exit_opt!(self.parse_expression()?, err);

                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.ok();
                }

                Some(Some(Statement {
                    range: Range {
                        start: throw.range.start,
                        end: expression.range.end,
                    },
                    statement_type: StatementType::Throw(expression),
                }))
            }
            TokenType::Keyword(tokens::Keyword::DESTROY) => {
                let destroy = self.next()?;
                let mut err = None;
                let expression = quick_exit_opt!(self.parse_expression()?, err);
                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.ok();
                }

                Some(Some(Statement {
                    range: Range {
                        start: destroy.range.start,
                        end: expression.range.end,
                    },
                    statement_type: StatementType::Destroy(expression),
                }))
            }
            TokenType::Keyword(tokens::Keyword::IF) => {
                let if_token = self.next()?;
                let mut end;
                let (condition, err) = match self.parse_expression()?.split() {
                    (Some(condition), mut err) => {
                        end = condition.range.end;
                        if err.is_none() {
                            match self.expect(TokenType::Keyword(tokens::Keyword::THEN))? {
                                Ok(then) => end = then.range.end,
                                Err(res_err) => err = Some(res_err),
                            }
                        }
                        (condition, err)
                    }
                    (None, err) => {
                        end = if_token.range.end;
                        (
                            Expression {
                                expression_type: ExpressionType::Error,
                                range: Range { start: end, end },
                            },
                            err,
                        )
                    }
                };

                match (self.peek()?.token_type, &err) {
                    (TokenType::NEWLINE, _) | (_, Some(_)) => {
                        if err.is_none() {
                            // Newline
                            self.next()?;
                        }

                        let mut ifs = IfStatement {
                            condition,
                            statements: Vec::new(),
                            elseif_statements: Vec::new(),
                            else_statements: Vec::new(),
                        };

                        let mut part = tokens::Keyword::IF;
                        let mut end;
                        'outer: loop {
                            loop {
                                match self.peek()?.token_type {
                                    TokenType::Keyword(tokens::Keyword::END) => {
                                        match self.peek_nth(1)?.token_type {
                                            TokenType::Keyword(tokens::Keyword::IF) => {
                                                self.next()?;
                                                end = self.next()?.range.end;
                                                self.expect(TokenType::NEWLINE)?.ok();
                                                break 'outer;
                                            }
                                            _ => {
                                                let range = self.peek()?.range;
                                                self.fatal::<()>(
                                                    &"Dangling END keyword, did you mean END IF"
                                                        .into(),
                                                    range,
                                                    true,
                                                )?
                                                .ok();
                                            }
                                        }
                                    }

                                    TokenType::Keyword(tokens::Keyword::ELSE) => {
                                        part = tokens::Keyword::ELSE;
                                        self.next()?;
                                        self.expect(TokenType::NEWLINE)?.ok();
                                        break;
                                    }
                                    TokenType::Keyword(tokens::Keyword::ELSEIF) => {
                                        part = tokens::Keyword::ELSEIF;
                                        end = self.next()?.range.end;

                                        let condition = match self.parse_expression()?.split() {
                                            (Some(condition), err) => {
                                                end = condition.range.end;

                                                if err.is_none()
                                                    && self
                                                        .expect(TokenType::Keyword(
                                                            tokens::Keyword::THEN,
                                                        ))?
                                                        .is_ok()
                                                {
                                                    self.expect(TokenType::NEWLINE)?.ok();
                                                }
                                                condition
                                            }
                                            (None, _) => Expression {
                                                expression_type: ExpressionType::Error,
                                                range: Range { start: end, end },
                                            },
                                        };

                                        ifs.elseif_statements.push((condition, Vec::new()));
                                        break;
                                    }
                                    _ => {
                                        if let Some(statement) = self.parse_statement()? {
                                            match part {
                                                tokens::Keyword::IF => &mut ifs.statements,
                                                tokens::Keyword::ELSEIF => {
                                                    &mut ifs.elseif_statements.last_mut().unwrap().1
                                                }
                                                tokens::Keyword::ELSE => &mut ifs.else_statements,
                                                _ => unreachable!(),
                                            }
                                            .push(statement);
                                        }
                                    }
                                }
                            }
                        }

                        Some(Some(Statement {
                            range: Range {
                                start: if_token.range.start,
                                end,
                            },
                            statement_type: StatementType::If(ifs),
                        }))
                    }
                    _ => {
                        let statement = self.parse_statement()?;

                        Some(Some(Statement {
                            range: Range {
                                start: if_token.range.start,
                                end: match &statement {
                                    Some(statement) => statement.range.end,
                                    None => end,
                                },
                            },
                            statement_type: StatementType::If(IfStatement {
                                condition,
                                statements: statement.into_iter().collect(),
                                elseif_statements: vec![],
                                else_statements: vec![],
                            }),
                        }))
                    }
                }
            }
            TokenType::Keyword(tokens::Keyword::RETURN) => {
                let ret = self.next()?;
                let value = match self.optional(TokenType::NEWLINE)? {
                    Some(_) => None,
                    None => {
                        let (expression, err) = self.parse_expression()?.split();
                        if err.is_none() {
                            self.expect(TokenType::NEWLINE)?.ok();
                        }
                        expression
                    }
                };

                Some(Some(Statement {
                    range: Range {
                        start: ret.range.start,
                        end: value.as_ref().map_or(ret.range.end, |val| val.range.end),
                    },
                    statement_type: StatementType::Return(value),
                }))
            }
            TokenType::Keyword(tokens::Keyword::FOR) => {
                let for_token = self.next()?;

                let name = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
                let _eq =
                    quick_exit_simple_opt!(self.expect(TokenType::Operator(tokens::Operator::EQ))?);
                let start = quick_exit_opt!(self.parse_expression()?);
                let _to =
                    quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::TO))?);
                let stop = quick_exit_opt!(self.parse_expression()?);

                let end = stop.range.end;

                let step = match self.optional(TokenType::Keyword(tokens::Keyword::STEP))? {
                    Some(_) => {
                        let (step, err) = self.parse_expression()?.split();
                        if err.is_none() {
                            self.expect(TokenType::NEWLINE)?.ok();
                        }

                        step
                    }
                    _ => None,
                };

                let mut statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::NEXT) => {
                            self.next();
                            self.expect(TokenType::NEWLINE)?.ok();
                            break;
                        }
                        _ => {
                            if let Some(statement) = self.parse_statement()? {
                                statements.push(statement);
                            }
                        }
                    }
                }

                Some(Some(Statement {
                    range: Range {
                        start: for_token.range.start,
                        end,
                    },
                    statement_type: StatementType::ForLoop(ForLoopStatement {
                        statements,
                        start,
                        stop,
                        step,
                        variable: VariableAccess { name },
                    }),
                }))
            }
            TokenType::Keyword(tokens::Keyword::DO) => {
                let do_token = self.next()?;
                let is_inversed;
                let mut is_until = false;

                let (mut condition, mut err) = match self.peek()?.token_type {
                    TokenType::Keyword(
                        keyword @ (tokens::Keyword::WHILE | tokens::Keyword::UNTIL),
                    ) => {
                        let mut err = None;
                        let condition = quick_exit_opt!(self.parse_expression()?, err);

                        is_inversed = false;
                        is_until = keyword == tokens::Keyword::UNTIL;

                        (Some(condition), err)
                    }
                    _ => {
                        is_inversed = true;
                        (None, None)
                    }
                };

                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.ok();
                }

                err = None;
                let mut statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::LOOP) => {
                            break;
                        }
                        _ => {
                            if let Some(statement) = self.parse_statement()? {
                                statements.push(statement);
                            }
                        }
                    }
                }

                let mut end = self.next()?.range.end;
                if is_inversed {
                    (condition, err) = match self.peek()?.token_type {
                        TokenType::Keyword(
                            keyword @ (tokens::Keyword::WHILE | tokens::Keyword::UNTIL),
                        ) => {
                            is_until = keyword == tokens::Keyword::UNTIL;
                            self.parse_expression()?
                        }
                        _ => {
                            let range = self.peek()?.range;
                            self.fatal_res(
                                &"Expected either WHILE or UNTIL".into(),
                                range,
                                true,
                                None,
                            )?
                        }
                    }
                    .split()
                }

                if err.is_none() {
                    self.expect(TokenType::NEWLINE);
                }

                let condition = match condition {
                    Some(condition) => {
                        end = condition.range.end;
                        condition
                    }
                    None => Expression {
                        expression_type: ExpressionType::Error,
                        range: Range { start: end, end },
                    },
                };

                Some(Some(Statement {
                    range: Range {
                        start: do_token.range.start,
                        end,
                    },
                    statement_type: StatementType::WhileLoop(WhileLoopStatement {
                        condition,
                        is_inversed,
                        is_until,
                        statements,
                    }),
                }))
            }
            TokenType::Keyword(tokens::Keyword::TRY) => {
                let try_token = self.next()?;
                self.expect(TokenType::NEWLINE)?.ok();

                let mut try_statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(
                            tokens::Keyword::CATCH
                            | tokens::Keyword::FINALLY
                            | tokens::Keyword::END,
                        ) => break,
                        _ => {
                            if let Some(statement) = self.parse_statement()? {
                                try_statements.push(statement)
                            }
                        }
                    }
                }

                let mut catches = Vec::new();

                if let TokenType::Keyword(tokens::Keyword::CATCH) = self.peek()?.token_type {
                    loop {
                        self.next()?;
                        let var = loop {
                            if self
                                .expect(TokenType::Symbol(tokens::Symbol::LPAREN))?
                                .is_err()
                            {
                                break None;
                            }

                            let Ok(data_type) = self.parse_type()? else {
                                break None;
                            };

                            let Ok(name) = self.expect(TokenType::ID)? else {
                                break None;
                            };

                            if self
                                .expect(TokenType::Symbol(tokens::Symbol::RPAREN))?
                                .is_ok()
                            {
                                self.expect(TokenType::NEWLINE);
                            }

                            break Some((data_type, name));
                        };

                        let mut statements = Vec::new();
                        let exit = loop {
                            match self.peek()?.token_type.clone() {
                                TokenType::Keyword(tokens::Keyword::END) => {
                                    match self.peek_nth(1)?.token_type {
                                        TokenType::Keyword(tokens::Keyword::TRY) => break true,
                                        _ => {
                                            let range = self.peek()?.range;
                                            self.fatal::<()>(
                                                &"Dangling END keyword, did you mean END TRY"
                                                    .into(),
                                                range,
                                                true,
                                            )
                                        }
                                    };
                                }
                                TokenType::Keyword(tokens::Keyword::FINALLY) => break true,
                                TokenType::Keyword(tokens::Keyword::CATCH) => break false,
                                _ => {
                                    if let Some(statement) = self.parse_statement()? {
                                        statements.push(statement)
                                    }
                                }
                            }
                        };

                        match var {
                            Some((data_type, name)) => {
                                let range = Range {
                                    start: data_type.range.start,
                                    end: name.range.end,
                                };
                                catches.push((
                                    Statement {
                                        statement_type: StatementType::Declaration(
                                            InstanceVariable {
                                                access: Access {
                                                    read: None,
                                                    write: None,
                                                },
                                                variable: Variable {
                                                    range,
                                                    constant: false,
                                                    data_type,
                                                    name,
                                                    initial_value: None,
                                                },
                                            },
                                        ),
                                        range,
                                    },
                                    statements,
                                ))
                            }
                            None => match catches.get_mut(0) {
                                Some((_, existing_statements)) => existing_statements,
                                None => &mut try_statements,
                            }
                            .extend(statements),
                        }

                        if exit {
                            break;
                        }
                    }
                }

                let finally = if self
                    .optional(TokenType::Keyword(tokens::Keyword::FINALLY))?
                    .is_some()
                {
                    self.expect(TokenType::NEWLINE)?.ok();

                    let mut statements = Vec::new();

                    loop {
                        match self.peek()?.token_type.clone() {
                            TokenType::Keyword(tokens::Keyword::END) => {
                                match self.peek_nth(1)?.token_type {
                                    TokenType::Keyword(tokens::Keyword::TRY) => break,
                                    _ => {
                                        let range = self.peek()?.range;
                                        self.fatal::<()>(
                                            &"Dangling END keyword, did you mean END TRY".into(),
                                            range,
                                            true,
                                        )
                                    }
                                };
                            }
                            _ => {
                                if let Some(statement) = self.parse_statement()? {
                                    statements.push(statement)
                                }
                            }
                        }
                    }

                    Some(statements)
                } else {
                    None
                };

                self.next()?; // end
                let end = self.next()?.range.end; // try
                self.expect(TokenType::NEWLINE)?.ok();

                Some(Some(Statement {
                    range: Range {
                        start: try_token.range.start,
                        end,
                    },
                    statement_type: StatementType::TryCatch(TryCatchStatement {
                        statements: try_statements,
                        catches,
                        finally,
                    }),
                }))
            }
            TokenType::Keyword(tokens::Keyword::CHOOSE) => {
                let choose_token = self.next()?;
                quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::CASE))?);

                let mut err = None;
                let choose = quick_exit_opt!(self.parse_expression()?, err);
                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.ok();
                }

                let mut cases = Vec::new();
                let mut end;

                'outer: loop {
                    end = self.peek()?.range.end;
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::END) => {
                            match self.peek_nth(1)?.token_type {
                                TokenType::Keyword(tokens::Keyword::CHOOSE) => {
                                    self.next()?;
                                    end = self.next()?.range.end;
                                    self.expect(TokenType::NEWLINE)?.ok();
                                    break;
                                }
                                _ => {
                                    let range = self.peek()?.range;
                                    self.fatal::<()>(
                                        &"Dangling END keyword, did you mean END CHOOSE".into(),
                                        range,
                                        true,
                                    )
                                }
                            };
                        }
                        _ => {}
                    }
                    if self
                        .expect(TokenType::Keyword(tokens::Keyword::CASE))?
                        .is_err()
                    {
                        break;
                    }

                    let mut specifiers = Vec::new();
                    loop {
                        let specifier = match self.peek()?.token_type {
                            TokenType::Keyword(tokens::Keyword::IS) => {
                                let is = self.next()?;
                                let operator = match self.peek()?.token_type {
                                    TokenType::Operator(
                                        operator @ (tokens::Operator::GTE
                                        | tokens::Operator::GT
                                        | tokens::Operator::LTE
                                        | tokens::Operator::LT),
                                    ) => operator,
                                    _ => {
                                        let range = self.peek()?.range;
                                        self.fatal::<()>(
                                            &"Expected a Comparison Operator (>=, >, <=, <)".into(),
                                            range,
                                            true,
                                        )?
                                        .ok();
                                        break;
                                    }
                                };
                                self.next()?;

                                let Some(literal) = self
                                    .expect(TokenType::Literal(tokens::Literal::NUMBER))?
                                    .ok()
                                else {
                                    break;
                                };

                                CaseSpecifier {
                                    specifier_type: CaseSpecifierType::Is(
                                        operator,
                                        Literal {
                                            literal_type: tokens::Literal::NUMBER,
                                            content: literal.content,
                                            range: literal.range,
                                        },
                                    ),
                                    range: Range {
                                        start: is.range.start,
                                        end: literal.range.end,
                                    },
                                }
                            }
                            TokenType::Literal(literal) => {
                                let token = self.next()?;
                                match literal {
                                    tokens::Literal::NUMBER
                                        if self
                                            .optional(TokenType::Keyword(tokens::Keyword::TO))?
                                            .is_some() =>
                                    {
                                        let Some(up_to_token) = self
                                            .expect(TokenType::Literal(tokens::Literal::NUMBER))?
                                            .ok()
                                        else {
                                            break;
                                        };

                                        CaseSpecifier {
                                            specifier_type: CaseSpecifierType::To(
                                                Literal {
                                                    literal_type: literal,
                                                    content: token.content,
                                                    range: token.range,
                                                },
                                                Literal {
                                                    literal_type: tokens::Literal::NUMBER,
                                                    content: up_to_token.content,
                                                    range: up_to_token.range,
                                                },
                                            ),
                                            range: Range {
                                                start: token.range.start,
                                                end: up_to_token.range.end,
                                            },
                                        }
                                    }
                                    _ => CaseSpecifier {
                                        specifier_type: CaseSpecifierType::Literals(Literal {
                                            literal_type: literal,
                                            content: token.content,
                                            range: token.range,
                                        }),
                                        range: token.range,
                                    },
                                }
                            }
                            TokenType::Keyword(tokens::Keyword::ELSE) => {
                                let token = self.next()?;
                                CaseSpecifier {
                                    specifier_type: CaseSpecifierType::Else,
                                    range: token.range,
                                }
                            }
                            _ => {
                                let range = self.peek()?.range;
                                self.fatal::<()>(
                                    &"Expected one of (LITERAL, IS, ELSE)".into(),
                                    range,
                                    true,
                                )?
                                .ok();
                                break;
                            }
                        };

                        specifiers.push(specifier);

                        match self.peek()?.token_type {
                            TokenType::Symbol(tokens::Symbol::COMMA) => {
                                self.next()?;
                            }
                            TokenType::NEWLINE => {
                                self.next()?;
                                break;
                            }
                            _ => {
                                let range = self.peek()?.range;
                                self.fatal::<()>(
                                    &"Expected either ',' or a Newline".into(),
                                    range,
                                    true,
                                )?
                                .ok();
                                break;
                            }
                        }
                    }

                    let mut statements = Vec::new();

                    loop {
                        match self.peek()?.token_type {
                            TokenType::Keyword(tokens::Keyword::END) => {
                                match self.peek_nth(1)?.token_type {
                                    TokenType::Keyword(tokens::Keyword::CHOOSE) => {
                                        self.next()?;
                                        end = self.next()?.range.end;
                                        self.expect(TokenType::NEWLINE)?.ok();
                                        cases.push((specifiers, statements));
                                        break 'outer;
                                    }
                                    _ => {
                                        let range = self.peek()?.range;
                                        self.fatal::<()>(
                                            &"Dangling END keyword, did you mean END CHOOSE".into(),
                                            range,
                                            true,
                                        )
                                    }
                                };
                            }
                            TokenType::Keyword(tokens::Keyword::CASE) => {
                                cases.push((specifiers, statements));
                                break;
                            }
                            _ => {
                                if let Some(statement) = self.parse_statement()? {
                                    statements.push(statement)
                                }
                            }
                        }
                    }
                }

                Some(Some(Statement {
                    range: Range {
                        start: choose_token.range.start,
                        end,
                    },
                    statement_type: StatementType::Choose(ChooseCaseStatement { choose, cases }),
                }))
            }
            TokenType::Keyword(tokens::Keyword::CALL) => {
                let call_token = self.next()?;

                let call_type = match self.peek()?.token_type {
                    TokenType::Keyword(tokens::Keyword::SUPER) => {
                        self.next()?;
                        CallType::Super
                    }
                    TokenType::ID => {
                        let (group, name) = quick_exit_opt!(self.parse_class_id()?);
                        CallType::Ancestor(group, name)
                    }
                    _ => {
                        return Some(
                            self.fatal(
                                &"Expected SUPER or an Ancestor Class".into(),
                                call_token.range,
                                true,
                            )?
                            .ok(),
                        );
                    }
                };

                quick_exit_simple_opt!(self.expect(TokenType::Symbol(tokens::Symbol::COLONCOLON))?);

                let mut err = None;
                let lvalue = quick_exit_opt!(self.parse_lvalue()?, err);
                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.ok();
                }

                let function = match lvalue.lvalue_type {
                    LValueType::Function(function) => function,
                    LValueType::Variable(function_name) => FunctionCall {
                        range: function_name.name.range,
                        name: function_name.name,
                        arguments: vec![],
                        dynamic: None,
                        event: None,
                        post: None,
                    },
                    _ => {
                        return Some(
                            self.fatal(
                                &"Expected Function Name or Call".into(),
                                lvalue.range,
                                true,
                            )?
                            .ok(),
                        );
                    }
                };

                Some(Some(Statement {
                    range: Range {
                        start: call_token.range.start,
                        end: function.range.end,
                    },
                    statement_type: StatementType::Call(CallStatement {
                        call_type,
                        function,
                    }),
                }))
            }
            TokenType::Keyword(tokens::Keyword::EXIT) => Some(Some(Statement {
                range: self.next()?.range,
                statement_type: StatementType::Exit,
            })),
            TokenType::Keyword(tokens::Keyword::CONTINUE) => Some(Some(Statement {
                range: self.next()?.range,
                statement_type: StatementType::Continue,
            })),
            TokenType::Keyword(
                tokens::Keyword::OPEN
                | tokens::Keyword::CLOSE
                | tokens::Keyword::COMMIT
                | tokens::Keyword::CONNECT
                | tokens::Keyword::DECLARE
                | tokens::Keyword::DELETE
                | tokens::Keyword::DISCONNECT
                | tokens::Keyword::EXECUTE
                | tokens::Keyword::FETCH
                | tokens::Keyword::INSERT
                | tokens::Keyword::ROLLBACK
                | tokens::Keyword::SELECT
                | tokens::Keyword::SELECTBLOB
                | tokens::Keyword::UPDATE
                | tokens::Keyword::UPDATEBLOB,
            ) => {
                let start = self.next()?.range.start;

                let end = loop {
                    let token = self.next()?;
                    match token.token_type {
                        TokenType::NEWLINE if token.content == ";" => {
                            break token.range.end;
                        }
                        _ => {}
                    }
                };

                Some(Some(Statement {
                    statement_type: StatementType::SQL,
                    range: Range { start, end },
                }))
            }
            _ => {
                let Token {
                    token_type, range, ..
                } = *self.peek()?;
                return Some(
                    self.fatal(
                        &format!("Unexpected TokenType for Statement {:?}", token_type),
                        range,
                        true,
                    )?
                    .ok(),
                );
            }
        }
    }

    fn parse_event(&mut self) -> EOFOr<Option<TopLevel>> {
        let event = quick_exit_opt!(self.parse_event_header()?);

        let mut statements = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::EVENT) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE)?.ok();
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END EVENT".into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                _ => {
                    if let Some(statement) = self.parse_statement()? {
                        statements.push(statement);
                    }
                }
            };
        }

        Some(Some(TopLevel {
            range: Range {
                start: event.range.start,
                end,
            },
            top_level_type: TopLevelType::EventBody(event, statements),
        }))
    }

    fn parse_function(&mut self) -> EOFOr<Option<TopLevel>> {
        let function = quick_exit_opt!(self.parse_function_header()?);

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

                        if (function.returns.is_none())
                            ^ (function_type == tokens::Keyword::SUBROUTINE)
                        {
                            self.error(
                                &if function.returns.is_none() {
                                    "Dangling END keyword, did you mean END SUBROUTINE"
                                } else {
                                    "Dangling END keyword, did you mean END FUNCTION"
                                }
                                .into(),
                                close.range,
                            );
                        }

                        self.expect(TokenType::NEWLINE)?.ok();
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &if function.returns.is_none() {
                                "Dangling END keyword, did you mean END SUBROUTINE"
                            } else {
                                "Dangling END keyword, did you mean END FUNCTION"
                            }
                            .into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                _ => {
                    if let Some(statement) = self.parse_statement()? {
                        statements.push(statement);
                    }
                }
            };
        }

        Some(Some(TopLevel {
            range: Range {
                start: function.range.start,
                end,
            },
            top_level_type: TopLevelType::FunctionBody(function, statements),
        }))
    }

    fn parse_on(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::ON))?)
            .range
            .start;
        let class = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
        quick_exit_simple_opt!(self.expect(TokenType::Symbol(tokens::Symbol::DOT))?);

        let name = match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::CREATE | tokens::Keyword::DESTROY) => {
                let name = self.next()?;
                self.expect(TokenType::NEWLINE);
                name
            }
            _ => {
                let range = self.peek()?.range;
                return Some(
                    self.fatal(&"Expected either CREATE or DESTROY".into(), range, true)?
                        .ok(),
                );
            }
        };

        let mut statements = Vec::new();
        let end;

        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::ON) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE)?.ok();
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END ON".into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                _ => {
                    if let Some(statement) = self.parse_statement()? {
                        statements.push(statement);
                    }
                }
            };
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::OnBody(On { class, name: name }, statements),
            range: Range { start, end },
        }))
    }

    fn parse_datatype_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.peek()?.range.start;

        let scope = if let TokenType::ScopeModif(scope) = self.peek()?.token_type {
            self.next()?;
            Some(scope)
        } else {
            None
        };

        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::TYPE))?);
        let name = quick_exit_simple_opt!(self.expect(TokenType::ID)?);

        let (base, mut err) = match self.expect(TokenType::Keyword(tokens::Keyword::FROM))? {
            Ok(_) => self.parse_class_id()?.split(),
            Err(err) => (None, Some(err)),
        };
        let base = base.unwrap_or_else(|| {
            (
                None,
                Token {
                    token_type: TokenType::ID,
                    content: "powerobject".into(),
                    range: Range::default(),
                    error: None,
                },
            )
        });

        let within = if err.is_none()
            && self
                .optional(TokenType::Keyword(tokens::Keyword::WITHIN))?
                .is_some()
        {
            let within;
            (within, err) = self.parse_class_id()?.split();
            within
        } else {
            None
        };

        if err.is_none() {
            self.expect(TokenType::NEWLINE)?.ok();
        }

        let end;
        let mut variables = Vec::new();
        let mut events = Vec::new();
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
                        self.error(&"Dangling END keyword, did you mean END TYPE".into(), range);
                        end = range.end;

                        self.consume_line()?;
                        break;
                    }
                },
                TokenType::Keyword(tokens::Keyword::EVENT) => {
                    if let Some(event) = self.parse_event_header()?.value() {
                        events.push(event);
                    }
                }
                _ => {
                    if let Some(statement) = self.parse_statement()? {
                        match statement.statement_type {
                            StatementType::Declaration(var) => {
                                variables.push(var);
                            }
                            _ => {
                                self.error(
                                    &"Type Block can only contain Variable Declarations".into(),
                                    statement.range,
                                );
                            }
                        }
                    }
                }
            }
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::DatatypeDecl(DatatypeDecl {
                class: Class {
                    scope,
                    name,
                    base,
                    within,
                },
                variables,
                events,
                range: Range { start, end },
            }),
            range: Range { start, end },
        }))
    }

    fn parse_scoped_variable_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.peek()?.range.start;

        let scope = match self.optional(TokenType::ScopeModif(tokens::ScopeModif::GLOBAL))? {
            Some(_) => ScopeModif::GLOBAL,
            None => ScopeModif::SHARED,
        };
        let variable = quick_exit_opt!(self.parse_variable_declaration()?);

        if let StatementType::Declaration(var) = variable.statement_type {
            if var.access.read.is_some() || var.access.write.is_some() {
                self.error(
                    &"Global variable cannot specify Access Rights".into(),
                    var.variable.range,
                );
            }

            Some(Some(TopLevel {
                range: Range {
                    start,
                    end: variable.range.end,
                },
                top_level_type: TopLevelType::ScopedVariableDecl(ScopedVariable {
                    scope,
                    variable: var.variable,
                }),
            }))
        } else {
            unreachable!();
        }
    }

    fn parse_scoped_variables_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.peek()?.range.start;

        let scope = match self.peek()?.token_type {
            TokenType::ScopeModif(scope) => scope,
            _ => {
                let range = self.peek()?.range;
                return Some(
                    self.fatal(
                        &"Expected a Scope Modifier (Global, Shared)".into(),
                        range,
                        true,
                    )?
                    .ok(),
                );
            }
        };
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::VARIABLES))?);
        self.expect(TokenType::NEWLINE)?.ok();

        let end;
        let mut declarations = Vec::new();
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
                        end = self.peek()?.range.end;
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END VARIABLES".into(),
                            range,
                            true,
                        )?
                        .ok();
                        break;
                    }
                },
                _ => {
                    if let Some(statement) = self.parse_variable_declaration()?.value() {
                        if let StatementType::Declaration(var) = statement.statement_type {
                            declarations.push(ScopedVariable {
                                scope,
                                variable: var.variable,
                            });
                        } else {
                            unreachable!();
                        }
                    }
                }
            }
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::ScopedVariablesDecl(declarations),
            range: Range { start, end },
        }))
    }

    fn parse_type_variables_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.peek()?.range.start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::TYPE))?);
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::VARIABLES))?);
        self.expect(TokenType::NEWLINE)?.ok();

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
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END VARIABLES".into(),
                            range,
                            true,
                        )?
                        .ok();
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
                            token.range,
                        );
                    }

                    self.next()?;
                    self.expect(TokenType::NEWLINE)?.ok();
                }
                _ => {
                    if let Some(statement) = self.parse_variable_declaration()?.value() {
                        if let StatementType::Declaration(var) = statement.statement_type {
                            declarations.push(InstanceVariable {
                                access: Access {
                                    read: var.access.read.clone().or(access),
                                    write: var.access.write.clone().or(access),
                                },
                                variable: var.variable,
                            });
                        } else {
                            unreachable!();
                        }
                    }
                }
            }
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::TypeVariablesDecl(declarations),
            range: Range { start, end },
        }))
    }

    fn parse_functions_forward_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::FORWARD))?)
                .range
                .start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))?);
        self.expect(TokenType::NEWLINE)?.ok();

        let mut functions = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::PROTOTYPES) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE);
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END PROTOTYPES".into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                _ => {
                    if let Some(function) = self.parse_function_header()?.value() {
                        functions.push(function);
                    }
                }
            };
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::FunctionsForwardDecl(functions),
            range: Range { start, end },
        }))
    }

    fn parse_external_functions(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::TYPE))?)
            .range
            .start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))?);
        self.expect(TokenType::NEWLINE)?.ok();

        let mut functions = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::PROTOTYPES) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE);
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END PROTOTYPES".into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                _ => {
                    if let Some(function) = self.parse_function_header()?.value() {
                        functions.push(function);
                    }
                }
            };
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::ExternalFunctions(functions),
            range: Range { start, end },
        }))
    }

    fn parse_forward_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokens::Keyword::FORWARD))?)
                .range
                .start;
        self.expect(TokenType::NEWLINE)?.ok();

        let mut types = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type.clone() {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::FORWARD) => {
                        self.next()?;
                        end = self.next()?.range.end;
                        self.expect(TokenType::NEWLINE)?.ok();
                        break;
                    }
                    _ => {
                        let range = self.next()?.range;
                        self.error(
                            &"Dangling END keyword, did you mean END FORWARD".into(),
                            range,
                        );
                        self.consume_line()?;
                    }
                },
                _ => match self.parse_datatype_decl()? {
                    Some(TopLevel {
                        top_level_type: TopLevelType::DatatypeDecl(datatype),
                        ..
                    }) => types.push(datatype),
                    Some(_) => unreachable!(),
                    None => {}
                },
            }
        }

        Some(Some(TopLevel {
            top_level_type: TopLevelType::ForwardDecl(types),
            range: Range { start, end },
        }))
    }

    fn parse_top_level(&mut self) -> EOFOr<Option<TopLevel>> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::FORWARD) => match self.peek_nth(1)?.token_type {
                TokenType::Keyword(tokens::Keyword::PROTOTYPES) => {
                    self.parse_functions_forward_decl()
                }
                TokenType::NEWLINE => self.parse_forward_decl(),
                _ => {
                    let range = self.peek()?.range;
                    Some(
                        self.fatal(
                            &"Unexpected Token for FORWARD, expected either PROTOTYPE or NEWLINE"
                                .into(),
                            range,
                            true,
                        )?
                        .ok(),
                    )
                }
            },
            TokenType::AccessType(_)
            | TokenType::Keyword(tokens::Keyword::FUNCTION | tokens::Keyword::SUBROUTINE) => {
                self.parse_function()
            }
            TokenType::Keyword(tokens::Keyword::EVENT) => self.parse_event(),
            TokenType::Keyword(tokens::Keyword::ON) => self.parse_on(),
            TokenType::Keyword(tokens::Keyword::TYPE) => match self.peek_nth(1)?.token_type {
                TokenType::Keyword(tokens::Keyword::VARIABLES) => self.parse_type_variables_decl(),
                TokenType::Keyword(tokens::Keyword::PROTOTYPES) => self.parse_external_functions(),
                _ => self.parse_datatype_decl(),
            },
            TokenType::ScopeModif(_) => match self.peek_nth(1)?.token_type {
                TokenType::Keyword(tokens::Keyword::TYPE) => self.parse_datatype_decl(),
                TokenType::Keyword(tokens::Keyword::VARIABLES) => {
                    self.parse_scoped_variables_decl()
                }
                _ => self.parse_scoped_variable_decl(),
            },
            TokenType::ID => self.parse_scoped_variable_decl(),
            TokenType::NEWLINE => {
                self.next();
                Some(None)
            }
            _ => {
                let range = self.peek()?.range;
                return Some(
                    self.fatal(&"Unexpected Token for Top Level".into(), range, true)?
                        .ok(),
                );
            }
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
