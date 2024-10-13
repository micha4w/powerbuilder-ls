use std::backtrace::Backtrace;
use std::iter::Filter;

use multipeek::{multipeek, MultiPeek};

use super::parser_types::*;
use super::tokenizer::*;
use super::tokenizer_types as tokens;
use super::tokenizer_types::Range;

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

    fn next(&mut self) -> EOFResult<Token> {
        self.last_token = self.tokens.next();
        self.last_token.clone()
    }

    fn peek(&mut self) -> EOFResult<&Token> {
        self.tokens.peek()
    }

    fn peek_nth(&mut self, n: usize) -> EOFResult<&Token> {
        self.tokens.peek_nth(n)
    }

    fn consume_line(&mut self) -> EOFResult<()> {
        loop {
            match self.next()?.token_type {
                TokenType::NEWLINE => break Some(()),
                _ => {}
            }
        }
    }

    fn error(&mut self, error: &String, range: Range) {
        self.syntax_errors.push(Diagnostic {
            severity: Severity::Error,
            // message: format!("[Parser] {}", error),
            message: format!("[Parser] {}\n{}", error, Backtrace::capture()),
            range: range,
        });
        // panic!("[Parser] {} {:?}\n", error, range);
    }

    fn fatal<T>(
        &mut self,
        error: &String,
        range: Range,
        consume_line: bool,
    ) -> EOFPossibleResult<T> {
        self.error(error, range);
        if consume_line {
            self.consume_line()?;
        }
        Some((None, Some(ParseError::UnexpectedToken)))
    }

    fn optional(&mut self, token_type: TokenType) -> EOFResult<Option<Token>> {
        if self.peek()?.token_type == token_type {
            Some(Some(self.next()?))
        } else {
            Some(None)
        }
    }

    fn expect(&mut self, token_type: TokenType) -> EOFPossibleResult<Token> {
        let token = self.next()?;
        if token.token_type == token_type {
            Some((Some(token), None))
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

    pub fn parse_type(&mut self) -> EOFPossibleResult<DataType> {
        let res = self.expect(TokenType::ID)?;
        if res.failed() {
            return Some((None, res.1));
        }
        let token = res.0.unwrap();

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

                EOFPossibleResultT::success(DataType {
                    data_type_type: DataTypeType::Decimal(precission),
                    range: Range {
                        start: token.range.start,
                        end,
                    },
                })
            } else {
                if let Some(_) = self.optional(TokenType::Symbol(tokens::Symbol::TICK))? {
                    let res = self.expect(TokenType::ID)?;
                    if let Some(err) = res.1 {
                        EOFPossibleResultT::fail(err)
                    } else {
                        let group = res.0.unwrap();

                        EOFPossibleResultT::success(DataType {
                            data_type_type: DataTypeType::Complex(group.content, token.content),
                            range: Range {
                                start: token.range.start,
                                end: group.range.end,
                            },
                        })
                    }
                } else {
                    EOFPossibleResultT::success(DataType {
                        data_type_type: DataTypeType::ID(token.content),
                        range: token.range,
                    })
                }
            },
        )
    }

    fn parse_expression(&mut self) -> EOFPossibleResult<Expression> {
        let start = self.peek()?.range.start;
        let mut end;
        let mut err = None;

        let expression_type = match self.peek()?.token_type {
            TokenType::Symbol(tokens::Symbol::LPAREN) => {
                self.next()?;
                let expression_res = self.parse_expression()?;
                if let Some(expression) = expression_res.0 {
                    let res = self.expect(TokenType::Symbol(tokens::Symbol::RPAREN))?;
                    end = if let Some(token) = res.0 {
                        token.range.end
                    } else {
                        expression.range.end
                    };
                    err = res.1;
                    ExpressionType::Parenthesized(Box::new(expression))
                } else {
                    return Some((None, expression_res.1));
                }
            }
            TokenType::Symbol(tokens::Symbol::LCURLY) => {
                end = self.next()?.range.end;
                let mut expressions = Vec::new();

                loop {
                    let res = self.parse_expression()?;
                    if let Some(expression) = res.0 {
                        end = expression.range.end;
                        expressions.push(expression);
                    }
                    if res.1.is_some() {
                        err = res.1;
                        break;
                    }

                    match self.peek()?.token_type {
                        TokenType::Symbol(tokens::Symbol::COMMA) => {
                            end = self.next()?.range.end;
                        }
                        TokenType::Symbol(tokens::Symbol::RCURLY) => {
                            end = self.next()?.range.end;
                            break;
                        }
                        _ => {
                            let range = self.peek()?.range;
                            err = self
                                .fatal::<()>(&"Expected either , or }".into(), range, true)?
                                .1;
                            break;
                        }
                    }
                }
                ExpressionType::ArrayLiteral(expressions)
            }
            TokenType::Keyword(tokens::Keyword::NOT) => {
                end = self.next()?.range.end;
                match self.parse_expression()? {
                    (Some(expression), res_err) => {
                        end = expression.range.end;
                        err = res_err;
                        ExpressionType::BooleanNot(Box::new(expression))
                    }
                    (None, res_err) => return Some((None, res_err)),
                }
            }
            TokenType::Operator(operator @ (tokens::Operator::PLUS | tokens::Operator::MINUS)) => {
                end = self.next()?.range.end;
                match self.parse_expression()? {
                    (Some(expression), res_err) => {
                        end = expression.range.end;
                        err = res_err;
                        ExpressionType::UnaryOperation(operator, Box::new(expression))
                    }
                    (None, res_err) => return Some((None, res_err)),
                }
            }
            TokenType::Keyword(tokens::Keyword::THIS)
            | TokenType::Keyword(tokens::Keyword::SUPER)
            | TokenType::Keyword(tokens::Keyword::PARENT)
            | TokenType::ID => match self.parse_lvalue()? {
                (Some(lvalue), res_err) => {
                    end = lvalue.range.end;
                    err = res_err;
                    ExpressionType::LValue(lvalue)
                }
                (None, res_err) => return Some((None, res_err)),
            },
            TokenType::Keyword(tokens::Keyword::CREATE) => {
                self.next()?;

                match self.peek()?.token_type {
                    TokenType::Keyword(tokens::Keyword::USING) => {
                        end = self.next()?.range.end;

                        match self.parse_expression()? {
                            (Some(class), res_err) => {
                                end = class.range.end;
                                err = res_err;
                                ExpressionType::CreateUsing(Box::new(class))
                            }
                            (None, res_err) => return Some((None, res_err)),
                        }
                    }
                    TokenType::ID => {
                        let class = self.next()?;

                        end = class.range.end;
                        ExpressionType::Create(class.content)
                    }
                    _ => {
                        let range = self.peek()?.range;
                        return self.fatal(&"Expected 'USING' or a Class Name".into(), range, true);
                    }
                }
            }
            TokenType::Literal(literal) => {
                end = self.next()?.range.end;
                ExpressionType::Literal(literal)
            }
            _ => {
                let range = self.peek()?.range;
                return self.fatal(&"Unexpected Token for Expression".into(), range, true);
            }
        };

        let mut expression = Expression {
            expression_type,
            range: Range { start, end },
        };
        if err.is_some() {
            return Some((Some(expression), err));
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
            match self.parse_expression()? {
                (Some(right_side), res_err) => {
                    err = res_err;
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
                (None, err) => return Some((Some(expression), err)),
            }
        }

        Some((Some(expression), err))
    }

    fn parse_lvalue(&mut self) -> EOFPossibleResult<LValue> {
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
                            LValueType::Variable(_) => prev.lvalue_type = LValueType::Super,
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
                        let _lbrace = self.next()?;
                        let res = self.parse_expression()?;
                        let (expression, mut end) = match res.0 {
                            Some(exp) => {
                                let end = exp.range.end;
                                (exp, end)
                            }
                            None => return Some((None, res.1)),
                        };
                        let mut err = res.1;
                        if err.is_none() {
                            match self.expect(TokenType::Symbol(tokens::Symbol::RBRACE))? {
                                (Some(rbrace), res_err) => {
                                    end = rbrace.range.end;
                                    err = res_err;
                                }
                                (None, res_err) => return Some((previous, res_err)),
                            }
                        }

                        previous = Some(LValue {
                            range: Range {
                                start: prev.range.start,
                                end: end,
                            },
                            lvalue_type: LValueType::Index(
                                Box::new(previous.take().unwrap()),
                                Box::new(expression),
                            ),
                        });

                        if err.is_some() {
                            return Some((previous, err));
                        }
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
                    let mut dynamics = 0;
                    let mut events = 0;
                    let mut posts = 0;
                    let mut statics = 0;
                    loop {
                        match self.peek()?.token_type {
                            TokenType::Keyword(tokens::Keyword::STATIC) => {
                                self.next()?;
                                statics += 1;
                            }
                            TokenType::Keyword(tokens::Keyword::DYNAMIC) => {
                                self.next()?;
                                dynamics += 1;
                            }
                            TokenType::Keyword(tokens::Keyword::EVENT) => {
                                self.next()?;
                                events += 1;
                            }
                            TokenType::Keyword(tokens::Keyword::POST) => {
                                self.next()?;
                                posts += 1;
                            }
                            _ => break,
                        }
                    }

                    let name = match self.expect(TokenType::ID)? {
                        (Some(token), _) => token,
                        (None, res_err) => return Some((previous, res_err)),
                    };

                    if dynamics > 0 && statics > 0 {
                        self.error(
                            &"Mixing DYNAMIC and STATIC is not allowed".into(),
                            name.range,
                        );
                    }
                    if dynamics > 1 {
                        self.error(&"Cannot have multiple DYNAMIC keywords".into(), name.range);
                    }
                    if statics > 1 {
                        self.error(&"Cannot have multiple STATIC keywords".into(), name.range);
                    }
                    if let Some(LValueType::Super) =
                        previous.as_ref().map(|lvalue| &lvalue.lvalue_type)
                    {
                        if dynamics > 0 {
                            self.error(
                                &"'SUPER::' calls do not support DYNAMIC".into(),
                                name.range,
                            );
                        }
                    }
                    if posts > 1 {
                        self.error(&"Cannot have multiple POST keywords".into(), name.range);
                    }
                    if events > 1 {
                        self.error(&"Cannot have multiple EVENT keywords".into(), name.range);
                    }

                    let mut arguments = Vec::new();

                    let res = self.expect(TokenType::Symbol(tokens::Symbol::LPAREN))?;
                    let mut err = None;
                    let mut end;
                    match res {
                        (Some(lparen), None) => {
                            end = lparen.range.end;
                            if self.peek()?.token_type != TokenType::Symbol(tokens::Symbol::RPAREN)
                            {
                                loop {
                                    let res = self.parse_expression()?;

                                    if let Some(arg) = res.0 {
                                        end = arg.range.end;
                                        arguments.push(arg);
                                    }
                                    if res.1.is_some() {
                                        err = res.1;
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
                                                .1;
                                            break;
                                        }
                                    }
                                }
                            } else {
                                end = self.next()?.range.end;
                            }
                        }
                        (_, res_err) => {
                            err = res_err;
                            end = name.range.end
                        }
                    }

                    let func = FunctionCall {
                        name: name.content,
                        arguments,
                        range: name.range,
                        dynamic: dynamics > 0,
                        event: events > 0,
                        post: posts > 0,
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
                            range: Range {
                                start: name.range.start,
                                end,
                            },
                            lvalue_type: LValueType::Function(func),
                        }),
                    };

                    if err.is_some() {
                        return Some((previous, err));
                    }
                }
                (TokenType::ID, _) => {
                    let name = self.next()?;

                    let var = VariableAccess {
                        name: name.content,
                        range: name.range,
                    };

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
                _ => {
                    let range = self.peek()?.range;
                    return Some((
                        previous,
                        self.fatal::<()>(&"Unexpected Token for LValue".into(), range, true)?
                            .1,
                    ));
                }
            }
        }

        match previous {
            Some(prev) => Some((Some(prev), None)),
            None => Some((None, Some(ParseError::UnexpectedToken))),
        }
    }

    fn parse_argument_list_declaration(
        &mut self,
    ) -> EOFPossibleResult<(Range, Vec<Argument>, bool)> {
        let start = match self.expect(TokenType::Symbol(tokens::Symbol::LBRACE))? {
            (Some(token), None) => token.range.start,
            (_, err) => return Some((None, err)),
        };
        let mut end = tokens::Position {
            line: start.line,
            column: start.column + 1,
        };
        let mut is_variadic = false;
        let mut err = None;

        let mut arguments = Vec::new();
        loop {
            match self.peek()?.token_type {
                TokenType::Symbol(tokens::Symbol::RPAREN) => {
                    end = self.next()?.range.end;
                    break;
                }
                TokenType::Symbol(tokens::Symbol::DOTDOTDOT) => {
                    is_variadic = true;
                    end = self.next()?.range.end;

                    let res = self.expect(TokenType::Symbol(tokens::Symbol::RPAREN))?;
                    err = res.1;
                    res.0.map(|token| end = token.range.end);
                    break;
                }
                _ => {}
            };

            let is_ref = self
                .optional(TokenType::Keyword(tokens::Keyword::REF))?
                .is_some();
            let is_readonly = self
                .optional(TokenType::Keyword(tokens::Keyword::READONLY))?
                .is_some();

            let data_type = match self.parse_type()? {
                (Some(data_type), None) => data_type,
                (_, res_err) => {
                    err = res_err;
                    break;
                }
            };
            end = data_type.range.end;

            let name = match self.expect(TokenType::ID)? {
                (Some(token), res_err) => {
                    err = res_err;
                    token
                }
                (None, res_err) => {
                    err = res_err;
                    break;
                }
            };
            end = name.range.end;

            arguments.push(Argument {
                is_ref,
                variable: Variable {
                    constant: is_readonly,
                    data_type,
                    name: name.content,
                    initial_value: None,
                    range: name.range,
                },
            });

            if err.is_some() {
                break;
            }

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
                        .1;
                }
            }
        }

        Some((Some((Range { start, end }, arguments, is_variadic)), err))
    }

    fn parse_event_header(&mut self) -> EOFPossibleResult<Event> {
        let start = match self.expect(TokenType::Keyword(tokens::Keyword::EVENT))? {
            (Some(token), _) => token.range.start,
            (None, res_err) => return Some((None, res_err)),
        };

        let returns = if self
            .optional(TokenType::Keyword(tokens::Keyword::TYPE))?
            .is_some()
        {
            let res = self.parse_type()?;
            if res.failed() {
                return Some((None, res.1));
            }
            res.0
        } else {
            None
        };

        let name = match self.expect(TokenType::ID)? {
            (Some(token), None) => token,
            (_, err) => return Some((None, err)),
        };

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
                match self.parse_argument_list_declaration()? {
                    (Some((range, arguments, _is_variadic)), res_err) => {
                        err = res_err;
                        end = range.end;
                        EventType::User(returns, arguments)
                    }
                    (None, res_err) => {
                        err = res_err;
                        EventType::User(returns, Vec::new())
                    }
                }
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
            err = self.expect(TokenType::NEWLINE)?.1;
        }

        Some((
            Some(Event {
                name: name.content,
                event_type,
                range: Range { start, end },
            }),
            err,
        ))
    }

    fn parse_function_header(&mut self) -> EOFPossibleResult<Function> {
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
                match self.parse_type()? {
                    (Some(data_type), None) => Some(data_type),
                    (_, err) => return Some((None, err)),
                }
            }
            TokenType::Keyword(tokens::Keyword::SUBROUTINE) => {
                self.next()?;
                None
            }
            _ => {
                let range = self.peek()?.range;
                return self.fatal(&"Expected FUNCTION or SUBROUTINE".into(), range, true);
            }
        };

        let (mut end, name) = match self.expect(TokenType::ID)? {
            (Some(token), None) => (token.range.end, token.content),
            (_, err) => return Some((None, err)),
        };

        let mut err;
        let (arguments, has_vararg) = match self.parse_argument_list_declaration()? {
            (Some(argument_list), res_err) => {
                err = res_err;
                end = argument_list.0.end;
                (argument_list.1, argument_list.2)
            }
            (None, res_err) => {
                err = res_err;
                (Vec::new(), false)
            }
        };

        if err.is_none() {
            if let Some(token) = self.optional(TokenType::Keyword(tokens::Keyword::THROWS))? {
                end = token.range.end;

                loop {
                    match self.expect(TokenType::ID)? {
                        (Some(token), _) => end = token.range.end,
                        (None, res_err) => {
                            err = res_err;
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
                    err = self.expect(TokenType::Literal(tokens::Literal::STRING))?.1;

                    if err.is_none()
                        && self
                            .optional(TokenType::Keyword(tokens::Keyword::ALIAS))?
                            .is_some()
                    {
                        err = self.expect(TokenType::Keyword(tokens::Keyword::FOR))?.1;
                        if err.is_none() {
                            err = self.expect(TokenType::Literal(tokens::Literal::STRING))?.1;
                        }
                    }
                }
                _ => {}
            }
        }

        if err.is_none() {
            err = self.expect(TokenType::NEWLINE)?.1;
        }

        Some((
            Some(Function {
                returns,
                scope_modif,
                access,
                name,
                arguments,
                has_vararg,

                range: Range { start, end },
            }),
            err,
        ))
    }

    fn parse_variable_declaration(&mut self) -> EOFPossibleResult<Statement> {
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

        let mut data_type = match self.parse_type()? {
            (Some(data_type), None) => data_type,
            (_, res_err) => return Some((None, res_err)),
        };
        let (name, mut err) = match self.expect(TokenType::ID)? {
            (None, res_err) => return Some((None, res_err)),
            (Some(name), res_err) => (name, res_err),
        };

        let mut end = name.range.end;
        if err.is_none()
            && self
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
                            .1;
                        break;
                    }
                }
                first = false;

                match self.expect(TokenType::Literal(tokens::Literal::NUMBER))? {
                    (_, Some(res_err)) => {
                        err = Some(res_err);
                        break;
                    }
                    (_, None) => {}
                }
                if self
                    .optional(TokenType::Keyword(tokens::Keyword::TO))?
                    .is_some()
                {
                    match self.expect(TokenType::Literal(tokens::Literal::NUMBER))? {
                        (_, Some(res_err)) => {
                            err = Some(res_err);
                            break;
                        }
                        (_, None) => {}
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
            let res = self.parse_expression()?;
            err = res.1;
            match res.0 {
                Some(ex) => {
                    end = ex.range.end;
                    Some(ex)
                }
                None => None,
            }
        } else {
            None
        };

        if err.is_none() {
            err = self.expect(TokenType::NEWLINE)?.1;
        }

        Some((
            Some(Statement {
                statement_type: StatementType::Declaration(InstanceVariable {
                    access,
                    variable: Variable {
                        data_type,
                        name: name.content,
                        constant,
                        initial_value: expression,
                        range: name.range,
                    },
                }),
                range: Range { start, end },
            }),
            err,
        ))
    }

    fn parse_class_id(&mut self) -> EOFPossibleResult<(Option<String>, String)> {
        let name = self.next()?;
        if self
            .optional(TokenType::Symbol(tokens::Symbol::TICK))?
            .is_some()
        {
            let sub_name_res = self.expect(TokenType::ID)?;
            let grouped_name = if let Some(sub_name) = sub_name_res.0 {
                (Some(name.content), sub_name.content)
            } else {
                (None, name.content)
            };

            Some((Some(grouped_name), sub_name_res.1))
        } else {
            Some((Some((None, name.content)), None))
        }
    }

    // Consumes trailing newlines
    fn parse_statement(&mut self) -> EOFResult<Option<Statement>> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokens::Keyword::CONSTANT) => {
                Some(self.parse_variable_declaration()?.0)
            }
            TokenType::ID
            | TokenType::Keyword(
                tokens::Keyword::THIS | tokens::Keyword::SUPER | tokens::Keyword::PARENT,
            ) => match self.peek_nth(1)?.token_type {
                TokenType::ID => Some(self.parse_variable_declaration()?.0),
                _ => {
                    match self.parse_expression()? {
                        (Some(expression), mut err) => {
                            match (expression.expression_type.clone(), self.peek()?.token_type) {
                                (
                                    ExpressionType::Operation(left, tokens::Operator::EQ, right),
                                    _,
                                ) if matches!(left.expression_type, ExpressionType::LValue(_),) => {
                                    if let ExpressionType::LValue(lvalue) = left.expression_type {
                                        if err.is_none() {
                                            err = self.expect(TokenType::NEWLINE)?.1;
                                        }
                                        Some(Some(Statement {
                                            range: expression.range,
                                            statement_type: StatementType::Assignment(
                                                lvalue, *right,
                                            ),
                                        }))
                                    } else {
                                        unreachable!()
                                    }
                                }
                                (_, TokenType::SpecialAssignment(_)) => {
                                    self.next()?;
                                    if err.is_none() {
                                        let res = self.parse_expression()?;
                                        err = res.1;

                                        if let Some(value) = res.0 {
                                            if let ExpressionType::LValue(lvalue) =
                                                expression.expression_type
                                            {
                                                if err.is_none() {
                                                    err = self.expect(TokenType::NEWLINE)?.1;
                                                }
                                                Some(Some(Statement {
                                                    range: expression.range,
                                                    statement_type: StatementType::Assignment(
                                                        lvalue, value,
                                                    ),
                                                }))
                                            } else {
                                                Some(
                                                    self.fatal(
                                                        &"Cannot assign to non-LValue".into(),
                                                        value.range,
                                                        true,
                                                    )?
                                                    .0,
                                                )
                                            }
                                        } else {
                                            Some(None)
                                        }
                                    } else {
                                        Some(None)
                                    }
                                }
                                _ => {
                                    self.expect(TokenType::NEWLINE)?;
                                    Some(Some(Statement {
                                        range: expression.range,
                                        statement_type: StatementType::Expression(expression),
                                    }))
                                }
                            }
                        }
                        (None, err) => Some(None),
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
                match self.parse_expression()? {
                    (Some(expression), mut err) => {
                        if err.is_none() {
                            err = self.expect(TokenType::NEWLINE)?.1;
                        }

                        Some(Some(Statement {
                            range: Range {
                                start: throw.range.start,
                                end: expression.range.end,
                            },
                            statement_type: StatementType::Throw(expression),
                        }))
                    }
                    (None, _) => Some(None),
                }
            }
            TokenType::Keyword(tokens::Keyword::DESTROY) => {
                let destroy = self.next()?;
                match self.parse_expression()? {
                    (Some(expression), mut err) => {
                        if err.is_none() {
                            err = self.expect(TokenType::NEWLINE)?.1;
                        }

                        Some(Some(Statement {
                            range: Range {
                                start: destroy.range.start,
                                end: expression.range.end,
                            },
                            statement_type: StatementType::Destroy(expression),
                        }))
                    }
                    (None, _) => Some(None),
                }
            }
            TokenType::Keyword(tokens::Keyword::IF) => {
                let if_token = self.next()?;
                let mut end;
                let (condition, err) = match self.parse_expression()? {
                    (Some(condition), mut err) => {
                        end = condition.range.end;
                        if err.is_none() {
                            let res = self.expect(TokenType::Keyword(tokens::Keyword::THEN))?;
                            err = res.1;
                            if let Some(then_token) = res.0 {
                                end = then_token.range.end;
                            }
                        }
                        (condition, err)
                    }
                    (None, err) => {
                        end = if_token.range.end;
                        (
                            Expression {
                                expression_type: ExpressionType::Literal(tokens::Literal::BOOLEAN),
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
                        let end;
                        'outer: loop {
                            loop {
                                match self.peek()?.token_type {
                                    TokenType::Keyword(tokens::Keyword::END) => {
                                        match self.peek_nth(1)?.token_type {
                                            TokenType::Keyword(tokens::Keyword::IF) => {
                                                self.next()?;
                                                end = self.next()?.range.end;
                                                self.expect(TokenType::NEWLINE)?;
                                                break 'outer;
                                            }
                                            _ => {
                                                let range = self.peek()?.range;
                                                self.fatal::<()>(
                                                    &"Dangling END keyword, did you mean END IF"
                                                        .into(),
                                                    range,
                                                    true,
                                                )?;
                                            }
                                        }
                                    }

                                    TokenType::Keyword(tokens::Keyword::ELSE) => {
                                        part = tokens::Keyword::ELSE;
                                        self.next()?;
                                        break;
                                    }
                                    TokenType::Keyword(tokens::Keyword::ELSEIF) => {
                                        part = tokens::Keyword::ELSEIF;
                                        let mut end = self.next()?.range.end;

                                        let condition = match self.parse_expression()? {
                                            (Some(condition), mut err) => {
                                                end = condition.range.end;

                                                if err.is_none() {
                                                    err = self
                                                        .expect(TokenType::Keyword(
                                                            tokens::Keyword::THEN,
                                                        ))?
                                                        .1;
                                                    if err.is_none() {
                                                        self.expect(TokenType::NEWLINE)?;
                                                    }
                                                }
                                                condition
                                            }
                                            (None, err) => Expression {
                                                expression_type: ExpressionType::Literal(
                                                    tokens::Literal::BOOLEAN,
                                                ),
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
                let (end, returns, err) = match self.optional(TokenType::NEWLINE)? {
                    Some(_) => (ret.range.end, None, None),
                    None => match self.parse_expression()? {
                        (Some(expression), mut err) => {
                            if err.is_none() {
                                err = self.expect(TokenType::NEWLINE)?.1;
                            }
                            (expression.range.end, Some(expression), err)
                        }
                        (None, err) => (ret.range.end, None, err),
                    },
                };

                Some(Some(Statement {
                    range: Range {
                        start: ret.range.start,
                        end,
                    },
                    statement_type: StatementType::Return(returns),
                }))
            }
            TokenType::Keyword(tokens::Keyword::FOR) => {
                let for_token = self.next()?;

                let name = match self.expect(TokenType::ID)? {
                    (Some(token), None) => token,
                    _ => return Some(None),
                };
                let eq = match self.expect(TokenType::Operator(tokens::Operator::EQ))? {
                    (Some(token), None) => token,
                    _ => return Some(None),
                };
                let start = match self.parse_expression()? {
                    (Some(exp), None) => exp,
                    _ => return Some(None),
                };
                let to = match self.expect(TokenType::Keyword(tokens::Keyword::TO))? {
                    (Some(token), None) => token,
                    _ => return Some(None),
                };
                let stop = match self.parse_expression()? {
                    (Some(exp), None) => exp,
                    _ => return Some(None),
                };
                let mut end = stop.range.end;

                let (step, mut err) =
                    match self.optional(TokenType::Keyword(tokens::Keyword::STEP))? {
                        Some(_) => self.parse_expression()?,
                        _ => (None, None),
                    };

                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?.1;
                }

                let mut statements = Vec::new();
                loop {
                    match self.peek()?.token_type {
                        TokenType::Keyword(tokens::Keyword::NEXT) => {
                            self.next();
                            err = self.expect(TokenType::NEWLINE)?.1;
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
                        variable: VariableAccess {
                            name: name.content,
                            range: name.range,
                        },
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
                        let (condition, err) = self.parse_expression()?;
                        if condition.is_none() {
                            return Some(None);
                        }

                        is_inversed = false;
                        is_until = keyword == tokens::Keyword::UNTIL;
                        (condition, err)
                    }
                    _ => {
                        is_inversed = true;
                        (None, None)
                    }
                };

                if err.is_none() {
                    self.expect(TokenType::NEWLINE)?;
                }

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

                            let res = self.parse_expression()?;
                            if res.1.is_none() {
                                self.expect(TokenType::NEWLINE)?;
                            }
                            res
                        }
                        _ => {
                            let range = self.peek()?.range;
                            self.fatal(&"Expected either WHILE or UNTIL".into(), range, true)?
                        }
                    }
                } else {
                    self.expect(TokenType::NEWLINE);
                }

                let condition = match condition {
                    Some(condition) => {
                        end = condition.range.end;
                        condition
                    }
                    None => Expression {
                        expression_type: ExpressionType::Literal(tokens::Literal::BOOLEAN),
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
                self.expect(TokenType::NEWLINE)?;

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
                                .1
                                .is_some()
                            {
                                break None;
                            }

                            let data_type = match self.parse_type()? {
                                (Some(data_type), None) => data_type,
                                _ => break None,
                            };

                            let name = match self.expect(TokenType::ID)? {
                                (Some(name), None) => name,
                                _ => break None,
                            };

                            if self
                                .expect(TokenType::Symbol(tokens::Symbol::RPAREN))?
                                .1
                                .is_none()
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
                            Some((data_type, name)) => catches.push((
                                Variable {
                                    range: Range {
                                        start: data_type.range.start,
                                        end: name.range.end,
                                    },
                                    constant: false,
                                    data_type,
                                    name: name.content,
                                    initial_value: None,
                                },
                                statements,
                            )),
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
                    self.expect(TokenType::NEWLINE)?;

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
                match self.expect(TokenType::Keyword(tokens::Keyword::CASE))? {
                    (Some(_), None) => {}
                    _ => return Some(None),
                }
                let choose = match self.parse_expression()? {
                    (Some(choose), err) => {
                        if err.is_none() {
                            self.expect(TokenType::NEWLINE)?;
                        }
                        choose
                    }
                    _ => return Some(None),
                };
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
                                    self.expect(TokenType::NEWLINE)?;
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
                    match self.expect(TokenType::Keyword(tokens::Keyword::CASE))? {
                        (Some(_), None) => {}
                        _ => break,
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
                                        )?;
                                        break;
                                    }
                                };
                                self.next()?;

                                let Some(literal) =
                                    self.expect(TokenType::Literal(tokens::Literal::NUMBER))?.0
                                else {
                                    break;
                                };

                                CaseSpecifier {
                                    specifier_type: CaseSpecifierType::Is(
                                        operator,
                                        tokens::Literal::NUMBER,
                                    ),
                                    range: Range {
                                        start: is.range.start,
                                        end: literal.range.end,
                                    },
                                }
                            }
                            TokenType::Literal(literal) => {
                                let token = self.next()?;
                                let spec = if let tokens::Literal::NUMBER = literal {
                                    if self
                                        .optional(TokenType::Keyword(tokens::Keyword::TO))?
                                        .is_some()
                                    {
                                        let Some(up_to_token) = self
                                            .expect(TokenType::Literal(tokens::Literal::NUMBER))?
                                            .0
                                        else {
                                            break;
                                        };

                                        Some(CaseSpecifier {
                                            specifier_type: CaseSpecifierType::To(
                                                literal,
                                                tokens::Literal::NUMBER,
                                            ),
                                            range: Range {
                                                start: token.range.start,
                                                end: up_to_token.range.end,
                                            },
                                        })
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };

                                match spec {
                                    Some(spec) => spec,
                                    None => CaseSpecifier {
                                        specifier_type: CaseSpecifierType::Literals(literal),
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
                                )?;
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
                                )?;
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
                                        self.expect(TokenType::NEWLINE)?;
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
                        let (group, name) = match self.parse_class_id()? {
                            (Some(grouped_name), None) => grouped_name,
                            _ => return Some(None),
                        };
                        CallType::Ancestor(group, name)
                    }
                    _ => {
                        return Some(
                            self.fatal(
                                &"Expected SUPER or an Ancestor Class".into(),
                                call_token.range,
                                true,
                            )?
                            .0,
                        );
                    }
                };

                match self.expect(TokenType::Symbol(tokens::Symbol::COLONCOLON))? {
                    (Some(_), None) => {}
                    _ => return Some(None),
                };
                let lvalue = match self.parse_lvalue()? {
                    (Some(lvalue), err) => {
                        if err.is_none() {
                            self.expect(TokenType::NEWLINE)?;
                        }
                        lvalue
                    }
                    _ => return Some(None),
                };

                let function = match lvalue.lvalue_type {
                    LValueType::Function(function) => function,
                    LValueType::Variable(function) => FunctionCall {
                        name: function.name,
                        arguments: vec![],
                        range: function.range,
                        dynamic: false,
                        event: false,
                        post: false,
                    },
                    _ => {
                        return Some(
                            self.fatal(
                                &"Expected Function Name or Call".into(),
                                lvalue.range,
                                true,
                            )?
                            .0,
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
                    .0,
                );
            }
        }
    }

    fn parse_event(&mut self) -> EOFResult<Option<TopLevel>> {
        let event = match self.parse_event_header()? {
            (Some(event), _) => event,
            _ => return Some(None),
        };

        let mut statements = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::EVENT) => {
                        self.next()?;
                        let close = self.next()?;
                        end = close.range.end;

                        self.expect(TokenType::NEWLINE)?;
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END EVENT".into(),
                            range,
                            true,
                        )?;
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

    fn parse_function(&mut self) -> EOFResult<Option<TopLevel>> {
        let function = match self.parse_function_header()? {
            (Some(function), _) => function,
            _ => return Some(None),
        };
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

                        self.expect(TokenType::NEWLINE)?;
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
                        )?;
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

    fn parse_on(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = match self.expect(TokenType::Keyword(tokens::Keyword::ON))? {
            (Some(token), None) => token.range.start,
            _ => return Some(None),
        };
        let class = match self.expect(TokenType::ID)? {
            (Some(token), None) => token.content,
            _ => return Some(None),
        };
        match self.expect(TokenType::Symbol(tokens::Symbol::DOT))? {
            (Some(_), None) => {}
            _ => return Some(None),
        };

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
                        .0,
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

                        self.expect(TokenType::NEWLINE)?;
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END ON".into(),
                            range,
                            true,
                        )?;
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
            top_level_type: TopLevelType::OnBody(
                On {
                    class,
                    name: name.content,
                },
                statements,
            ),
            range: Range { start, end },
        }))
    }

    fn parse_datatype_decl(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = self.peek()?.range.start;

        let scope = if let TokenType::ScopeModif(scope) = self.peek()?.token_type {
            self.next()?;
            Some(scope)
        } else {
            None
        };

        match self.expect(TokenType::Keyword(tokens::Keyword::TYPE))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        let name = match self.expect(TokenType::ID)? {
            (Some(token), None) => token.content,
            _ => return Some(None),
        };

        let (base, mut err) = match self.expect(TokenType::Keyword(tokens::Keyword::FROM))? {
            (Some(_), None) => match self.parse_class_id()? {
                (Some(name), err) => (name, err),
                (_, err) => ((None, "nonvisualobject".into()), err),
            },
            (_, err) => ((None, "nonvisualobject".into()), err),
        };

        let within = if err.is_none()
            && self
                .optional(TokenType::Keyword(tokens::Keyword::WITHIN))?
                .is_some()
        {
            let res = self.parse_class_id()?;
            err = res.1;
            res.0
        } else {
            None
        };

        if err.is_none() {
            self.expect(TokenType::NEWLINE)?;
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
                TokenType::Keyword(tokens::Keyword::EVENT) => match self.parse_event_header()? {
                    (Some(event), _) => events.push(event),
                    _ => {}
                },
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

    fn parse_global_variable_decl(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = self.peek()?.range.start;

        match self.expect(TokenType::ScopeModif(tokens::ScopeModif::GLOBAL))? {
            (Some(_), None) => {}
            _ => return Some(None),
        };
        let variable = match self.parse_variable_declaration()? {
            (Some(var), _) => var,
            _ => return Some(None),
        };

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
                top_level_type: TopLevelType::GlobalVariableDecl(var.variable),
            }))
        } else {
            unreachable!();
        }
    }

    fn parse_scoped_variables_decl(&mut self) -> EOFResult<Option<TopLevel>> {
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
                    .0,
                );
            }
        };
        match self.expect(TokenType::Keyword(tokens::Keyword::VARIABLES))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        self.expect(TokenType::NEWLINE)?;

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
                        )?;
                        break;
                    }
                },
                _ => {
                    if let Some(statement) = self.parse_variable_declaration()?.0 {
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

    fn parse_type_variables_decl(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = self.peek()?.range.start;
        match self.expect(TokenType::Keyword(tokens::Keyword::TYPE))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        match self.expect(TokenType::Keyword(tokens::Keyword::VARIABLES))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        self.expect(TokenType::NEWLINE)?;

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
                        )?;
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
                    self.expect(TokenType::NEWLINE)?;
                }
                _ => {
                    if let Some(statement) = self.parse_variable_declaration()?.0 {
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

    fn parse_functions_forward_decl(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = match self.expect(TokenType::Keyword(tokens::Keyword::FORWARD))? {
            (Some(token), None) => token.range.start,
            _ => return Some(None),
        };

        match self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        self.expect(TokenType::NEWLINE)?;

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
                        )?;
                    }
                },
                _ => {
                    if let Some(function) = self.parse_function_header()?.0 {
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

    fn parse_external_functions(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = match self.expect(TokenType::Keyword(tokens::Keyword::TYPE))? {
            (Some(token), None) => token.range.start,
            _ => return Some(None),
        };

        match self.expect(TokenType::Keyword(tokens::Keyword::PROTOTYPES))? {
            (Some(_), None) => {}
            _ => return Some(None),
        }
        self.expect(TokenType::NEWLINE)?;

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
                        )?;
                    }
                },
                _ => {
                    if let Some(function) = self.parse_function_header()?.0 {
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

    fn parse_forward_decl(&mut self) -> EOFResult<Option<TopLevel>> {
        let start = match self.expect(TokenType::Keyword(tokens::Keyword::FORWARD))? {
            (Some(token), None) => token.range.start,
            _ => return Some(None),
        };
        self.expect(TokenType::NEWLINE)?;

        let mut types = Vec::new();
        let end;
        loop {
            match self.peek()?.token_type.clone() {
                TokenType::Keyword(tokens::Keyword::END) => match self.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokens::Keyword::FORWARD) => {
                        self.next()?;
                        end = self.next()?.range.end;
                        self.expect(TokenType::NEWLINE)?;
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

    fn parse_top_level(&mut self) -> EOFResult<Option<TopLevel>> {
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
                        .0,
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
                _ => self.parse_global_variable_decl(),
            },
            _ => {
                let range = self.peek()?.range;
                return Some(
                    self.fatal(&"Unexpected Token for Top Level".into(), range, true)?
                        .0,
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
