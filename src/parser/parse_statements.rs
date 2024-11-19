use super::*;
use crate::{
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl Parser {
    pub fn parse_throw(&mut self) -> EOFOr<Option<Statement>> {
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

    pub fn parse_return(&mut self) -> EOFOr<Option<Statement>> {
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

    pub fn parse_destroy(&mut self) -> EOFOr<Option<Statement>> {
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

    pub fn parse_variable_declaration(&mut self) -> EOFOrParserResult<Statement> {
        let start = self.peek()?.range.start;

        let base = match self.peek()?.token_type {
            TokenType::AccessType(
                access_type @ (tokenizer::AccessType::PUBLIC
                | tokenizer::AccessType::PRIVATE
                | tokenizer::AccessType::PROTECTED),
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
                    tokenizer::AccessType::PUBLIC
                    | tokenizer::AccessType::PRIVATE
                    | tokenizer::AccessType::PROTECTED => {
                        self.error(&"Only one Access Right allowed".into(), token.range);
                    }
                    tokenizer::AccessType::PRIVATEWRITE
                    | tokenizer::AccessType::PROTECTEDWRITE
                    | tokenizer::AccessType::SYSTEMWRITE => {
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
                    tokenizer::AccessType::PRIVATEREAD
                    | tokenizer::AccessType::PROTECTEDREAD
                    | tokenizer::AccessType::SYSTEMREAD => {
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
            .optional(TokenType::Keyword(tokenizer::Keyword::CONSTANT))?
            .is_some();

        let mut data_type = quick_exit!(self.parse_type()?);
        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

        let mut end = name.range.end;
        let mut err = None;
        if self
            .optional(TokenType::Symbol(tokenizer::Symbol::LBRACE))?
            .is_some()
        {
            let mut first = true;
            loop {
                match self.peek()?.token_type {
                    TokenType::Symbol(tokenizer::Symbol::RBRACE) => {
                        end = self.next()?.range.end;
                        break;
                    }
                    TokenType::Symbol(tokenizer::Symbol::COMMA) if !first => {
                        end = self.next()?.range.end;
                    }
                    TokenType::Literal(tokenizer::Literal::NUMBER) if first => {}
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
                    .expect(TokenType::Literal(tokenizer::Literal::NUMBER))?
                    .err();
                if err.is_some() {
                    break;
                }

                if self
                    .optional(TokenType::Keyword(tokenizer::Keyword::TO))?
                    .is_some()
                {
                    err = self
                        .expect(TokenType::Literal(tokenizer::Literal::NUMBER))?
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
                data_type_type: DataTypeType::Array(Box::new(data_type.data_type_type)),
            }
        }

        let expression = if err.is_none()
            && self
                .optional(TokenType::Operator(tokenizer::Operator::EQ))?
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

        Some(ParseResult::new(
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
        ))
    }

    pub fn parse_expression_or_assignment(&mut self) -> EOFOr<Option<Statement>> {
        match self.parse_expression()?.split() {
            (Some(expression), mut err) => match (expression, self.peek()?.token_type) {
                (
                    expression @ Expression {
                        expression_type: ExpressionType::Operation(_, tokenizer::Operator::EQ, _),
                        ..
                    },
                    assigner,
                )
                | (expression, assigner @ TokenType::SpecialAssignment(_)) => {
                    let (left, right) = match expression.expression_type {
                        ExpressionType::Operation(left, tokenizer::Operator::EQ, right) => {
                            (*left, *right)
                        }
                        _ => {
                            self.next()?;
                            (
                                expression,
                                match self.parse_expression()? {
                                    Ok(right) => right,
                                    Err((res_err, Some(right))) => {
                                        err = Some(res_err);
                                        right
                                    }
                                    Err((_, None)) => return Some(None),
                                },
                            )
                        }
                    };

                    let operator = match assigner {
                        TokenType::SpecialAssignment(operator) => Some(operator),
                        _ => None,
                    };

                    match left.expression_type {
                        ExpressionType::LValue(mut lvalue) => match lvalue.lvalue_type {
                            LValueType::Variable(ref mut access)
                            | LValueType::Member(_, ref mut access) => {
                                access.is_write = true;

                                if err.is_none() {
                                    self.expect(TokenType::NEWLINE)?.ok();
                                }

                                Some(Some(Statement {
                                    range: Range {
                                        start: left.range.start,
                                        end: right.range.end,
                                    },
                                    statement_type: StatementType::Assignment(
                                        lvalue, operator, right,
                                    ),
                                }))
                            }
                            _ => Some(
                                self.fatal(
                                    &"Can only assign to LValue of type Variable or Member".into(),
                                    left.range,
                                    true,
                                )?
                                .ok(),
                            ),
                        },
                        _ => Some(
                            self.fatal(&"Cannot assign to non-LValue".into(), left.range, true)?
                                .ok(),
                        ),
                    }
                }
                (expression, _) => {
                    self.expect(TokenType::NEWLINE)?.ok();
                    Some(Some(Statement {
                        range: expression.range,
                        statement_type: StatementType::Expression(expression),
                    }))
                }
            },
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

    pub fn parse_if_statement(&mut self) -> EOFOr<Option<Statement>> {
        let if_token = self.next()?;
        let mut end;
        let (condition, err) = match self.parse_expression()?.split() {
            (Some(condition), mut err) => {
                end = condition.range.end;
                if err.is_none() {
                    match self.expect(TokenType::Keyword(tokenizer::Keyword::THEN))? {
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

                let mut part = tokenizer::Keyword::IF;
                let mut end;
                'outer: loop {
                    loop {
                        match self.peek()?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::END) => {
                                match self.peek_nth(1)?.token_type {
                                    TokenType::Keyword(tokenizer::Keyword::IF) => {
                                        self.next()?;
                                        end = self.next()?.range.end;
                                        self.expect(TokenType::NEWLINE)?.ok();
                                        break 'outer;
                                    }
                                    _ => {
                                        let range = self.peek()?.range;
                                        self.fatal::<()>(
                                            &"Dangling END keyword, did you mean END IF".into(),
                                            range,
                                            true,
                                        )?
                                        .ok();
                                    }
                                }
                            }

                            TokenType::Keyword(tokenizer::Keyword::ELSE) => {
                                part = tokenizer::Keyword::ELSE;
                                self.next()?;
                                self.expect(TokenType::NEWLINE)?.ok();
                                break;
                            }
                            TokenType::Keyword(tokenizer::Keyword::ELSEIF) => {
                                part = tokenizer::Keyword::ELSEIF;
                                end = self.next()?.range.end;

                                let condition = match self.parse_expression()?.split() {
                                    (Some(condition), err) => {
                                        if err.is_none()
                                            && self
                                                .expect(TokenType::Keyword(
                                                    tokenizer::Keyword::THEN,
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
                                        tokenizer::Keyword::IF => &mut ifs.statements,
                                        tokenizer::Keyword::ELSEIF => {
                                            &mut ifs.elseif_statements.last_mut().unwrap().1
                                        }
                                        tokenizer::Keyword::ELSE => &mut ifs.else_statements,
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

    pub fn parse_for_loop(&mut self) -> EOFOr<Option<Statement>> {
        let for_token = self.next()?;

        let name = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
        let _eq =
            quick_exit_simple_opt!(self.expect(TokenType::Operator(tokenizer::Operator::EQ))?);
        let start = quick_exit_opt!(self.parse_expression()?);
        let _to = quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::TO))?);
        let stop = quick_exit_opt!(self.parse_expression()?);

        let (step, err) = match self.optional(TokenType::Keyword(tokenizer::Keyword::STEP))? {
            Some(_) => self.parse_expression()?.split(),
            _ => (None, None),
        };
        if err.is_none() {
            self.expect(TokenType::NEWLINE)?.ok();
        }

        let end;
        let mut statements = Vec::new();
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::NEXT) => {
                    end = self.next()?.range.end;
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
                variable: VariableAccess {
                    name,
                    is_write: false,
                },
            }),
        }))
    }

    pub fn parse_while_loop(&mut self) -> EOFOr<Option<Statement>> {
        let do_token = self.next()?;
        let is_inversed;
        let mut is_until = false;

        let (mut condition, mut err) = match self.peek()?.token_type {
            TokenType::Keyword(
                keyword @ (tokenizer::Keyword::WHILE | tokenizer::Keyword::UNTIL),
            ) => {
                let mut err = None;
                let condition = quick_exit_opt!(self.parse_expression()?, err);

                is_inversed = false;
                is_until = keyword == tokenizer::Keyword::UNTIL;

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
                TokenType::Keyword(tokenizer::Keyword::LOOP) => {
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
                    keyword @ (tokenizer::Keyword::WHILE | tokenizer::Keyword::UNTIL),
                ) => {
                    is_until = keyword == tokenizer::Keyword::UNTIL;
                    self.parse_expression()?
                }
                _ => {
                    let range = self.peek()?.range;
                    self.fatal_res(&"Expected either WHILE or UNTIL".into(), range, true, None)?
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

    pub fn parse_try_catch(&mut self) -> EOFOr<Option<Statement>> {
        let try_token = self.next()?;
        self.expect(TokenType::NEWLINE)?.ok();

        let mut try_statements = Vec::new();
        loop {
            match self.peek()?.token_type {
                TokenType::Keyword(
                    tokenizer::Keyword::CATCH
                    | tokenizer::Keyword::FINALLY
                    | tokenizer::Keyword::END,
                ) => break,
                _ => {
                    if let Some(statement) = self.parse_statement()? {
                        try_statements.push(statement)
                    }
                }
            }
        }

        let mut catches = Vec::new();

        if let TokenType::Keyword(tokenizer::Keyword::CATCH) = self.peek()?.token_type {
            loop {
                self.next()?;
                let var = loop {
                    if self
                        .expect(TokenType::Symbol(tokenizer::Symbol::LPAREN))?
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
                        .expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))?
                        .is_ok()
                    {
                        self.expect(TokenType::NEWLINE);
                    }

                    break Some((data_type, name));
                };

                let mut statements = Vec::new();
                let exit = loop {
                    match self.peek()?.token_type.clone() {
                        TokenType::Keyword(tokenizer::Keyword::END) => {
                            match self.peek_nth(1)?.token_type {
                                TokenType::Keyword(tokenizer::Keyword::TRY) => break true,
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
                        TokenType::Keyword(tokenizer::Keyword::FINALLY) => break true,
                        TokenType::Keyword(tokenizer::Keyword::CATCH) => break false,
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
                                statement_type: StatementType::Declaration(InstanceVariable {
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
                                }),
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
            .optional(TokenType::Keyword(tokenizer::Keyword::FINALLY))?
            .is_some()
        {
            self.expect(TokenType::NEWLINE)?.ok();

            let mut statements = Vec::new();

            loop {
                match self.peek()?.token_type.clone() {
                    TokenType::Keyword(tokenizer::Keyword::END) => {
                        match self.peek_nth(1)?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::TRY) => break,
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

    pub fn parse_choose_case(&mut self) -> EOFOr<Option<Statement>> {
        let choose_token = self.next()?;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::CASE))?);

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
                TokenType::Keyword(tokenizer::Keyword::END) => {
                    match self.peek_nth(1)?.token_type {
                        TokenType::Keyword(tokenizer::Keyword::CHOOSE) => {
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
                .expect(TokenType::Keyword(tokenizer::Keyword::CASE))?
                .is_err()
            {
                break;
            }

            let mut specifiers = Vec::new();
            loop {
                let specifier = match self.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::IS) => {
                        let is = self.next()?;
                        let operator = match self.peek()?.token_type {
                            TokenType::Operator(
                                operator @ (tokenizer::Operator::GTE
                                | tokenizer::Operator::GT
                                | tokenizer::Operator::LTE
                                | tokenizer::Operator::LT),
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
                            .expect(TokenType::Literal(tokenizer::Literal::NUMBER))?
                            .ok()
                        else {
                            break;
                        };

                        CaseSpecifier {
                            specifier_type: CaseSpecifierType::Is(
                                operator,
                                Literal {
                                    literal_type: tokenizer::Literal::NUMBER,
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
                            tokenizer::Literal::NUMBER
                                if self
                                    .optional(TokenType::Keyword(tokenizer::Keyword::TO))?
                                    .is_some() =>
                            {
                                let Some(up_to_token) = self
                                    .expect(TokenType::Literal(tokenizer::Literal::NUMBER))?
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
                                            literal_type: tokenizer::Literal::NUMBER,
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
                    TokenType::Keyword(tokenizer::Keyword::ELSE) => {
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
                    TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                        self.next()?;
                    }
                    TokenType::NEWLINE => {
                        self.next()?;
                        break;
                    }
                    _ => {
                        let range = self.peek()?.range;
                        self.fatal::<()>(&"Expected either ',' or a Newline".into(), range, true)?
                            .ok();
                        break;
                    }
                }
            }

            let mut statements = Vec::new();

            loop {
                match self.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::END) => {
                        match self.peek_nth(1)?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::CHOOSE) => {
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
                    TokenType::Keyword(tokenizer::Keyword::CASE) => {
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

    pub fn parse_call_statement(&mut self) -> EOFOr<Option<Statement>> {
        let call_token = self.next()?;

        let call_type = match self.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::SUPER) => {
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

        quick_exit_simple_opt!(self.expect(TokenType::Symbol(tokenizer::Symbol::COLONCOLON))?);

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
                    self.fatal(&"Expected Function Name or Call".into(), lvalue.range, true)?
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

    pub fn parse_statement(&mut self) -> EOFOr<Option<Statement>> {
        match self.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::CONSTANT) => {
                Some(self.parse_variable_declaration()?.value())
            }
            TokenType::ID
            | TokenType::Keyword(
                tokenizer::Keyword::THIS | tokenizer::Keyword::SUPER | tokenizer::Keyword::PARENT,
            ) => match self.peek_nth(1)?.token_type {
                TokenType::ID => Some(self.parse_variable_declaration()?.value()),
                _ => self.parse_expression_or_assignment(),
            },
            TokenType::Keyword(tokenizer::Keyword::THROW) => self.parse_throw(),
            TokenType::Keyword(tokenizer::Keyword::DESTROY) => self.parse_destroy(),
            TokenType::Keyword(tokenizer::Keyword::IF) => self.parse_if_statement(),
            TokenType::Keyword(tokenizer::Keyword::RETURN) => self.parse_return(),
            TokenType::Keyword(tokenizer::Keyword::FOR) => self.parse_for_loop(),
            TokenType::Keyword(tokenizer::Keyword::DO) => self.parse_while_loop(),
            TokenType::Keyword(tokenizer::Keyword::TRY) => self.parse_try_catch(),
            TokenType::Keyword(tokenizer::Keyword::CHOOSE) => self.parse_choose_case(),
            TokenType::Keyword(tokenizer::Keyword::CALL) => self.parse_call_statement(),
            TokenType::Keyword(tokenizer::Keyword::EXIT) => Some(Some(Statement {
                range: self.next()?.range,
                statement_type: StatementType::Exit,
            })),
            TokenType::Keyword(tokenizer::Keyword::CONTINUE) => Some(Some(Statement {
                range: self.next()?.range,
                statement_type: StatementType::Continue,
            })),
            TokenType::Keyword(
                tokenizer::Keyword::OPEN
                | tokenizer::Keyword::CLOSE
                | tokenizer::Keyword::COMMIT
                | tokenizer::Keyword::CONNECT
                | tokenizer::Keyword::DECLARE
                | tokenizer::Keyword::DELETE
                | tokenizer::Keyword::DISCONNECT
                | tokenizer::Keyword::EXECUTE
                | tokenizer::Keyword::FETCH
                | tokenizer::Keyword::INSERT
                | tokenizer::Keyword::ROLLBACK
                | tokenizer::Keyword::SELECT
                | tokenizer::Keyword::SELECTBLOB
                | tokenizer::Keyword::UPDATE
                | tokenizer::Keyword::UPDATEBLOB,
            ) => {
                // TODO actually do something
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
}
