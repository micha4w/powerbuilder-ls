use super::*;
use crate::{
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl Parser {
    pub fn parse_throw(&mut self) -> EOFOr<Option<Statement>> {
        let throw = self.tokens.next()?;
        let mut err = None;
        let expression = quick_exit_opt!(self.parse_expression(false)?, err);

        if err.is_none() {
            self.expect_newline()?.ok();
        }

        Some(Some(Statement {
            range: Range {
                start: throw.range.start,
                end: expression.range.end,
                uri: self.uri(),
            },
            statement_type: StatementType::Throw(expression),
        }))
    }

    pub fn parse_return(&mut self) -> EOFOr<Option<Statement>> {
        let ret = self.tokens.next()?;
        let value = match self.optional_newline()? {
            Some(_) => None,
            None => {
                let (expression, err) = self.parse_expression(false)?.split();
                if err.is_none() {
                    self.expect_newline()?.ok();
                }
                expression
            }
        };

        Some(Some(Statement {
            range: Range {
                start: ret.range.start,
                end: value.as_ref().map_or(ret.range.end, |val| val.range.end),
                uri: self.uri(),
            },
            statement_type: StatementType::Return(value),
        }))
    }

    pub fn parse_destroy(&mut self) -> EOFOr<Option<Statement>> {
        let destroy = self.tokens.next()?;
        let mut err = None;
        let expression = quick_exit_opt!(self.parse_expression(false)?, err);
        if err.is_none() {
            self.expect(TokenType::NEWLINE)?.ok();
        }

        Some(Some(Statement {
            range: Range {
                start: destroy.range.start,
                end: expression.range.end,
                uri: self.uri(),
            },
            statement_type: StatementType::Destroy(expression),
        }))
    }

    pub fn parse_variable_declaration(&mut self) -> EOFOrParserResult<Statement> {
        let start = self.tokens.peek()?.range.start;

        let base = match self.tokens.peek()?.token_type {
            TokenType::AccessType(
                access_type @ (tokenizer::AccessType::PUBLIC
                | tokenizer::AccessType::PRIVATE
                | tokenizer::AccessType::PROTECTED),
            ) => {
                self.tokens.next()?;
                Some(access_type)
            }
            _ => None,
        };

        let mut access = Access {
            read: base,
            write: base,
        };

        loop {
            if let TokenType::AccessType(access_type) = self.tokens.peek()?.token_type {
                let token = self.tokens.next()?;
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

        let mut var_start = None;
        let constant =
            if let Some(token) = self.optional(TokenType::Keyword(tokenizer::Keyword::CONSTANT))? {
                var_start = Some(token.range.start);
                true
            } else {
                false
            };

        let mut data_type = quick_exit!(self.parse_type()?);
        let var_start = var_start.unwrap_or(data_type.range.start);

        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

        let mut end = name.range.end;
        let mut err = None;
        if self
            .optional(TokenType::Symbol(tokenizer::Symbol::LBRACE))?
            .is_some()
        {
            let mut first = true;
            loop {
                match self.tokens.peek()?.token_type {
                    TokenType::Symbol(tokenizer::Symbol::RBRACE) => {
                        end = self.tokens.next()?.range.end;
                        break;
                    }
                    TokenType::Symbol(tokenizer::Symbol::COMMA) if !first => {
                        end = self.tokens.next()?.range.end;
                    }
                    TokenType::Literal(tokenizer::Literal::NUMBER) if first => {}
                    _ => {
                        let range = self.tokens.peek()?.range;
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
                range: data_type.range.expanded(&end),
                data_type_type: DataTypeType::Array(Box::new(data_type.data_type_type)),
            }
        }

        let expression = if err.is_none()
            && self
                .optional(TokenType::Operator(tokenizer::Operator::EQ))?
                .is_some()
        {
            let expression;
            (expression, err) = self.parse_expression(false)?.split();
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
                        range: Range {
                            start: var_start,
                            end,
                            uri: self.uri(),
                        },
                        data_type,
                        access: VariableAccess {
                            name,
                            is_write: true,
                        },
                        constant,
                        initial_value: expression,
                    },
                }),
                range: Range {
                    start,
                    end,
                    uri: self.uri(),
                },
            },
            err,
        ))
    }

    pub fn parse_expression_or_assignment(&mut self) -> EOFOr<Option<Statement>> {
        match self.parse_expression(false)?.split() {
            (Some(expression), _err) => match (expression, self.tokens.peek()?.token_type) {
                (
                    expression @ Expression {
                        expression_type: ExpressionType::Operation(_, tokenizer::Operator::EQ, _),
                        ..
                    },
                    assigner, // dummy
                )
                | (expression, assigner @ TokenType::SpecialAssignment(_)) => {
                    let (right, mut left) = match expression.expression_type {
                        ExpressionType::Operation(left, tokenizer::Operator::EQ, right) => {
                            (*right, *left)
                        }
                        _ => {
                            self.tokens.next()?;
                            (
                                match self.parse_expression(false)?.value() {
                                    Some(right) => right,
                                    None => {
                                        return Some(Some(Statement {
                                            range: expression.range.clone(),
                                            statement_type: StatementType::Expression(expression),
                                        }))
                                    }
                                },
                                expression,
                            )
                        }
                    };
                    // FIXME: how does this handle the case `x = y += 10` or `x = y = 10`

                    let operator = match assigner {
                        TokenType::SpecialAssignment(operator) => Some(operator),
                        _ => None,
                    };

                    if let Some((err, range)) = left.set_write() {
                        self.error(&err, range);
                    }
                    self.expect(TokenType::NEWLINE)?.ok();
                    match left.expression_type {
                        ExpressionType::LValue(lvalue) => Some(Some(Statement {
                            range: left.range.merged(&right.range),
                            statement_type: StatementType::Assignment(lvalue, operator, right),
                        })),
                        _ => Some(None),
                    }
                }
                (expression, _) => {
                    self.expect(TokenType::NEWLINE)?.ok();
                    Some(Some(Statement {
                        range: expression.range.clone(),
                        statement_type: StatementType::Expression(expression),
                    }))
                }
            },
            _ => Some(None),
        }
        // | TokenType::SpecialAssignment(_) => match expression.expression_type {
        //     ExpressionType::LValue(lvalue) => {
        //         let operator = self.next()?;
        //         let value = self.parse_expression(false)?;
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
        let if_token = self.tokens.next()?;
        let mut end;
        let (condition, err) = match self.parse_expression(false)?.split() {
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
                        range: Range::new_point(end, self.uri()),
                    },
                    err,
                )
            }
        };

        match (self.tokens.peek()?.token_type, &err) {
            (TokenType::NEWLINE, _) | (_, Some(_)) => {
                if err.is_none() {
                    // Newline
                    self.tokens.next()?;
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
                        match self.tokens.peek()?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::END) => {
                                match self.tokens.peek_nth(1)?.token_type {
                                    TokenType::Keyword(tokenizer::Keyword::IF) => {
                                        self.tokens.next()?;
                                        end = self.tokens.next()?.range.end;
                                        self.expect(TokenType::NEWLINE)?.ok();
                                        break 'outer;
                                    }
                                    _ => {
                                        let range = self.tokens.peek()?.range;
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
                                self.tokens.next()?;
                                self.expect(TokenType::NEWLINE)?.ok();
                                break;
                            }
                            TokenType::Keyword(tokenizer::Keyword::ELSEIF) => {
                                part = tokenizer::Keyword::ELSEIF;
                                end = self.tokens.next()?.range.end;

                                let condition = match self.parse_expression(false)?.split() {
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
                                        range: Range::new_point(end, self.uri()),
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
                    range: if_token.range.expanded(&end),
                    statement_type: StatementType::If(ifs),
                }))
            }
            _ => {
                let statement = self.parse_statement()?;

                Some(Some(Statement {
                    range: if_token.range.expanded(match &statement {
                        Some(statement) => &statement.range.end,
                        None => &end,
                    }),
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
        let for_token = self.tokens.next()?;

        let name = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
        let _eq =
            quick_exit_simple_opt!(self.expect(TokenType::Operator(tokenizer::Operator::EQ))?);
        let start = quick_exit_opt!(self.parse_expression(false)?);
        let _to = quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::TO))?);
        let stop = quick_exit_opt!(self.parse_expression(false)?);

        let (step, err) = match self.optional(TokenType::Keyword(tokenizer::Keyword::STEP))? {
            Some(_) => self.parse_expression(false)?.split(),
            _ => (None, None),
        };
        if err.is_none() {
            self.expect_newline()?.ok();
        }

        let end;
        let mut statements = Vec::new();
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::NEXT) => {
                    end = self.tokens.next()?.range.end;
                    self.expect_newline()?.ok();
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
            range: for_token.range.expanded(&end),
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
        let do_token = self.tokens.next()?;
        let is_inversed;
        let mut is_until = false;

        let (mut condition, mut err) = match self.tokens.peek()?.token_type {
            TokenType::Keyword(
                keyword @ (tokenizer::Keyword::WHILE | tokenizer::Keyword::UNTIL),
            ) => {
                let mut err = None;
                let condition = quick_exit_opt!(self.parse_expression(false)?, err);

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
            self.expect_newline()?.ok();
        }

        err = None;
        let mut statements = Vec::new();
        loop {
            match self.tokens.peek()?.token_type {
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

        let mut end = self.tokens.next()?.range.end;
        if is_inversed {
            (condition, err) = match self.tokens.peek()?.token_type {
                TokenType::Keyword(
                    keyword @ (tokenizer::Keyword::WHILE | tokenizer::Keyword::UNTIL),
                ) => {
                    is_until = keyword == tokenizer::Keyword::UNTIL;
                    self.parse_expression(false)?
                }
                _ => {
                    let range = self.tokens.peek()?.range;
                    self.fatal_res(&"Expected either WHILE or UNTIL".into(), range, true, None)?
                }
            }
            .split()
        }

        if err.is_none() {
            self.expect_newline()?.ok();
        }

        let condition = match condition {
            Some(condition) => {
                end = condition.range.end;
                condition
            }
            None => Expression {
                expression_type: ExpressionType::Error,
                range: Range::new_point(end, self.uri()),
            },
        };

        Some(Some(Statement {
            range: do_token.range.expanded(&end),
            statement_type: StatementType::WhileLoop(WhileLoopStatement {
                condition,
                is_inversed,
                is_until,
                statements,
            }),
        }))
    }

    pub fn parse_try_catch(&mut self) -> EOFOr<Option<Statement>> {
        let try_token = self.tokens.next()?;
        self.expect_newline()?.ok();

        let mut try_statements = Vec::new();
        loop {
            match self.tokens.peek()?.token_type {
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

        if let TokenType::Keyword(tokenizer::Keyword::CATCH) = self.tokens.peek()?.token_type {
            loop {
                self.tokens.next()?;
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
                        self.expect_newline()?.ok();
                    }

                    break Some((data_type, name));
                };

                let mut statements = Vec::new();
                let exit = loop {
                    match self.tokens.peek()?.token_type.clone() {
                        TokenType::Keyword(tokenizer::Keyword::END) => {
                            match self.tokens.peek_nth(1)?.token_type {
                                TokenType::Keyword(tokenizer::Keyword::TRY) => break true,
                                _ => {
                                    let range = self.tokens.peek()?.range;
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
                        let range = data_type.range.clone().merged(&name.range);
                        catches.push((
                            Statement {
                                statement_type: StatementType::Declaration(InstanceVariable {
                                    access: Access {
                                        read: None,
                                        write: None,
                                    },
                                    variable: Variable {
                                        range: range.clone(),
                                        constant: false,
                                        data_type,
                                        access: VariableAccess {
                                            name,
                                            is_write: true,
                                        },
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
            self.expect_newline()?.ok();

            let mut statements = Vec::new();

            loop {
                match self.tokens.peek()?.token_type.clone() {
                    TokenType::Keyword(tokenizer::Keyword::END) => {
                        match self.tokens.peek_nth(1)?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::TRY) => break,
                            _ => {
                                let range = self.tokens.peek()?.range;
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

        self.tokens.next()?; // end
        let end = self.tokens.next()?.range.end; // try
        self.expect_newline()?.ok();

        Some(Some(Statement {
            range: Range {
                start: try_token.range.start,
                end,
                uri: self.uri(),
            },
            statement_type: StatementType::TryCatch(TryCatchStatement {
                statements: try_statements,
                catches,
                finally,
            }),
        }))
    }

    pub fn parse_choose_case(&mut self) -> EOFOr<Option<Statement>> {
        let choose_token = self.tokens.next()?;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::CASE))?);

        let mut err = None;
        let choose = quick_exit_opt!(self.parse_expression(false)?, err);
        if err.is_none() {
            self.expect_newline()?.ok();
        }

        let mut cases = Vec::new();
        let mut end;

        'outer: loop {
            end = self.tokens.peek()?.range.end;
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => {
                    match self.tokens.peek_nth(1)?.token_type {
                        TokenType::Keyword(tokenizer::Keyword::CHOOSE) => {
                            self.tokens.next()?;
                            end = self.tokens.next()?.range.end;
                            self.expect_newline()?.ok();
                            break;
                        }
                        _ => {
                            let range = self.tokens.peek()?.range;
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
                let specifier = match self.tokens.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::IS) => {
                        let is = self.tokens.next()?;
                        let operator = match self.tokens.peek()?.token_type {
                            TokenType::Operator(
                                operator @ (tokenizer::Operator::GTE
                                | tokenizer::Operator::GT
                                | tokenizer::Operator::LTE
                                | tokenizer::Operator::LT),
                            ) => operator,
                            _ => {
                                let range = self.tokens.peek()?.range;
                                self.fatal::<()>(
                                    &"Expected a Comparison Operator (>=, >, <=, <)".into(),
                                    range,
                                    true,
                                )?
                                .ok();
                                break;
                            }
                        };
                        self.tokens.next()?;

                        let Some(literal) = self
                            .expect(TokenType::Literal(tokenizer::Literal::NUMBER))?
                            .ok()
                        else {
                            break;
                        };

                        CaseSpecifier {
                            range: is.range.merged(&literal.range),
                            specifier_type: CaseSpecifierType::Is(
                                operator,
                                Literal {
                                    literal_type: tokenizer::Literal::NUMBER,
                                    content: literal.content,
                                    range: literal.range,
                                },
                            ),
                        }
                    }
                    TokenType::Literal(literal) => {
                        let token = self.tokens.next()?;
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
                                    range: token.range.clone().merged(&up_to_token.range),
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
                                }
                            }
                            _ => CaseSpecifier {
                                range: token.range.clone(),
                                specifier_type: CaseSpecifierType::Literals(Literal {
                                    literal_type: literal,
                                    content: token.content,
                                    range: token.range,
                                }),
                            },
                        }
                    }
                    TokenType::Keyword(tokenizer::Keyword::ELSE) => {
                        let token = self.tokens.next()?;
                        CaseSpecifier {
                            specifier_type: CaseSpecifierType::Else,
                            range: token.range,
                        }
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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

                match self.tokens.peek()?.token_type {
                    TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                        self.tokens.next()?;
                    }
                    TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                        self.tokens.next()?;
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
                        self.fatal::<()>(&"Expected either ',' or a Newline".into(), range, true)?
                            .ok();
                        break;
                    }
                }
            }

            let mut statements = Vec::new();

            loop {
                match self.tokens.peek()?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::END) => {
                        match self.tokens.peek_nth(1)?.token_type {
                            TokenType::Keyword(tokenizer::Keyword::CHOOSE) => {
                                self.tokens.next()?;
                                end = self.tokens.next()?.range.end;
                                self.expect_newline()?.ok();
                                cases.push((specifiers, statements));
                                break 'outer;
                            }
                            _ => {
                                let range = self.tokens.peek()?.range;
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
                uri: self.uri(),
            },
            statement_type: StatementType::Choose(ChooseCaseStatement { choose, cases }),
        }))
    }

    pub fn parse_call_statement(&mut self) -> EOFOr<Option<Statement>> {
        let call_token = self.tokens.next()?;

        let call_type = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::SUPER) => {
                self.tokens.next()?;
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
        let lvalue = quick_exit_opt!(self.parse_lvalue(false)?, err);
        if err.is_none() {
            self.expect_newline()?.ok();
        }

        let function = match lvalue.lvalue_type {
            LValueType::Function(function) => function,
            LValueType::Variable(function_name) => FunctionCall {
                range: function_name.name.range.clone(),
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
                uri: self.uri(),
            },
            statement_type: StatementType::Call(CallStatement {
                call_type,
                function,
            }),
        }))
    }

    pub fn parse_statement(&mut self) -> EOFOr<Option<Statement>> {
        match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::CONSTANT) => {
                Some(self.parse_variable_declaration()?.value())
            }
            TokenType::ID
            | TokenType::Keyword(
                tokenizer::Keyword::THIS | tokenizer::Keyword::SUPER | tokenizer::Keyword::PARENT,
            ) => match self.tokens.peek_nth(1)?.token_type {
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
                range: self.tokens.next()?.range,
                statement_type: StatementType::Exit,
            })),
            TokenType::Keyword(tokenizer::Keyword::CONTINUE) => Some(Some(Statement {
                range: self.tokens.next()?.range,
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
            ) => self.parse_sql_statement(),
            TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                self.tokens.next()?;
                Some(None)
            }
            _ => {
                let Token {
                    token_type, range, ..
                } = self.tokens.peek()?;
                return Some(
                    self.fatal(
                        &format!("Unexpected TokenType for Statement: {:?}", token_type),
                        range,
                        true,
                    )?
                    .ok(),
                );
            }
        }
    }
}
