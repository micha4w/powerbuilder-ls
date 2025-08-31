use super::*;
use crate::{
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl<I: Iterator<Item = char>> Parser<I> {
    fn expression_list(
        &mut self,
        err: &mut Option<ParseError>,
        range: &mut Range,
    ) -> EOFOr<Vec<Expression>> {
        let mut expressions = Vec::new();
        loop {
            let expression;
            (expression, *err) = self.parse_expression(true)?.split();
            let Some(expression) = expression else {
                break;
            };

            if let ExpressionType::LValue(LValue {
                lvalue_type: LValueType::SQLAccess(_, lvalue),
                ..
            }) = &expression.expression_type
            {
                if !matches!(
                    lvalue.lvalue_type,
                    LValueType::Variable(..) | LValueType::Member(..)
                ) {
                    self.error(
                        &"Expected ':<Variable or Member>'".into(),
                        lvalue.range.clone(),
                    );
                }
            }

            range.merge(&expression.range);
            expressions.push(expression);

            if err.is_some() {
                break;
            }

            let Some(comma) = self.optional(TokenType::Symbol(tokenizer::Symbol::COMMA))? else {
                break;
            };
            range.merge(&comma.range);
        }

        Some(expressions)
    }

    fn lvalue_list(
        &mut self,
        err: &mut Option<ParseError>,
        range: &mut Range,
        is_into: bool,
    ) -> EOFOr<Vec<LValue>> {
        let mut intos = Vec::new();
        loop {
            let lvalue;
            (lvalue, *err) = self.parse_lvalue(true)?.split();
            let Some(mut lvalue) = lvalue else {
                break;
            };
            range.merge(&lvalue.range);

            if is_into {
                if !matches!(&lvalue.lvalue_type, LValueType::SQLAccess(..)) {
                    self.error(
                        &"Expected ':<Variable or Member>'".into(),
                        lvalue.range.clone(),
                    );
                }
            } else {
                if let LValueType::SQLAccess(_, access) = &lvalue.lvalue_type {
                    if !matches!(
                        access.lvalue_type,
                        LValueType::Variable(..) | LValueType::Member(..)
                    ) {
                        self.error(
                            &"Expected ':<Variable or Member>'".into(),
                            access.range.clone(),
                        );
                    }
                }
            }

            if is_into {
                if let Some((err, range)) = lvalue.set_write() {
                    self.error(&err, range);
                }
            }
            intos.push(lvalue);

            if err.is_some() {
                break;
            }

            let Some(comma) = self.optional(TokenType::Symbol(tokenizer::Symbol::COMMA))? else {
                break;
            };
            range.merge(&comma.range);
        }
        Some(intos)
    }

    fn optional_using(
        &mut self,
        err: &mut Option<ParseError>,
        range: &mut Range,
    ) -> EOFOr<UsingTranscation> {
        Some(
            if err.is_none()
                && self
                    .optional(TokenType::Keyword(tokenizer::Keyword::USING))?
                    .is_some()
            {
                Some(VariableAccess {
                    name: self.id_or_invalid(err, range)?,
                    is_write: false,
                })
            } else {
                None
            },
        )
    }

    fn parse_sql_open(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let err = None;
        let cursor = self.id_or_invalid(&err, &mut range)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::OPEN(cursor)),
                range,
            },
            err,
        ))
    }

    fn parse_sql_close(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let err = None;
        let cursor = self.id_or_invalid(&err, &mut range)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::CLOSE(cursor)),
                range,
            },
            err,
        ))
    }

    fn parse_sql_connect(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;
        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::CONNECT(
                    self.optional_using(&mut err, &mut range)?,
                )),
                range,
            },
            err,
        ))
    }

    fn parse_sql_disconnect(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;
        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::DISCONNECT(
                    self.optional_using(&mut err, &mut range)?,
                )),
                range,
            },
            err,
        ))
    }

    fn parse_sql_commit(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;
        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::COMMIT(
                    self.optional_using(&mut err, &mut range)?,
                )),
                range,
            },
            err,
        ))
    }

    fn parse_sql_declare(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        let name = self.id_or_invalid(&err, &mut range)?;

        match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::CURSOR) => {
                self.tokens.next()?;
                ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::FOR))?);

                let stmt = ret_res!(self.parse_sql_select()?, err);
                let select = match stmt.statement_type {
                    StatementType::SQL(SQLStatement::SELECT(select)) => select,
                    _ => unreachable!(),
                };

                Some(ParseResult::new(
                    Statement {
                        statement_type: StatementType::SQL(SQLStatement::DECLARE_CURSOR(
                            name, select,
                        )),
                        range: range.expanded(&stmt.range.end),
                    },
                    err,
                ))
            }
            TokenType::Keyword(tokenizer::Keyword::PROCEDURE) => {
                self.tokens.next()?;
                ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::FOR))?);
                let stored_procedure_name = self.id_or_invalid(&err, &mut range)?;

                let mut params = Vec::new();
                loop {
                    let Some(at) = self.optional(TokenType::Symbol(tokenizer::Symbol::AT))? else {
                        break;
                    };
                    range.merge(&at.range);

                    let param_name = self.id_or_invalid(&err, &mut range)?;

                    if err.is_none() {
                        match self.expect(TokenType::Operator(tokenizer::Operator::EQ))? {
                            Ok(eq) => {
                                range.merge(&eq.range);
                            }
                            Err(_err) => {
                                err = Some(_err);
                                break;
                            }
                        }
                    }

                    if err.is_none() {
                        params.push((param_name, ret_res!(self.parse_expression(true)?, err)));
                    }

                    if err.is_some()
                        || self
                            .optional(TokenType::Symbol(tokenizer::Symbol::COMMA))?
                            .is_none()
                    {
                        break;
                    };
                }

                let transaction = self.optional_using(&mut err, &mut range)?;

                Some(ParseResult::new(
                    Statement {
                        statement_type: StatementType::SQL(SQLStatement::DECLARE_PROCEDURE(
                            SQLDeclareProcedureStatement {
                                procedure_name: name,
                                stored_procedure_name,
                                params,
                                transaction,
                            },
                        )),
                        range: range,
                    },
                    err,
                ))
            }
            _ => {
                let range = self.tokens.peek()?.range;
                return self.fatal_res(
                    &"Expected either CURSOR or PROCEDURE".into(),
                    range,
                    true,
                    None,
                );
            }
        }
    }

    fn parse_sql_execute(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let err = None;
        let procedure = self.id_or_invalid(&err, &mut range)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::EXECUTE(procedure)),
                range,
            },
            err,
        ))
    }

    fn parse_sql_fetch(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        let cursor_or_procedure = self.id_or_invalid(&err, &mut range)?;
        let into = ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::INTO))?);
        range.merge(&into.range);

        let intos = self.lvalue_list(&mut err, &mut range, true)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::FETCH(cursor_or_procedure, intos)),
                range,
            },
            err,
        ))
    }

    fn parse_sql_rollback(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;
        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::ROLLBACK(
                    self.optional_using(&mut err, &mut range)?,
                )),
                range,
            },
            err,
        ))
    }

    fn parse_sql_delete(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::FROM))?);
        let table = self.id_or_invalid(&err, &mut range)?;

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::WHERE))?);

        if self
            .optional(TokenType::Keyword(tokenizer::Keyword::CURRENT))?
            .is_some()
        {
            ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::OF))?);
            let cursor = self.id_or_invalid(&err, &mut range)?;

            Some(ParseResult::new(
                Statement {
                    statement_type: StatementType::SQL(SQLStatement::DELETE_OF_CURSOR(
                        table, cursor,
                    )),
                    range,
                },
                err,
            ))
        } else {
            let clause = ret_res!(self.parse_expression(true)?, err);
            range.merge(&clause.range);

            let transaction = self.optional_using(&mut err, &mut range)?;

            Some(ParseResult::new(
                Statement {
                    statement_type: StatementType::SQL(SQLStatement::DELETE(
                        table,
                        clause,
                        transaction,
                    )),
                    range,
                },
                err,
            ))
        }
    }

    fn parse_sql_select(&mut self) -> EOFOr<ParseResult<Statement>> {
        let is_blob = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::SELECT) => false,
            TokenType::Keyword(tokenizer::Keyword::SELECTBLOB) => true,
            _ => unreachable!(),
        };
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        let lparen = self.optional(TokenType::Symbol(tokenizer::Symbol::LPAREN))?;

        let fields = self.expression_list(&mut err, &mut range)?;
        if let Some(err) = err {
            return Some(Err((err, None)));
        }

        if lparen.is_some() {
            ret_res!(self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))?);
        }

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::INTO))?);

        // let lparen = self.optional(TokenType::Symbol(tokenizer::Symbol::LPAREN))?;

        let intos = self.lvalue_list(&mut err, &mut range, true)?;
        if let Some(err) = err {
            return Some(Err((err, None)));
        }

        // if lparen.is_some() {
        //     ret_res!(self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))?);
        // }

        range.merge(
            &ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::FROM))?).range,
        );
        let mut err = None;
        let table = self.id_or_invalid(&err, &mut range)?;

        let clause = if err.is_none()
            && self
                .optional(TokenType::Keyword(tokenizer::Keyword::WHERE))?
                .is_some()
        {
            let clause = ret_res!(self.parse_expression(true)?, err);
            range.merge(&clause.range);

            Some(clause)
        } else {
            None
        };

        let transaction = self.optional_using(&mut err, &mut range)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::SELECT(SQLSelectStatement {
                    is_blob,
                    fields,
                    intos,
                    table,
                    clause,
                    transaction,
                })),
                range,
            },
            err,
        ))
    }

    fn parse_sql_insert(&mut self) -> EOFOr<ParseResult<Statement>> {
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::INTO))?);

        let table = self.id_or_invalid(&err, &mut range)?;
        if let Some(err) = err {
            return Some(Err((err, None)));
        }

        ret_res!(self.expect(TokenType::Symbol(tokenizer::Symbol::LPAREN))?);
        let fields = self.lvalue_list(&mut err, &mut range, false)?;
        if let Some(err) = err {
            return Some(Err((err, None)));
        }
        ret_res!(self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))?);

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::VALUES))?);

        let mut values = Vec::new();
        loop {
            match self.expect(TokenType::Symbol(tokenizer::Symbol::LPAREN))? {
                Ok(paren) => {
                    range.merge(&paren.range);
                }
                Err(_err) => {
                    err = Some(_err);
                    break;
                }
            }

            // TODO is expression or lvalue ? INSERT INTO t (y) VALUES (x > 0);
            values.push(self.expression_list(&mut err, &mut range)?);

            if err.is_some() {
                break;
            }

            match self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))? {
                Ok(paren) => {
                    range.merge(&paren.range);
                }
                Err(_err) => {
                    err = Some(_err);
                    break;
                }
            }

            let Some(comma) = self.optional(TokenType::Symbol(tokenizer::Symbol::COMMA))? else {
                break;
            };
            range.merge(&comma.range);
        }

        let transaction = self.optional_using(&mut err, &mut range)?;

        Some(ParseResult::new(
            Statement {
                statement_type: StatementType::SQL(SQLStatement::INSERT(SQLInsertStatement {
                    table,
                    fields,
                    values,
                    transaction,
                })),
                range,
            },
            err,
        ))
    }

    fn parse_sql_update(&mut self) -> EOFOr<ParseResult<Statement>> {
        let is_blob = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::UPDATE) => false,
            TokenType::Keyword(tokenizer::Keyword::UPDATEBLOB) => true,
            _ => unreachable!(),
        };
        let mut range = self.tokens.next()?.range;
        let mut err = None;

        let table = self.id_or_invalid(&err, &mut range)?;

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::SET))?);
        let set = ret_res!(self.parse_expression(true)?);

        ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::WHERE))?);
        if self
            .optional(TokenType::Keyword(tokenizer::Keyword::CURRENT))?
            .is_some()
        {
            ret_res!(self.expect(TokenType::Keyword(tokenizer::Keyword::OF))?);
            let cursor = self.id_or_invalid(&err, &mut range)?;

            Some(ParseResult::new(
                Statement {
                    statement_type: StatementType::SQL(SQLStatement::UPDATE_OF_CURSOR(
                        SQLUpdateCursorStatement {
                            is_blob,
                            table,
                            set,
                            cursor,
                        },
                    )),
                    range,
                },
                err,
            ))
        } else {
            let clause = ret_res!(self.parse_expression(true)?, err);

            let transaction = self.optional_using(&mut err, &mut range)?;

            Some(ParseResult::new(
                Statement {
                    statement_type: StatementType::SQL(SQLStatement::UPDATE(SQLUpdateStatement {
                        is_blob,
                        table,
                        set,
                        clause,
                        transaction,
                    })),
                    range,
                },
                err,
            ))
        }
    }

    pub fn parse_sql_statement(&mut self) -> EOFOr<Option<Statement>> {
        self.tokens.ignore_newlines = true;

        let ret = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::OPEN) => self.parse_sql_open(),
            TokenType::Keyword(tokenizer::Keyword::CLOSE) => self.parse_sql_close(),

            TokenType::Keyword(tokenizer::Keyword::CONNECT) => self.parse_sql_connect(),
            TokenType::Keyword(tokenizer::Keyword::DISCONNECT) => self.parse_sql_disconnect(),

            TokenType::Keyword(tokenizer::Keyword::COMMIT) => self.parse_sql_commit(),
            TokenType::Keyword(tokenizer::Keyword::DECLARE) => self.parse_sql_declare(),
            TokenType::Keyword(tokenizer::Keyword::EXECUTE) => self.parse_sql_execute(),
            TokenType::Keyword(tokenizer::Keyword::FETCH) => self.parse_sql_fetch(),
            TokenType::Keyword(tokenizer::Keyword::ROLLBACK) => self.parse_sql_rollback(),

            TokenType::Keyword(tokenizer::Keyword::DELETE) => self.parse_sql_delete(),
            TokenType::Keyword(tokenizer::Keyword::INSERT) => self.parse_sql_insert(),
            TokenType::Keyword(tokenizer::Keyword::SELECT | tokenizer::Keyword::SELECTBLOB) => {
                self.parse_sql_select()
            }
            TokenType::Keyword(tokenizer::Keyword::UPDATE | tokenizer::Keyword::UPDATEBLOB) => {
                self.parse_sql_update()
            }
            _ => panic!(),
        };

        let mut err = None;
        let mut token = ret_opt!(ret?, err);
        if err.is_none() {
            if let Ok(semi) = self.expect(TokenType::Symbol(tokenizer::Symbol::SEMICOLON))? {
                token.range.end = semi.range.end;
            }
        }

        self.tokens.ignore_newlines = false;
        Some(Some(token))
    }
}
