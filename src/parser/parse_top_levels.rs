use super::*;
use crate::{
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl Parser {
    pub fn parse_argument_list_declaration(
        &mut self,
    ) -> EOFOrParserResult<(Range, Vec<Argument>, Option<Token>)> {
        let start = quick_exit_simple!(self.expect(TokenType::Symbol(tokenizer::Symbol::LPAREN))?)
            .range
            .start;

        let mut end = Position {
            line: start.line,
            column: start.column + 1,
        };
        let mut vararg = None;
        let mut err = None;

        let mut arguments = Vec::new();
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::RPAREN) => {
                    end = self.tokens.next()?.range.end;
                    break;
                }
                TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT) => {
                    let token = self.tokens.next()?;
                    end = self.tokens.next()?.range.end;
                    vararg = Some(token);

                    match self.expect(TokenType::Symbol(tokenizer::Symbol::RPAREN))? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => err = Some(res_err),
                    }
                    break;
                }
                _ => {}
            };

            let var_start = self.tokens.peek()?.range.end;
            let is_ref = self
                .optional(TokenType::Keyword(tokenizer::Keyword::REF))?
                .is_some();
            let is_readonly = self
                .optional(TokenType::Keyword(tokenizer::Keyword::READONLY))?
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
                    access: VariableAccess { name, is_write: true },
                    initial_value: None,
                    range: Range {
                        start: var_start,
                        end,
                    },
                },
            });

            match self.tokens.peek()?.token_type {
                TokenType::Symbol(tokenizer::Symbol::COMMA) => {
                    end = self.tokens.next()?.range.end;
                }
                TokenType::Symbol(tokenizer::Symbol::RPAREN) => {
                    end = self.tokens.next()?.range.end;
                    break;
                }
                _ => {
                    let range = self.tokens.peek()?.range;
                    err = self
                        .fatal::<()>(&"Expected ',' or ')'".into(), range, true)?
                        .err();
                }
            }
        }

        Some(ParseResult::new(
            (Range { start, end }, arguments, vararg),
            err,
        ))
    }

    pub fn parse_event_header(&mut self) -> EOFOrParserResult<Event> {
        let start = quick_exit_simple!(self.expect(TokenType::Keyword(tokenizer::Keyword::EVENT))?)
            .range
            .start;

        let returns = if self
            .optional(TokenType::Keyword(tokenizer::Keyword::TYPE))?
            .is_some()
        {
            Some(quick_exit!(self.parse_type()?))
        } else {
            None
        };

        let name = quick_exit_simple!(self.expect(TokenType::ID)?);

        let mut end;
        let mut err = None;
        let event_type = match self.tokens.peek()?.token_type {
            TokenType::ID => {
                let system = self.tokens.next()?;
                end = system.range.end;

                if returns.is_some() {
                    self.error(
                        &"Can't specify a return Type when declaring a System Event".into(),
                        system.range,
                    );
                }

                EventType::System(system.content)
            }
            TokenType::Symbol(tokenizer::Symbol::LPAREN) => {
                end = self.tokens.peek()?.range.end;
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
            err = self.expect_newline()?.err();
        }

        let event = Event {
            name,
            event_type,
            range: Range { start, end },
        };

        Some(ParseResult::new(event, err))
    }

    pub fn parse_function_header(&mut self) -> EOFOrParserResult<Function> {
        let start = self.tokens.peek()?.range.start;

        let scope_modif = if let TokenType::ScopeModif(scope_modif) = self.tokens.peek()?.token_type {
            self.tokens.next()?;
            Some(scope_modif)
        } else {
            None
        };

        let access = if let TokenType::AccessType(access) = self.tokens.peek()?.token_type {
            self.tokens.next()?;
            Some(access)
        } else {
            None
        };

        let returns = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::FUNCTION) => {
                self.tokens.next()?;
                Some(quick_exit!(self.parse_type()?))
            }
            TokenType::Keyword(tokenizer::Keyword::SUBROUTINE) => {
                self.tokens.next()?;
                None
            }
            _ => {
                let range = self.tokens.peek()?.range;
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
            if let Some(token) = self.optional(TokenType::Keyword(tokenizer::Keyword::THROWS))? {
                end = token.range.end;

                loop {
                    match self.expect(TokenType::ID)? {
                        Ok(token) => end = token.range.end,
                        Err(res_err) => {
                            err = Some(res_err);
                            break;
                        }
                    }

                    match self.optional(TokenType::Symbol(tokenizer::Symbol::COMMA))? {
                        Some(token) => end = token.range.end,
                        None => break,
                    }
                }
            }
        }

        if err.is_none() {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::RPCFUNC) => {
                    end = self.tokens.next()?.range.end;
                }
                TokenType::Keyword(tokenizer::Keyword::LIBRARY) => {
                    end = self.tokens.next()?.range.end;
                    err = self
                        .expect(TokenType::Literal(tokenizer::Literal::STRING))?
                        .err();

                    if err.is_none()
                        && self
                            .optional(TokenType::Keyword(tokenizer::Keyword::ALIAS))?
                            .is_some()
                    {
                        err = self
                            .expect(TokenType::Keyword(tokenizer::Keyword::FOR))?
                            .err();
                        if err.is_none() {
                            err = self
                                .expect(TokenType::Literal(tokenizer::Literal::STRING))?
                                .err();
                        }
                    }
                }
                _ => {}
            }
        }

        if err.is_none() {
            err = self.expect_newline()?.err();
        }

        Some(ParseResult::new(
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
        ))
    }

    pub fn parse_event(&mut self) -> EOFOr<Option<TopLevel>> {
        let event = quick_exit_opt!(self.parse_event_header()?);

        let mut statements = Vec::new();
        let end;
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::EVENT) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        self.expect_newline()?.ok();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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

    pub fn parse_function(&mut self) -> EOFOr<Option<TopLevel>> {
        let function = quick_exit_opt!(self.parse_function_header()?);

        let mut statements = Vec::new();
        let end;

        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(
                        function_type @ (tokenizer::Keyword::FUNCTION
                        | tokenizer::Keyword::SUBROUTINE),
                    ) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        if (function.returns.is_none())
                            ^ (function_type == tokenizer::Keyword::SUBROUTINE)
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

                        self.expect_newline()?.ok();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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

    pub fn parse_on(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::ON))?)
                .range
                .start;
        let class = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
        quick_exit_simple_opt!(self.expect(TokenType::Symbol(tokenizer::Symbol::DOT))?);

        let name = match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::CREATE | tokenizer::Keyword::DESTROY) => {
                let name = self.tokens.next()?;
                self.expect_newline()?.ok();
                name
            }
            _ => {
                let range = self.tokens.peek()?.range;
                return Some(
                    self.fatal(&"Expected either CREATE or DESTROY".into(), range, true)?
                        .ok(),
                );
            }
        };

        let mut statements = Vec::new();
        let end;

        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::ON) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        self.expect_newline()?.ok();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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
            top_level_type: TopLevelType::OnBody(On { class, name }, statements),
            range: Range { start, end },
        }))
    }

    pub fn parse_datatype_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.tokens.peek()?.range.start;

        let scope = if let TokenType::ScopeModif(scope) = self.tokens.peek()?.token_type {
            self.tokens.next()?;
            Some(scope)
        } else {
            None
        };

        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::TYPE))?);
        let id = quick_exit_simple_opt!(self.expect(TokenType::ID)?);
        let name = DataType {
            data_type_type: DataTypeType::Complex(GroupedName::new(None, id.content)),
            range: id.range,
        };
        let mut base_pos = name.range.end;

        let (base, mut err) = match self.expect(TokenType::Keyword(tokenizer::Keyword::FROM))? {
            Ok(from) => {
                base_pos = from.range.end;
                self.parse_type()?.split()
            }
            Err(err) => (None, Some(err)),
        };
        let base = base.unwrap_or_else(|| DataType {
            data_type_type: DataTypeType::Complex(GroupedName::new(None, "powerobject".into())),
            range: base_pos.into(),
        });

        let within = if err.is_none()
            && self
                .optional(TokenType::Keyword(tokenizer::Keyword::WITHIN))?
                .is_some()
        {
            let within;
            (within, err) = self.parse_type()?.split();
            within
        } else {
            None
        };

        if err.is_none() {
            self.expect_newline()?.ok();
        }

        let end;
        let mut variables = Vec::new();
        let mut events = Vec::new();
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::TYPE) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        self.expect_newline()?.ok();
                        break;
                    }
                    _ => {
                        let range = self.tokens.next()?.range;
                        self.error(&"Dangling END keyword, did you mean END TYPE".into(), range);
                        end = range.end;

                        self.consume_line()?;
                        break;
                    }
                },
                TokenType::Keyword(tokenizer::Keyword::EVENT) => {
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

    pub fn parse_scoped_variable_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.tokens.peek()?.range.start;

        let scope = match self.optional(TokenType::ScopeModif(tokenizer::ScopeModif::GLOBAL))? {
            Some(_) => tokenizer::ScopeModif::GLOBAL,
            None => tokenizer::ScopeModif::SHARED,
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

    pub fn parse_scoped_variables_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.tokens.peek()?.range.start;

        let scope = match self.tokens.peek()?.token_type {
            TokenType::ScopeModif(scope) => scope,
            _ => {
                let range = self.tokens.peek()?.range;
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
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::VARIABLES))?);
        self.expect_newline()?.ok();

        let end;
        let mut declarations = Vec::new();
        loop {
            match self.tokens.peek()?.token_type.clone() {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::VARIABLES) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        let _ = self.expect_newline();
                        break;
                    }
                    _ => {
                        end = self.tokens.peek()?.range.end;
                        let range = self.tokens.peek()?.range;
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

    pub fn parse_type_variables_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start = self.tokens.peek()?.range.start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::TYPE))?);
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::VARIABLES))?);
        self.expect_newline()?.ok();

        let end;
        let mut declarations = Vec::new();
        let mut access = None;
        loop {
            match self.tokens.peek()?.token_type.clone() {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::VARIABLES) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        let _ = self.expect_newline();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
                        self.fatal::<()>(
                            &"Dangling END keyword, did you mean END VARIABLES".into(),
                            range,
                            true,
                        )?
                        .ok();
                    }
                },
                TokenType::AccessType(new_access)
                    if self.tokens.peek_nth(1)?.token_type
                        == TokenType::Symbol(tokenizer::Symbol::COLON) =>
                {
                    let token = self.tokens.next()?;

                    if new_access.is_general() {
                        access = Some(new_access);
                    } else {
                        self.error(
                            &"Access Section must be a General Access Type".into(),
                            token.range,
                        );
                    }

                    self.tokens.next()?;
                    self.expect_newline()?.ok();
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

    pub fn parse_functions_forward_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::FORWARD))?)
                .range
                .start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::PROTOTYPES))?);
        self.expect_newline()?.ok();

        let mut functions = Vec::new();
        let end;
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::PROTOTYPES) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        let _ = self.expect_newline();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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

    pub fn parse_external_functions(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::TYPE))?)
                .range
                .start;
        quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::PROTOTYPES))?);
        self.expect_newline()?.ok();

        let mut functions = Vec::new();
        let end;
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::PROTOTYPES) => {
                        self.tokens.next()?;
                        let close = self.tokens.next()?;
                        end = close.range.end;

                        let _ = self.expect_newline();
                        break;
                    }
                    _ => {
                        let range = self.tokens.peek()?.range;
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

    pub fn parse_forward_decl(&mut self) -> EOFOr<Option<TopLevel>> {
        let start =
            quick_exit_simple_opt!(self.expect(TokenType::Keyword(tokenizer::Keyword::FORWARD))?)
                .range
                .start;
        self.expect_newline()?.ok();

        let mut types = Vec::new();
        let end;
        loop {
            match self.tokens.peek()?.token_type.clone() {
                TokenType::Keyword(tokenizer::Keyword::END) => match self.tokens.peek_nth(1)?.token_type {
                    TokenType::Keyword(tokenizer::Keyword::FORWARD) => {
                        self.tokens.next()?;
                        end = self.tokens.next()?.range.end;
                        let _ = self.expect_newline()?;
                        break;
                    }
                    _ => {
                        let range = self.tokens.next()?.range;
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

    pub fn parse_top_level(&mut self) -> EOFOr<Option<TopLevel>> {
        match self.tokens.peek()?.token_type {
            TokenType::Keyword(tokenizer::Keyword::FORWARD) => match self.tokens.peek_nth(1)?.token_type {
                TokenType::Keyword(tokenizer::Keyword::PROTOTYPES) => {
                    self.parse_functions_forward_decl()
                }
                TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                    self.parse_forward_decl()
                }
                _ => {
                    let range = self.tokens.peek()?.range;
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
            | TokenType::Keyword(tokenizer::Keyword::FUNCTION | tokenizer::Keyword::SUBROUTINE) => {
                self.parse_function()
            }
            TokenType::Keyword(tokenizer::Keyword::EVENT) => self.parse_event(),
            TokenType::Keyword(tokenizer::Keyword::ON) => self.parse_on(),
            TokenType::Keyword(tokenizer::Keyword::TYPE) => match self.tokens.peek_nth(1)?.token_type {
                TokenType::Keyword(tokenizer::Keyword::VARIABLES) => {
                    self.parse_type_variables_decl()
                }
                TokenType::Keyword(tokenizer::Keyword::PROTOTYPES) => {
                    self.parse_external_functions()
                }
                _ => self.parse_datatype_decl(),
            },
            TokenType::ScopeModif(_) => match self.tokens.peek_nth(1)?.token_type {
                TokenType::Keyword(tokenizer::Keyword::TYPE) => self.parse_datatype_decl(),
                TokenType::Keyword(tokenizer::Keyword::VARIABLES) => {
                    self.parse_scoped_variables_decl()
                }
                _ => self.parse_scoped_variable_decl(),
            },
            TokenType::ID => self.parse_scoped_variable_decl(),
            TokenType::NEWLINE | TokenType::Symbol(tokenizer::Symbol::SEMICOLON) => {
                self.tokens.next();
                Some(None)
            }
            _ => {
                let range = self.tokens.peek()?.range;
                return Some(
                    self.fatal(&"Unexpected Token for Top Level".into(), range, true)?
                        .ok(),
                );
            }
        }
    }
}
