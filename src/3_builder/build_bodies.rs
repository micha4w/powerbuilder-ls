use std::sync::Arc;

use super::{builder::Builder, types::*};
use crate::parser;

impl Builder {
    fn build_expression(&mut self, expression: &parser::Expression) {
        match &expression.expression_type {
            parser::ExpressionType::Create(data_type) => {
                self.request_class_load(&PowerScriptType::new(data_type))
            }
            parser::ExpressionType::CreateUsing(_class) => {} // TODO: do something here?
            _ => {}
        }
    }

    fn build_statement(&mut self, statement: &parser::Statement, body: &mut Body) {
        match &statement.statement_type {
            parser::StatementType::Expression(expression) => {
                self.build_expression(expression);
            }
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                elseif_statements,
                else_statements,
            }) => {
                self.build_expression(condition);
                self.build_statements(statements, body);

                for (condition, statements) in elseif_statements {
                    self.build_expression(condition);
                    self.build_statements(statements, body);
                }

                self.build_statements(else_statements, body);
            }
            parser::StatementType::Throw(exception) => self.build_expression(exception),
            parser::StatementType::Destroy(object) => self.build_expression(object),
            parser::StatementType::Declaration(vars) => {
                vars.get(0)
                    .map(|v| self.request_class_load(&PowerScriptType::new(&v.variable.data_type)));

                for var in vars {
                    if let Some(initial_value) = &var.variable.initial_value {
                        self.build_expression(initial_value);
                    }

                    body.variables.insert(
                        (&var.variable.access.name.content).into(),
                        Arc::new(Variable::new_local(var.variable.clone())),
                    );
                }
            }
            parser::StatementType::Assignment(_lval, _op, expression) => {
                self.build_expression(expression);
            }
            parser::StatementType::TryCatch(parser::TryCatchStatement {
                statements,
                catches,
                finally,
            }) => {
                self.build_statements(statements, body);
                for (var, statements) in catches {
                    self.build_statement(var, body);
                    self.build_statements(statements, body);
                }
                if let Some(statements) = finally {
                    self.build_statements(statements, body);
                }
            }
            parser::StatementType::ForLoop(parser::ForLoopStatement {
                start,
                stop,
                step,
                variable: _,
                statements,
            }) => {
                self.build_expression(start);
                self.build_expression(stop);

                if let Some(step) = step {
                    self.build_expression(step);
                }

                self.build_statements(statements, body);
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                statements,
                ..
            }) => {
                self.build_expression(condition);
                self.build_statements(statements, body);
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                self.build_expression(choose);

                for (cases, statements) in cases {
                    for case in cases {
                        for expression in case.get_expressions() {
                            self.build_expression(expression);
                        }
                    }

                    self.build_statements(statements, body);
                }
            }
            parser::StatementType::Return(ret) => {
                ret.as_ref().map(|r| self.build_expression(r));
            }
            parser::StatementType::Call(parser::CallStatement {
                call_type,
                function,
            }) => {
                for e in &function.arguments {
                    self.build_expression(e);
                }

                match call_type {
                    parser::CallType::Super => {}
                    parser::CallType::Ancestor(ancestor) => {
                        self.load_requests.insert((&ancestor.name.content).into());
                    }
                };
            }
            parser::StatementType::Goto(_) => {}
            parser::StatementType::Label(token) => {
                body.labels.insert((&token.content).into(), token.clone());
            }
            parser::StatementType::Exit => {}
            parser::StatementType::Continue => {}
            parser::StatementType::SQL(_) => {}
            parser::StatementType::Error => {}
        };
    }

    pub(super) fn build_statements(
        &mut self,
        statements: &Vec<parser::Statement>,
        body: &mut Body,
    ) {
        for statement in statements {
            self.build_statement(statement, body);
        }
    }
}
