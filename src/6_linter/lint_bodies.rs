use std::{collections::HashMap, sync::Arc};

use super::linter::{Linter, Scope};
use crate::{
    builder,
    parser::{self, GroupedName},
    project,
    resolver::{self, ResolvedType},
    tokenizer,
    types::*,
};

impl<'a> Linter<'a> {
    pub fn lint_datatype(&self, data_type: &parser::DataType) -> &'a ResolvedType<'a> {
        let typ = self.annotations.must_type(&data_type.range);
        if let ResolvedType::Unknown = typ {
            self.diagnostic_error("Class not found".into(), data_type.range.clone());
        }
        typ
    }

    pub fn lint_lvalue(&self, lvalue: &parser::LValue, scope: &Scope) -> &'a ResolvedType<'a> {
        let resolved = self.annotations.lvalue(&lvalue);
        let typ = self.annotations.must_type(&lvalue.range);

        match &lvalue.lvalue_type {
            // TODO is unwrap stupid?
            parser::LValueType::This => {
                if self.class.is_none() {
                    self.diagnostic_error(
                        "Cannot use `this` outside of a class".into(),
                        lvalue.range.clone(),
                    );
                }
            }
            parser::LValueType::Super => {
                if self.class.is_none() {
                    self.diagnostic_error(
                        "Cannot use `super` outside of a class".into(),
                        lvalue.range.clone(),
                    );
                }
            }
            parser::LValueType::Parent => {
                if let Some(fc) = self.class {
                    if let ResolvedType::Unknown = typ {
                        if fc.class.within().is_some() {
                            // The None case should be handled in the class definition
                            self.diagnostic_error(
                                "Class does not have a within class and does not inherit from windowobject".into(),
                                lvalue.range.clone(),
                            );
                        }
                    }
                } else {
                    self.diagnostic_error(
                        "Cannot use `parent` outside of a class".into(),
                        lvalue.range.clone(),
                    );
                }
            }
            parser::LValueType::Variable(variable) => {
                if !resolved.found() {
                    // TODO(diagnostic): put the possibilities into the error message
                    self.diagnostic_error("Variable not found".into(), variable.name.range.clone());
                }
            }
            parser::LValueType::Function(call) => {
                let types = self.lint_expressions(&call.arguments, scope);

                if !resolved.found() {
                    // TODO(diagnostic): ...
                    self.diagnostic_error(
                        "Could not find a function that is callable by the Arguments".into(),
                        call.name.range.clone(),
                    );
                }
            }
            parser::LValueType::Method(lvalue, call) => {
                let data_type = self.lint_lvalue(lvalue, scope);
                let types = self.lint_expressions(&call.arguments, scope);

                match data_type {
                    ResolvedType::Complex(project::Complex::Class(fc)) => {
                        if !resolved.found() {
                            // TODO(diagnostic): show args, also do events
                            self.diagnostic_error(
                                "Could not find a method that is callable by the Arguments".into(),
                                call.name.range.clone(),
                            );
                        }
                    }
                    ResolvedType::Base(builder::BaseType::Any) | ResolvedType::Unknown => {}
                    _ => {
                        self.diagnostic_error(
                            "Cannot call a method of a non Class".into(),
                            call.name.range.clone(),
                        );
                    }
                }
            }
            parser::LValueType::Member(lvalue, member) => {
                let data_type = self.lint_lvalue(lvalue, scope);
                match data_type {
                    ResolvedType::Complex(project::Complex::Class(fc)) => {
                        if !resolved.found() {
                            // TODO(diagnostic): ...
                            self.diagnostic_error(
                                "Member Variable not found or not accessible".into(),
                                member.name.range.clone(),
                            );
                        }
                    }
                    ResolvedType::Base(builder::BaseType::Any) | ResolvedType::Unknown => {}
                    _ => {
                        self.diagnostic_error(
                            "Cannot get a member of a non Class".into(),
                            member.name.range.clone(),
                        );
                    }
                }
            }
            parser::LValueType::Index(array, index) => {
                let array_type = self.lint_lvalue(array, scope);
                let index_type = self.lint_expression(index, scope);

                // TODO(arrays): check if the index is within bounds
                // TODO(arrays): multi dimensional arrays
                if !index_type.is_numeric() {
                    self.diagnostic_error(
                        "Index for subscript operator must be numerical".into(),
                        index.range.clone(),
                    );
                }

                match array_type {
                    ResolvedType::Array(_) => {}
                    ResolvedType::Unknown | ResolvedType::Base(builder::BaseType::Any) => {}
                    _ => {
                        self.diagnostic_error(
                            "Subscript Operator can only be applied to Array".into(),
                            array.range.clone(),
                        );
                    }
                }
            }
            // TODO(sql): ...
            parser::LValueType::SQLAccess(_, access) => match self.lint_lvalue(access, scope) {
                ResolvedType::Base(_) | ResolvedType::Unknown => {}
                _ => {
                    self.diagnostic_error(
                        "SQL statements read or write to Complex data types".into(),
                        lvalue.range.clone(),
                    );
                }
            },
        }

        typ
    }

    pub fn lint_expression(
        &self,
        expression: &parser::Expression,
        scope: &Scope,
    ) -> &'a ResolvedType<'a> {
        let typ = self.annotations.must_type(&expression.range);

        match &expression.expression_type {
            parser::ExpressionType::Literal(literal) => {
                if let ResolvedType::Unknown = typ {
                    assert!(matches!(literal.literal_type, tokenizer::Literal::ENUM));
                    self.diagnostic_error("Unknown Enum Literal".into(), literal.range.clone());
                }
            }
            parser::ExpressionType::ArrayLiteral(expressions) => {
                let types = self.lint_expressions(expressions, scope);

                if let Some(data_type) = types.iter().find(|t| !matches!(t, ResolvedType::Unknown))
                {
                    for expression_type in &types {
                        if !self.proj.is_convertible(data_type, expression_type) {
                            self.diagnostic_error(
                                "Array Literal contains differing types".into(),
                                expression.range.clone(),
                            );
                            break;
                        }
                    }
                }
            }
            parser::ExpressionType::Operation(left, op, right) => {
                let left_type = self.lint_expression(left, scope);
                let right_type = self.lint_expression(right, scope);

                match op {
                    tokenizer::Operator::AND | tokenizer::Operator::OR => {
                        if !self.proj.is_convertible(
                            &left_type,
                            &ResolvedType::Base(builder::BaseType::Boolean),
                        ) {
                            self.diagnostic_error(
                                "Invalid type for Operation, expected boolean".into(),
                                left.range.clone(),
                            );
                        }

                        if !self.proj.is_convertible(
                            &right_type,
                            &ResolvedType::Base(builder::BaseType::Boolean),
                        ) {
                            self.diagnostic_error(
                                "Invalid type for Operation, expected boolean".into(),
                                right.range.clone(),
                            );
                        }
                    }
                    tokenizer::Operator::EQ | tokenizer::Operator::GTLT => {
                        if !self.proj.is_convertible(&left_type, &right_type) {
                            self.diagnostic_error(
                                "Types do not match".into(),
                                expression.range.clone(),
                            );
                        }
                    }
                    tokenizer::Operator::GT
                    | tokenizer::Operator::GTE
                    | tokenizer::Operator::LT
                    | tokenizer::Operator::LTE => {
                        if !(left_type.is_numeric() && right_type.is_numeric()) {
                            self.diagnostic_error(
                                "Invalid types for Operation, expected numeric".into(),
                                expression.range.clone(),
                            );
                        }
                    }
                    tokenizer::Operator::PLUS
                        if self.proj.is_convertible(
                            &left_type,
                            &ResolvedType::Base(builder::BaseType::String),
                        ) && self.proj.is_convertible(
                            &right_type,
                            &ResolvedType::Base(builder::BaseType::String),
                        ) => {}
                    _ => {
                        match (
                            left_type.numeric_precedence(),
                            right_type.numeric_precedence(),
                        ) {
                            (Some(left), Some(right)) => {}
                            (..) => {
                                self.diagnostic_error(
                                    "Invalid types for Operation".into(),
                                    expression.range.clone(),
                                );
                            }
                        }
                    }
                }
            }
            parser::ExpressionType::UnaryOperation(_operator, expression) => {
                let data_type = self.lint_expression(expression, scope);

                if !data_type.is_numeric() {
                    self.diagnostic_error(
                        "Invalid type, expected number".into(),
                        expression.range.clone(),
                    );
                }
            }
            parser::ExpressionType::BooleanNot(expression) => {
                let expression_type = self.lint_expression(expression, scope);

                if !self.proj.is_convertible(
                    &expression_type,
                    &ResolvedType::Base(builder::BaseType::Boolean),
                ) {
                    self.diagnostic_error(
                        "Invalid type, expected boolean".into(),
                        expression.range.clone(),
                    );
                }
            }
            parser::ExpressionType::Parenthesized(expression) => {
                self.lint_expression(expression, scope);
            }
            parser::ExpressionType::Create(data_type) => match self.lint_datatype(data_type) {
                ResolvedType::Unknown => {}
                ResolvedType::Complex(project::Complex::Class(fc)) => {
                    if fc.class.parsed.class.autoinstantiate.is_some() {
                        self.diagnostic_error(
                            "Cannot create an Autoinstantiated Class".into(),
                            data_type.range.clone(),
                        );
                    }
                }
                _ => {
                    self.diagnostic_error(
                        "Create Expression requires a Class".into(),
                        data_type.range.clone(),
                    );
                }
            },
            parser::ExpressionType::CreateUsing(class) => {
                let class_type = self.lint_expression(class, scope);

                if !self
                    .proj
                    .is_convertible(&class_type, &ResolvedType::Base(builder::BaseType::String))
                {
                    self.diagnostic_error(
                        "Invalid type, expected String".into(),
                        class.range.clone(),
                    );
                }
            }
            parser::ExpressionType::LValue(lvalue) => {
                self.lint_lvalue(lvalue, scope);
            }
            parser::ExpressionType::IncrementDecrement(expression, _) => {
                let expression_type = self.lint_expression(expression, scope);

                if !expression_type.is_numeric() {
                    self.diagnostic_error(
                        "Cannot increment on non Numeric DataType".into(),
                        expression.range.clone(),
                    );
                }
            }
            parser::ExpressionType::Error => {}
        }

        typ
    }

    fn lint_sql_statement(&self, statement: &parser::SQLStatement, scope: &Scope) {
        if let Some(access) = statement.get_transaction() {
            // TODO(sql): ...
            // let trans_type = self.lint_variable_access(None, access, scope);

            // let correct_class = match trans_type {
            //     Found::Yes(PowerScriptType::Complex(cls)) => {
            //         let transation_class = self.proj.builtin_class("transaction");
            //         match self.proj.find_class(Some(self.file), &cls) {
            //             Found::Yes(cplx @ Complex::Class(_)) => self
            //                 .proj
            //                 .inherits_from(&cplx, &project::Complex::Class(transation_class))
            //                 .unwrap_or(false),
            //             _ => false,
            //         }
            //     }
            //     _ => false,
            // };

            // if !correct_class {
            //     self.diagnostic_error(
            //         "Transaction Object needs to be or inherit from `transcation`".into(),
            //         access.name.range.clone(),
            //     );
            // }
        };

        match statement {
            parser::SQLStatement::OPEN(_cursor_token) => {
                // TODO(sql): ...
                // if let MaybeMut::Mut(file) = self.file {
                //     let iname = (&cursor_token.content).into();
                //     if let Some(cursor) = file.sql_cursors.get(&iname) {
                //         cursor.lock().references.push(cursor_token.clone());
                //     } else {
                //         self.diagnostic_warning(
                //             "Cursor might not have been declared".to_owned(),
                //             cursor_token.range,
                //         );
                //         file.sql_procedures.insert(
                //             iname,
                //             arc_mut(SQLProcedure {
                //                 definitions: vec![],
                //                 references: vec![cursor_token.clone()],
                //             }),
                //         );
                //     }
                // }
            }
            parser::SQLStatement::CLOSE(_cursor_or_procedure) => {}
            parser::SQLStatement::CONNECT(_trans) => {}
            parser::SQLStatement::DISCONNECT(_trans) => {}
            parser::SQLStatement::COMMIT(_trans) => {}
            parser::SQLStatement::DECLARE_CURSOR(_token, _select) => {
                // TODO(sql): ...
                // if let MaybeMut::Mut(file) = self.file {
                //     let iname = (&token.content).into();
                //     if let Some(cursor) = file.sql_cursors.get(&iname) {
                //         cursor
                //             .lock()
                //
                //             .definitions
                //             .push((token.clone(), select.clone()));
                //     } else {
                //         file.sql_cursors.insert(
                //             iname,
                //             arc_mut(SQLThing {
                //                 definitions: vec![(token.clone(), select.clone())],
                //                 references: vec![],
                //             }),
                //         );
                //     }
                // }
            }
            parser::SQLStatement::DECLARE_PROCEDURE(_procedure) => {
                // TODO(sql): ...
                // if let MaybeMut::Mut(file) = self.file {
                //     let iname = (&procedure.procedure_name.content).into();
                //     if let Some(cursor) = file.sql_procedures.get(&iname) {
                //         cursor.lock().definitions.push(procedure.clone());
                //     } else {
                //         file.sql_procedures.insert(
                //             iname,
                //             arc_mut(SQLProcedure {
                //                 definitions: vec![procedure.clone()],
                //                 references: vec![],
                //             }),
                //         );
                //     }
                // }
            }
            parser::SQLStatement::EXECUTE(_procedure) => {
                // TODO(sql): ...
                // if let MaybeMut::Mut(file) = self.file {
                //     let iname = (&procedure.content).into();
                //     if let Some(cursor) = file.sql_procedures.get(&iname) {
                //         cursor.lock().references.push(procedure.clone());
                //     } else {
                //         self.diagnostic_warning(
                //             "Procedure might not have been declared".to_owned(),
                //             procedure.range,
                //         );
                //         file.sql_procedures.insert(
                //             iname,
                //             arc_mut(SQLProcedure {
                //                 definitions: vec![],
                //                 references: vec![procedure.clone()],
                //             }),
                //         );
                //     }
                // }
            }
            parser::SQLStatement::FETCH(_token, _lvalues) => {}
            parser::SQLStatement::ROLLBACK(_token) => {}
            parser::SQLStatement::DELETE(_token, _expression, _token1) => {}
            parser::SQLStatement::DELETE_OF_CURSOR(_token, _token1) => {}
            parser::SQLStatement::INSERT(_sqlinsert_statement) => {}
            parser::SQLStatement::SELECT(_sqlselect_statement) => {}
            parser::SQLStatement::UPDATE(_sqlupdate_statement) => {}
            parser::SQLStatement::UPDATE_OF_CURSOR(_sqlupdate_cursor_statement) => {}
        }
    }

    pub fn lint_statement(&self, statement: &parser::Statement, scope: &Scope) {
        match &statement.statement_type {
            parser::StatementType::Expression(expression) => {
                self.lint_expression(expression, scope);
            }
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                elseif_statements,
                else_statements,
            }) => {
                fn check_if(
                    linter: &Linter<'_>,
                    condition: &parser::Expression,
                    statements: &Vec<parser::Statement>,
                    scope: &Scope,
                ) {
                    let condition_type = linter.lint_expression(condition, scope);
                    if !linter.proj.is_convertible(
                        &condition_type,
                        &ResolvedType::Base(builder::BaseType::Boolean),
                    ) {
                        linter.diagnostic_error(
                            "Condition for if must be of type Boolean".into(),
                            condition.range.clone(),
                        );
                    }

                    statements
                        .iter()
                        .for_each(|s| linter.lint_statement(s, scope));
                }

                check_if(self, condition, statements, scope);

                for (condition, statements) in elseif_statements {
                    check_if(self, condition, statements, scope);
                }

                else_statements
                    .iter()
                    .for_each(|s| self.lint_statement(s, scope));
            }
            parser::StatementType::Throw(exception) => {
                let ex = self.lint_expression(exception, scope);
                // TODO(diagnostic): check if the exception is of a type that can be thrown
            }
            parser::StatementType::Destroy(object) => {
                let data_type = self.lint_expression(object, scope);
                match data_type {
                    ResolvedType::Complex(project::Complex::Class(_)) => {}
                    ResolvedType::Base(builder::BaseType::Any) | ResolvedType::Unknown => {}
                    _ => {
                        self.diagnostic_error(
                            "Can only destroy Objects".into(),
                            object.range.clone(),
                        );
                    }
                }
            }
            parser::StatementType::Declaration(vars) => {
                if vars.is_empty() {
                    return;
                }

                let dt = self.lint_datatype(&vars[0].variable.data_type);

                for var in vars {
                    if let Some(body) = scope.context.body {
                        let def = body
                            .variables
                            .get(&(&var.variable.access.name.content).into())
                            .unwrap();

                        if def.parsed().access.name.range != var.variable.access.name.range {
                            self.diagnostic_error(
                                "Variable already declared".into(),
                                var.variable.access.name.range.clone(),
                            );
                            self.diagnostic_hint(
                                "Variable already declared here".into(),
                                def.parsed().access.name.range.clone(),
                            );
                        }
                    } else {
                        self.diagnostic_error(
                            "Variables can only be declared within a body".into(),
                            var.variable.access.name.range.clone(),
                        );
                    }

                    if let Some(initial_value) = &var.variable.initial_value {
                        let initial_type = self.lint_expression(initial_value, scope);

                        if !self.proj.is_convertible(&initial_type, &dt) {
                            self.diagnostic_error(
                                "Type's are not convertible".into(),
                                initial_value.range.clone(),
                            );
                        }
                    }
                }
            }
            parser::StatementType::Assignment(lvalue, special_assignment, expression) => {
                let lvalue_type = self.lint_lvalue(lvalue, scope);
                let expression_type = self.lint_expression(expression, scope);

                match special_assignment {
                    None => {
                        if !self.proj.is_convertible(&expression_type, &lvalue_type) {
                            self.diagnostic_error(
                                "Type's are not convertible".into(),
                                expression.range.clone(),
                            );
                        }
                    }
                    Some(tokenizer::SpecialAssignment::PLUSEQ)
                        if self.proj.is_convertible(
                            &lvalue_type,
                            &ResolvedType::Base(builder::BaseType::String),
                        ) =>
                    {
                        if !self.proj.is_convertible(
                            &expression_type,
                            &ResolvedType::Base(builder::BaseType::String),
                        ) {
                            if !expression_type.is_numeric() {
                                self.diagnostic_error(
                                    "Needs to be a String".into(),
                                    expression.range.clone(),
                                );
                            }
                        }
                    }
                    Some(_) => {
                        if !lvalue_type.is_numeric() {
                            self.diagnostic_error(
                                "Cannot do a Special Assignment on a Type that is neither Numeric or a String".into(),
                                lvalue.range.clone(),
                            );
                        } else if !expression_type.is_numeric() {
                            self.diagnostic_error(
                                "Needs to be a Numeric Type".into(),
                                expression.range.clone(),
                            );
                        }
                    }
                }
            }
            parser::StatementType::TryCatch(parser::TryCatchStatement {
                statements,
                catches,
                finally,
            }) => {
                statements
                    .iter()
                    .for_each(|s| self.lint_statement(s, scope));
                for (var, statements) in catches {
                    self.lint_statement(var, scope);
                    statements
                        .iter()
                        .for_each(|s| self.lint_statement(s, scope));
                }
                if let Some(statements) = finally {
                    statements
                        .iter()
                        .for_each(|s| self.lint_statement(s, scope));
                }
            }
            parser::StatementType::ForLoop(
                for_loop @ parser::ForLoopStatement {
                    start,
                    stop,
                    step,
                    variable,
                    statements,
                },
            ) => {
                let variable_type = self.lint_lvalue(variable, scope);
                let start_type = self.lint_expression(start, scope);
                let stop_type = self.lint_expression(stop, scope);

                let mut to_check = vec![
                    (variable_type, &for_loop.variable().name.range),
                    (start_type, &start.range),
                    (stop_type, &stop.range),
                ];

                if let Some(step) = step {
                    let step_type = self.lint_expression(step, scope);
                    to_check.push((step_type, &step.range));
                }

                for (data_type, range) in to_check {
                    if !data_type.is_numeric() {
                        self.diagnostic_error("Needs to be a Numeric Type".into(), range.clone());
                    }
                }

                statements
                    .iter()
                    .for_each(|s| self.lint_statement(s, scope));
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                statements,
                ..
            }) => {
                let condition_type = self.lint_expression(condition, scope);

                statements
                    .iter()
                    .for_each(|s| self.lint_statement(s, scope));

                if !self.proj.is_convertible(
                    &condition_type,
                    &ResolvedType::Base(builder::BaseType::Boolean),
                ) {
                    self.diagnostic_error(
                        format!(
                            "Condition for while loop must be Boolean, got {}",
                            condition_type
                        ),
                        condition.range.clone(),
                    );
                }
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                let choose_type = self.lint_expression(choose, scope);

                for (cases, statements) in cases {
                    for case in cases {
                        if let parser::CaseSpecifierType::Else = &case.specifier_type {
                            if cases.len() != 1 {
                                self.diagnostic_error(
                                    "CASE ELSE must be alone".into(),
                                    case.range.clone(),
                                );
                            }
                        }

                        for expression in case.get_expressions() {
                            let literal_type = self.lint_expression(expression, scope);
                            if !self.proj.is_convertible(&choose_type, &literal_type) {
                                self.diagnostic_error(
                                    format!(
                                        "Wrong Literal Type, got {}, expected {}",
                                        literal_type, choose_type
                                    ),
                                    expression.range.clone(),
                                );
                            }
                        }
                    }

                    statements
                        .iter()
                        .for_each(|s| self.lint_statement(s, scope));
                }
            }
            parser::StatementType::Return(ret) => {
                let dt = ret.as_ref().map(|r| self.lint_expression(r, scope));
                match (&dt, &scope.return_type) {
                    (Some(returned), Some(expected))
                        if !self.proj.is_convertible(returned, expected) =>
                    {
                        self.diagnostic_error(
                            format!(
                                "Return type {} is not convertible into {}",
                                returned, expected
                            ),
                            statement.range.clone(),
                        );
                    }
                    (Some(_returned), None) => {
                        self.diagnostic_error(
                            "Did not expect a return".into(),
                            statement.range.clone(),
                        );
                    }
                    (None, Some(expected)) => {
                        self.diagnostic_error(
                            format!("Expected a return value (of type {})", expected),
                            statement.range.clone(),
                        );
                    }
                    _ => {}
                };
            }
            parser::StatementType::Call(parser::CallStatement {
                call_type,
                function,
            }) => {
                let types = self.lint_expressions(&function.arguments, scope);

                match call_type {
                    parser::CallType::Super => {
                        if self.class.is_none() {
                            self.diagnostic_error(
                                "Cannot use `super` outside of a class".into(),
                                function.range.clone(),
                            );
                        }
                    }
                    parser::CallType::Ancestor(GroupedName { name, .. }) => {
                        // (Some((&name.content).into()), &name.range)
                    }
                };

                // TODO(call):
                // if let Some(base) = base {
                //     if let Found::Yes(ancestor) = self.lint_class_usage(&base, range) {
                //         let inherits = self.map_class_or(Found::No, |fc| {
                //             self.proj.inherits_from(&project::Complex::Class(*fc), &ancestor)
                //         });

                //         if let Found::Yes(false) = inherits {
                //             self.diagnostic_error(
                //                 "Class is not an Ancestor".into(),
                //                 statement.range.clone(),
                //             );
                //         } else {
                //             match ancestor {
                //                 Complex::Class(class) => {
                //                     self.lint_function_call(Some(class), function, &types, scope);
                //                 }
                //                 Complex::Enum(_) => {
                //                     self.diagnostic_error(
                //                         "Cannot call a Method on an Enum Ancestor".into(),
                //                         statement.range.clone(),
                //                     );
                //                 }
                //             }
                //         }
                //     } else {
                //         self.diagnostic_error("Ancestor not found".into(), statement.range.clone());
                //     }
                // }
            }
            parser::StatementType::Goto(label) => {
                if let Some(body) = scope.context.body {
                    let iname = (&label.content).into();
                    if let None = body.labels.get(&iname) {
                        self.diagnostic_error("Label not found".into(), label.range.clone());
                    }
                } else {
                    self.diagnostic_error(
                        "Cannot have a goto outside a body".into(),
                        label.range.clone(),
                    );
                }
            }
            parser::StatementType::Label(label) => {
                if let Some(body) = scope.context.body {
                    let iname = (&label.content).into();
                    if let Some(prev) = body.labels.get(&iname) {
                        if prev.range != label.range {
                            self.diagnostic_error(
                                "Label already declared".into(),
                                label.range.clone(),
                            );
                            self.diagnostic_hint(
                                "Label already declared here".into(),
                                prev.range.clone(),
                            );
                        }
                    }
                } else {
                    self.diagnostic_error(
                        "Cannot declare a label outside a body".into(),
                        label.range.clone(),
                    );
                }
            }
            parser::StatementType::Exit => {}
            parser::StatementType::Continue => {}
            parser::StatementType::SQL(sql) => self.lint_sql_statement(sql, scope),
            parser::StatementType::Error => {}
        }
    }

    pub fn lint_expressions(
        &self,
        expressions: &Vec<parser::Expression>,
        scope: &Scope,
    ) -> Vec<&'a ResolvedType<'a>> {
        expressions
            .iter()
            .map(|e| self.lint_expression(e, scope))
            .collect()
    }

    pub(super) fn lint_arguments_and_returns(
        &self,
        returns: &Option<builder::DataType>,
        arguments: impl Iterator<Item = &'a Arc<builder::Variable>>,
    ) -> (
        HashMap<IString, &'a Arc<builder::Variable>>,
        Option<&'a ResolvedType<'a>>,
    ) {
        let mut args = HashMap::new();
        for arg in arguments {
            let range = arg.parsed().access.name.range.clone();

            let prev = args.insert((&arg.parsed().access.name.content).into(), arg);

            if prev.is_some() {
                self.diagnostic_error(
                    "Duplicate argument, shadowing previous argument".into(),
                    range,
                );
            }
        }

        let return_type = returns.as_ref().map(|r| self.lint_datatype(&r.parsed));

        (args, return_type)
    }

    pub(super) fn lint_statements_in_block(
        &self,
        statements: impl Iterator<Item = &'a parser::Statement>,
        returns: &Option<builder::DataType>,
        arguments: impl Iterator<Item = &'a Arc<builder::Variable>>,
        body: &builder::Body,
    ) {
        let (args_map, return_type) = self.lint_arguments_and_returns(returns, arguments);

        let mut scope = Scope {
            return_type,
            context: resolver::Context::new_for_body(self.proj, self.file, self.class),
        };
        scope.context.body = Some(body);
        scope.context.arguments = Some(args_map);

        statements.for_each(|s| self.lint_statement(s, &scope));
    }
}
