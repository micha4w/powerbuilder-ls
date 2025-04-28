use futures::{future::BoxFuture, FutureExt};
use tokio::sync::Mutex;

use super::{linter::Linter, types::*};
use crate::{
    parser::{self, GroupedName},
    tokenizer,
};

impl<'a> Linter<'a> {
    async fn lint_variable_access(
        &mut self,
        access: &parser::VariableAccess,
    ) -> parser::DataTypeType {
        match self.find_variable(access, false).await {
            Some(var) => {
                // var.uses.push(self.range);
                var.lock().await.data_type.clone()
            }
            None => {
                self.diagnostic_error("Variable not found".into(), access.name.range.clone());
                parser::DataTypeType::Unknown
            }
        }
    }

    async fn lint_literal(&mut self, literal: &parser::Literal) -> parser::DataTypeType {
        match literal.literal_type {
            tokenizer::Literal::NUMBER => parser::DataTypeType::Int,
            tokenizer::Literal::DATE => parser::DataTypeType::Date,
            tokenizer::Literal::TIME => parser::DataTypeType::Time,
            tokenizer::Literal::STRING => parser::DataTypeType::String,
            tokenizer::Literal::BOOLEAN => parser::DataTypeType::Boolean,
            tokenizer::Literal::ENUM => {
                for enumerated_mut in self.proj.builtin_enums.values() {
                    let enumerated = enumerated_mut.lock().await;
                    if enumerated.values.contains(&(literal.content.clone() + "!")) {
                        return parser::DataTypeType::Complex(GroupedName::new(
                            None,
                            enumerated.name.clone(),
                        ));
                    }
                }

                self.diagnostic_error("Unknown Enum Literal".into(), literal.range.clone());
                parser::DataTypeType::Unknown
            }
        }
    }

    pub fn lint_lvalue<'b>(
        &'b mut self,
        lvalue: &'b parser::LValue,
    ) -> BoxFuture<'b, parser::DataTypeType> {
        async move {
            match &lvalue.lvalue_type {
                parser::LValueType::This => parser::DataTypeType::Complex(GroupedName::new(
                    None,
                    self.class.as_ref().unwrap().lock().await.name.clone(),
                )),
                parser::LValueType::Super => parser::DataTypeType::Complex(
                    self.class.as_ref().unwrap().lock().await.base.clone(),
                ),
                parser::LValueType::Parent => {
                    let class = self
                        .find_class(
                            &self
                                .class
                                .as_ref()
                                .unwrap()
                                .lock()
                                .await
                                .within
                                .as_ref()
                                .unwrap(),
                        )
                        .await;
                    match class {
                        Some(complex) => match &complex.unwrap_class().lock().await.within {
                            Some(parent) => parser::DataTypeType::Complex(parent.clone()),
                            None => {
                                self.diagnostic_error(
                                    "Parent Class not found".into(),
                                    lvalue.range.clone(),
                                );
                                parser::DataTypeType::Unknown
                            }
                        },
                        None => {
                            self.diagnostic_error(
                                "Class does not have a Parent".into(),
                                lvalue.range.clone(),
                            );
                            parser::DataTypeType::Unknown
                        }
                    }
                }
                parser::LValueType::Variable(variable) => self.lint_variable_access(variable).await,
                parser::LValueType::Function(call) => {
                    // let types = Vec::new();
                    // for arg in &call.arguments {
                    //     self.lint_expression(arg).await;
                    // }
                    let types = self.lint_expressions(&call.arguments).await;

                    let ret = if call.event.is_some() {
                        let class = self.class.as_ref().unwrap();
                        let event = self
                            .find_callable_event_in_class(&class, &call.name.content, &types)
                            .await;
                        match event {
                            Some(event) => {
                                // event.uses.push(call.range.clone());
                                event.lock().await.returns.clone()
                            }
                            None => {
                                if call.dynamic.is_none() {
                                    self.diagnostic_error("Event not found".into(), lvalue.range.clone());
                                }
                                parser::DataTypeType::Unknown
                            }
                        }
                    } else {
                        match self.find_function(&call.name.content, &types).await {
                            Some(func) => {
                                // func.uses.push(call.range.clone());
                                func.lock().await.returns.clone()
                            }
                            None => {
                                if call.dynamic.is_none() {
                                    self.diagnostic_error(
                                        "Function not found".into(),
                                        lvalue.range.clone(),
                                    );
                                }
                                parser::DataTypeType::Unknown
                            }
                        }
                    };

                    if call.post.is_some() {
                        parser::DataTypeType::Void
                    } else {
                        ret
                    }
                }
                parser::LValueType::Method(lvalue, call) => {
                    let data_type = self.lint_lvalue(lvalue).await;

                    let types = self.lint_expressions(&call.arguments).await;

                    match data_type {
                        parser::DataTypeType::Complex(name) => match self.find_class(&name).await {
                            Some(Complex::Class(class_mut)) => {
                                let ret = if call.event.is_some() {
                                    match self
                                        .find_callable_event_in_class(
                                            &class_mut,
                                            &call.name.content,
                                            &types,
                                        )
                                        .await
                                    {
                                        Some(event) => {
                                            // event.uses.push(call.range);
                                            event.lock().await.returns.clone()
                                        }
                                        None => {
                                            if call.dynamic.is_none() {
                                                self.diagnostic_error(
                                                    "Event not found".into(),
                                                    lvalue.range.clone(),
                                                );
                                            }
                                            parser::DataTypeType::Unknown
                                        }
                                    }
                                } else {
                                    match self
                                        .find_callable_function_in_class(
                                            &class_mut,
                                            &call.name.content,
                                            &types,
                                            &tokenizer::AccessType::PUBLIC,
                                        )
                                        .await
                                    {
                                        Some(func) => {
                                            // func.borrow_mut().uses.push(call.range);
                                            func.lock().await.returns.clone()
                                        }
                                        None => {
                                            if call.dynamic.is_none() {
                                                self.diagnostic_error(
                                                    "Method not found".into(),
                                                    call.name.range.clone(),
                                                );
                                            }
                                            parser::DataTypeType::Unknown
                                        }
                                    }
                                };

                                if call.post.is_some() {
                                    parser::DataTypeType::Void
                                } else {
                                    ret
                                }
                            }
                            Some(Complex::Enum(_)) => {
                                self.diagnostic_error(
                                    "Cannot get a method of an Enum".into(),
                                    call.name.range.clone(),
                                );
                                parser::DataTypeType::Unknown
                            }
                            None => parser::DataTypeType::Unknown,
                        },
                        parser::DataTypeType::Any | parser::DataTypeType::Unknown => {
                            parser::DataTypeType::Unknown
                        }
                        _ => {
                            self.diagnostic_error(
                                "Cannot call a method of a non Class".into(),
                                lvalue.range.clone(),
                            );
                            parser::DataTypeType::Unknown
                        }
                    }
                }
                parser::LValueType::Member(lvalue, member) => {
                    let data_type = self.lint_lvalue(lvalue).await;

                    match data_type {
                        parser::DataTypeType::Complex(name) => match self.find_class(&name).await {
                            Some(Complex::Class(class)) => {
                                match class
                                    .lock()
                                    .await
                                    .find_variable(
                                        self,
                                        &member,
                                        &tokenizer::AccessType::PUBLIC,
                                        false,
                                    )
                                    .await
                                {
                                    Some(var) => {
                                        // var.uses.push(member.range);
                                        var.lock().await.data_type.clone()
                                    }
                                    None => {
                                        self.diagnostic_error(
                                            "Member not found".into(),
                                            member.name.range.clone(),
                                        );
                                        parser::DataTypeType::Unknown
                                    }
                                }
                            }
                            Some(Complex::Enum(_)) => {
                                self.diagnostic_error(
                                    "Cannot get a member of an Enum".into(),
                                    lvalue.range.clone(),
                                );
                                parser::DataTypeType::Unknown
                            }
                            None => {
                                self.diagnostic_error("Class not found".into(), lvalue.range.clone());
                                parser::DataTypeType::Unknown
                            }
                        },
                        parser::DataTypeType::Any | parser::DataTypeType::Unknown => {
                            parser::DataTypeType::Unknown
                        }
                        _ => {
                            self.diagnostic_error(
                                "Cannot get a member of a non Class".into(),
                                lvalue.range.clone(),
                            );
                            parser::DataTypeType::Unknown
                        }
                    }
                }
                parser::LValueType::Index(array, index) => {
                    let array_type = self.lint_lvalue(array).await;
                    let index_type = self.lint_expression(index).await;

                    if !index_type.is_numeric() {
                        self.diagnostic_error(
                            "Index for subscript operator must be numerical".into(),
                            index.range.clone(),
                        );
                    }

                    match array_type {
                        parser::DataTypeType::Array(sub_type) => *sub_type,
                        parser::DataTypeType::Unknown => parser::DataTypeType::Unknown,
                        _ => {
                            self.diagnostic_error(
                                "Subscript Operator can only be applied to Array".into(),
                                array.range.clone(),
                            );
                            parser::DataTypeType::Unknown
                        }
                    }
                }
                parser::LValueType::SQLAccess(_, access) => match self.lint_lvalue(access).await {
                    parser::DataTypeType::Complex(..) | parser::DataTypeType::Array(..) => {
                        self.diagnostic_error(
                            "SQL statements read or write to Complex data types".into(),
                            lvalue.range.clone(),
                        );
                        parser::DataTypeType::Unknown
                    }
                    access_type => access_type,
                },
            }
        }
        .boxed()
    }

    pub fn lint_expression<'b>(
        &'b mut self,
        expression: &'b parser::Expression,
    ) -> BoxFuture<'b, parser::DataTypeType> {
        async move {
            match &expression.expression_type {
                parser::ExpressionType::Literal(literal) => self.lint_literal(literal).await,
                parser::ExpressionType::ArrayLiteral(expressions) => {
                    let types = self.lint_expressions(expressions).await;

                    parser::DataTypeType::Array(Box::new(match types.first() {
                        Some(data_type) => {
                            for (expression_type, sub_expression) in
                                types.iter().zip(expressions).skip(1)
                            {
                                if !self.is_convertible(data_type, expression_type).await {
                                    self.diagnostic_error(
                                        "Array Literal contains different types".into(),
                                        sub_expression.range.clone(),
                                    );
                                    break;
                                }
                            }
                            data_type.clone()
                        }
                        None => parser::DataTypeType::Unknown,
                    }))
                }
                parser::ExpressionType::Operation(left, op, right) => {
                    let left_type = self.lint_expression(left).await;
                    let right_type = self.lint_expression(right).await;

                    match op {
                        tokenizer::Operator::AND | tokenizer::Operator::OR => {
                            if self
                                .is_convertible(&left_type, &parser::DataTypeType::Boolean)
                                .await
                                || self
                                    .is_convertible(&right_type, &parser::DataTypeType::Boolean)
                                    .await
                            {
                                self.diagnostic_error(
                                    "Invalid types for Operation, expected booleans".into(),
                                    expression.range.clone(),
                                );
                            }

                            parser::DataTypeType::Boolean
                        }
                        tokenizer::Operator::EQ | tokenizer::Operator::GTLT => {
                            if !(self.is_convertible(&left_type, &right_type).await
                                || self.is_convertible(&left_type, &right_type).await)
                            {
                                self.diagnostic_error(
                                    "Types do not match".into(),
                                    expression.range.clone(),
                                );
                            }

                            parser::DataTypeType::Boolean
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

                            parser::DataTypeType::Boolean
                        }
                        tokenizer::Operator::PLUS
                            if self
                                .is_convertible(&left_type, &parser::DataTypeType::String)
                                .await
                                && self
                                    .is_convertible(&right_type, &parser::DataTypeType::String)
                                    .await =>
                        {
                            parser::DataTypeType::String
                        }
                        _ => {
                            match (
                                left_type.numeric_precedence(),
                                right_type.numeric_precedence(),
                            ) {
                                (Some(left), Some(right)) => {
                                    if left > right {
                                        left_type
                                    } else {
                                        right_type
                                    }
                                }
                                (..) => {
                                    self.diagnostic_error(
                                        "Invalid types for Operation".into(),
                                        expression.range.clone(),
                                    );
                                    parser::DataTypeType::Unknown
                                }
                            }
                        }
                    }
                }
                parser::ExpressionType::UnaryOperation(_operator, expression) => {
                    let data_type = self.lint_expression(expression).await;

                    if data_type.is_numeric() {
                        data_type
                    } else {
                        self.diagnostic_error(
                            "Invalid type, expected number".into(),
                            expression.range.clone(),
                        );

                        parser::DataTypeType::Unknown
                    }
                }
                parser::ExpressionType::BooleanNot(expression) => {
                    let expression_type = self.lint_expression(expression).await;

                    if !self
                        .is_convertible(&expression_type, &parser::DataTypeType::Boolean)
                        .await
                    {
                        self.diagnostic_error(
                            "Invalid type, expected boolean".into(),
                            expression.range.clone(),
                        );
                    }

                    parser::DataTypeType::Boolean
                }
                parser::ExpressionType::Parenthesized(expression) => {
                    self.lint_expression(expression).await
                }
                parser::ExpressionType::Create(name) => match &name.data_type_type {
                    parser::DataTypeType::Complex(grouped_name) => {
                        match self.find_class(&grouped_name).await {
                            Some(Complex::Class(_)) => {
                                parser::DataTypeType::Complex(grouped_name.clone())
                            }
                            Some(Complex::Enum(_)) => {
                                self.diagnostic_error("Cannot create an Enum".into(), name.range.clone());
                                parser::DataTypeType::Unknown
                            }
                            None => {
                                self.diagnostic_error("Class not found".into(), name.range.clone());
                                parser::DataTypeType::Unknown
                            }
                        }
                    }
                    _ => {
                        self.diagnostic_error(
                            "Create Expression requires a Class".into(),
                            name.range.clone(),
                        );
                        parser::DataTypeType::Unknown
                    }
                },
                parser::ExpressionType::CreateUsing(class) => {
                    let class_type = self.lint_expression(class).await;

                    if !self
                        .is_convertible(&class_type, &parser::DataTypeType::String)
                        .await
                    {
                        self.diagnostic_error("Invalid type, expected String".into(), class.range.clone());
                    }

                    parser::DataTypeType::Unknown
                }
                parser::ExpressionType::LValue(lvalue) => self.lint_lvalue(lvalue).await,
                parser::ExpressionType::IncrementDecrement(expression, _) => {
                    let expression_type = self.lint_expression(expression).await;

                    if !expression_type.is_numeric() {
                        self.diagnostic_error(
                            "Cannot increment on non Numeric parser::DataTypeType".into(),
                            expression.range.clone(),
                        );
                    }

                    expression_type
                }
                parser::ExpressionType::Error => parser::DataTypeType::Unknown,
            }
        }
        .boxed()
    }

    fn lint_sql_statement<'b>(
        &'b mut self,
        statement: &'b parser::SQLStatement,
    ) -> BoxFuture<'b, ()> {
        async move {
            if let Some(access) = statement.get_transaction() {
                let trans_type = self.lint_variable_access(access).await;
                match trans_type {
                    parser::DataTypeType::Complex(grouped_name) => {
                        if let Some(class) = self.find_class(&grouped_name).await {
                            if let Some(false) = self
                                .inherits_from(
                                    &grouped_name,
                                    &GroupedName::simple("transaction".into()),
                                )
                                .await
                            {
                                self.diagnostic_error(
                                    "Transaction Object needs to be of type `transcation`".into(),
                                    access.name.range.clone(),
                                );
                            }
                        }
                    }
                    _ => {
                        self.diagnostic_error(
                            "Transaction Object needs to be of type `transcation`".into(),
                            access.name.range.clone(),
                        );
                    }
                }
            };

            match statement {
                parser::SQLStatement::OPEN(cursor_token) => {
                    // TODO
                    // if let MaybeMut::Mut(file) = self.file {
                    //     let iname = (&cursor_token.content).into();
                    //     if let Some(cursor) = file.sql_cursors.get(&iname) {
                    //         cursor.lock().await.references.push(cursor_token.clone());
                    //     } else {
                    //         self.diagnostic_warning(
                    //             "Cursor might not have been declared".to_owned(),
                    //             cursor_token.range,
                    //         );
                    //         file.sql_procedures.insert(
                    //             iname,
                    //             Mutex::new(SQLProcedure {
                    //                 definitions: vec![],
                    //                 references: vec![cursor_token.clone()],
                    //             })
                    //             .into(),
                    //         );
                    //     }
                    // }
                }
                parser::SQLStatement::CLOSE(cursor_or_procedure) => {}
                parser::SQLStatement::CONNECT(trans) => {}
                parser::SQLStatement::DISCONNECT(trans) => {}
                parser::SQLStatement::COMMIT(trans) => {}
                parser::SQLStatement::DECLARE_CURSOR(token, select) => {
                    // TODO
                    // if let MaybeMut::Mut(file) = self.file {
                    //     let iname = (&token.content).into();
                    //     if let Some(cursor) = file.sql_cursors.get(&iname) {
                    //         cursor
                    //             .lock()
                    //             .await
                    //             .definitions
                    //             .push((token.clone(), select.clone()));
                    //     } else {
                    //         file.sql_cursors.insert(
                    //             iname,
                    //             Mutex::new(SQLThing {
                    //                 definitions: vec![(token.clone(), select.clone())],
                    //                 references: vec![],
                    //             })
                    //             .into(),
                    //         );
                    //     }
                    // }
                }
                parser::SQLStatement::DECLARE_PROCEDURE(procedure) => {
                    // TODO
                    // if let MaybeMut::Mut(file) = self.file {
                    //     let iname = (&procedure.procedure_name.content).into();
                    //     if let Some(cursor) = file.sql_procedures.get(&iname) {
                    //         cursor.lock().await.definitions.push(procedure.clone());
                    //     } else {
                    //         file.sql_procedures.insert(
                    //             iname,
                    //             Mutex::new(SQLProcedure {
                    //                 definitions: vec![procedure.clone()],
                    //                 references: vec![],
                    //             })
                    //             .into(),
                    //         );
                    //     }
                    // }
                }
                parser::SQLStatement::EXECUTE(procedure) => {
                    // TODO
                    // if let MaybeMut::Mut(file) = self.file {
                    //     let iname = (&procedure.content).into();
                    //     if let Some(cursor) = file.sql_procedures.get(&iname) {
                    //         cursor.lock().await.references.push(procedure.clone());
                    //     } else {
                    //         self.diagnostic_warning(
                    //             "Procedure might not have been declared".to_owned(),
                    //             procedure.range,
                    //         );
                    //         file.sql_procedures.insert(
                    //             iname,
                    //             Mutex::new(SQLProcedure {
                    //                 definitions: vec![],
                    //                 references: vec![procedure.clone()],
                    //             })
                    //             .into(),
                    //         );
                    //     }
                    // }
                }
                parser::SQLStatement::FETCH(token, lvalues) => {}
                parser::SQLStatement::ROLLBACK(token) => {}
                parser::SQLStatement::DELETE(token, expression, token1) => {}
                parser::SQLStatement::DELETE_OF_CURSOR(token, token1) => {}
                parser::SQLStatement::INSERT(sqlinsert_statement) => {}
                parser::SQLStatement::SELECT(sqlselect_statement) => {}
                parser::SQLStatement::UPDATE(sqlupdate_statement) => {}
                parser::SQLStatement::UPDATE_OF_CURSOR(sqlupdate_cursor_statement) => {}
            }
        }
        .boxed()
    }

    fn lint_statement<'b>(&'b mut self, statement: &'b parser::Statement) -> BoxFuture<'b, ()> {
        async move {
            match &statement.statement_type {
                parser::StatementType::Expression(expression) => {
                    self.lint_expression(expression).await;
                }
                parser::StatementType::If(parser::IfStatement {
                    condition,
                    statements,
                    elseif_statements,
                    else_statements,
                }) => {
                    async fn check_if(
                        proj: &mut Linter<'_>,
                        condition: &parser::Expression,
                        statements: &Vec<parser::Statement>,
                    ) {
                        let condition_type = proj.lint_expression(condition).await;
                        if !proj
                            .is_convertible(&condition_type, &parser::DataTypeType::Boolean)
                            .await
                        {
                            proj.diagnostic_error(
                                "Condition for if must be of type Boolean".into(),
                                condition.range.clone(),
                            );
                        }

                        proj.lint_statements(statements).await;
                    }

                    check_if(self, condition, statements).await;

                    for (condition, statements) in elseif_statements {
                        check_if(self, condition, statements).await;
                    }

                    self.lint_statements(else_statements).await;
                }
                parser::StatementType::Throw(exception) => {
                    self.lint_expression(exception).await;
                }
                parser::StatementType::Destroy(object) => {
                    let data_type = self.lint_expression(object).await;
                    match data_type {
                        parser::DataTypeType::Complex(_) => {}
                        // TODO enum
                        // parser::DataTypeType::Array(_) => {} // TODO
                        parser::DataTypeType::Any => {}
                        parser::DataTypeType::Unknown => {}
                        _ => {
                            self.diagnostic_error("Can only destroy Objects".into(), object.range.clone());
                        }
                    }
                }
                parser::StatementType::Declaration(var) => {
                    let data_type = var.variable.data_type.data_type_type.clone();

                    if let parser::DataTypeType::Complex(name) = &data_type {
                        if self.find_class(&name).await.is_none() {
                            self.diagnostic_error(
                                "Class not found".into(),
                                var.variable.data_type.range.clone(),
                            );
                        }
                    }

                    if let Some(initial_value) = &var.variable.initial_value {
                        let initial_type = self.lint_expression(initial_value).await;

                        if !self.is_convertible(&initial_type, &data_type).await {
                            self.diagnostic_error(
                                "Type's are not convertible".into(),
                                initial_value.range.clone(),
                            );
                        }
                    }

                    self.variables.insert(
                        (&var.variable.access.name.content).into(),
                        Mutex::new(Variable {
                            variable_type: VariableType::Local(var.variable.clone()),
                            data_type,
                            // uses: Vec::new(),
                        })
                        .into(),
                    );
                }
                parser::StatementType::Assignment(lvalue, operator, expression) => {
                    let lvalue_type = self.lint_lvalue(lvalue).await;
                    let expression_type = self.lint_expression(expression).await;

                    match operator {
                        Some(tokenizer::SpecialAssignment::PLUSEQ)
                            if self
                                .is_convertible(&lvalue_type, &parser::DataTypeType::String)
                                .await =>
                        {
                            if !self
                                .is_convertible(&expression_type, &parser::DataTypeType::String)
                                .await
                            {
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
                                    "Cannot do a Special Assignment on a Non-Numeric Type".into(),
                                    lvalue.range.clone(),
                                );
                            }
                            if !expression_type.is_numeric() {
                                self.diagnostic_error(
                                    "Needs to be a Numeric Type".into(),
                                    expression.range.clone(),
                                );
                            }
                        }
                        None => {
                            if !self.is_convertible(&expression_type, &lvalue_type).await {
                                self.diagnostic_error(
                                    "Type's are not convertible".into(),
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
                    self.lint_statements(statements).await;
                    for (var, statements) in catches {
                        self.lint_statement(var).await;
                        self.lint_statements(statements).await;
                    }
                    if let Some(statements) = finally {
                        self.lint_statements(statements).await;
                    }
                }
                parser::StatementType::ForLoop(parser::ForLoopStatement {
                    start,
                    stop,
                    step,
                    variable,
                    statements,
                }) => {
                    let variable_type = self.lint_variable_access(variable).await;
                    let start_type = self.lint_expression(start).await;
                    let stop_type = self.lint_expression(stop).await;

                    let mut to_check = vec![
                        (variable_type, &variable.name.range),
                        (start_type, &start.range),
                        (stop_type, &stop.range),
                    ];

                    if let Some(step) = step {
                        let step_type = self.lint_expression(step).await;
                        to_check.push((step_type, &step.range));
                    }

                    for (data_type, range) in to_check {
                        if !data_type.is_numeric() {
                            self.diagnostic_error("Needs to be a Numeric Type".into(), range.clone());
                        }
                    }

                    self.lint_statements(statements).await;
                }
                parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                    condition,
                    statements,
                    ..
                }) => {
                    let condition_type = self.lint_expression(condition).await;

                    self.lint_statements(statements).await;

                    if !self
                        .is_convertible(&condition_type, &parser::DataTypeType::Boolean)
                        .await
                    {
                        self.diagnostic_error(
                            "Condition for while loop must be of type Boolean".into(),
                            condition.range.clone(),
                        );
                    }
                }
                parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                    let choose_type = self.lint_expression(choose).await;

                    for (cases, statements) in cases {
                        let mut literals = Vec::new();

                        for case in cases {
                            match &case.specifier_type {
                                parser::CaseSpecifierType::Literals(literal) => {
                                    literals.push((literal, &case.range))
                                }
                                parser::CaseSpecifierType::To(from, to) => {
                                    literals.push((from, &case.range));
                                    literals.push((to, &case.range));
                                }
                                parser::CaseSpecifierType::Is(_, literal) => {
                                    literals.push((literal, &case.range));
                                }
                                parser::CaseSpecifierType::Else => {
                                    if cases.len() != 1 {
                                        self.diagnostic_error(
                                            "CASE ELSE must be alone".into(),
                                            case.range.clone(),
                                        );
                                    }
                                }
                            }
                        }

                        for (literal, range) in literals {
                            let literal_type = self.lint_literal(literal).await;
                            if self.is_convertible(&choose_type, &literal_type).await {
                                self.diagnostic_error("Wrong Literal Type".into(), range.clone());
                            }
                        }

                        self.lint_statements(statements).await;
                    }
                }
                parser::StatementType::Return(ret) => {
                    let ret_type = match ret {
                        Some(ret) => self.lint_expression(ret).await,
                        None => parser::DataTypeType::Void,
                    };

                    if !self.is_convertible(&ret_type, &self.return_type).await {
                        self.diagnostic_error("Wrong return Type".into(), statement.range.clone());
                    }
                }
                parser::StatementType::Call(parser::CallStatement {
                    call_type,
                    function,
                }) => {
                    let types = self.lint_expressions(&function.arguments).await;

                    let base = match call_type {
                        parser::CallType::Super => {
                            self.find_class(&self.class.as_ref().unwrap().lock().await.base)
                                .await
                        }
                        parser::CallType::Ancestor(group, name) => {
                            let grouped_name = (group, name).into();

                            if self
                                .inherits_from(
                                    &GroupedName::new(
                                        None,
                                        self.class.clone().unwrap().lock().await.name.clone(),
                                    ),
                                    &grouped_name,
                                )
                                .await
                                .is_some_and(|inherits| !inherits)
                            {
                                self.diagnostic_error(
                                    "Class is not an Ancestor".into(),
                                    statement.range.clone(),
                                );
                            }

                            let base = self.find_class(&grouped_name).await;

                            base
                        }
                    };

                    match base {
                        Some(Complex::Class(class_mut)) => {
                            match self
                                .find_callable_function_in_class(
                                    &class_mut,
                                    &function.name.content,
                                    &types,
                                    &tokenizer::AccessType::PROTECTED,
                                )
                                .await
                            {
                                Some(_) => {}
                                None => {
                                    if function.dynamic.is_none() {
                                        self.diagnostic_error(
                                            "Method not found".into(),
                                            function.range.clone(),
                                        );
                                    }
                                }
                            }
                        }
                        Some(Complex::Enum(_)) => {
                            self.diagnostic_error(
                                "Cannot call a Method on an Enum Ancestor".into(),
                                statement.range.clone(),
                            );
                        }
                        None => {
                            self.diagnostic_error("Ancestor not found".into(), statement.range.clone());
                        }
                    }
                }
                parser::StatementType::Exit => {} // TODO stack?
                parser::StatementType::Continue => {}
                parser::StatementType::SQL(sql) => self.lint_sql_statement(sql).await,
                parser::StatementType::Error => {}
            }
        }
        .boxed()
    }

    pub async fn lint_expressions(
        &mut self,
        expressions: &Vec<parser::Expression>,
    ) -> Vec<parser::DataTypeType> {
        let mut types = Vec::new();
        for expression in expressions {
            types.push(self.lint_expression(expression).await);
        }
        types
    }

    pub(crate) async fn lint_statements(&mut self, statements: &Vec<parser::Statement>) {
        for statement in statements {
            self.lint_statement(statement).await;
        }
    }
}
