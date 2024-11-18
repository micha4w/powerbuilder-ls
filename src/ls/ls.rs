use std::collections::HashMap;
use std::sync::Weak;
use std::{mem::swap, path::PathBuf, sync::Arc};

use super::ls_types::*;
use super::powerbuilder_proto::{self, variable};
use anyhow::anyhow;
use futures::{future::BoxFuture, FutureExt};
use prost::{bytes::Bytes, Message};
use tokio::sync::Mutex;

use crate::parser::parser_types::EOFPossibleResultT as _;
use crate::parser::tokenizer::{Token, TokenType};
use crate::parser::tokenizer_types::SpecialAssignment;
use crate::parser::{
    // parser::*,
    parser_types as parser,
    tokenize,
    tokenizer_types::{self as tokens, Range},
};

impl<'a> LintState<'a> {
    async fn lint_variable_access(&mut self, access: &parser::VariableAccess) -> DataType {
        match self.find_variable(access, false).await {
            Some(var) => {
                // var.uses.push(self.range);
                var.lock().await.data_type.clone()
            }
            None => {
                self.diagnostic_error("Variable not found".into(), access.name.range);
                DataType::Unknown
            }
        }
    }

    async fn lint_literal(&mut self, literal: &parser::Literal) -> DataType {
        match literal.literal_type {
            tokens::Literal::NUMBER => DataType::Int,
            tokens::Literal::DATE => DataType::Date,
            tokens::Literal::TIME => DataType::Time,
            tokens::Literal::STRING => DataType::String,
            tokens::Literal::BOOLEAN => DataType::Boolean,
            tokens::Literal::ENUM => {
                for enumerated_mut in self.proj.read().await.builtin_enums.values() {
                    let enumerated = enumerated_mut.lock().await;
                    if enumerated.values.contains(&(literal.content.clone() + "!")) {
                        return DataType::Complex(GroupedName::new(None, enumerated.name.clone()));
                    }
                }

                self.diagnostic_error("Unknown Enum Literal".into(), literal.range);
                DataType::Unknown
            }
        }
    }

    pub fn lint_lvalue<'b>(&'b mut self, lvalue: &'b parser::LValue) -> BoxFuture<'b, DataType> {
        async move {
            match &lvalue.lvalue_type {
                parser::LValueType::This => DataType::Complex(GroupedName::new(
                    None,
                    self.class.as_ref().unwrap().lock().await.name.clone(),
                )),
                parser::LValueType::Super => {
                    DataType::Complex(self.class.as_ref().unwrap().lock().await.base.clone())
                }
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
                            Some(parent) => DataType::Complex(parent.clone()),
                            None => {
                                self.diagnostic_error(
                                    "Parent Class not found".into(),
                                    lvalue.range,
                                );
                                DataType::Unknown
                            }
                        },
                        None => {
                            self.diagnostic_error(
                                "Class does not have a Parent".into(),
                                lvalue.range,
                            );
                            DataType::Unknown
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
                                    self.diagnostic_error("Event not found".into(), lvalue.range);
                                }
                                DataType::Unknown
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
                                        lvalue.range,
                                    );
                                }
                                DataType::Unknown
                            }
                        }
                    };

                    if call.post.is_some() {
                        DataType::Void
                    } else {
                        ret
                    }
                }
                parser::LValueType::Method(lvalue, call) => {
                    let data_type = self.lint_lvalue(lvalue).await;

                    let types = self.lint_expressions(&call.arguments).await;

                    match data_type {
                        DataType::Complex(name) => match self.find_class(&name).await {
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
                                                    lvalue.range,
                                                );
                                            }
                                            DataType::Unknown
                                        }
                                    }
                                } else {
                                    match self
                                        .find_callable_function_in_class(
                                            &class_mut,
                                            &call.name.content,
                                            &types,
                                            &tokens::AccessType::PUBLIC,
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
                                                    lvalue.range,
                                                );
                                            }
                                            DataType::Unknown
                                        }
                                    }
                                };

                                if call.post.is_some() {
                                    DataType::Void
                                } else {
                                    ret
                                }
                            }
                            Some(Complex::Enum(_)) => {
                                self.diagnostic_error(
                                    "Cannot get a method of an Enum".into(),
                                    lvalue.range,
                                );
                                DataType::Unknown
                            }
                            None => DataType::Unknown,
                        },
                        DataType::Any | DataType::Unknown => DataType::Unknown,
                        _ => {
                            self.diagnostic_error(
                                "Cannot call a method of a non Class".into(),
                                lvalue.range,
                            );
                            DataType::Unknown
                        }
                    }
                }
                parser::LValueType::Member(lvalue, member) => {
                    let data_type = self.lint_lvalue(lvalue).await;

                    match data_type {
                        DataType::Complex(name) => match self.find_class(&name).await {
                            Some(Complex::Class(class)) => {
                                match class
                                    .lock()
                                    .await
                                    .find_variable(
                                        self,
                                        &member,
                                        &tokens::AccessType::PUBLIC,
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
                                            member.name.range,
                                        );
                                        DataType::Unknown
                                    }
                                }
                            }
                            Some(Complex::Enum(_)) => {
                                self.diagnostic_error(
                                    "Cannot get a member of an Enum".into(),
                                    lvalue.range,
                                );
                                DataType::Unknown
                            }
                            None => {
                                self.diagnostic_error("Class not found".into(), lvalue.range);
                                DataType::Unknown
                            }
                        },
                        DataType::Any | DataType::Unknown => DataType::Unknown,
                        _ => {
                            self.diagnostic_error(
                                "Cannot get a member of a non Class".into(),
                                lvalue.range,
                            );
                            DataType::Unknown
                        }
                    }
                }
                parser::LValueType::Index(array, index) => {
                    let array_type = self.lint_lvalue(array).await;
                    let index_type = self.lint_expression(index).await;

                    if !index_type.is_numeric() {
                        self.diagnostic_error(
                            "Index for subscript operator must be numerical".into(),
                            index.range,
                        );
                    }

                    match array_type {
                        DataType::Array(sub_type) => *sub_type,
                        DataType::Unknown => DataType::Unknown,
                        _ => {
                            self.diagnostic_error(
                                "Subscript Operator can only be applied to Array".into(),
                                array.range,
                            );
                            DataType::Unknown
                        }
                    }
                }
            }
        }
        .boxed()
    }

    pub fn lint_expression<'b>(
        &'b mut self,
        expression: &'b parser::Expression,
    ) -> BoxFuture<'b, DataType> {
        async move {
            match &expression.expression_type {
                parser::ExpressionType::Literal(literal) => self.lint_literal(literal).await,
                parser::ExpressionType::ArrayLiteral(expressions) => {
                    let types = self.lint_expressions(expressions).await;

                    DataType::Array(Box::new(match types.first() {
                        Some(data_type) => {
                            for (expression_type, sub_expression) in
                                types.iter().zip(expressions).skip(1)
                            {
                                if !self.is_convertible(data_type, expression_type).await {
                                    self.diagnostic_error(
                                        "Array Literal contains different types".into(),
                                        sub_expression.range,
                                    );
                                    break;
                                }
                            }
                            data_type.clone()
                        }
                        None => DataType::Unknown,
                    }))
                }
                parser::ExpressionType::Operation(left, op, right) => {
                    let left_type = self.lint_expression(left).await;
                    let right_type = self.lint_expression(right).await;

                    match op {
                        tokens::Operator::AND | tokens::Operator::OR => {
                            if self.is_convertible(&left_type, &DataType::Boolean).await
                                || self.is_convertible(&right_type, &DataType::Boolean).await
                            {
                                self.diagnostic_error(
                                    "Invalid types for Operation, expected booleans".into(),
                                    expression.range,
                                );
                            }

                            DataType::Boolean
                        }
                        tokens::Operator::EQ | tokens::Operator::GTLT => {
                            if !(self.is_convertible(&left_type, &right_type).await
                                || self.is_convertible(&left_type, &right_type).await)
                            {
                                self.diagnostic_error(
                                    "Types do not match".into(),
                                    expression.range,
                                );
                            }

                            DataType::Boolean
                        }
                        tokens::Operator::GT
                        | tokens::Operator::GTE
                        | tokens::Operator::LT
                        | tokens::Operator::LTE => {
                            if !(left_type.is_numeric() && right_type.is_numeric()) {
                                self.diagnostic_error(
                                    "Invalid types for Operation, expected numeric".into(),
                                    expression.range,
                                );
                            }

                            DataType::Boolean
                        }
                        tokens::Operator::PLUS
                            if self.is_convertible(&left_type, &DataType::String).await
                                && self.is_convertible(&right_type, &DataType::String).await =>
                        {
                            DataType::String
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
                                        expression.range,
                                    );
                                    DataType::Unknown
                                }
                            }
                        }
                    }
                }
                parser::ExpressionType::UnaryOperation(_operator, expression) => {
                    let data_type = self.lint_expression(expression).await;

                    if !data_type.is_numeric() {
                        self.diagnostic_error(
                            "Invalid type, expected number".into(),
                            expression.range,
                        );

                        if data_type.numeric_precedence() >= DataType::Long.numeric_precedence() {
                            DataType::Long
                        } else {
                            DataType::Int
                        }
                    } else {
                        DataType::Unknown
                    }
                }
                parser::ExpressionType::BooleanNot(expression) => {
                    let expression_type = self.lint_expression(expression).await;

                    if !self
                        .is_convertible(&expression_type, &DataType::Boolean)
                        .await
                    {
                        self.diagnostic_error(
                            "Invalid type, expected boolean".into(),
                            expression.range,
                        );
                    }

                    DataType::Boolean
                }
                parser::ExpressionType::Parenthesized(expression) => {
                    self.lint_expression(expression).await
                }
                parser::ExpressionType::Create(name) => {
                    let data_type = Into::<DataType>::into(&name.data_type_type);
                    match data_type {
                        DataType::Complex(grouped_name) => {
                            match self.find_class(&grouped_name).await {
                                Some(Complex::Class(_)) => DataType::Complex(grouped_name),
                                Some(Complex::Enum(_)) => {
                                    self.diagnostic_error(
                                        "Cannot create an Enum".into(),
                                        name.range,
                                    );
                                    DataType::Unknown
                                }
                                None => {
                                    self.diagnostic_error("Class not found".into(), name.range);
                                    DataType::Unknown
                                }
                            }
                        }
                        _ => {
                            self.diagnostic_error(
                                "Create Expression requires a Class".into(),
                                name.range,
                            );
                            DataType::Unknown
                        }
                    }
                }
                parser::ExpressionType::CreateUsing(class) => {
                    let class_type = self.lint_expression(class).await;

                    if !self.is_convertible(&class_type, &DataType::String).await {
                        self.diagnostic_error("Invalid type, expected String".into(), class.range);
                    }

                    DataType::Unknown
                }
                parser::ExpressionType::LValue(lvalue) => self.lint_lvalue(lvalue).await,
                parser::ExpressionType::IncrementDecrement(expression, _) => {
                    let expression_type = self.lint_expression(expression).await;

                    if !expression_type.is_numeric() {
                        self.diagnostic_error(
                            "Cannot increment on non Numeric DataType".into(),
                            expression.range,
                        );
                    }

                    expression_type
                }
                parser::ExpressionType::Error => DataType::Unknown,
            }
        }
        .boxed()
    }

    pub async fn lint_expressions(&mut self, expressions: &Vec<parser::Expression>) -> Vec<DataType> {
        let mut types = Vec::new();
        for expression in expressions {
            types.push(self.lint_expression(expression).await);
        }
        types

        // futures::future::join_all(
        //     expressions
        //         .iter()
        //         .map(|expression| self.lint_expression(expression)),
        // )
        // .await
    }

    async fn lint_statements(&mut self, statements: &Vec<parser::Statement>) {
        for statement in statements {
            self.lint_statement(statement).await;
        }

        // futures::future::join_all(
        //     statements
        //         .iter()
        //         .map(|statement| self.lint_statement(statement)),
        // )
        // .await;
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
                        proj: &mut LintState<'_>,
                        condition: &parser::Expression,
                        statements: &Vec<parser::Statement>,
                    ) {
                        let condition_type = proj.lint_expression(condition).await;
                        if !proj
                            .is_convertible(&condition_type, &DataType::Boolean)
                            .await
                        {
                            proj.diagnostic_error(
                                "Condition for if must be of type Boolean".into(),
                                condition.range,
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
                        DataType::Complex(_) => {}
                        // TODO enum
                        // DataType::Array(_) => {} // TODO
                        DataType::Any => {}
                        DataType::Unknown => {}
                        _ => {
                            self.diagnostic_error("Can only destroy Objects".into(), object.range);
                        }
                    }
                }
                parser::StatementType::Declaration(var) => {
                    let data_type = (&var.variable.data_type).into();

                    if let DataType::Complex(name) = &data_type {
                        if self.find_class(&name).await.is_none() {
                            self.diagnostic_error(
                                "Class not found".into(),
                                var.variable.data_type.range,
                            );
                        }
                    }

                    if let Some(initial_value) = &var.variable.initial_value {
                        let initial_type = self.lint_expression(initial_value).await;

                        if !self.is_convertible(&initial_type, &data_type).await {
                            self.diagnostic_error(
                                "Type's are not convertible".into(),
                                initial_value.range,
                            );
                        }
                    }

                    self.variables.insert(
                        (&var.variable.name.content).into(),
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
                        Some(SpecialAssignment::PLUSEQ)
                            if self.is_convertible(&lvalue_type, &DataType::String).await =>
                        {
                            if !self
                                .is_convertible(&expression_type, &DataType::String)
                                .await
                            {
                                if !expression_type.is_numeric() {
                                    self.diagnostic_error(
                                        "Needs to be a String".into(),
                                        expression.range,
                                    );
                                }
                            }
                        }
                        Some(_) => {
                            if !lvalue_type.is_numeric() {
                                self.diagnostic_error(
                                    "Cannot do a Special Assignment on a Non-Numeric Type".into(),
                                    lvalue.range,
                                );
                            }
                            if !expression_type.is_numeric() {
                                self.diagnostic_error(
                                    "Needs to be a Numeric Type".into(),
                                    expression.range,
                                );
                            }
                        }
                        None => {
                            if !self.is_convertible(&expression_type, &lvalue_type).await {
                                self.diagnostic_error(
                                    "Type's are not convertible".into(),
                                    expression.range,
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
                            self.diagnostic_error("Needs to be a Numeric Type".into(), *range);
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
                        .is_convertible(&condition_type, &DataType::Boolean)
                        .await
                    {
                        self.diagnostic_error(
                            "Condition for while loop must be of type Boolean".into(),
                            condition.range,
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
                                            case.range,
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
                        None => DataType::Void,
                    };

                    if !self.is_convertible(&ret_type, &self.return_type).await {
                        self.diagnostic_error("Wrong return Type".into(), statement.range);
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
                                    statement.range,
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
                                    &tokens::AccessType::PROTECTED,
                                )
                                .await
                            {
                                Some(_) => {}
                                None => {
                                    if function.dynamic.is_none() {
                                        self.diagnostic_error(
                                            "Method not found".into(),
                                            function.range,
                                        );
                                    }
                                }
                            }
                        }
                        Some(Complex::Enum(_)) => {
                            self.diagnostic_error(
                                "Cannot call a Method on an Enum Ancestor".into(),
                                statement.range,
                            );
                        }
                        None => {
                            self.diagnostic_error("Ancestor not found".into(), statement.range);
                        }
                    }
                }
                parser::StatementType::Exit => {} // TODO stack?
                parser::StatementType::Continue => {}
                parser::StatementType::SQL => {}
                parser::StatementType::Error => {}
            }
        }
        .boxed()
    }

    async fn lint_datatype_decl(&mut self, decl: &parser::DatatypeDecl) -> Arc<Mutex<Class>> {
        let within = match &decl.class.within {
            Some((group, name)) => {
                let within_name = (group, name).into();
                if self.find_class(&within_name).await.is_none() {
                    self.diagnostic_error("Within Class not found".into(), decl.range);
                }

                Some(within_name)
            }
            None => None,
        };

        let (group, name) = decl.class.base.clone();
        let base_name = (&group, &name).into();
        let base = self.find_class(&base_name).await;

        if base.is_none() {
            self.diagnostic_error("Base Class not found".into(), decl.range);
        }
        let new_class_mut = Arc::new(Mutex::new(Class::new(
            decl.class.name.content.clone(),
            base_name,
            within,
            matches!(decl.class.scope, Some(tokens::ScopeModif::GLOBAL)),
        )));

        {
            let mut new_class = new_class_mut.lock().await;

            for var in &decl.variables {
                new_class.instance_variables.insert(
                    (&var.variable.name.content).into(),
                    Mutex::new(Variable {
                        variable_type: VariableType::Instance((
                            Arc::downgrade(&new_class_mut),
                            var.clone(),
                        )),
                        data_type: (&var.variable.data_type).into(),
                        // uses: Vec::new(),
                    })
                    .into(),
                );
            }

            for event in &decl.events {
                new_class.events.insert(
                    (&event.name.content).into(),
                    Mutex::new(Event::new(event.clone(), Some(event.range), None)).into(),
                );
            }
        }

        new_class_mut
    }

    fn require_class(&mut self, top_level_type: String, range: Range) -> Option<Arc<Mutex<Class>>> {
        if self.class.is_none() {
            self.diagnostic_error(
                top_level_type + " have to come after the Type Definition that they refer to",
                range,
            );
        }
        self.class.clone()
    }

    pub fn lint_file(&mut self) -> BoxFuture<()> {
        async fn add_function(
            state: &mut LintState<'_>,
            function_mut: &Arc<Mutex<Function>>,
            class: &mut Class,
            is_external: bool,
        ) {
            let new_func = function_mut.lock().await;

            match class.find_conflicting_function(&new_func).await {
                Some(func) => {
                    let mut func = func.lock().await;
                    if func.returns != new_func.parsed.returns.as_ref().into() {
                        state.diagnostic_error(
                            "Same function with different return type already exists".into(),
                            new_func.parsed.range,
                        );
                        if let Some(declaration) = func.declaration {
                            state.diagnostic_hint(
                                "Function with different return type declared here".into(),
                                declaration,
                            );
                        }
                    }

                    if let Some(declaration) = func.declaration {
                        state.diagnostic_error(
                            "Function already forward declared".into(),
                            new_func.parsed.range,
                        );
                        state.diagnostic_hint("Already forward declared here".into(), declaration);
                    } else {
                        func.declaration = Some(new_func.parsed.range);
                    }
                }
                None => {
                    let functions = if is_external {
                        &mut class.external_functions
                    } else {
                        &mut class.functions
                    };

                    let iname = IString::from(&new_func.parsed.name.content);
                    match functions.get_mut(&iname) {
                        Some(funcs) => funcs.push(function_mut.clone()),
                        None => {
                            functions.insert(iname, vec![function_mut.clone()]);
                        }
                    }
                }
            }
        }

        async move {
            let mut top_levels = ProgressedTopLevels::None(Vec::new());
            swap(&mut self.file.unwrap_mut().top_levels, &mut top_levels);

            let new_top_levels = match top_levels {
                ProgressedTopLevels::None(prev_top_levels) => ProgressedTopLevels::OnlyTypes(
                    prev_top_levels
                        .into_iter()
                        .map(|top_level| {
                            (
                                top_level.range,
                                match top_level.top_level_type {
                                    parser::TopLevelType::ForwardDecl(types) => {
                                        let mut classes = HashMap::new();

                                        for datatype in &types {
                                            let mut new_class = Class::new(
                                                datatype.class.name.content.clone(),
                                                (&datatype.class.base).into(),
                                                datatype.class.within.as_ref().map(Into::into),
                                                matches!(
                                                    datatype.class.scope,
                                                    Some(tokens::ScopeModif::GLOBAL)
                                                ),
                                            );
                                            new_class.usage.declaration = Some(datatype.range);

                                            classes.insert(
                                                (&datatype.class.name.content).into(),
                                                Mutex::new(new_class).into(),
                                            );
                                        }
                                        self.file.unwrap_mut().classes.extend(classes.clone());
                                        TopLevelType::ForwardDecl((types, classes))
                                    }
                                    parser::TopLevelType::ScopedVariablesDecl(vec) => {
                                        let mut new_vars = HashMap::new();
                                        for var in &vec {
                                            let new_var = Arc::new(Mutex::new(Variable {
                                                data_type: (&var.variable.data_type).into(),
                                                variable_type: VariableType::Scoped(var.clone()),
                                            }));
                                            new_vars.insert(
                                                (&var.variable.name.content).into(),
                                                new_var.clone(),
                                            );
                                        }

                                        self.file.unwrap_mut().variables.extend(new_vars.clone());
                                        TopLevelType::ScopedVariablesDecl((vec, new_vars))
                                    }
                                    parser::TopLevelType::ScopedVariableDecl(var) => {
                                        let new_var = Arc::new(Mutex::new(Variable {
                                            data_type: (&var.variable.data_type).into(),
                                            variable_type: VariableType::Scoped(var.clone()),
                                        }));
                                        self.file.unwrap_mut().variables.insert(
                                            (&var.variable.name.content).into(),
                                            new_var.clone(),
                                        );
                                        TopLevelType::ScopedVariableDecl((var, new_var))
                                    }
                                    parser::TopLevelType::DatatypeDecl(datatype_decl) => {
                                        TopLevelType::DatatypeDecl(datatype_decl)
                                    }
                                    parser::TopLevelType::TypeVariablesDecl(vec) => {
                                        TopLevelType::TypeVariablesDecl(vec)
                                    }
                                    parser::TopLevelType::FunctionsForwardDecl(vec) => {
                                        TopLevelType::FunctionsForwardDecl(vec)
                                    }
                                    parser::TopLevelType::ExternalFunctions(vec) => {
                                        TopLevelType::ExternalFunctions(vec)
                                    }
                                    parser::TopLevelType::FunctionBody(function, vec) => {
                                        TopLevelType::FunctionBody((function, vec))
                                    }
                                    parser::TopLevelType::EventBody(event, vec) => {
                                        TopLevelType::EventBody((event, vec))
                                    }
                                    parser::TopLevelType::OnBody(on, vec) => {
                                        TopLevelType::OnBody((on, vec))
                                    }
                                },
                            )
                        })
                        .collect(),
                ),
                ProgressedTopLevels::OnlyTypes(prev_top_levels) => {
                    let mut new_vec = Vec::new();
                    for (range, top_level) in prev_top_levels {
                        new_vec.push((
                            range,
                            match top_level {
                                TopLevelType::DatatypeDecl(datatype) => {
                                    let new_class_mut = self.lint_datatype_decl(&datatype).await;

                                    let class = match self
                                        .file
                                        .unwrap_mut()
                                        .classes
                                        .get(&(&datatype.class.name.content).into())
                                        .cloned()
                                    {
                                        Some(class_arc) => {
                                            let new_class = Arc::try_unwrap(new_class_mut)
                                                .unwrap()
                                                .into_inner();

                                            self.class = Some(class_arc.clone());
                                            let mut class = class_arc.lock().await;
                                            match class.usage.definition {
                                                Some(def) => {
                                                    self.diagnostic_error(
                                                        "Type already defined".into(),
                                                        datatype.range,
                                                    );
                                                    self.diagnostic_hint(
                                                        "Type already defined here".into(),
                                                        def,
                                                    );
                                                }
                                                None => {
                                                    class.usage.definition =
                                                        new_class.usage.definition;
                                                    // swap(&mut class.events, &mut new_class.events);
                                                    // swap(
                                                    //     &mut class.instance_variables,
                                                    //     &mut new_class.instance_variables,
                                                    // );
                                                    // swap(&mut class.base, &mut new_class.base);
                                                    // swap(&mut class.within, &mut new_class.within);
                                                }
                                            }

                                            for (iname, var) in new_class.instance_variables {
                                                if !class.instance_variables.contains_key(&iname) {
                                                    if let VariableType::Instance(inst) =
                                                        &mut var.lock().await.variable_type
                                                    {
                                                        inst.0 = Arc::downgrade(&class_arc);
                                                    }
                                                    class.instance_variables.insert(iname, var);
                                                }
                                            }
                                            for (iname, event) in new_class.events {
                                                if !class.events.contains_key(&iname) {
                                                    class.events.insert(iname, event);
                                                }
                                            }

                                            class_arc.clone()
                                        }
                                        None => {
                                            {
                                                let new_class = new_class_mut.lock().await;
                                                let iname = (&new_class.name).into();
                                                if new_class.is_global {
                                                    self.diagnostic_warning(
                                                        "Global Classes should be ".to_owned()
                                                            + "Forward Declared, otherwise they "
                                                            + "might not be seen by other Files",
                                                        datatype.range,
                                                    );
                                                }
                                                self.class = Some(new_class_mut.clone());
                                                self.file
                                                    .unwrap_mut()
                                                    .classes
                                                    .insert(iname, new_class_mut.clone());
                                            }

                                            new_class_mut
                                        }
                                    };

                                    TopLevelType::DatatypeDecl((datatype, class))
                                }
                                TopLevelType::TypeVariablesDecl(vars) => {
                                    {
                                        let mut new_vars = HashMap::new();
                                        for var in &vars {
                                            let data_type = (&var.variable.data_type).into();

                                            new_vars.insert(
                                                (&var.variable.name.content).into(),
                                                Mutex::new(Variable {
                                                    variable_type: VariableType::Instance((
                                                        self.class
                                                            .as_ref()
                                                            .map_or(Weak::new(), |class| {
                                                                Arc::downgrade(&class)
                                                            }),
                                                        var.clone(),
                                                    )),
                                                    data_type,
                                                    // uses: Vec::new(),
                                                })
                                                .into(),
                                            );
                                        }

                                        if let Some(class) =
                                            self.require_class("Type Variables".into(), range)
                                        {
                                            class
                                                .lock()
                                                .await
                                                .instance_variables
                                                .extend(new_vars.clone());
                                        }

                                        TopLevelType::TypeVariablesDecl((
                                            vars,
                                            (self.class.clone(), new_vars),
                                        ))
                                    }
                                }
                                TopLevelType::FunctionsForwardDecl(functions) => {
                                    let mut new_functions = HashMap::new();
                                    for function in &functions {
                                        new_functions.insert(
                                            (&function.name.content).into(),
                                            Arc::new(Mutex::new(Function::new(
                                                function.clone(),
                                                Some(function.range),
                                                None,
                                                None,
                                            ))),
                                        );
                                    }

                                    if let Some(class_arc) = self.require_class(
                                        "Function Forward Declarations".into(),
                                        range,
                                    ) {
                                        let mut class = class_arc.lock().await;
                                        for (_, function) in &new_functions {
                                            add_function(self, function, &mut class, false).await;
                                        }
                                    }

                                    TopLevelType::FunctionsForwardDecl((
                                        functions,
                                        (self.class.clone(), new_functions),
                                    ))
                                }
                                TopLevelType::ExternalFunctions(functions) => {
                                    let mut new_functions = HashMap::new();
                                    for function in &functions {
                                        new_functions.insert(
                                            (&function.name.content).into(),
                                            Arc::new(Mutex::new(Function::new(
                                                function.clone(),
                                                Some(function.range),
                                                None,
                                                None,
                                            ))),
                                        );
                                    }

                                    if let Some(class_arc) =
                                        self.require_class("External Functions".into(), range)
                                    {
                                        let mut class = class_arc.lock().await;
                                        for (_, function) in &new_functions {
                                            add_function(self, &function, &mut class, true).await;
                                        }
                                    }
                                    TopLevelType::ExternalFunctions((
                                        functions,
                                        (self.class.clone(), new_functions),
                                    ))
                                }
                                TopLevelType::ForwardDecl(prev) => TopLevelType::ForwardDecl(prev),
                                TopLevelType::ScopedVariableDecl(prev) => {
                                    TopLevelType::ScopedVariableDecl(prev)
                                }
                                TopLevelType::ScopedVariablesDecl(prev) => {
                                    TopLevelType::ScopedVariablesDecl(prev)
                                }
                                TopLevelType::FunctionBody(prev) => {
                                    TopLevelType::FunctionBody(prev)
                                }
                                TopLevelType::EventBody(prev) => TopLevelType::EventBody(prev),
                                TopLevelType::OnBody(prev) => TopLevelType::OnBody(prev),
                            },
                        ))
                    }
                    ProgressedTopLevels::Shallow(new_vec)
                }
                ProgressedTopLevels::Shallow(prev_top_levels) => {
                    let mut new_vec = Vec::new();
                    for (range, top_level) in prev_top_levels {
                        new_vec.push((
                            range,
                            match top_level {
                                TopLevelType::DatatypeDecl((class_decl, class)) => {
                                    self.class = Some(class.clone());
                                    TopLevelType::DatatypeDecl((class_decl, class))
                                }
                                TopLevelType::FunctionBody((function, statements)) => {
                                    let mut new_func =
                                        Function::new(function.clone(), None, None, None);
                                    let mut definition = FunctionDefinition::new(
                                        &function.arguments,
                                        function.range,
                                    );

                                    swap(&mut self.variables, &mut definition.variables);
                                    self.return_type = new_func.returns.clone();

                                    self.lint_statements(&statements).await;

                                    swap(&mut self.variables, &mut definition.variables);
                                    new_func.definition = Some(definition);

                                    let new_function = match self
                                        .require_class("Function Bodies".into(), range)
                                    {
                                        None => Mutex::new(new_func).into(),
                                        Some(class_arc) => {
                                            let mut class = class_arc.lock().await;
                                            let new_func_mut = match class
                                                .find_conflicting_function(&new_func)
                                                .await
                                            {
                                                Some(existing_arc) => {
                                                    let mut existing = existing_arc.lock().await;
                                                    if let Some(definition) = &existing.definition {
                                                        self.diagnostic_error(
                                                            "Function already defined".into(),
                                                            function.range,
                                                        );
                                                        self.diagnostic_hint(
                                                            "Already defined here".into(),
                                                            definition.range,
                                                        );
                                                        new_func.declaration = existing.declaration;
                                                        None
                                                    } else {
                                                        existing.definition =
                                                            new_func.definition.take();
                                                        Some(existing_arc.clone())
                                                    }
                                                }
                                                None => {
                                                    self.diagnostic_error(
                                                        "Function is missing a Forward Declaration"
                                                            .into(),
                                                        function.range,
                                                    );
                                                    None
                                                }
                                            };

                                            new_func_mut.unwrap_or_else(|| {
                                                let iname =
                                                    IString::from(&new_func.parsed.name.content);
                                                let new_func_mut = Arc::new(Mutex::new(new_func));
                                                match class.functions.get_mut(&iname) {
                                                    Some(funcs) => funcs.push(new_func_mut.clone()),
                                                    None => {
                                                        class.functions.insert(
                                                            iname,
                                                            vec![new_func_mut.clone()],
                                                        );
                                                    }
                                                }
                                                new_func_mut
                                            })
                                        }
                                    };
                                    TopLevelType::FunctionBody((
                                        (function, statements),
                                        (self.class.clone(), new_function),
                                    ))
                                }
                                TopLevelType::EventBody((event, statements)) => {
                                    let mut new_event = Event::new(event.clone(), None, None);
                                    let args = Vec::new();
                                    let mut definition = FunctionDefinition::new(
                                        event.get_arguments().unwrap_or(&args),
                                        event.range,
                                    );

                                    swap(&mut self.variables, &mut definition.variables);
                                    self.return_type = new_event.returns.clone();

                                    self.lint_statements(&statements).await;

                                    swap(&mut self.variables, &mut definition.variables);
                                    new_event.definition = Some(definition);

                                    let event_mut = match self
                                        .require_class("Event Bodies".into(), range)
                                    {
                                        None => Arc::new(Mutex::new(new_event)),
                                        Some(class_arc) => {
                                            let mut class = class_arc.lock().await;
                                            let new_event_mut = match class
                                                .find_conflicting_event(&new_event)
                                                .await
                                            {
                                                Some(existing_arc) => {
                                                    let mut existing = existing_arc.lock().await;
                                                    if let Some(definition) = &existing.definition {
                                                        self.diagnostic_error(
                                                            "Event already defined".into(),
                                                            event.range,
                                                        );
                                                        self.diagnostic_hint(
                                                            "Already defined here".into(),
                                                            definition.range,
                                                        );
                                                        new_event.declaration =
                                                            existing.declaration;
                                                        None
                                                    } else {
                                                        existing.definition =
                                                            new_event.definition.take();
                                                        Some(existing_arc.clone())
                                                    }
                                                }
                                                None => {
                                                    self.diagnostic_error(
                                                        "Event is missing a Forward Declaration"
                                                            .into(),
                                                        event.range,
                                                    );
                                                    None
                                                }
                                            };

                                            new_event_mut.unwrap_or_else(|| {
                                                let iname =
                                                    IString::from(&new_event.parsed.name.content);
                                                let new_event_mut = Arc::new(Mutex::new(new_event));
                                                class.events.insert(iname, new_event_mut.clone());
                                                new_event_mut
                                            })
                                        }
                                    };

                                    TopLevelType::EventBody((
                                        (event, statements),
                                        (self.class.clone(), event_mut),
                                    ))
                                }
                                TopLevelType::OnBody((on, statements)) => {
                                    self.class = match self
                                        .find_class(&GroupedName::new(
                                            None,
                                            on.class.content.clone(),
                                        ))
                                        .await
                                    {
                                        Some(Complex::Class(class)) => Some(class),
                                        _ => {
                                            self.diagnostic_error(
                                                "On Body for Enum or non Existing Class".into(),
                                                range,
                                            );
                                            None
                                        }
                                    };

                                    self.variables = HashMap::new();
                                    self.return_type = DataType::Void;

                                    self.lint_statements(&statements).await;

                                    TopLevelType::OnBody(((on, statements), (self.class.clone(),)))
                                    // TODO do something with the on?
                                }
                                TopLevelType::ForwardDecl(prev) => TopLevelType::ForwardDecl(prev),
                                TopLevelType::ScopedVariableDecl(prev) => {
                                    TopLevelType::ScopedVariableDecl(prev)
                                }
                                TopLevelType::ScopedVariablesDecl(prev) => {
                                    TopLevelType::ScopedVariablesDecl(prev)
                                }
                                TopLevelType::TypeVariablesDecl(prev) => {
                                    TopLevelType::TypeVariablesDecl(prev)
                                }
                                TopLevelType::FunctionsForwardDecl(prev) => {
                                    TopLevelType::FunctionsForwardDecl(prev)
                                }
                                TopLevelType::ExternalFunctions(prev) => {
                                    TopLevelType::ExternalFunctions(prev)
                                }
                            },
                        ));
                    }
                    ProgressedTopLevels::Complete(new_vec)
                }
                ProgressedTopLevels::Complete(prev_top_levels) => {
                    ProgressedTopLevels::Complete(prev_top_levels)
                }
            };

            self.file.unwrap_mut().top_levels = new_top_levels;
        }
        .boxed()
    }
}

impl Project {
    // #region Proto Builtin Loading
    fn load_proto_function(
        func: &powerbuilder_proto::Function,
    ) -> anyhow::Result<(Option<parser::DataType>, Vec<parser::Argument>, bool)> {
        let mut has_vararg = false;
        let mut returns = None;
        let mut arguments = Vec::new();

        if let Some(ret) = &func.ret {
            if ret != "\u{1}void" {
                returns = Some(
                    tokenize(&(ret.to_owned() + "\n"))?
                        .parse_type()
                        .ok_or(anyhow!("Unexpected EOF"))?
                        .value()
                        .ok_or(anyhow!("Invalid Type"))?,
                );
            }
        }

        for arg in &func.argument {
            let flags = arg.flags.unwrap_or(0);

            if flags & variable::Flag::IsVarlist as u32 > 0 {
                has_vararg = true;
            } else {
                arguments.push(parser::Argument {
                    is_ref: flags & variable::Flag::IsRef as u32 > 0,
                    variable: parser::Variable {
                        constant: flags & variable::Flag::NoWrite as u32 > 0,
                        data_type: tokenize(&(arg.r#type.as_ref().unwrap().to_owned() + "\n"))?
                            .parse_type()
                            .ok_or(anyhow!("Unexpected EOF"))?
                            .value()
                            .ok_or(anyhow!("Invalid Type"))?,
                        name: Token {
                            token_type: TokenType::ID,
                            content: arg.name.clone().unwrap(),
                            range: Range::default(),
                            error: None,
                        },
                        initial_value: None,
                        range: Default::default(),
                    },
                })
            }
        }

        Ok((returns, arguments, has_vararg))
    }

    pub fn load_enums(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let enums = powerbuilder_proto::Enums::decode(buf)?;

        self.builtin_enums
            .extend(enums.r#enum.into_iter().map(|en| {
                (
                    (&en.name).into(),
                    Mutex::new(Enum {
                        name: en.name,
                        help: en.help,
                        values: en.value,
                    })
                    .into(),
                )
            }));

        Ok(())
    }

    pub async fn load_builtin_classes(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let classes = powerbuilder_proto::Classes::decode(buf)?;

        // TODO make not stupid
        let mut skipped = std::collections::VecDeque::<powerbuilder_proto::Class>::new();
        skipped.extend(classes.class.iter().cloned());

        loop {
            let class = match skipped.pop_front() {
                Some(class) => class,
                None => break,
            };

            match self
                .builtin_classes
                .get(&(&class.base).into())
                .cloned()
                .map(Complex::Class)
                .or_else(|| {
                    self.builtin_enums
                        .get(&(&class.base).into())
                        .cloned()
                        .map(Complex::Enum)
                }) {
                Some(_) => {
                    let iname = (&class.name).into();
                    let new_class_mut = Arc::new(Mutex::new(Class::new(
                        class.name,
                        GroupedName::new(None, class.base),
                        None,
                        true,
                    )));
                    {
                        let mut new_class = new_class_mut.lock().await;
                        new_class.help = class.help;

                        for var in class.variable {
                            let iname = var.name.as_ref().unwrap().into();
                            let parsed = parser::Variable {
                                constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32
                                    > 0,
                                data_type: tokenize(&(var.r#type.unwrap() + "\n"))?
                                    .parse_type()
                                    .ok_or(anyhow!("Unexpected EOF"))?
                                    .value()
                                    .ok_or(anyhow!("Invalid Type"))?,
                                name: Token {
                                    token_type: TokenType::ID,
                                    content: var.name.unwrap(),
                                    range: Range::default(),
                                    error: None,
                                },
                                initial_value: None,
                                range: Default::default(),
                            };
                            new_class.instance_variables.insert(
                                iname,
                                Mutex::new(Variable {
                                    data_type: (&parsed.data_type).into(),
                                    variable_type: VariableType::Instance((
                                        Arc::downgrade(&new_class_mut),
                                        parser::InstanceVariable {
                                            variable: parsed,
                                            access: parser::Access {
                                                read: None,
                                                write: None,
                                            },
                                        },
                                    )),
                                })
                                .into(),
                            );
                        }

                        for func in class.function {
                            let (returns, arguments, has_vararg) =
                                Self::load_proto_function(&func)?;

                            let iname = (&func.name).into();
                            let new_func = Mutex::new(Function::new(
                                parser::Function {
                                    returns,
                                    scope_modif: None,
                                    access: None,
                                    name: Token {
                                        token_type: TokenType::ID,
                                        content: func.name,
                                        range: Range::default(),
                                        error: None,
                                    },
                                    arguments,
                                    vararg: has_vararg.then(|| Token {
                                        token_type: TokenType::Symbol(tokens::Symbol::DOTDOTDOT),
                                        content: "...".into(),
                                        range: Range::default(),
                                        error: None,
                                    }),
                                    range: Default::default(),
                                },
                                None,
                                None,
                                func.help,
                            ))
                            .into();

                            match new_class.functions.get_mut(&iname) {
                                Some(funcs) => funcs.push(new_func),
                                None => {
                                    new_class.functions.insert(iname, vec![new_func]);
                                }
                            };
                        }

                        for event in class.event {
                            let (returns, arguments, has_vararg) =
                                Self::load_proto_function(&event)?;
                            if has_vararg {
                                todo!();
                            }
                            new_class.events.insert(
                                (&event.name).into(),
                                Mutex::new(Event::new(
                                    parser::Event {
                                        name: Token {
                                            token_type: TokenType::ID,
                                            content: event.name,
                                            range: Range::default(),
                                            error: None,
                                        },
                                        range: Default::default(),
                                        event_type: parser::EventType::User(returns, arguments),
                                    },
                                    None,
                                    None,
                                ))
                                .into(),
                            );
                        }
                    }

                    self.builtin_classes.insert(iname, new_class_mut);
                }
                None => skipped.push_back(class),
            }
        }

        Ok(())
    }

    pub fn load_builtin_functions(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let funcs = powerbuilder_proto::Functions::decode(buf)?;

        for func in funcs.function {
            let (returns, arguments, has_vararg) = Self::load_proto_function(&func)?;
            let iname = (&func.name).into();
            let new_func = Mutex::new(Function::new(
                parser::Function {
                    returns,
                    scope_modif: None,
                    access: None,
                    name: Token {
                        token_type: TokenType::ID,
                        content: func.name,
                        range: Range::default(),
                        error: None,
                    },
                    arguments,
                    vararg: has_vararg.then(|| Token {
                        token_type: TokenType::Symbol(tokens::Symbol::DOTDOTDOT),
                        content: "...".into(),
                        range: Range::default(),
                        error: None,
                    }),
                    range: Default::default(),
                },
                None,
                None,
                func.help,
            ))
            .into();

            match self.builtin_functions.get_mut(&iname) {
                Some(funcs) => funcs.push(new_func),
                None => {
                    self.builtin_functions.insert(iname, vec![new_func]);
                }
            }
        }

        Ok(())
    }
    // #endregion
}
