use std::{
    cell::{Ref, RefCell},
    env::{current_dir, var},
    mem::swap,
    path::PathBuf,
    sync::{Arc, Weak},
    thread::current,
};

use super::lsp_types::*;
use super::powerbuilder_proto::{self, variable};
use futures::{future::BoxFuture, task::waker, FutureExt};
use prost::{bytes::Bytes, Message};
use tokio::sync::RwLock;

use crate::parser::{
    parser::*,
    parser_types as parser, tokenize, tokenize_file,
    tokenizer_types::{self as tokens, Position, Range},
};

impl<'a> LintState<'a> {
    async fn lint_variable_access(&self, access: &parser::VariableAccess) -> DataType {
        match self.find_variable(access, false).await {
            Some(var) => {
                // var.uses.push(self.range);
                var.data_type.clone()
            }
            None => {
                self.diagnostic_error("Variable not found".into(), access.range);
                DataType::Unknown
            }
        }
    }

    fn lint_lvalue(&self, lvalue: &parser::LValue) -> BoxFuture<DataType> {
        async move {
            match &lvalue.lvalue_type {
                parser::LValueType::This => DataType::Complex(GroupedName::new(
                    None,
                    self.class.as_ref().unwrap().name.clone(),
                )),
                parser::LValueType::Super => {
                    DataType::Complex(self.class.as_ref().unwrap().base.clone())
                }
                parser::LValueType::Parent => {
                    match self
                        .find_class(&self.class.as_ref().unwrap().within.as_ref().unwrap())
                        .await
                    {
                        Some(complex) => match &complex.unwrap_class().within {
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

                    let ret = if call.event {
                        match self
                            .class
                            .as_ref()
                            .unwrap()
                            .find_callable_event(&call.name, &types)
                        {
                            Some(event) => {
                                // event.uses.push(call.range.clone());
                                event.returns.clone()
                            }
                            None => {
                                if !call.dynamic {
                                    self.diagnostic_error("Event not found".into(), lvalue.range);
                                }
                                DataType::Unknown
                            }
                        }
                    } else {
                        match self.class.as_ref().unwrap().find_callable_function(
                            &call.name,
                            &types,
                            &tokens::AccessType::PRIVATE,
                        ) {
                            Some(func) => {
                                // func.uses.push(call.range.clone());
                                func.returns.clone()
                            }
                            None => {
                                if !call.dynamic {
                                    self.diagnostic_error(
                                        "Function not found".into(),
                                        lvalue.range,
                                    );
                                }
                                DataType::Unknown
                            }
                        }
                    };

                    if call.post {
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
                            Some(Complex::Class(class)) => {
                                let ret = if call.event {
                                    match class.find_callable_event(&call.name, &types) {
                                        Some(event) => {
                                            // event.uses.push(call.range);
                                            event.returns.clone()
                                        }
                                        None => {
                                            if !call.dynamic {
                                                self.diagnostic_error(
                                                    "Event not found".into(),
                                                    lvalue.range,
                                                );
                                            }
                                            DataType::Unknown
                                        }
                                    }
                                } else {
                                    match class.find_callable_function(
                                        &call.name,
                                        &types,
                                        &tokens::AccessType::PUBLIC,
                                    ) {
                                        Some(func) => {
                                            // func.borrow_mut().uses.push(call.range);
                                            func.returns.clone()
                                        }
                                        None => {
                                            if !call.dynamic {
                                                self.diagnostic_error(
                                                    "Method not found".into(),
                                                    lvalue.range,
                                                );
                                            }
                                            DataType::Unknown
                                        }
                                    }
                                };

                                if call.post {
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
                            None => {
                                self.diagnostic_error("Class not found".into(), lvalue.range);
                                DataType::Unknown
                            }
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
                                        var.data_type.clone()
                                    }
                                    None => {
                                        self.diagnostic_error(
                                            "Member not found".into(),
                                            member.range,
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

    fn lint_expression(&self, expression: &parser::Expression) -> BoxFuture<DataType> {
        async move {
            match &expression.expression_type {
                parser::ExpressionType::Literal(literal) => literal.into(),
                parser::ExpressionType::ArrayLiteral(expressions) => {
                    let types = self.lint_expressions(expressions).await;

                    DataType::Array(Box::new(match types.first() {
                        Some(data_type) => {
                            if types
                                .iter()
                                .skip(1)
                                .any(|expression_type| !data_type.is_convertible(expression_type))
                            {
                                self.diagnostic_error(
                                    "Array Literal contains different types".into(),
                                    expression.range,
                                )
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
                            if left_type.is_convertible(&DataType::Boolean)
                                || right_type.is_convertible(&DataType::Boolean)
                            {
                                self.diagnostic_error(
                                    "Invalid types for Operation, expected booleans".into(),
                                    expression.range,
                                );
                            }

                            DataType::Boolean
                        }
                        tokens::Operator::EQ | tokens::Operator::GTLT => {
                            if !(left_type.is_convertible(&right_type)
                                || right_type.is_convertible(&left_type))
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
                            if !(left_type.is_numeric() || right_type.is_numeric()) {
                                self.diagnostic_error(
                                    "Invalid types for Operation, expected numeric".into(),
                                    expression.range,
                                );
                            }

                            DataType::Boolean
                        }
                        tokens::Operator::PLUS
                            if left_type.is_convertible(&DataType::String)
                                && right_type.is_convertible(&DataType::String) =>
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

                    if !expression_type.is_convertible(&DataType::Boolean) {
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
                    let grouped_name = GroupedName::new(None, name.clone());
                    match self.find_class(&grouped_name).await {
                        Some(Complex::Class(class)) => DataType::Complex(grouped_name),
                        Some(Complex::Enum(_)) => {
                            self.diagnostic_error("Cannot create Enum".into(), expression.range);
                            DataType::Unknown
                        }
                        None => {
                            self.diagnostic_error("Class not found".into(), expression.range);
                            DataType::Unknown
                        }
                    }
                }
                parser::ExpressionType::CreateUsing(class) => {
                    let class_type = self.lint_expression(class).await;

                    if !class_type.is_convertible(&DataType::String) {
                        self.diagnostic_error(
                            "Invalid type, expected string".into(),
                            expression.range,
                        );
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
            }
        }
        .boxed()
    }

    async fn lint_expressions(&self, expressions: &Vec<parser::Expression>) -> Vec<DataType> {
        futures::future::join_all(
            expressions
                .iter()
                .map(|expression| self.lint_expression(expression)),
        )
        .await
    }

    async fn lint_statements(&self, statements: &Vec<parser::Statement>) {
        futures::future::join_all(
            statements
                .iter()
                .map(|statement| self.lint_statement(statement)),
        )
        .await;
    }

    fn lint_statement(&self, statement: &parser::Statement) -> BoxFuture<()> {
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
                    fn check_if<'a>(
                        proj: &'a LintState<'a>,
                        condition: &'a parser::Expression,
                        statements: &'a Vec<parser::Statement>,
                    ) -> BoxFuture<'a, ()> {
                        async move {
                            let condition_type = proj.lint_expression(condition).await;
                            if !condition_type.is_convertible(&DataType::Boolean) {
                                proj.diagnostic_error(
                                    "Condition for if must be of type Boolean".into(),
                                    condition.range,
                                );
                            }

                            proj.lint_statements(statements).await;
                        }
                        .boxed()
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

                    if let DataType::Unknown = data_type {
                        self.diagnostic_error("Type not found".into(), var.variable.range);
                    }

                    if let Some(initial_value) = &var.variable.initial_value {
                        let initial_type = self.lint_expression(initial_value).await;

                        if !initial_type.is_convertible(&data_type) {
                            self.diagnostic_error(
                                "Type's are not convertible".into(),
                                initial_value.range,
                            );
                        }
                    }

                    self.unwrap_file().variables.insert(
                        (&var.variable.name).into(),
                        Variable {
                            variable_type: VariableType::Local(var.variable.clone()),
                            data_type,
                            // uses: Vec::new(),
                        }
                        .into(),
                    );
                }
                parser::StatementType::Assignment(lvalue, expression) => {
                    let lvalue_type = self.lint_lvalue(lvalue).await;
                    let expression_type = self.lint_expression(expression).await;

                    if !expression_type.is_convertible(&lvalue_type) {
                        self.diagnostic_error(
                            "Type's are not convertible".into(),
                            expression.range,
                        );
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
                        (variable_type, &variable.range),
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

                    if !condition_type.is_convertible(&DataType::Boolean) {
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
                                parser::CaseSpecifierType::Is(operator, literal) => {
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
                            if choose_type.is_convertible(&literal.into()) {
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

                    if !ret_type.is_convertible(&self.return_type) {
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
                            self.find_class(&self.class.as_ref().unwrap().base).await
                        }
                        parser::CallType::Ancestor(group, name) => {
                            let grouped_name = GroupedName::new(group.clone(), name.clone());

                            if self
                                .inherits_from(
                                    &GroupedName::new(
                                        None,
                                        self.class.clone().unwrap().name.clone(),
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
                        Some(Complex::Class(class)) => {
                            match class.find_callable_function(
                                &function.name,
                                &types,
                                &tokens::AccessType::PROTECTED,
                            ) {
                                Some(_) => todo!(),
                                None => todo!(),
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
                parser::StatementType::Error => {}
                parser::StatementType::Empty => {}
                parser::StatementType::SQL => {}
            }
        }
        .boxed()
    }

    async fn lint_datatype_decl(&self, decl: &parser::DatatypeDecl) -> Class {
        let within = match &decl.class.within {
            Some((group, name)) => {
                let within_name = GroupedName::new(group.clone(), name.clone());
                if self.find_class(&within_name).await.is_none() {
                    self.diagnostic_error("Within Class not found".into(), decl.range);
                }

                Some(within_name)
            }
            None => None,
        };

        let (group, name) = decl.class.base.clone();
        let base_name = GroupedName::new(group, name);
        let base = self.find_class(&base_name).await;

        if base.is_none() {
            self.diagnostic_error("Base Class not found".into(), decl.range);
        }
        let mut new_class = Class::new(
            decl.class.name.clone(),
            base_name,
            within,
            matches!(decl.class.scope, Some(tokens::ScopeModif::GLOBAL)),
        );

        for var in &decl.variables {
            new_class.instance_variables.insert(
                (&var.variable.name).into(),
                Variable {
                    variable_type: VariableType::Instance(var.clone()),
                    data_type: (&var.variable.data_type).into(),
                    // uses: Vec::new(),
                }
                .into(),
            );
        }

        for event in &decl.events {
            new_class.events.insert(
                (&event.name).into(),
                Event::new(event.clone(), Some(event.range), None).into(),
            );
        }

        new_class
    }

    fn require_class(&self, top_level_type: String, range: Range) -> Option<Arc<Class>> {
        let class = self.class;
        match class {
            Some(_) => {}
            None => self.diagnostic_error(
                top_level_type + " have to come after the Type Definition that they refer to",
                range,
            ),
        }
        class
    }

    pub async fn lint_file(&mut self, lint_progress: LintProgress) {
        let add_function = |state: &Self,
                            function: &parser::Function,
                            class: &mut Class,
                            is_external: bool| {
            let mut new_func = Function::new(function.clone(), None, None);

            match class.find_conflicting_function(&new_func) {
                Some(func) => {
                    if func.returns != function.returns.as_ref().into() {
                        state.diagnostic_error(
                            "Same function with different return type already exists".into(),
                            function.range,
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
                            function.range,
                        );
                        state.diagnostic_hint("Already forward declared here".into(), declaration);
                    } else {
                        func.declaration = Some(function.range);
                    }
                }
                None => {
                    let functions = if is_external {
                        &mut class.external_functions
                    } else {
                        &mut class.functions
                    };

                    new_func.declaration = Some(function.range);
                    let iname = IString::from(&new_func.parsed.name);
                    match functions.get_mut(&iname) {
                        Some(funcs) => funcs.push(new_func.into()),
                        None => {
                            functions.insert(iname, vec![new_func.into()]);
                        }
                    }
                }
            }
        };

        let mut top_levels = Vec::new();
        swap(&mut self.unwrap_file().top_levels, &mut top_levels);
        for top_level in &top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::ForwardDecl(types) => {
                    if matches!(lint_progress, LintProgress::OnlyTypes) {
                        for datatype in types {
                            let mut new_class = Class::new(
                                datatype.class.name.clone(),
                                datatype.class.base.clone().into(),
                                datatype.class.within.clone().map(Into::into),
                                matches!(datatype.class.scope, Some(tokens::ScopeModif::GLOBAL)),
                            );
                            new_class.usage.declaration = Some(datatype.range);

                            self.unwrap_file()
                                .classes
                                .insert((&datatype.class.name).into(), new_class.into());
                        }
                    }
                }

                // #region LintState::Shallow
                parser::TopLevelType::DatatypeDecl(datatype)
                    if matches!(lint_progress, LintProgress::Shallow) =>
                {
                    let mut new_class = self.lint_datatype_decl(datatype).await;

                    match self
                        .unwrap_file()
                        .classes
                        .get(&(&datatype.class.name).into())
                    {
                        Some(class) => {
                            self.class = Some(class.clone());
                            match class.usage.definition {
                                Some(def) => {
                                    self.diagnostic_error(
                                        "Type already defined".into(),
                                        datatype.range,
                                    );
                                    self.diagnostic_hint("Type already defined here".into(), def);
                                }
                                None => {
                                    class.usage.definition = new_class.usage.definition;
                                    swap(&mut class.events, &mut new_class.events);
                                    swap(
                                        &mut class.instance_variables,
                                        &mut new_class.instance_variables,
                                    );
                                    swap(&mut class.base, &mut new_class.base);
                                    swap(&mut class.within, &mut new_class.within);
                                }
                            }
                        }
                        None => {
                            let new_class: Arc<_> = new_class.into();
                            self.class = Some(new_class.clone());
                            if new_class.is_global {
                                self.diagnostic_warning("Global Classes should be Forward Declared, otherwise they might not be seen by other Files".into(), datatype.range);
                            }
                            self.unwrap_file()
                                .classes
                                .insert((&new_class.name).into(), new_class);
                        }
                    }
                }
                parser::TopLevelType::TypeVariablesDecl(vars) => {
                    if matches!(lint_progress, LintProgress::Shallow) {
                        if let Some(mut class) =
                            self.require_class("Type Variables".into(), top_level.range)
                        {
                            for var in vars {
                                let data_type = (&var.variable.data_type).into();

                                class.instance_variables.insert(
                                    (&var.variable.name).into(),
                                    Variable {
                                        variable_type: VariableType::Instance(var.clone()),
                                        data_type,
                                        // uses: Vec::new(),
                                    }
                                    .into(),
                                );
                            }
                        }
                    }
                }
                parser::TopLevelType::ScopedVariablesDecl(_) => {
                    if matches!(lint_progress, LintProgress::Shallow) { /* TODO shared + global */ }
                }
                parser::TopLevelType::GlobalVariableDecl(_) => {
                    if matches!(lint_progress, LintProgress::Shallow) { /* TODO implicitly shared + global + shared scope keyword invalid */
                    }
                }
                parser::TopLevelType::ConstantDecl => {
                    if matches!(lint_progress, LintProgress::Shallow) { /* TODO */ }
                }
                parser::TopLevelType::FunctionForwardDecl => {
                    if matches!(lint_progress, LintProgress::Shallow) { /* TODO */ }
                }
                parser::TopLevelType::FunctionsForwardDecl(functions) => {
                    if matches!(lint_progress, LintProgress::Shallow) {
                        if let Some(mut class) = self
                            .require_class("Function Forward Declarations".into(), top_level.range)
                        {
                            for function in functions {
                                add_function(self, function, &mut class, false);
                            }
                        }
                    }
                }
                parser::TopLevelType::ExternalFunctions(functions) => {
                    if matches!(lint_progress, LintProgress::Shallow) {
                        if let Some(class) =
                            self.require_class("External Functions".into(), top_level.range)
                        {
                            for function in functions {
                                add_function(self, function, &mut class, true);
                            }
                        }
                    }
                }
                // #endregion

                // #region LintProgress::Complete
                parser::TopLevelType::DatatypeDecl(parser::DatatypeDecl { class, .. })
                    if matches!(lint_progress, LintProgress::Complete) =>
                {
                    self.class = match self.find_class(&GroupedName::new(None, class.name)).await {
                        Some(class) => Some(class.unwrap_class().clone()),
                        None => None,
                    }
                }
                parser::TopLevelType::FunctionBody(function, statements) => {
                    if matches!(lint_progress, LintProgress::Complete) {
                        if let Some(class) =
                            self.require_class("Function Bodies".into(), top_level.range)
                        {
                            let new_func =
                                Function::new(function.clone(), None, Some(function.range));

                            self.variables = new_func.arguments.clone();
                            self.return_type = new_func.returns.clone();
                            self.lint_statements(statements).await;

                            match class.find_conflicting_function(&new_func) {
                                Some(existing) => {
                                    let mut existing = existing;
                                    if let Some(definition) = existing.definition {
                                        self.diagnostic_error(
                                            "Function already defined".into(),
                                            function.range,
                                        );
                                        self.diagnostic_hint(
                                            "Already defined here".into(),
                                            definition,
                                        );
                                    } else {
                                        existing.definition = Some(function.range);
                                    }
                                }
                                None => {
                                    self.diagnostic_error(
                                        "Function is missing a Forward Declaration".into(),
                                        function.range,
                                    );

                                    let iname = IString::from(&new_func.parsed.name);
                                    match class.functions.get(&iname) {
                                        Some(funcs) => funcs.push(new_func.into()),
                                        None => {
                                            class.functions.insert(iname, vec![new_func.into()]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                parser::TopLevelType::OnBody(on, statements) => {
                    if matches!(lint_progress, LintProgress::Complete) {
                        if let Some(class) = self
                            .find_class(&GroupedName::new(None, on.class.clone()).clone())
                            .await
                        {
                            self.variables = Vec::new();
                            self.return_type = DataType::Void;

                            self.lint_statements(statements).await;
                        } else {
                            self.diagnostic_error(
                                "On Body for non Existing Class".into(),
                                top_level.range.clone(),
                            );
                        }
                    }
                }
                parser::TopLevelType::EventBody(event, statements) => {
                    if matches!(lint_progress, LintProgress::Complete) {
                        if let Some(class) =
                            self.require_class("Event Bodies".into(), top_level.range)
                        {
                            let new_event = Event::new(event.clone(), Some(event.range), None);

                            self.variables = new_event.arguments.clone();
                            self.return_type = new_event.returns.clone();

                            self.lint_statements(statements).await;

                            match class.find_conflicting_event(&new_event) {
                                Some(existing) => {
                                    if let Some(definition) = existing.definition {
                                        self.diagnostic_error(
                                            "Event already defined".into(),
                                            event.range,
                                        );
                                        self.diagnostic_hint(
                                            "Already defined here".into(),
                                            definition,
                                        );
                                    } else {
                                        existing.definition = Some(event.range);
                                    }
                                }
                                None => {
                                    self.diagnostic_error(
                                        "Event is missing a Forward Declaration".into(),
                                        event.range,
                                    );

                                    class
                                        .events
                                        .insert((&new_event.parsed.name).into(), new_event.into());
                                }
                            }
                        }
                    }
                } // #endregion

                parser::TopLevelType::DatatypeDecl(..) => {
                    if !matches!(
                        lint_progress,
                        LintProgress::Shallow | LintProgress::Complete
                    ) {}
                }
            }
        }

        swap(&mut self.unwrap_file().top_levels, &mut top_levels);
        self.unwrap_file().lint_progress = lint_progress;
    }
}

pub async fn add_file(
    proj: Arc<RwLock<Project>>,
    path: PathBuf,
    lint_progress: LintProgress,
) -> anyhow::Result<()> {
    if !proj.read().await.files.contains_key(&path) {
        proj.write()
            .await
            .files
            .insert(path.clone(), RwLock::new(File::new(path.clone())?));
    }

    let proj_r = proj.read().await;
    let _file_lock = proj_r.files.get(&path).unwrap();
    let mut current_progress = _file_lock.read().await.lint_progress.clone();

    while current_progress < lint_progress {
        match current_progress.next() {
            Some(progress) => {
                let mut file = _file_lock.write().await;
                LintState::new(proj.clone(), Some(&mut file)).lint_file(progress.clone());
                current_progress = progress;
            }
            None => break,
        }
    }

    for diagnostic in &_file_lock.read().await.diagnostics {
        println!("{} - {}", diagnostic.range, diagnostic.message)
    }

    Ok(())
}

// #region Proto Builtin Loading
fn load_proto_function(
    func: &powerbuilder_proto::Function,
) -> anyhow::Result<(Option<parser::DataType>, Vec<parser::Argument>, bool)> {
    let mut has_vararg = false;
    let mut returns = None;
    let mut arguments = Vec::new();

    if let Some(ret) = &func.ret {
        if ret != "\u{1}void" {
            returns = Some(tokenize(&ret)?.parse_type()?);
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
                    data_type: tokenize(&arg.r#type.as_ref().unwrap())?.parse_type()?,
                    name: arg.name.clone().unwrap(),
                    initial_value: None,
                    range: Default::default(),
                },
            })
        }
    }

    Ok((returns, arguments, has_vararg))
}

pub fn load_enums(proj: &mut Project, path: PathBuf) -> anyhow::Result<()> {
    let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
    let enums = powerbuilder_proto::Enums::decode(buf)?;

    proj.builtin_enums
        .extend(enums.r#enum.into_iter().map(|en| {
            (
                (&en.name).into(),
                Enum {
                    name: en.name,
                    help: en.help,
                    values: en.value,
                }
                .into(),
            )
        }));

    Ok(())
}

pub fn load_builtin_classes(proj: &mut Project, path: PathBuf) -> anyhow::Result<()> {
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

        match proj
            .builtin_classes
            .get(&(&class.base).into())
            .cloned()
            .map(Complex::Class)
            .or_else(|| {
                proj.builtin_enums
                    .get(&(&class.base).into())
                    .cloned()
                    .map(Complex::Enum)
            }) {
            Some(base) => {
                let mut new_class =
                    Class::new(class.name, GroupedName::new(None, class.base), None, true);

                for var in class.variable {
                    let parsed = parser::Variable {
                        constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32 > 0,
                        data_type: tokenize(&var.r#type.unwrap())?.parse_type()?,
                        name: var.name.unwrap(),
                        initial_value: None,
                        range: Default::default(),
                    };
                    new_class.instance_variables.insert(
                        (&var.name.unwrap()).into(),
                        Variable {
                            data_type: (&parsed.data_type).into(),
                            variable_type: VariableType::Instance(parser::InstanceVariable {
                                variable: parsed,
                                access: parser::Access {
                                    read: None,
                                    write: None,
                                },
                            }),
                        }
                        .into(),
                    );
                }

                for func in class.function {
                    let (returns, arguments, has_vararg) = load_proto_function(&func)?;
                    let new_func: Arc<_> = Function::new(
                        parser::Function {
                            returns,
                            scope_modif: None,
                            access: None,
                            name: func.name,
                            arguments,
                            has_vararg,
                            range: Default::default(),
                        },
                        None,
                        None,
                    )
                    .into();
                    let iname = (&new_func.parsed.name).into();
                    match new_class.functions.get(&iname) {
                        Some(funcs) => funcs.push(new_func),
                        None => {
                            new_class.functions.insert(iname, vec![new_func]);
                        }
                    };
                }

                for event in class.event {
                    let (returns, arguments, has_vararg) = load_proto_function(&event)?;
                    if has_vararg {
                        todo!();
                    }
                    new_class.events.insert(
                        (&event.name).into(),
                        Event::new(
                            parser::Event {
                                name: event.name,
                                range: Default::default(),
                                event_type: parser::EventType::User(returns, arguments),
                            },
                            None,
                            None,
                        )
                        .into(),
                    );
                }

                proj.builtin_classes
                    .insert((&class.name).into(), new_class.into());
            }
            None => skipped.push_back(class),
        }
    }

    Ok(())
}

pub fn load_builtin_functions(proj: &mut Project, path: PathBuf) -> anyhow::Result<()> {
    let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
    let funcs = powerbuilder_proto::Functions::decode(buf)?;

    for func in funcs.function {
        let (returns, arguments, has_vararg) = load_proto_function(&func)?;
        let new_func = Function::new(
            parser::Function {
                returns,
                scope_modif: None,
                access: None,
                name: func.name,
                arguments,
                has_vararg,
                range: Default::default(),
            },
            None,
            None,
        )
        .into();

        let iname = (&func.name).into();
        match proj.builtin_functions.get(&iname) {
            Some(funcs) => funcs.push(new_func),
            None => {
                proj.builtin_functions.insert(iname, vec![new_func]);
            }
        }
    }

    Ok(())
}
// #endregion
