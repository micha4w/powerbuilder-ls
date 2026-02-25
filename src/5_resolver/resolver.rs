use std::{collections::hash_map, sync::Arc};

use tower_lsp::lsp_types::OneOf;

use super::{file_annotations::*, types::*, Context};
use crate::{
    builder::{self, BuiltFile},
    parser,
    project::{self, Project},
    tokenizer,
    types::*,
};

pub struct Resolver<'proj> {
    ctx: Context<'proj>,
}

impl<'a, 'proj: 'a> Resolver<'proj> {
    fn resolve_function_call(
        &'a self,
        method_of: Option<project::ClassRef<'proj>>,
        call: &'proj parser::FunctionCall,
        arguments: &'a Vec<&'a ResolvedType<'proj>>,
    ) -> (ResolvedType<'proj>, Option<ResolvedLValue<'proj>>) {
        let iname = (&call.name.content).into();

        let resolve_return_value =
            |slf: &Self, fc: Option<project::ClassRef<'proj>>, ret: &Option<builder::DataType>| {
                ret.as_ref().map_or(ResolvedType::Void, |r| {
                    slf.ctx
                        .proj
                        .to_resolved_type(fc.and_then(|fc| fc.file), &r.powerscript_type)
                })
            };

        if call.event.is_some() {
            let Some(class) = method_of else {
                return (ResolvedType::Unknown, None);
            };

            let mut events = self
                .ctx
                .proj
                .events_in_class(class, EventFilter::WithName(&iname));

            match events.next() {
                Some((fc, event)) => {
                    return (
                        resolve_return_value(self, Some(fc), &event.header().returns),
                        Some(ResolvedLValue::Method(class, OneOf::Right(event))),
                    )
                }

                None => {}
            }
        } else {
            match method_of {
                Some(class) => {
                    let funcs = self.ctx.functions_in_class(
                        class,
                        FunctionFilter::ForCall(
                            &iname,
                            &arguments,
                            tokenizer::AccessType::SYSTEMREAD,
                        ),
                    );

                    match Context::first(funcs) {
                        Found::Yes(((fc, func), _err)) => {
                            return (
                                resolve_return_value(self, Some(fc), &func.header().returns),
                                Some(ResolvedLValue::Method(fc, OneOf::Left(func))),
                            );
                        }
                        Found::No => {}
                    }
                }
                None => {
                    let funcs = self.ctx.functions(FunctionFilter::ForCall(
                        &iname,
                        &arguments,
                        tokenizer::AccessType::SYSTEMREAD,
                    ));

                    match Context::first(funcs) {
                        Found::Yes(((fc, func), _err)) => {
                            return (
                                resolve_return_value(self, fc, &func.header().returns),
                                Some(ResolvedLValue::Function(fc, func)),
                            );
                        }
                        Found::No => {}
                    }
                }
            }
        }
        (ResolvedType::Unknown, None)
    }

    fn resolve_datatype(
        &'a self,
        data_type: &'proj parser::DataType,
        annot: &'a mut AnnotationTree<'proj>,
    ) -> &'a Annotation<'proj> {
        let resolved_type = self.ctx.proj.to_resolved_type(
            Some(self.ctx.file),
            &builder::PowerScriptType::new(data_type),
        );

        annot.add_child(
            &data_type.range,
            Annotation {
                resolved_type,
                lvalue: None,
            },
        )
    }

    pub fn resolve_lvalue(
        &'a self,
        lvalue: &'proj parser::LValue,
        annot: &'a mut AnnotationTree<'proj>,
    ) -> &'a Annotation<'proj> {
        let annot = annot.add_empty_child(&lvalue.range);

        let resolved_type;
        let resolved_lvalue;
        macro_rules! not_found {
            () => {
                (resolved_type, resolved_lvalue) = (ResolvedType::Unknown, None)
            };
        }

        match &lvalue.lvalue_type {
            parser::LValueType::This => {
                resolved_lvalue = self.ctx.class.map(ResolvedLValue::This);

                resolved_type = self.ctx.class.map_or(ResolvedType::Unknown, |fc| {
                    ResolvedType::Complex(project::Complex::Class(fc))
                });
            }
            parser::LValueType::Super => {
                let base = self
                    .ctx
                    .class
                    .and_then(|fc| self.ctx.find_class(&fc.class.base().into()).as_option());

                resolved_lvalue = base.map(ResolvedLValue::Super);
                resolved_type = base.map_or(ResolvedType::Unknown, ResolvedType::Complex);
            }
            parser::LValueType::Parent => {
                let parent = self.ctx.class.and_then(|fc| {
                    match fc.class.within() {
                        Some(within) => self.ctx.find_class(&within.into()).as_option(),
                        None => {
                            // TODO(parent): is this correct?
                            let window_object = project::Complex::Class(
                                self.ctx.proj.builtin_class("windowobject"),
                            );
                            match self
                                .ctx
                                .proj
                                .inherits_from(&project::Complex::Class(fc), &window_object)
                            {
                                Found::Yes(true) => Some(window_object),
                                _ => None,
                            }
                        }
                    }
                });

                resolved_lvalue = parent.map(ResolvedLValue::Parent);
                resolved_type = parent.map_or(ResolvedType::Unknown, ResolvedType::Complex);
            }
            parser::LValueType::Variable(access) => {
                let vars = self.ctx.variables(VariableFilter::ForAccess(
                    &access,
                    tokenizer::AccessType::SYSTEMREAD,
                ));

                match Context::first(vars) {
                    Found::Yes((var, _err)) => {
                        resolved_lvalue =
                            Some(ResolvedLValue::Variable(var.class_ref(), var.variable));
                        resolved_type = self
                            .ctx
                            .proj
                            .to_resolved_type(var.file, &var.variable.data_type.powerscript_type);
                    }
                    _ => not_found!(),
                }
            }
            parser::LValueType::Member(lvalue, member) => {
                let lval_type = self.resolve_lvalue(lvalue, annot).resolved_type.clone();

                match lval_type {
                    ResolvedType::Complex(project::Complex::Class(class))
                        if member.name.token_type != tokenizer::TokenType::INVALID =>
                    {
                        let vars = self.ctx.variables_in_class(
                            class,
                            VariableFilter::ForAccess(&member, tokenizer::AccessType::SYSTEMREAD),
                        );

                        match Context::first(vars) {
                            Found::Yes(((fc, var), _err)) => {
                                resolved_lvalue = Some(ResolvedLValue::Member(fc, var));
                                resolved_type = self
                                    .ctx
                                    .proj
                                    .to_resolved_type(fc.file, &var.data_type.powerscript_type);
                            }
                            _ => not_found!(),
                        }
                    }
                    _ => not_found!(),
                }
            }
            parser::LValueType::Function(call) => {
                let args = self.resolve_expressions(&call.arguments, annot);
                (resolved_type, resolved_lvalue) = self.resolve_function_call(None, call, &args);
            }
            parser::LValueType::Method(lvalue, call) => {
                let lval_type = self.resolve_lvalue(lvalue, annot).resolved_type.clone();
                let args = self.resolve_expressions(&call.arguments, annot);

                match lval_type {
                    ResolvedType::Complex(project::Complex::Class(class)) => {
                        (resolved_type, resolved_lvalue) =
                            self.resolve_function_call(Some(class), call, &args);
                    }
                    _ => not_found!(),
                }
            }
            parser::LValueType::Index(array, index) => {
                let array_type = self.resolve_lvalue(array, annot).resolved_type.clone();
                self.resolve_expression(index, annot);

                resolved_lvalue = None;
                resolved_type = match array_type {
                    ResolvedType::Array(sub_type) => *sub_type,
                    _ => ResolvedType::Unknown,
                };
            }
            parser::LValueType::SQLAccess(_, access) => {
                resolved_lvalue = None;
                resolved_type = self.resolve_lvalue(access, annot).resolved_type.clone();
            }
        };

        annot.annotation.insert(Annotation {
            resolved_type,
            lvalue: resolved_lvalue,
        })
    }

    fn resolve_expression(
        &'a self,
        expression: &'proj parser::Expression,
        annot: &'a mut AnnotationTree<'proj>,
    ) -> &'a Annotation<'proj> {
        if let parser::ExpressionType::LValue(lvalue) = &expression.expression_type {
            return &self.resolve_lvalue(lvalue, annot);
        }

        let annot = annot.add_empty_child(&expression.range);

        // TODO: create some reusable AST walker, its probably time...
        let resolved_type = match &expression.expression_type {
            parser::ExpressionType::Create(data_type) => self
                .resolve_datatype(data_type, annot)
                .resolved_type
                .clone(),
            parser::ExpressionType::CreateUsing(class) => {
                self.resolve_expression(class, annot);
                // TODO(create using): do something with the class?
                ResolvedType::Unknown
            }
            parser::ExpressionType::Literal(literal) => match literal.literal_type {
                tokenizer::Literal::NUMBER => {
                    if literal.content.contains('.') {
                        ResolvedType::Base(builder::BaseType::Double)
                    } else {
                        ResolvedType::Base(builder::BaseType::Int)
                    }
                }
                tokenizer::Literal::DATE => ResolvedType::Base(builder::BaseType::Date),
                tokenizer::Literal::TIME => ResolvedType::Base(builder::BaseType::Time),
                tokenizer::Literal::STRING => ResolvedType::Base(builder::BaseType::String),
                tokenizer::Literal::BOOLEAN => ResolvedType::Base(builder::BaseType::Boolean),
                tokenizer::Literal::ENUM => {
                    if let Some(name) = self
                        .ctx
                        .proj
                        .builtins
                        .enums_value_cache
                        .get(&(&literal.content).into())
                    {
                        ResolvedType::Complex(project::Complex::Enum(
                            self.ctx.proj.builtins.enums.get(name).unwrap(),
                        ))
                    } else {
                        ResolvedType::Unknown
                    }
                }
            },
            parser::ExpressionType::ArrayLiteral(expressions) => {
                for expression in expressions {
                    self.resolve_expression(expression, annot);
                }

                let types: Vec<_> = annot
                    .children
                    .iter()
                    .map(|child| &child.annotation.as_ref().unwrap().resolved_type)
                    .collect();

                let mut inner = None;
                match types.iter().find(|t| !matches!(t, ResolvedType::Unknown)) {
                    Some(&child_type) => {
                        inner = Some(child_type);
                        for other_type in types {
                            if !self.ctx.proj.is_convertible(child_type, other_type) {
                                inner = None;
                                break;
                            }
                        }
                    }
                    None => {}
                };
                ResolvedType::Array(Box::new(inner.cloned().unwrap_or(ResolvedType::Unknown)))
            }
            parser::ExpressionType::Operation(left, op, right) => {
                let annot = annot.add_empty_child(&expression.range);

                self.resolve_expression(left, annot);
                self.resolve_expression(right, annot);

                let left_type = &annot.children[0].annotation.as_ref().unwrap().resolved_type;
                let right_type = &annot.children[1].annotation.as_ref().unwrap().resolved_type;

                match op {
                    tokenizer::Operator::AND
                    | tokenizer::Operator::OR
                    | tokenizer::Operator::EQ
                    | tokenizer::Operator::GTLT
                    | tokenizer::Operator::GT
                    | tokenizer::Operator::GTE
                    | tokenizer::Operator::LT
                    | tokenizer::Operator::LTE => ResolvedType::Base(builder::BaseType::Boolean),

                    tokenizer::Operator::PLUS
                        if matches!(
                            left_type,
                            ResolvedType::Base(builder::BaseType::String | builder::BaseType::Char)
                        ) && matches!(
                            right_type,
                            ResolvedType::Base(builder::BaseType::String | builder::BaseType::Char)
                        ) =>
                    {
                        ResolvedType::Base(builder::BaseType::String)
                    }
                    _ => {
                        match (
                            left_type.numeric_precedence(),
                            right_type.numeric_precedence(),
                        ) {
                            (Some(left), Some(right)) => {
                                if left > right {
                                    left_type.clone()
                                } else {
                                    right_type.clone()
                                }
                            }
                            (..) => ResolvedType::Unknown,
                        }
                    }
                }
            }
            parser::ExpressionType::UnaryOperation(_op, expression) => {
                let data_type = &self.resolve_expression(expression, annot).resolved_type;

                if data_type.is_numeric() {
                    data_type.clone()
                } else {
                    ResolvedType::Base(builder::BaseType::Int)
                }
            }
            parser::ExpressionType::IncrementDecrement(expression, _op) => {
                let data_type = &self.resolve_expression(expression, annot).resolved_type;

                if data_type.is_numeric() {
                    data_type.clone()
                } else {
                    ResolvedType::Base(builder::BaseType::Int)
                }
            }
            parser::ExpressionType::BooleanNot(expression) => {
                self.resolve_expression(expression, annot);
                ResolvedType::Base(builder::BaseType::Boolean)
            }
            parser::ExpressionType::Parenthesized(expression) => self
                .resolve_expression(expression, annot)
                .resolved_type
                .clone(),
            parser::ExpressionType::LValue(_) => unreachable!(),
            parser::ExpressionType::Error => ResolvedType::Unknown,
        };

        annot.annotation.insert(Annotation {
            resolved_type,
            lvalue: None,
        })
    }

    fn resolve_expressions(
        &'a self,
        expression: impl IntoIterator<Item = &'proj parser::Expression>,
        annot: &'a mut AnnotationTree<'proj>,
    ) -> Vec<&'a ResolvedType<'proj>> {
        let prev = annot.children.len();

        for expr in expression {
            self.resolve_expression(expr, annot);
        }

        annot
            .children
            .iter()
            .skip(prev)
            .map(|child| &child.annotation.as_ref().unwrap().resolved_type)
            .collect()
    }

    fn resolve_statement(
        &'a self,
        statement: &'proj parser::Statement,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        match &statement.statement_type {
            parser::StatementType::Expression(expression) => {
                self.resolve_expression(expression, annot);
            }
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                elseif_statements,
                else_statements,
            }) => {
                self.resolve_expression(condition, annot);
                self.resolve_statements(statements, annot);

                for (condition, statements) in elseif_statements {
                    self.resolve_expression(condition, annot);
                    self.resolve_statements(statements, annot);
                }

                self.resolve_statements(else_statements, annot);
            }
            parser::StatementType::Throw(exception) => {
                self.resolve_expression(exception, annot);
            }
            parser::StatementType::Destroy(object) => {
                self.resolve_expression(object, annot);
            }
            parser::StatementType::Declaration(vars) => {
                for var in vars {
                    self.resolve_datatype(&var.variable.data_type, annot);

                    if let Some(initial_value) = &var.variable.initial_value {
                        self.resolve_expression(initial_value, annot);
                    }
                }
            }
            parser::StatementType::Assignment(lval, _op, expression) => {
                self.resolve_lvalue(lval, annot);
                self.resolve_expression(expression, annot);
            }
            parser::StatementType::TryCatch(parser::TryCatchStatement {
                statements,
                catches,
                finally,
            }) => {
                self.resolve_statements(statements, annot);
                for (var, statements) in catches {
                    self.resolve_statement(var, annot);
                    self.resolve_statements(statements, annot);
                }
                if let Some(statements) = finally {
                    self.resolve_statements(statements, annot);
                }
            }
            parser::StatementType::ForLoop(parser::ForLoopStatement {
                start,
                stop,
                step,
                variable,
                statements,
            }) => {
                self.resolve_expression(start, annot);
                self.resolve_expression(stop, annot);
                self.resolve_lvalue(variable, annot);

                // TODO: put variable type inside the tree

                if let Some(step) = step {
                    self.resolve_expression(step, annot);
                }

                self.resolve_statements(statements, annot);
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                statements,
                ..
            }) => {
                self.resolve_expression(condition, annot);
                self.resolve_statements(statements, annot);
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                self.resolve_expression(choose, annot);

                for (cases, statements) in cases {
                    for case in cases {
                        for expression in case.get_expressions() {
                            self.resolve_expression(expression, annot);
                        }
                    }

                    self.resolve_statements(statements, annot);
                }
            }
            parser::StatementType::Return(ret) => {
                ret.as_ref().map(|r| self.resolve_expression(r, annot));
            }
            parser::StatementType::Call(parser::CallStatement {
                function,
                call_type: _,
            }) => {
                // TODO(call): ...?
                for e in &function.arguments {
                    self.resolve_expression(e, annot);
                }
            }
            // TODO(cleanup): resolve labels?
            parser::StatementType::Goto(_) => {}
            parser::StatementType::Label(_) => {}
            parser::StatementType::Exit => {}
            parser::StatementType::Continue => {}
            parser::StatementType::SQL(_) => {}
            parser::StatementType::Error => {}
        };
    }

    pub(super) fn resolve_statements(
        &'a self,
        statements: &'proj Vec<parser::Statement>,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        for statement in statements {
            self.resolve_statement(statement, annot);
        }
    }

    pub fn resolve_callable_header(
        &'a self,
        return_type: Option<&'proj parser::DataType>,
        arguments: impl Iterator<Item = &'proj parser::Variable>,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        if let Some(ret) = return_type {
            self.resolve_datatype(&ret, annot);
        }
        for arg in arguments {
            let annotation = self.resolve_datatype(&arg.data_type, annot).clone();
            annot.add_child(&arg.access.name.range, annotation);
        }
    }

    pub fn resolve_variable_declaration(
        &'a self,
        variable: &'proj builder::Variable,
        is_scoped: bool,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        let mut annotation = self
            .resolve_datatype(&variable.parsed().data_type, annot)
            .clone();

        if is_scoped {
            annotation.lvalue = Some(ResolvedLValue::Variable(None, variable));
        } else if let Some(fc) = self.ctx.class {
            annotation.lvalue = Some(ResolvedLValue::Member(fc, variable));
        }

        annot.add_child(&variable.parsed().access.name.range, annotation);
        if let Some(expr) = &variable.parsed().initial_value {
            self.resolve_expression(expr, annot);
        }
    }

    pub fn resolve_function_header(
        &'a self,
        header: &'proj builder::FunctionHeader,
        is_external: bool,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        self.resolve_callable_header(
            header.returns.as_ref().map(|r| &r.parsed),
            header.arguments.iter().map(|arg| arg.parsed()),
            annot,
        );

        // TODO(annot): order is wrong, binary search wont work
        if let Some(fc) = self.ctx.class {
            let funcs = if is_external {
                &fc.class.external_functions
            } else {
                &fc.class.functions
            };
            let func = funcs
                .get(&header.iname())
                .unwrap()
                .get(&header.signature())
                .unwrap();

            annot.add_child(
                &header.parsed.name.range,
                Annotation {
                    resolved_type: ResolvedType::Unknown,
                    lvalue: Some(ResolvedLValue::Method(fc, OneOf::Left(func))),
                },
            );
        }
    }

    pub fn resolve_event_header(
        &'a self,
        header: &'proj builder::EventHeader,
        annot: &'a mut AnnotationTree<'proj>,
    ) {
        if let parser::EventType::User(ret, args) = &header.parsed.event_type {
            self.resolve_callable_header(ret.as_ref(), args.iter().map(|arg| &arg.variable), annot);
        }

        // TODO(annot): order is wrong, binary search wont work
        if let Some(fc) = self.ctx.class {
            let event = fc.class.events.get(&header.iname()).unwrap();

            annot.add_child(
                &header.parsed.name.range,
                Annotation {
                    resolved_type: ResolvedType::Unknown,
                    lvalue: Some(ResolvedLValue::Method(fc, OneOf::Right(event))),
                },
            );
        }
    }

    pub fn resolve_file(proj: &'proj Project, file: &'proj BuiltFile) -> FileAnnotations<'proj> {
        assert!(file.bodies_processed, "resolve_file should only be called on files that have not have had their bodies processed");

        let mut annotator = Resolver {
            ctx: Context {
                proj,
                file,

                class: None,
                arguments: None,
                body: None,
            },
        };
        let mut annotations = Vec::new();

        for top_level in &file.top_levels {
            let mut annot = AnnotationTree {
                range: &top_level.range,
                annotation: None,
                children: Vec::new(),
            };

            match &top_level.top_level_type {
                builder::TopLevelType::ForwardDecl(decls, vars) => {
                    for decl in &decls.classes {
                        annotator.resolve_datatype(&decl.class.name, &mut annot);
                        annotator.resolve_datatype(&decl.class.base, &mut annot);
                        if let Some(within) = &decl.class.within {
                            annotator.resolve_datatype(&within, &mut annot);
                        }
                    }

                    for var in vars {
                        annotator.resolve_variable_declaration(var, true, &mut annot);
                    }

                    // TODO(forward): functions, events, variables
                    // TODO(globals): ...
                }
                builder::TopLevelType::DatatypeDecl(decl) => {
                    let fc = project::ClassRef::new(file, &decl.class);
                    annotator.ctx.class = Some(fc);

                    annot.add_child(
                        &decl.class.parsed.class.name.range,
                        Annotation {
                            resolved_type: ResolvedType::Complex(project::Complex::Class(fc)),
                            lvalue: None,
                        },
                    );

                    annotator.resolve_datatype(&decl.class.parsed.class.base, &mut annot);

                    if let Some(within) = &decl.class.parsed.class.within {
                        annotator.resolve_datatype(&within, &mut annot);
                    }

                    for event in &decl.events {
                        annotator.resolve_event_header(&event.header, &mut annot);
                    }
                    for func in &decl.functions {
                        annotator.resolve_function_header(&func.header, false, &mut annot);
                    }
                    for var in &decl.variables {
                        annotator.resolve_variable_declaration(var, false, &mut annot);
                    }
                }

                builder::TopLevelType::FunctionsForwardDecl(funcs) => {
                    for func in funcs {
                        annotator.resolve_function_header(&func.header, false, &mut annot);
                    }
                }
                builder::TopLevelType::ExternalFunctions(funcs) => {
                    for func in funcs {
                        annotator.resolve_function_header(&func.header, true, &mut annot);
                    }
                }
                builder::TopLevelType::ScopedVariableDecl(vars)
                | builder::TopLevelType::ScopedVariablesDecl(vars) => {
                    for var in vars {
                        annotator.resolve_variable_declaration(var, true, &mut annot);
                    }
                }
                builder::TopLevelType::TypeVariablesDecl(vars) => {
                    for var in vars {
                        annotator.resolve_variable_declaration(var, false, &mut annot);
                    }
                }

                builder::TopLevelType::FunctionBody(func) => {
                    annotator.resolve_function_header(&func.header, false, &mut annot);
                    annotator.ctx.load_body(&func.body, &func.header.arguments);
                    annotator.resolve_statements(&func.parsed.statements, &mut annot);
                }
                builder::TopLevelType::EventBody(event) => {
                    annotator.resolve_event_header(&event.header, &mut annot);

                    annotator
                        .ctx
                        .load_body(&event.body, &event.header.arguments);
                    annotator.resolve_statements(&event.parsed.statements, &mut annot);
                }
                builder::TopLevelType::OnBody(on) => {
                    // TODO(on): ...

                    let resolved_type = file
                        .classes
                        .get(&(&on.header.class.name.content).into())
                        .map_or(ResolvedType::Unknown, |class| {
                            ResolvedType::Complex(project::Complex::Class(project::ClassRef::new(
                                file, class,
                            )))
                        });
                    annot.add_child(
                        &on.header.class.range,
                        Annotation {
                            resolved_type,
                            lvalue: None,
                        },
                    );

                    annotator.resolve_statements(&on.statements, &mut annot);
                }
            }

            annotations.push(annot);
        }

        FileAnnotations {
            top_levels: annotations,
        }
    }
}
