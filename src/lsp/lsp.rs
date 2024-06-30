use std::{
    cell::RefCell,
    env::current_dir,
    path::{Path, PathBuf},
    rc::{Rc, Weak},
};

use crate::parser::{
    parse_file,
    parser::*,
    parser_types as parser,
    tokenizer_types::{Operator, Range, Type},
};

struct Usage {
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

struct Class {
    events: Vec<(Usage, Rc<parser::Function>)>,
    type_variables: Vec<(Usage, Rc<parser::Variable>)>,
    instance_variables: Vec<(Usage, Rc<parser::Variable>)>,
    shared_variables: Vec<(Usage, Rc<parser::Variable>)>,
    functions: Vec<(Usage, Rc<parser::Function>)>,
}

impl Class {
    fn find_function(
        &mut self,
        function: &parser::Function,
    ) -> Option<&mut (Usage, Rc<parser::Function>)> {
        self.functions.iter_mut().find(|(usage, func)| {
            func.name == function.name
                && function.returns == func.returns
                && func
                    .arguments
                    .iter()
                    .zip(function.arguments.iter())
                    .all(|((existing_type, _), (function_type, _))| function_type == existing_type)
        })
    }

    fn find_callable_function(
        &mut self,
        function: &parser::Function,
    ) -> Option<&mut (Usage, Rc<parser::Function>)> {
        self.functions.iter_mut().find(|(usage, func)| {
            func.name == function.name
                && func
                    .returns
                    .data_type
                    .is_convertible(&function.returns.data_type)
                && func.arguments.iter().zip(function.arguments.iter()).all(
                    |((existing_type, _), (function_type, _))| {
                        function_type
                            .data_type
                            .is_convertible(&existing_type.data_type)
                    },
                )
        })
    }
}

struct File {
    lsp: Weak<RefCell<LSPData>>,

    path: PathBuf,
    top_levels: Vec<parser::TopLevel>,
    diagnostics: Vec<parser::Diagnostic>,
    class: Class,

    shallow: bool,
}

impl File {
    fn new(data: Weak<RefCell<LSPData>>, path: PathBuf, shallow: bool) -> anyhow::Result<File> {
        Ok(File {
            lsp: data,

            top_levels: parse_file(&path)?,
            path,
            class: Class {
                events: Vec::new(),
                type_variables: Vec::new(),
                instance_variables: Vec::new(),
                shared_variables: Vec::new(),
                functions: Vec::new(),
            },
            diagnostics: Vec::new(),
            shallow: false,
        })
    }

    // TODO set the types during linting
    fn lint_lvalue(&mut self, lvalue: &parser::LValue) {
        match &lvalue.lvalue_type {
            parser::LValueType::Super => todo!(),
            parser::LValueType::Variable(var) => todo!("Check if variable exists"),
            parser::LValueType::Function(_, arguments) => {
                arguments
                    .iter()
                    .for_each(|expression| self.lint_expression(expression));
                todo!("Check if function exists");
            }
            parser::LValueType::Method(lvalue, _, arguments) => {
                self.lint_lvalue(lvalue);
                arguments
                    .iter()
                    .for_each(|expression| self.lint_expression(expression));
                todo!("Check if function exists");
            }
            parser::LValueType::Member(_, _) => todo!(),
            parser::LValueType::Index(array, index) => {
                self.lint_lvalue(array);
                self.lint_expression(index);

                if let parser::DataTypeType::Array(_) = array.get_type() {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Subscript Operator can only be applied to Array".to_string(),
                        range: array.range,
                    });
                }
                if index.get_type().is_numeric() {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Index for subscript operator must be numerical".to_string(),
                        range: index.range,
                    });
                }
            }
        }
    }

    fn lint_expression(&mut self, expression: &parser::Expression) {
        match &expression.expression_type {
            parser::ExpressionType::Literal(_) => {}
            parser::ExpressionType::ArrayLiteral(expressions) => {
                expressions
                    .iter()
                    .for_each(|expr| self.lint_expression(expr));

                if expressions.len() > 1 {
                    let data_type = expressions[0].get_type();
                    if expressions
                        .iter()
                        .skip(1)
                        .any(|expression| data_type.is_convertible(&expression.get_type()))
                    {
                        self.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Array Literal contains different types".to_string(),
                            range: expression.range,
                        })
                    }
                }
            }
            parser::ExpressionType::Operation(left, op, right) => {
                self.lint_expression(left);
                self.lint_expression(right);

                match op {
                    Operator::AND | Operator::OR => {
                        if left
                            .get_type()
                            .is_convertible(&parser::DataTypeType::Primitive(Type::BOOLEAN))
                            || right
                                .get_type()
                                .is_convertible(&parser::DataTypeType::Primitive(Type::BOOLEAN))
                        {
                            self.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected booleans"
                                    .to_string(),
                                range: expression.range,
                            })
                        }
                    }
                    Operator::PLUS => {
                        if !((left
                            .get_type()
                            .is_convertible(&parser::DataTypeType::Primitive(Type::STRING))
                            && right
                                .get_type()
                                .is_convertible(&parser::DataTypeType::Primitive(Type::STRING)))
                            || (left.get_type().is_numeric() && right.get_type().is_numeric()))
                        {
                            self.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers or strings"
                                    .to_string(),
                                range: expression.range,
                            })
                        }
                    }
                    _ => {
                        if !left.get_type().is_numeric() || !right.get_type().is_numeric() {
                            self.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers"
                                    .to_string(),
                                range: expression.range,
                            })
                        }
                    }
                };
            }
            parser::ExpressionType::BooleanNot(expression) => {
                self.lint_expression(expression);

                if expression
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(Type::BOOLEAN))
                {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".to_string(),
                        range: expression.range,
                    })
                }
            }
            parser::ExpressionType::Parenthesized(expression) => {
                self.lint_expression(expression);
            }
            parser::ExpressionType::Create(class) => todo!("Check if class exists"),
            parser::ExpressionType::CreateUsing(class) => {
                self.lint_expression(class);

                if expression
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(Type::STRING))
                {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected string".to_string(),
                        range: expression.range,
                    })
                }
            }
            parser::ExpressionType::LValue(lvalue) => {
                self.lint_lvalue(lvalue);
            }
        }
    }

    fn lint_statement(&mut self, statement: &parser::Statement) {
        match &statement.statement_type {
            parser::StatementType::IncrementDecrement(inc_dec) => {
                if !inc_dec.value.get_type().is_numeric() {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Cannot increment on non Numeric DataType".to_string(),
                        range: inc_dec.value.range,
                    });
                }
            }
            parser::StatementType::Expression(expression) => self.lint_expression(expression),
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                else_statements,
            }) => {
                self.lint_expression(condition);
                statements
                    .iter()
                    .for_each(|statement| self.lint_statement(statement));
                else_statements
                    .iter()
                    .for_each(|statement| self.lint_statement(statement));

                if condition
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(Type::BOOLEAN))
                {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for if must be of type Boolean".to_string(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Throw(exception) => self.lint_expression(exception),
            parser::StatementType::Assignment(lvalue, expression) => {
                self.lint_lvalue(lvalue);
                self.lint_expression(expression);

                if !expression.get_type().is_convertible(&lvalue.get_type()) {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Type's are not convertible".to_string(),
                        range: expression.range,
                    });
                }
            }
            parser::StatementType::TryCatch(parser::TryCatchStatement {
                statements,
                catches,
            }) => {
                statements
                    .iter()
                    .for_each(|statement| self.lint_statement(statement));
                catches.iter().for_each(|(_, statements)| {
                    statements
                        .iter()
                        .for_each(|statement| self.lint_statement(statement))
                });
            }
            parser::StatementType::Declaration(_) => todo!(),
            parser::StatementType::ForLoop(parser::ForLoopStatement {
                start,
                stop,
                step,
                variable,
                statements,
            }) => {
                self.lint_expression(start);
                self.lint_expression(stop);
                if let Some(exp) = step {
                    self.lint_expression(exp);
                }
                statements
                    .iter()
                    .for_each(|statement| self.lint_statement(statement));
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                is_do_while,
                statements,
            }) => {
                self.lint_expression(condition);
                statements
                    .iter()
                    .for_each(|statement| self.lint_statement(statement));

                if condition
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(Type::BOOLEAN))
                {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for while loop must be of type Boolean".to_string(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                self.lint_expression(choose);
                cases.iter().for_each(|(lvalue, statements)| {
                    statements
                        .iter()
                        .for_each(|statement| self.lint_statement(statement));

                    if !lvalue.is_type(&choose.get_type()) {
                        self.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Condition for while loop must be of type Boolean".to_string(),
                            range: choose.range,
                        });
                    }
                })
            }
            parser::StatementType::Return(ret) => {
                self.lint_expression(ret);
            }
            parser::StatementType::Exit => todo!(),
            parser::StatementType::Continue => todo!(),
            parser::StatementType::Empty => todo!(),
            parser::StatementType::Error => todo!(),
        }
    }

    fn lint(&mut self) {
        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(_, vars) => {
                    for var in vars {
                        self.class.type_variables.push((
                            Usage {
                                declaration: Some(var.range),
                                definition: None,
                                uses: Vec::new(),
                            },
                            var.clone(),
                        ));
                    }
                }
                parser::TopLevelType::ForwardDecl => todo!(),
                parser::TopLevelType::TypeVariablesDecl(vars) => {
                    for var in vars {
                        self.class.instance_variables.push((
                            Usage {
                                declaration: Some(var.range),
                                definition: None,
                                uses: Vec::new(),
                            },
                            var.clone(),
                        ));
                    }
                }
                parser::TopLevelType::GlobalVariablesDecl(_) => todo!(),
                parser::TopLevelType::VariableDecl(_) => todo!(),
                parser::TopLevelType::ConstantDecl => todo!(),
                parser::TopLevelType::FunctionForwardDecl => todo!(),
                parser::TopLevelType::FunctionsForwardDecl(functions) => {
                    for function in functions {
                        match self.class.find_function(&function) {
                            Some((usage, _)) => {
                                if let Some(declaration) = usage.declaration {
                                    self.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Function already forward declared".to_string(),
                                        range: function.range,
                                    });
                                    self.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Already forward declared here".to_string(),
                                        range: declaration,
                                    });

                                    let definition = usage.definition;
                                    self.class.functions.push((
                                        Usage {
                                            declaration: Some(function.range),
                                            definition,
                                            uses: Vec::new(),
                                        },
                                        Rc::new((**function).clone()),
                                    ))
                                } else {
                                    usage.declaration = Some(function.range);
                                }
                            }
                            None => self.class.functions.push((
                                Usage {
                                    declaration: Some(function.range),
                                    definition: None,
                                    uses: Vec::new(),
                                },
                                function.clone(),
                            )),
                        }
                    }
                }
                parser::TopLevelType::FunctionBody(function, _statements_todo) => {
                    if self.shallow {
                        continue;
                    }

                    match self.class.find_function(&function) {
                        Some((usage, _)) => {
                            if let Some(definition) = usage.definition {
                                self.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Function already defined".to_string(),
                                    range: function.range,
                                });
                                self.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Hint,
                                    message: "Already defined here".to_string(),
                                    range: definition,
                                });

                                let declaration = usage.declaration;
                                self.class.functions.push((
                                    Usage {
                                        declaration,
                                        definition: Some(function.range),
                                        uses: Vec::new(),
                                    },
                                    function.clone(),
                                ))
                            } else {
                                usage.declaration = Some(function.range);
                            }
                        }
                        None => self.class.functions.push((
                            Usage {
                                declaration: None,
                                definition: Some(function.range),
                                uses: Vec::new(),
                            },
                            function.clone(),
                        )),
                    }
                }
                parser::TopLevelType::OnBody(_, _) => {
                    if self.shallow {
                        continue;
                    }
                }
                parser::TopLevelType::EventBody(_, _) => {
                    if self.shallow {
                        continue;
                    }
                }
            }
        }

        for (usage, _) in &self.class.functions {
            if usage.declaration.is_none() {
                if let Some(definition) = usage.definition {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Function is missing a Forward Declaration".to_string(),
                        range: definition,
                    })
                } else {
                    todo!();
                }
            }

            if usage.definition.is_none() {
                if let Some(declaration) = usage.declaration {
                    self.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Function is missing a Definition".to_string(),
                        range: declaration,
                    })
                } else {
                    todo!();
                }
            }
        }
    }
}

pub struct LSPOptions {
    root: PathBuf,
}

struct LSPData {
    files: Vec<File>,
    options: LSPOptions,
}

pub struct LSP {
    data: Rc<RefCell<LSPData>>,
}

impl LSP {
    pub fn new() -> anyhow::Result<LSP> {
        Ok(LSP {
            data: Rc::new(
                LSPData {
                    files: Vec::new(),
                    options: LSPOptions {
                        root: current_dir()?,
                    },
                }
                .into(),
            ),
        })
    }

    pub fn add_file(&mut self, path: PathBuf, shallow: bool) -> anyhow::Result<()> {
        let mut data = self.data.borrow_mut();

        match data.files.iter_mut().find(|file| file.path == path) {
            Some(file) => {
                if file.shallow {
                    file.shallow = false;
                    file.lint();
                }
            }
            None => {
                let file = File::new(Rc::downgrade(&self.data), path, shallow)?;

                data.files.push(file);
                data.files.last_mut().unwrap().lint();
            }
        }

        Ok(())
    }
}
