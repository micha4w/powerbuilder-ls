use std::{
    borrow::Borrow,
    cell::RefCell,
    env::current_dir,
    path::PathBuf,
    rc::{Rc, Weak},
};

use crate::parser::{
    parse_file,
    parser::*,
    parser_types::{self as parser},
    tokenizer_types::{self as tokens, Range},
};

struct Usage {
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

struct Class {
    name: String,
    base: Weak<RefCell<Class>>,

    within: Option<Weak<RefCell<Class>>>,
    events: Vec<(Usage, Rc<parser::Function>)>,
    instance_variables: Vec<(Usage, Rc<parser::Variable>)>,
}

impl Class {
    fn new(
        name: String,
        base: Weak<RefCell<Class>>,
        within: Option<Weak<RefCell<Class>>>,
    ) -> Class {
        Class {
            name,
            base,
            within,

            events: Vec::new(),
            instance_variables: Vec::new(),
        }
    }
}

pub trait Lintable {
    fn lint(&self, lsp: &mut LSPData, file: &mut FileData);
}

impl Lintable for parser::LValue {
    fn lint(&self, lsp: &mut LSPData, file: &mut FileData) {
        match &self.lvalue_type {
            parser::LValueType::Super => todo!(),
            parser::LValueType::Variable(var) => todo!("Check if variable exists"),
            parser::LValueType::Function(_, arguments) => {
                arguments
                    .iter()
                    .for_each(|expression| expression.lint(lsp, file));
                todo!("Check if function exists");
            }
            parser::LValueType::Method(lvalue, _, arguments) => {
                lvalue.lint(lsp, file);
                arguments
                    .iter()
                    .for_each(|expression| expression.lint(lsp, file));
                todo!("Check if function exists");
            }
            parser::LValueType::Member(_, _) => todo!(),
            parser::LValueType::Index(array, index) => {
                array.lint(lsp, file);
                index.lint(lsp, file);

                if let parser::DataTypeType::Array(_) = array.get_type() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Subscript Operator can only be applied to Array".to_string(),
                        range: array.range,
                    });
                }
                if index.get_type().is_numeric() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Index for subscript operator must be numerical".to_string(),
                        range: index.range,
                    });
                }
            }
        }
    }
}

impl Lintable for parser::Expression {
    // TODO set the types during linting
    fn lint(&self, lsp: &mut LSPData, file: &mut FileData) {
        match &self.expression_type {
            parser::ExpressionType::Literal(_) => {}
            parser::ExpressionType::ArrayLiteral(expressions) => {
                expressions.iter().for_each(|expr| expr.lint(lsp, file));

                if expressions.len() > 1 {
                    let data_type = expressions[0].get_type();
                    if expressions
                        .iter()
                        .skip(1)
                        .any(|expression| data_type.is_convertible(&expression.get_type()))
                    {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Array Literal contains different types".to_string(),
                            range: self.range,
                        })
                    }
                }
            }
            parser::ExpressionType::Operation(left, op, right) => {
                left.lint(lsp, file);
                right.lint(lsp, file);

                match op {
                    tokens::Operator::AND | tokens::Operator::OR => {
                        if left
                            .get_type()
                            .is_convertible(&parser::DataTypeType::Primitive(tokens::Type::BOOLEAN))
                            || right
                                .get_type()
                                .is_convertible(&parser::DataTypeType::Primitive(
                                    tokens::Type::BOOLEAN,
                                ))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected booleans"
                                    .to_string(),
                                range: self.range,
                            })
                        }
                    }
                    tokens::Operator::PLUS => {
                        if !((left
                            .get_type()
                            .is_convertible(&parser::DataTypeType::Primitive(
                                tokens::Type::STRING,
                            ))
                            && right
                                .get_type()
                                .is_convertible(&parser::DataTypeType::Primitive(
                                    tokens::Type::STRING,
                                )))
                            || (left.get_type().is_numeric() && right.get_type().is_numeric()))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers or strings"
                                    .to_string(),
                                range: self.range,
                            })
                        }
                    }
                    _ => {
                        if !left.get_type().is_numeric() || !right.get_type().is_numeric() {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers"
                                    .to_string(),
                                range: self.range,
                            })
                        }
                    }
                };
            }
            parser::ExpressionType::BooleanNot(expression) => {
                expression.lint(lsp, file);

                if expression
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".to_string(),
                        range: expression.range,
                    })
                }
            }
            parser::ExpressionType::Parenthesized(expression) => {
                expression.lint(lsp, file);
            }
            parser::ExpressionType::Create(class) => todo!("Check if class exists"),
            parser::ExpressionType::CreateUsing(class) => {
                class.lint(lsp, file);

                if self
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(tokens::Type::STRING))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected string".to_string(),
                        range: self.range,
                    })
                }
            }
            parser::ExpressionType::LValue(lvalue) => {
                lvalue.lint(lsp, file);
            }
        }
    }
}

impl Lintable for parser::Statement {
    fn lint(&self, lsp: &mut LSPData, file: &mut FileData) {
        match &self.statement_type {
            parser::StatementType::IncrementDecrement(inc_dec) => {
                if !inc_dec.value.get_type().is_numeric() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Cannot increment on non Numeric DataType".to_string(),
                        range: inc_dec.value.range,
                    });
                }
            }
            parser::StatementType::Expression(expression) => expression.lint(lsp, file),
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                else_statements,
            }) => {
                condition.lint(lsp, file);

                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file));
                else_statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file));

                if condition
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for if must be of type Boolean".to_string(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Throw(exception) => exception.lint(lsp, file),
            parser::StatementType::Assignment(lvalue, expression) => {
                lvalue.lint(lsp, file);
                expression.lint(lsp, file);

                if !expression.get_type().is_convertible(&lvalue.get_type()) {
                    file.diagnostics.push(parser::Diagnostic {
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
                    .for_each(|statement| statement.lint(lsp, file));
                catches.iter().for_each(|(_, statements)| {
                    statements
                        .iter()
                        .for_each(|statement| statement.lint(lsp, file));
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
                start.lint(lsp, file);
                stop.lint(lsp, file);

                step.as_ref().map(|exp| exp.lint(lsp, file));
                // TODO check start, stop and step type

                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file));
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                is_do_while,
                statements,
            }) => {
                condition.lint(lsp, file);
                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file));

                if condition
                    .get_type()
                    .is_convertible(&parser::DataTypeType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for while loop must be of type Boolean".to_string(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                choose.lint(lsp, file);
                cases.iter().for_each(|(lvalue, statements)| {
                    statements
                        .iter()
                        .for_each(|statement| statement.lint(lsp, file));

                    if !lvalue.is_type(&choose.get_type()) {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Condition for while loop must be of type Boolean".to_string(),
                            range: choose.range,
                        });
                    }
                })
            }
            parser::StatementType::Return(ret) => {
                ret.lint(lsp, file);
            }
            parser::StatementType::Exit => todo!(), // TODO stack?
            parser::StatementType::Continue => todo!(),
            parser::StatementType::Error => todo!(),
            parser::StatementType::Empty => {}
        }
    }
}

struct FileData {
    classes: Vec<Class>,
    // Per instance
    group_variables: Vec<(Usage, Rc<parser::Variable>)>, // TODO why Rc, just copy to new type?
    // Shared with all isntances
    shared_variables: Vec<(Usage, Rc<parser::Variable>)>,

    functions: Vec<(Usage, Rc<parser::Function>)>,
    external_functions: Vec<(Usage, Rc<parser::Function>)>,

    diagnostics: Vec<parser::Diagnostic>,
}

impl FileData {
    fn find_function_exact(
        &mut self,
        function: &parser::Function,
        check_return: bool,
    ) -> Option<&mut (Usage, Rc<parser::Function>)> {
        self.functions.iter_mut().find(|(usage, func)| {
            func.name == function.name
                && (!check_return || function.returns == func.returns)
                && func
                    .arguments
                    .iter()
                    .zip(function.arguments.iter())
                    .all(|((existing_type, _), (function_type, _))| function_type == existing_type)
        })
    }

    fn find_function(
        &mut self,
        function: &parser::Function,
        min_access: &tokens::AccessType,
    ) -> Option<&mut (Usage, Rc<parser::Function>)> {
        self.functions
            .iter_mut()
            .find(|(usage, func)| func.is_callable(function, min_access))
    }
}

struct File {
    lsp: Weak<RefCell<LSPData>>,
    data: FileData,

    top_levels: Vec<parser::TopLevel>,
    path: PathBuf,
    shallow: bool,
}

impl File {
    fn new(data: Weak<RefCell<LSPData>>, path: PathBuf, shallow: bool) -> anyhow::Result<File> {
        Ok(File {
            lsp: data,
            data: FileData {
                classes: Vec::new(),

                group_variables: Vec::new(),
                shared_variables: Vec::new(),

                functions: Vec::new(),
                external_functions: Vec::new(),

                diagnostics: Vec::new(),
            },

            top_levels: parse_file(&path)?,
            path,
            shallow: false,
        })
    }

    fn lint(&mut self) {
        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(scope, vars) => {
                    for var in vars {
                        self.data.group_variables.push((
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
                        self.data.group_variables.push((
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
                        match self
                            .lsp
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .find_function(function)
                        {
                            Some((usage, _)) => {
                                if let Some(declaration) = usage.declaration {
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Function already forward declared".to_string(),
                                        range: function.range,
                                    });
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Already forward declared here".to_string(),
                                        range: declaration,
                                    });

                                    let definition = usage.definition;
                                    self.data.functions.push((
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
                            None => self.data.functions.push((
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
                parser::TopLevelType::FunctionBody(function, statements) => {
                    if self.shallow {
                        continue;
                    }

                    // let mut errors = Vec::new();
                    // match self.find_function_exact(&function, false) {
                    //     Some((usage, _)) => {
                    //         if let Some(definition) = usage.definition {
                    //             errors.push(parser::Diagnostic {
                    //                 severity: parser::Severity::Error,
                    //                 message: "Function already defined".to_string(),
                    //                 range: function.range,
                    //             });
                    //             errors.push(parser::Diagnostic {
                    //                 severity: parser::Severity::Hint,
                    //                 message: "Already defined here".to_string(),
                    //                 range: definition,
                    //             });

                    //             let declaration = usage.declaration;
                    //             self.functions.push((
                    //                 Usage {
                    //                     declaration,
                    //                     definition: Some(function.range),
                    //                     uses: Vec::new(),
                    //                 },
                    //                 function.clone(),
                    //             ))
                    //         } else {
                    //             usage.declaration = Some(function.range);
                    //         }
                    //     }
                    //     None => self.functions.push((
                    //         Usage {
                    //             declaration: None,
                    //             definition: Some(function.range),
                    //             uses: Vec::new(),
                    //         },
                    //         function.clone(),
                    //     )),
                    // }

                    statements.iter().for_each(|statement| {
                        statement.lint(&mut self.lsp.upgrade().unwrap().borrow_mut(), &mut self.data)
                    });
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

        for (usage, _) in &self.data.functions {
            if usage.declaration.is_none() {
                if let Some(definition) = usage.definition {
                    self.data.diagnostics.push(parser::Diagnostic {
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
                    self.data.diagnostics.push(parser::Diagnostic {
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
    global_variables: Vec<(Usage, Rc<parser::Variable>)>,
    global_functions: Vec<(Usage, Rc<parser::Function>)>,

    options: LSPOptions,
}

impl LSPData {
    // tODO find global function
    fn find_function(
        &mut self,
        function: &Rc<parser::Function>,
    ) -> Option<&mut (Usage, Rc<parser::Function>)> {
        None
        // self.files.iter_mut().find_map(|file| {
        //     file.classes
        //         .iter_mut()
        //         .find_map(|class| class.find_function(function, &tokens::AccessType::PUBLIC))
        // }).or_else(|| {
        //     self.global_functions.iter_mut().find_map(|global|
        //         global.
        //     )
        // })
    }
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
                    global_variables: Vec::new(),
                    global_functions: Vec::new(),

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
