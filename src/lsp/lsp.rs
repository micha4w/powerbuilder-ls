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
    parser_types as parser,
    tokenizer_types::{self as tokens, Literal, Range},
};

#[derive(Default)]
struct Usage {
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

fn find_class(group: &Option<String>, class: &String) -> (File, Class) {
    unimplemented!();
}

struct Class {
    name: String,
    base: Weak<RefCell<Class>>,

    within: Option<Weak<RefCell<Class>>>,
    events: Vec<Function>,
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

trait Lintable {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
        return_type: &parser::DataType,
    );
}

trait LintableType {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
    ) -> parser::DataType;
}

impl LintableType for parser::Variable {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
    ) -> parser::DataType {
        match variables
            .iter_mut()
            .find(|(_, var)| var.name == self.name)
            .or_else(|| {
                file.find_variable(self, &tokens::AccessType::PRIVATE)
                    .or_else(|| lsp.find_global_variable(self))
            }) {
            Some((usage, var)) => {
                usage.uses.push(self.range);
                var.data_type.clone()
            }
            None => {
                file.diagnostics.push(parser::Diagnostic {
                    severity: parser::Severity::Error,
                    message: "Variable not found".into(),
                    range: self.range,
                });
                parser::DataType::Unknown
            }
        }
    }
}

impl LintableType for parser::LValue {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
    ) -> parser::DataType {
        match &self.lvalue_type {
            parser::LValueType::Super => todo!(),
            parser::LValueType::Variable(variable) => variable.lint(lsp, file, variables),
            parser::LValueType::Function(function, arguments) => {
                let types = arguments
                    .iter()
                    .map(|expression| (expression.lint(lsp, file, variables), String::new()))
                    .collect::<Vec<_>>();

                match file
                    .find_function(
                        &parser::Function {
                            returns: parser::DataType::Unknown,
                            arguments: types,
                            ..(**function).clone()
                        },
                        &tokens::AccessType::PRIVATE,
                    )
                    .or_else(|| lsp.find_global_function(function))
                {
                    Some(func) => func.parsed.returns.clone(),
                    None => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Function not found".into(),
                            range: self.range,
                        });
                        parser::DataType::Unknown
                    }
                }
            }
            parser::LValueType::Method(lvalue, function, arguments) => {
                let class = lvalue.lint(lsp, file, variables);

                let types = arguments
                    .iter()
                    .map(|expression| (expression.lint(lsp, file, variables), String::new()))
                    .collect::<Vec<_>>();

                match class {
                    parser::DataType::Complex(group, class) => {
                        let (mut class_file, _) = find_class(&group, &class);

                        let func = class_file.data.find_function(
                            &parser::Function {
                                arguments: types,
                                ..(**function).clone()
                            },
                            &tokens::AccessType::PUBLIC,
                        );

                        match func {
                            Some(func) => {
                                func.uses.push(function.range);
                                func.parsed.returns.clone()
                            }
                            None => {
                                file.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Method not found".into(),
                                    range: self.range,
                                });
                                parser::DataType::Unknown
                            }
                        }
                    }
                    _ => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Cannot call a method of a non Complex type".into(),
                            range: self.range,
                        });
                        parser::DataType::Unknown
                    }
                }
            }
            parser::LValueType::Member(_, _) => todo!(),
            parser::LValueType::Index(array, index) => {
                let array_type = array.lint(lsp, file, variables);
                let index_type = index.lint(lsp, file, variables);

                if !index_type.is_numeric() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Index for subscript operator must be numerical".into(),
                        range: index.range,
                    });
                }

                match array_type {
                    parser::DataType::Array(sub_type) => *sub_type,
                    parser::DataType::Unknown => parser::DataType::Unknown,
                    _ => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Subscript Operator can only be applied to Array".into(),
                            range: array.range,
                        });
                        parser::DataType::Unknown
                    }
                }
            }
        }
    }
}

impl LintableType for parser::Expression {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
    ) -> parser::DataType {
        match &self.expression_type {
            parser::ExpressionType::Literal(literal) => literal.get_type(),
            parser::ExpressionType::ArrayLiteral(expressions) => {
                let types = expressions
                    .iter()
                    .map(|expr| expr.lint(lsp, file, variables))
                    .collect::<Vec<_>>();

                parser::DataType::Array(Box::new(match types.first() {
                    Some(data_type) => {
                        if types
                            .iter()
                            .skip(1)
                            .any(|expression_type| !data_type.is_convertible(expression_type))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Array Literal contains different types".into(),
                                range: self.range,
                            })
                        }
                        data_type.clone()
                    }
                    None => parser::DataType::Unknown,
                }))
            }
            parser::ExpressionType::Operation(left, op, right) => {
                let left_type = left.lint(lsp, file, variables);
                let right_type = right.lint(lsp, file, variables);

                match op {
                    tokens::Operator::AND | tokens::Operator::OR => {
                        if left_type
                            .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                            || right_type
                                .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected booleans".into(),
                                range: self.range,
                            });
                        }

                        parser::DataType::Primitive(tokens::Type::BOOLEAN)
                    }
                    tokens::Operator::PLUS => {
                        if !((left_type
                            .is_convertible(&parser::DataType::Primitive(tokens::Type::STRING))
                            && right_type.is_convertible(&parser::DataType::Primitive(
                                tokens::Type::STRING,
                            )))
                            || (left_type.is_numeric() && right_type.is_numeric()))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers or strings"
                                    .into(),
                                range: self.range,
                            });
                        }

                        parser::DataType::Primitive(tokens::Type::BOOLEAN)
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
                                file.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Invalid types for operation, expected numbers".into(),
                                    range: self.range,
                                });
                                parser::DataType::Unknown
                            }
                        }
                    }
                }
            }
            parser::ExpressionType::BooleanNot(expression) => {
                let expression_type = expression.lint(lsp, file, variables);

                if !expression_type
                    .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".into(),
                        range: expression.range,
                    });
                }

                parser::DataType::Primitive(tokens::Type::BOOLEAN)
            }
            parser::ExpressionType::Parenthesized(expression) => {
                expression.lint(lsp, file, variables)
            }
            parser::ExpressionType::Create(class) => todo!("Check if class exists"),
            parser::ExpressionType::CreateUsing(class) => {
                let class_type = class.lint(lsp, file, variables);

                if !class_type.is_convertible(&parser::DataType::Primitive(tokens::Type::STRING)) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected string".into(),
                        range: self.range,
                    });
                }

                parser::DataType::Complex(None, "powerobject".into())
            }
            parser::ExpressionType::LValue(lvalue) => lvalue.lint(lsp, file, variables),
            parser::ExpressionType::IncrementDecrement(lvalue, _) => {
                let lvalue_type = lvalue.lint(lsp, file, variables);

                if !lvalue_type.is_numeric() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Cannot increment on non Numeric DataType".into(),
                        range: lvalue.range,
                    });
                }

                lvalue_type
            }
        }
    }
}

impl Lintable for parser::Statement {
    fn lint(
        &self,
        lsp: &mut LSP,
        file: &mut FileData,
        variables: &mut Vec<(Usage, Rc<parser::Variable>)>,
        return_type: &parser::DataType,
    ) {
        match &self.statement_type {
            parser::StatementType::Expression(expression) => {
                expression.lint(lsp, file, variables);
            }
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                elseif_statements,
                else_statements,
            }) => {
                let condition_type = condition.lint(lsp, file, variables);

                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file, variables, return_type));
                elseif_statements
                    .iter()
                    .for_each(|(condition, statements)| {
                        let condition_type = condition.lint(lsp, file, variables);

                        if !condition_type
                            .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Condition for if must be of type Boolean".into(),
                                range: condition.range,
                            });
                        }

                        statements
                            .iter()
                            .for_each(|statement| statement.lint(lsp, file, variables, return_type))
                    });

                else_statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file, variables, return_type));

                if !condition_type
                    .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for if must be of type Boolean".into(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Throw(exception) => {
                exception.lint(lsp, file, variables);
            }
            parser::StatementType::Declaration(variable) => {
                variables.push((
                    Usage {
                        declaration: Some(variable.range),
                        definition: None,
                        uses: Vec::new(),
                    },
                    variable.clone(),
                ));

                if let Some(initial_value) = &variable.initial_value {
                    match initial_value.expression_type {
                        parser::ExpressionType::Literal(literal) => {
                            if !literal.is_type(&variable.data_type) {
                                file.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Type's are not convertible".into(),
                                    range: initial_value.range,
                                });
                            }
                        }
                        parser::ExpressionType::ArrayLiteral(_) => todo!(),
                        _ => {}
                    }
                }
            }
            parser::StatementType::Assignment(lvalue, expression) => {
                let lvalue_type = lvalue.lint(lsp, file, variables);
                let expression_type = expression.lint(lsp, file, variables);

                if !expression_type.is_convertible(&lvalue_type) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Type's are not convertible".into(),
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
                    .for_each(|statement| statement.lint(lsp, file, variables, return_type));
                catches.iter().for_each(|(_, statements)| {
                    statements
                        .iter()
                        .for_each(|statement| statement.lint(lsp, file, variables, return_type));
                });
            }
            parser::StatementType::ForLoop(parser::ForLoopStatement {
                start,
                stop,
                step,
                variable,
                statements,
            }) => {
                let variable_type = variable.lint(lsp, file, variables);
                let start_type = start.lint(lsp, file, variables);
                let stop_type = stop.lint(lsp, file, variables);

                let mut to_check = vec![
                    (variable_type, &variable.range),
                    (start_type, &start.range),
                    (stop_type, &stop.range),
                ];

                if let Some(step) = step {
                    let step_type = step.lint(lsp, file, variables);
                    to_check.push((step_type, &step.range));
                }

                for (data_type, range) in to_check {
                    if !data_type.is_numeric() {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Needs to be a Numeric Type".into(),
                            range: *range,
                        });
                    }
                }

                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file, variables, return_type));
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                statements,
                ..
            }) => {
                let condition_type = condition.lint(lsp, file, variables);

                statements
                    .iter()
                    .for_each(|statement| statement.lint(lsp, file, variables, return_type));

                if !condition_type
                    .is_convertible(&parser::DataType::Primitive(tokens::Type::BOOLEAN))
                {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for while loop must be of type Boolean".into(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                let choose_type = choose.lint(lsp, file, variables);
                cases.iter().for_each(|(cases, statements)| {
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
                                    file.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "CASE ELSE must be alone".into(),
                                        range: case.range,
                                    });
                                }
                            }
                        }
                    }

                    for (literal, range) in literals {
                        if !literal.is_type(&choose_type) {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Wrong Literal Type".into(),
                                range: range.clone(),
                            });
                        }
                    }

                    statements
                        .iter()
                        .for_each(|statement| statement.lint(lsp, file, variables, return_type));
                })
            }
            parser::StatementType::Return(ret) => {
                let ret_type = ret.lint(lsp, file, variables);

                if !ret_type.is_convertible(return_type) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Wrong return Type".into(),
                        range: ret.range,
                    });
                }
            }
            parser::StatementType::Exit => todo!(), // TODO stack?
            parser::StatementType::Continue => todo!(),
            parser::StatementType::Error => todo!(),
            parser::StatementType::Empty => {}
        }
    }
}

struct Function {
    parsed: Rc<parser::Function>,

    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

struct FileData {
    classes: Vec<Class>,
    // Per instance
    group_variables: Vec<(Usage, Rc<parser::Variable>)>, // TODO why Rc, just copy to new type?
    // Shared with all isntances
    shared_variables: Vec<(Usage, Rc<parser::Variable>)>,

    functions: Vec<Function>,
    external_functions: Vec<Function>,

    diagnostics: Vec<parser::Diagnostic>,
}

impl FileData {
    fn find_variable(
        &mut self,
        variable: &parser::Variable,
        min_access: &tokens::AccessType,
    ) -> Option<&mut (Usage, Rc<parser::Variable>)> {
        self.group_variables
            .iter_mut()
            .find(|(_, var)| {
                var.name == variable.name
                    && min_access.strictness()
                        >= var
                            .access
                            .read
                            .map(|access| access.strictness())
                            .unwrap_or(0)
            })
            .or_else(|| {
                self.shared_variables.iter_mut().find(|(_, var)| {
                    var.name == variable.name
                        && min_access.strictness()
                            >= var
                                .access
                                .read
                                .map(|access| access.strictness())
                                .unwrap_or(0)
                })
            })
    }

    fn find_exact_function(&mut self, function: &parser::Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.parsed.equals(function))
    }

    fn find_conflicting_function(&mut self, function: &parser::Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.parsed.conflicts(function))
    }

    fn find_function(
        &mut self,
        function: &parser::Function,
        min_access: &tokens::AccessType,
    ) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.parsed.is_callable(function, min_access))
    }
}

struct File {
    data: FileData,

    top_levels: Vec<parser::TopLevel>,
    path: PathBuf,
    shallow: bool,
}

impl File {
    fn new(path: PathBuf, shallow: bool) -> anyhow::Result<File> {
        Ok(File {
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

    fn lint_rest(&mut self, lsp: &mut LSP) {
        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(..) => {}
                parser::TopLevelType::ForwardDecl => {}
                parser::TopLevelType::TypeVariablesDecl(..) => {}
                parser::TopLevelType::GlobalVariablesDecl(..) => {}
                parser::TopLevelType::VariableDecl(..) => {}
                parser::TopLevelType::ConstantDecl => {}
                parser::TopLevelType::FunctionForwardDecl => {}
                parser::TopLevelType::FunctionsForwardDecl(..) => {}
                parser::TopLevelType::ExternalFunctions(..) => {}
                parser::TopLevelType::FunctionBody(function, statements) => {
                    let mut variables = Vec::new();

                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, &mut self.data, &mut variables, &function.returns)
                    });

                    match self.data.find_conflicting_function(&function) {
                        Some(func) => {
                            if let Some(definition) = func.definition {
                                let declaration = func.declaration;
                                self.data.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Function already defined".into(),
                                    range: function.range,
                                });
                                self.data.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Hint,
                                    message: "Already defined here".into(),
                                    range: definition,
                                });

                                self.data.functions.push(Function {
                                    parsed: function.clone(),

                                    declaration,
                                    definition: Some(function.range),
                                    uses: Vec::new(),
                                });
                            } else {
                                func.definition = Some(function.range);
                            }
                        }
                        None => {
                            self.data.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Function is missing a Forward Declaration".into(),
                                range: function.range,
                            });

                            self.data.functions.push(Function {
                                parsed: function.clone(),

                                declaration: None,
                                definition: Some(function.range),
                                uses: Vec::new(),
                            })
                        }
                    }
                }
                parser::TopLevelType::OnBody(_, statements) => {
                    let mut variables = Vec::new();
                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, &mut self.data, &mut variables, &parser::DataType::Void)
                    });
                }
                parser::TopLevelType::EventBody(function, statements) => {
                    let mut variables = Vec::new();
                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, &mut self.data, &mut variables, &function.returns)
                    });
                }
            }
        }

        for func in &self.data.functions {
            if func.declaration.is_none() {
                if let Some(definition) = func.definition {
                    self.data.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Function is missing a Forward Declaration".into(),
                        range: definition,
                    })
                } else {
                    todo!();
                }
            }

            if func.definition.is_none() {
                if let Some(declaration) = func.declaration {
                    self.data.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Function is missing a Definition".into(),
                        range: declaration,
                    })
                } else {
                    todo!();
                }
            }
        }
    }

    fn lint_shallow(&mut self, lsp: &mut LSP) {
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
                parser::TopLevelType::ForwardDecl => { /* TODO */ }
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
                parser::TopLevelType::GlobalVariablesDecl(_) => { /* TODO */ }
                parser::TopLevelType::VariableDecl(_) => { /* TODO */ }
                parser::TopLevelType::ConstantDecl => { /* TODO */ }
                parser::TopLevelType::FunctionForwardDecl => { /* TODO */ }
                parser::TopLevelType::FunctionsForwardDecl(functions) => {
                    for function in functions {
                        match self.data.find_conflicting_function(function) {
                            Some(func) => {
                                if let Some(declaration) = func.declaration {
                                    let definition = func.definition;
                                    self.data.functions.push(Function {
                                        parsed: function.clone(),
                                        declaration: Some(function.range),
                                        definition,
                                        uses: Vec::new(),
                                    });

                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Function already forward declared".into(),
                                        range: function.range,
                                    });
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Already forward declared here".into(),
                                        range: declaration,
                                    });
                                } else {
                                    func.declaration = Some(function.range);
                                }
                            }
                            None => self.data.functions.push(Function {
                                parsed: function.clone(),

                                declaration: Some(function.range),
                                definition: None,
                                uses: Vec::new(),
                            }),
                        }
                    }
                }
                parser::TopLevelType::FunctionBody(..) => {}
                parser::TopLevelType::OnBody(..) => {}
                parser::TopLevelType::EventBody(..) => {}
                parser::TopLevelType::ExternalFunctions(functions) => {
                    for function in functions {
                        match self.data.find_conflicting_function(function) {
                            Some(func) => {
                                if let Some(declaration) = func.declaration {
                                    let definition = func.definition;
                                    self.data.external_functions.push(Function {
                                        parsed: function.clone(),
                                        declaration: Some(function.range),
                                        definition,
                                        uses: Vec::new(),
                                    });

                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Function already forward declared".into(),
                                        range: function.range,
                                    });
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Already forward declared here".into(),
                                        range: declaration,
                                    });
                                } else {
                                    func.declaration = Some(function.range);
                                }
                            }
                            None => self.data.external_functions.push(Function {
                                parsed: function.clone(),

                                declaration: Some(function.range),
                                definition: None,
                                uses: Vec::new(),
                            }),
                        }
                    }
                }
            }
        }
    }
}

pub struct LSPOptions {
    root: PathBuf,
}

pub struct LSP {
    files: Vec<File>,
    global_variables: Vec<(Usage, Rc<parser::Variable>)>,
    global_functions: Vec<Function>,

    options: LSPOptions,
}

impl LSP {
    fn find_global_variable(
        &mut self,
        variable: &parser::Variable,
    ) -> Option<&mut (Usage, Rc<parser::Variable>)> {
        self.global_variables
            .iter_mut()
            .find(|(_, var)| var.name == variable.name)
    }
    fn find_global_function(&mut self, function: &Rc<parser::Function>) -> Option<&mut Function> {
        // No need to look for instance functions
        self.global_functions.iter_mut().find(|global| {
            global
                .parsed
                .is_callable(function, &tokens::AccessType::PUBLIC)
        })
    }

    pub fn new() -> anyhow::Result<LSP> {
        Ok(LSP {
            files: Vec::new(),
            global_variables: Vec::new(),
            global_functions: Vec::new(),

            options: LSPOptions {
                root: current_dir()?,
            },
        })
    }

    pub fn add_file(&mut self, path: PathBuf, shallow: bool) -> anyhow::Result<()> {
        let pos = self.files.iter().position(|file| file.path == path);
        match pos {
            Some(pos) => {
                if !shallow && self.files[pos].shallow {
                    let mut file = self.files.remove(pos);

                    if file.shallow {
                        file.shallow = false;
                        file.lint_rest(self);
                    }

                    self.files.push(file);
                }
            }
            None => {
                let mut file = File::new(path, true)?;
                file.lint_shallow(self);

                if !shallow {
                    file.shallow = false;
                    file.lint_rest(self);
                }

                self.files.push(file);
            }
        }

        for diagnostic in &self.files.last().unwrap().data.diagnostics {
            println!("{} - {}", diagnostic.range, diagnostic.message)
        }

        Ok(())
    }
}
