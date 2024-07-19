use std::{
    cell::RefCell,
    env::{current_dir, current_exe},
    f32::consts::E,
    path::PathBuf,
    ptr::read_unaligned,
    rc::{Rc, Weak},
};

use crate::parser::{
    parse_file,
    parser::*,
    parser_types as parser,
    tokenizer_types::{self as tokens, Range},
};

#[derive(Clone, Debug)]
pub enum DataType {
    Blob,
    Boolean,
    Byte,
    Char,
    Date,
    Datetime,
    Double,
    Int,
    Long,
    Longlong,
    Longptr,
    Real,
    String,
    Time,
    Uint,
    Ulong,
    Decimal(Option<usize>),
    Class(Rc<RefCell<Class>>),
    Array(Box<DataType>),

    Any,
    Unknown,
    Void,
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Class(l0), Self::Class(r0)) => Rc::ptr_eq(l0, r0),
            (Self::Decimal(l0), Self::Decimal(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<&tokens::Literal> for DataType {
    fn from(src: &tokens::Literal) -> DataType {
        match src {
            tokens::Literal::NUMBER => DataType::Int,
            tokens::Literal::DATE => DataType::Date,
            tokens::Literal::TIME => DataType::Time,
            tokens::Literal::STRING => DataType::String,
            tokens::Literal::BOOLEAN => DataType::Boolean,
            tokens::Literal::ENUM => DataType::Any, // TODO scrape https://docs.appeon.com/pb2022/powerscript_reference
        }
    }
}

impl From<&Option<parser::DataType>> for DataType {
    fn from(src: &Option<parser::DataType>) -> DataType {
        match src {
            Some(data_type) => data_type.into(),
            None => DataType::Void,
        }
    }
}

impl From<&parser::DataType> for DataType {
    fn from(src: &parser::DataType) -> DataType {
        match src {
            parser::DataType::Decimal(precission) => {
                DataType::Decimal(precission.as_ref().map(|str| str.parse().unwrap()))
            }
            parser::DataType::Array(sub_type) => DataType::Array(Box::new((&**sub_type).into())),
            parser::DataType::Complex(group, class) => {
                DataType::Class(find_class(Some(group), &class))
            }
            parser::DataType::ID(id) => match id.to_lowercase().as_str() {
                "blob" => DataType::Blob,
                "boolean" => DataType::Boolean,
                "byte" => DataType::Byte,
                "char" => DataType::Char,
                "date" => DataType::Date,
                "datetime" => DataType::Datetime,
                "double" => DataType::Double,
                "integer" | "int" => DataType::Int,
                "long" => DataType::Long,
                "longlong" => DataType::Longlong,
                "longptr" => DataType::Longptr,
                "real" => DataType::Real,
                "string" => DataType::String,
                "time" => DataType::Time,
                "unsignedinteger" | "uint" => DataType::Uint,
                "unsignedlong" | "ulong" => DataType::Ulong,
                _ => DataType::Class(find_class(None, &id)),
            },
        }
    }
}

impl DataType {
    fn is_numeric(&self) -> bool {
        self.numeric_precedence().is_some()
    }

    fn numeric_precedence(&self) -> Option<u8> {
        match self {
            DataType::Int => Some(0),
            DataType::Uint => Some(1),
            DataType::Long => Some(2),
            DataType::Ulong => Some(3),
            DataType::Longlong => Some(4),
            DataType::Longptr => Some(5),
            DataType::Real => Some(6),
            DataType::Double => Some(7),
            DataType::Decimal(_) => Some(8),
            DataType::Any => Some(9),
            DataType::Unknown => Some(10),

            DataType::Blob
            | DataType::Boolean
            | DataType::Byte
            | DataType::Char
            | DataType::Date
            | DataType::Datetime
            | DataType::String
            | DataType::Time
            | DataType::Class(_)
            | DataType::Array(_)
            | DataType::Void => None,
        }
    }

    pub fn is_convertible(&self, other: &DataType) -> bool {
        match (self, other) {
            (DataType::Unknown, _) | (_, DataType::Unknown) => true,
            (DataType::Any, _) | (_, DataType::Any) => true,
            (DataType::Array(self_type), DataType::Array(other_type)) => {
                self_type.is_convertible(&other_type)
            }
            _ => self == other || (self.is_numeric() && other.is_numeric()),
        }
    }
}

#[derive(Default, Clone, Debug)]
struct Usage {
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

// TODO class not found
fn find_class(group: Option<&String>, class: &String) -> Rc<RefCell<Class>> {
    unimplemented!();
}

struct LintState {
    current_class: Rc<RefCell<Class>>,
    variables: Vec<(Usage, parser::Variable)>,
    // TODO stack<loop, throw, ...>
    return_type: DataType,
}

trait Lintable {
    fn lint(&self, lsp: &mut LSP, file: &mut FileData, state: &mut LintState) -> DataType;
}

impl Lintable for parser::VariableAccess {
    fn lint(&self, lsp: &mut LSP, file: &mut FileData, state: &mut LintState) -> DataType {
        let mut class = state.current_class.borrow_mut();

        match state
            .variables
            .iter_mut()
            .find(|(_, var)| var.name == self.name)
            .map(|(usage, var)| (usage, var))
            .or_else(|| {
                class
                    .find_variable(self, &tokens::AccessType::PRIVATE)
                    .map(|(usage, var)| (usage, &mut var.variable))
                    .or_else(|| {
                        file.find_variable(self, &tokens::AccessType::PRIVATE)
                            .map(|(usage, var)| (usage, &mut var.variable))
                            .or_else(|| {
                                lsp.find_global_variable(self)
                                    .map(|(usage, var)| (usage, var))
                            })
                    })
            }) {
            Some((usage, var)) => {
                usage.uses.push(self.range);
                (&var.data_type).into()
            }
            None => {
                file.diagnostics.push(parser::Diagnostic {
                    severity: parser::Severity::Error,
                    message: "Variable not found".into(),
                    range: self.range,
                });
                DataType::Unknown
            }
        }
    }
}

impl Lintable for parser::LValue {
    fn lint(&self, lsp: &mut LSP, file: &mut FileData, state: &mut LintState) -> DataType {
        match &self.lvalue_type {
            parser::LValueType::This => todo!(),
            parser::LValueType::Super => todo!(),
            parser::LValueType::Parent => todo!(),

            parser::LValueType::Variable(variable) => variable.lint(lsp, file, state),
            parser::LValueType::Function(call) => {
                let types = call
                    .arguments
                    .iter()
                    .map(|expression| expression.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                match state
                    .current_class
                    .borrow_mut()
                    .find_callable_function(&call.name, &types, &tokens::AccessType::PRIVATE)
                    .or_else(|| lsp.find_global_function(&call.name, &types))
                {
                    Some(func) => match &func.parsed.returns {
                        Some(returns) => returns.into(),
                        None => DataType::Void,
                    },
                    None => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Function not found".into(),
                            range: self.range,
                        });
                        DataType::Unknown
                    }
                }
            }
            parser::LValueType::Method(lvalue, call) => {
                let class = lvalue.lint(lsp, file, state);

                let types = call
                    .arguments
                    .iter()
                    .map(|expression| expression.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                match class {
                    DataType::Class(class_rc) => {
                        let mut class = class_rc.borrow_mut();
                        let func = class.find_callable_function(
                            &call.name,
                            &types,
                            &tokens::AccessType::PUBLIC,
                        );

                        match func {
                            Some(func) => {
                                func.uses.push(call.range);
                                (&func.parsed.returns).into()
                            }
                            None => {
                                file.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Method not found".into(),
                                    range: self.range,
                                });
                                DataType::Unknown
                            }
                        }
                    }
                    _ => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Cannot call a method of a non Complex type".into(),
                            range: self.range,
                        });
                        DataType::Unknown
                    }
                }
            }
            parser::LValueType::Member(_, _) => todo!(),
            parser::LValueType::Index(array, index) => {
                let array_type = array.lint(lsp, file, state);
                let index_type = index.lint(lsp, file, state);

                if !index_type.is_numeric() {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Index for subscript operator must be numerical".into(),
                        range: index.range,
                    });
                }

                match array_type {
                    DataType::Array(sub_type) => *sub_type,
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Subscript Operator can only be applied to Array".into(),
                            range: array.range,
                        });
                        DataType::Unknown
                    }
                }
            }
        }
    }
}

impl Lintable for parser::Expression {
    fn lint(&self, lsp: &mut LSP, file: &mut FileData, state: &mut LintState) -> DataType {
        match &self.expression_type {
            parser::ExpressionType::Literal(literal) => literal.into(),
            parser::ExpressionType::ArrayLiteral(expressions) => {
                let types = expressions
                    .iter()
                    .map(|expr| expr.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                DataType::Array(Box::new(match types.first() {
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
                    None => DataType::Unknown,
                }))
            }
            parser::ExpressionType::Operation(left, op, right) => {
                let left_type = left.lint(lsp, file, state);
                let right_type = right.lint(lsp, file, state);

                match op {
                    tokens::Operator::AND | tokens::Operator::OR => {
                        if left_type.is_convertible(&DataType::Boolean)
                            || right_type.is_convertible(&DataType::Boolean)
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected booleans".into(),
                                range: self.range,
                            });
                        }

                        DataType::Boolean
                    }
                    tokens::Operator::PLUS => {
                        if !((left_type.is_convertible(&DataType::String)
                            && right_type.is_convertible(&DataType::String))
                            || (left_type.is_numeric() && right_type.is_numeric()))
                        {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for operation, expected numbers or strings"
                                    .into(),
                                range: self.range,
                            });
                        }

                        DataType::Boolean
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
                                DataType::Unknown
                            }
                        }
                    }
                }
            }
            parser::ExpressionType::BooleanNot(expression) => {
                let expression_type = expression.lint(lsp, file, state);

                if !expression_type.is_convertible(&DataType::Boolean) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".into(),
                        range: expression.range,
                    });
                }

                DataType::Boolean
            }
            parser::ExpressionType::Parenthesized(expression) => expression.lint(lsp, file, state),
            parser::ExpressionType::Create(class) => todo!("Check if class exists"),
            parser::ExpressionType::CreateUsing(class) => {
                let class_type = class.lint(lsp, file, state);

                if !class_type.is_convertible(&DataType::String) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected string".into(),
                        range: self.range,
                    });
                }

                DataType::Class(find_class(None, &"powerobject".into()))
            }
            parser::ExpressionType::LValue(lvalue) => lvalue.lint(lsp, file, state),
            parser::ExpressionType::IncrementDecrement(lvalue, _) => {
                let lvalue_type = lvalue.lint(lsp, file, state);

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
    fn lint(&self, lsp: &mut LSP, file: &mut FileData, state: &mut LintState) -> DataType {
        match &self.statement_type {
            parser::StatementType::Expression(expression) => {
                expression.lint(lsp, file, state);
            }
            parser::StatementType::If(parser::IfStatement {
                condition,
                statements,
                elseif_statements,
                else_statements,
            }) => {
                let condition_type = condition.lint(lsp, file, state);

                statements.iter().for_each(|statement| {
                    statement.lint(lsp, file, state);
                });
                elseif_statements
                    .iter()
                    .for_each(|(condition, statements)| {
                        let condition_type = condition.lint(lsp, file, state);

                        if !condition_type.is_convertible(&DataType::Boolean) {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Condition for if must be of type Boolean".into(),
                                range: condition.range,
                            });
                        }

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, file, state);
                        })
                    });

                else_statements.iter().for_each(|statement| {
                    statement.lint(lsp, file, state);
                });

                if !condition_type.is_convertible(&DataType::Boolean) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for if must be of type Boolean".into(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Throw(exception) => {
                exception.lint(lsp, file, state);
            }
            parser::StatementType::Destroy(object) => {
                let data_type = object.lint(lsp, file, state);
                match data_type {
                    DataType::Class(_) => {}
                    // DataType::Array(_) => {} // TODO
                    DataType::Any => {}
                    DataType::Unknown => {}
                    _ => {
                        file.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Can only destroy Objects".into(),
                            range: object.range,
                        });
                    }
                }
            }
            parser::StatementType::Declaration(var) => {
                state.variables.push((
                    Usage {
                        declaration: Some(var.variable.range),
                        definition: None,
                        uses: Vec::new(),
                    },
                    var.variable.clone(),
                ));

                if let Some(initial_value) = &var.variable.initial_value {
                    match initial_value.expression_type {
                        parser::ExpressionType::Literal(literal) => {
                            if !DataType::from(&literal)
                                .is_convertible(&(&var.variable.data_type).into())
                            {
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
                let lvalue_type = lvalue.lint(lsp, file, state);
                let expression_type = expression.lint(lsp, file, state);

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
                finally,
            }) => {
                statements.iter().for_each(|statement| {
                    statement.lint(lsp, file, state);
                });
                catches.iter().for_each(|(_, _, statements)| {
                    // TODO lint the capture type and name
                    // TODO check if the all thrown errors are caught
                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, file, state);
                    });
                });
                finally.as_ref().map(|statements| {
                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, file, state);
                    })
                });
            }
            parser::StatementType::ForLoop(parser::ForLoopStatement {
                start,
                stop,
                step,
                variable,
                statements,
            }) => {
                let variable_type = variable.lint(lsp, file, state);
                let start_type = start.lint(lsp, file, state);
                let stop_type = stop.lint(lsp, file, state);

                let mut to_check = vec![
                    (variable_type, &variable.range),
                    (start_type, &start.range),
                    (stop_type, &stop.range),
                ];

                if let Some(step) = step {
                    let step_type = step.lint(lsp, file, state);
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

                statements.iter().for_each(|statement| {
                    statement.lint(lsp, file, state);
                });
            }
            parser::StatementType::WhileLoop(parser::WhileLoopStatement {
                condition,
                statements,
                ..
            }) => {
                let condition_type = condition.lint(lsp, file, state);

                statements.iter().for_each(|statement| {
                    statement.lint(lsp, file, state);
                });

                if !condition_type.is_convertible(&DataType::Boolean) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Condition for while loop must be of type Boolean".into(),
                        range: condition.range,
                    });
                }
            }
            parser::StatementType::Choose(parser::ChooseCaseStatement { choose, cases }) => {
                let choose_type = choose.lint(lsp, file, state);
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
                        if choose_type.is_convertible(&literal.into()) {
                            file.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Wrong Literal Type".into(),
                                range: range.clone(),
                            });
                        }
                    }

                    statements.iter().for_each(|statement| {
                        statement.lint(lsp, file, state);
                    });
                })
            }
            parser::StatementType::Return(ret) => {
                let ret_type = ret
                    .as_ref()
                    .map(|ret| ret.lint(lsp, file, state))
                    .unwrap_or(DataType::Void);

                if !ret_type.is_convertible(&state.return_type) {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Wrong return Type".into(),
                        range: self.range,
                    });
                }
            }
            parser::StatementType::Call(call) => {
                todo!("Function call");
            }
            parser::StatementType::Exit => {} // TODO stack?
            parser::StatementType::Continue => {}
            parser::StatementType::Error => {}
            parser::StatementType::Empty => {}
        }

        DataType::Void
    }
}

#[derive(Clone, Debug)]
struct Event {
    parsed: parser::Event,

    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

#[derive(Clone, Debug)]
struct Function {
    parsed: parser::Function,

    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

impl Function {
    pub fn is_callable(&self, arguments: &Vec<DataType>, min_access: &tokens::AccessType) -> bool {
        min_access.strictness()
            >= self
                .parsed
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && self
                .parsed
                .arguments
                .iter()
                .zip(arguments)
                .all(|(self_arg, call_arg)| {
                    call_arg.is_convertible(&(&self_arg.variable.data_type).into())
                })
    }
}

#[derive(Clone, Debug)]
struct Class {
    name: String,
    base: Rc<RefCell<Class>>,
    within: Option<Rc<RefCell<Class>>>,

    ons: Vec<Function>,
    events: Vec<Event>,
    functions: Vec<Function>,
    external_functions: Vec<Function>,

    instance_variables: Vec<(Usage, parser::InstanceVariable)>,
}

impl Class {
    fn new(name: String, base: Rc<RefCell<Class>>, within: Option<Rc<RefCell<Class>>>) -> Class {
        Class {
            name,
            base,
            within,

            events: Vec::new(),
            instance_variables: Vec::new(),
            functions: Vec::new(),
            ons: Vec::new(),
            external_functions: Vec::new(),
        }
    }

    fn find_variable(
        &mut self,
        variable: &parser::VariableAccess,
        min_access: &tokens::AccessType,
    ) -> Option<&mut (Usage, parser::InstanceVariable)> {
        self.instance_variables.iter_mut().find(|(_, var)| {
            var.variable.name == variable.name
                && min_access.strictness()
                    >= var
                        .access
                        .read
                        .map(|access| access.strictness())
                        .unwrap_or(0)
        })
    }

    fn find_exact_function(&mut self, function: &parser::Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.parsed.equals(function))
            .or_else(|| {
                self.external_functions
                    .iter_mut()
                    .find(|func| func.parsed.equals(function))
            })
    }

    fn find_conflicting_function(&mut self, function: &parser::Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.parsed.conflicts(function))
            .or_else(|| {
                self.external_functions
                    .iter_mut()
                    .find(|func| func.parsed.conflicts(function))
            })
    }

    fn find_callable_function(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| &func.parsed.name == name && func.is_callable(arguments, min_access))
            .or_else(|| {
                self.external_functions.iter_mut().find(|func| {
                    &func.parsed.name == name && func.is_callable(arguments, min_access)
                })
            })
    }
}
struct FileData {
    classes: Vec<Rc<RefCell<Class>>>,
    // Shared with all isntances
    shared_variables: Vec<(Usage, parser::InstanceVariable)>,

    diagnostics: Vec<parser::Diagnostic>,
}

impl FileData {
    fn find_variable(
        &mut self,
        variable: &parser::VariableAccess,
        min_access: &tokens::AccessType,
    ) -> Option<&mut (Usage, parser::InstanceVariable)> {
        self.shared_variables.iter_mut().find(|(_, var)| {
            var.variable.name == variable.name
                && min_access.strictness()
                    >= var
                        .access
                        .read
                        .map(|access| access.strictness())
                        .unwrap_or(0)
        })
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
                diagnostics: Vec::new(),
                shared_variables: Vec::new(),
            },

            top_levels: parse_file(&path)?,
            path,
            shallow: false,
        })
    }

    fn require_class(
        data: &mut FileData,
        current_class: &Weak<RefCell<Class>>,
        top_level_type: String,
        range: Range,
    ) -> Option<Rc<RefCell<Class>>> {
        let class = current_class.upgrade();
        match class {
            Some(_) => {}
            None => data.diagnostics.push(parser::Diagnostic {
                severity: parser::Severity::Error,
                message: top_level_type
                    + " have to come after the Type Definition that they refer to",
                range,
            }),
        }
        class
    }

    fn lint_rest(&mut self, lsp: &mut LSP) {
        let current_class = Weak::<RefCell<Class>>::new();
        let finish_linting_class = || match current_class.upgrade() {
            Some(class) => {
                for func in &class.borrow_mut().functions {
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
            None => unreachable!(),
        };

        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(..) => {}
                parser::TopLevelType::ForwardDecl => {}
                parser::TopLevelType::TypeVariablesDecl(..) => {}
                parser::TopLevelType::ScopedVariablesDecl(..) => {}
                parser::TopLevelType::GlobalVariableDecl(..) => {}
                parser::TopLevelType::ConstantDecl => {}
                parser::TopLevelType::FunctionForwardDecl => {}
                parser::TopLevelType::FunctionsForwardDecl(..) => {}
                parser::TopLevelType::ExternalFunctions(..) => {}
                parser::TopLevelType::FunctionBody(function, statements) => {
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "Function Bodies".into(),
                        top_level.range,
                    ) {
                        let mut state = LintState {
                            current_class: class.clone(),
                            variables: Vec::new(),
                            return_type: DataType::Void,
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });

                        match class.borrow_mut().find_conflicting_function(&function) {
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

                                    class.borrow_mut().functions.push(Function {
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

                                class.borrow_mut().functions.push(Function {
                                    parsed: function.clone(),

                                    declaration: None,
                                    definition: Some(function.range),
                                    uses: Vec::new(),
                                })
                            }
                        }
                    }
                }
                parser::TopLevelType::OnBody(on, statements) => {
                    // TODO doesnt actualyl need to be next to the class
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "On Bodies".into(),
                        top_level.range,
                    ) {
                        let mut state = LintState {
                            current_class: class,
                            variables: Vec::new(),
                            return_type: DataType::Void,
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });
                    }
                }
                parser::TopLevelType::EventBody(event, statements) => match current_class.upgrade()
                {
                    Some(class) => {
                        let mut state = LintState {
                            current_class: class,
                            variables: Vec::new(),
                            return_type: match &event.event_type {
                                parser::EventType::User(ret, _) => ret.into(),
                                parser::EventType::System(_) => DataType::Void,
                            },
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });
                    }
                    None => self.data.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Event Declarations have to come after a Type Definition".into(),
                        range: top_level.range,
                    }),
                },
            }
        }
    }

    fn lint_shallow(&mut self, lsp: &mut LSP) {
        let current_class = Weak::<RefCell<Class>>::new();

        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(class, vars, events) => {
                    let within = class
                        .within
                        .as_ref()
                        .map(|within_name| {
                            let within = self
                                .data
                                .classes
                                .iter()
                                .find(|within| &within.borrow().name == within_name)
                                .cloned();

                            if within.is_none() {
                                self.data.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Within Class not found".into(),
                                    range: top_level.range,
                                });
                            }

                            within
                        })
                        .flatten();

                    let mut new_class =
                        Class::new(class.name.clone(), find_class(None, &class.base), within);

                    for var in vars {
                        new_class.instance_variables.push((
                            Usage {
                                declaration: Some(var.variable.range),
                                definition: None,
                                uses: Vec::new(),
                            },
                            var.clone(),
                        ));
                    }

                    for event in events {
                        new_class.events.push(Event {
                            parsed: event.clone(),

                            declaration: Some(event.range),
                            definition: None,
                            uses: Vec::new(),
                        });
                    }

                    self.data.classes.push(Rc::new(RefCell::new(new_class)));
                }
                parser::TopLevelType::ForwardDecl => { /* TODO */ }
                parser::TopLevelType::TypeVariablesDecl(vars) => {
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "Type Variables".into(),
                        top_level.range,
                    ) {
                        for var in vars {
                            class.borrow_mut().instance_variables.push((
                                Usage {
                                    declaration: Some(var.variable.range),
                                    definition: None,
                                    uses: Vec::new(),
                                },
                                var.clone(),
                            ));
                        }
                    }
                }
                parser::TopLevelType::ScopedVariablesDecl(_) => { /* TODO shared + global */ }
                parser::TopLevelType::GlobalVariableDecl(_) => { /* TODO implicitly shared + global + shared scope keyword invalid */
                }
                parser::TopLevelType::ConstantDecl => { /* TODO */ }
                parser::TopLevelType::FunctionForwardDecl => { /* TODO */ }
                parser::TopLevelType::FunctionsForwardDecl(functions) => {
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "On Bodies".into(),
                        top_level.range,
                    ) {
                        for function in functions {
                            match class.borrow_mut().find_conflicting_function(function) {
                                Some(func) => {
                                    if let Some(declaration) = func.declaration {
                                        let definition = func.definition;
                                        class.borrow_mut().functions.push(Function {
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
                                None => class.borrow_mut().functions.push(Function {
                                    parsed: function.clone(),

                                    declaration: Some(function.range),
                                    definition: None,
                                    uses: Vec::new(),
                                }),
                            }
                        }
                    }
                }
                parser::TopLevelType::FunctionBody(..) => {}
                parser::TopLevelType::OnBody(..) => {}
                parser::TopLevelType::EventBody(..) => {}
                parser::TopLevelType::ExternalFunctions(functions) => {
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "External Functions".into(),
                        top_level.range,
                    ) {
                        for function in functions {
                            match class.borrow_mut().find_conflicting_function(function) {
                                Some(func) => {
                                    if let Some(declaration) = func.declaration {
                                        let definition = func.definition;
                                        class.borrow_mut().external_functions.push(Function {
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
                                None => class.borrow_mut().external_functions.push(Function {
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
}

pub struct LSPOptions {
    root: PathBuf,
}

pub struct LSP {
    files: Vec<File>,
    global_variables: Vec<(Usage, parser::Variable)>,
    global_functions: Vec<Function>,

    options: LSPOptions,
}

impl LSP {
    fn find_global_variable(
        &mut self,
        variable: &parser::VariableAccess,
    ) -> Option<&mut (Usage, parser::Variable)> {
        self.global_variables
            .iter_mut()
            .find(|(_, var)| var.name == variable.name)
    }

    fn find_global_function(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<&mut Function> {
        // No need to look for instance functions
        self.global_functions.iter_mut().find(|global| {
            &global.parsed.name == name
                && global.is_callable(arguments, &tokens::AccessType::PUBLIC)
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
