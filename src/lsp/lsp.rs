use std::{
    cell::{Ref, RefCell},
    env::current_dir,
    mem::swap,
    path::PathBuf,
    rc::{Rc, Weak},
};

use super::powerbuilder_proto::{self, variable};
use prost::{bytes::Bytes, Message};

use crate::parser::{
    parser::*,
    parser_types::{self as parser},
    tokenize, tokenize_file,
    tokenizer_types::{self as tokens, Position, Range},
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
    Enum(Rc<RefCell<Enum>>),
    Array(Box<DataType>),

    Any,
    Unknown,
    Void,
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Class(l), Self::Class(r)) => Rc::ptr_eq(l, r),
            (Self::Decimal(l), Self::Decimal(r)) => l == r,
            (Self::Array(l), Self::Array(r)) => l == r,
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
            | DataType::Enum(_)
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
            // TODO class
            _ => self == other || (self.is_numeric() && other.is_numeric()),
        }
    }
}

trait LintableDataType {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>) -> DataType;
}

impl LintableDataType for parser::DataType {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>) -> DataType {
        match self {
            parser::DataType::Decimal(precission) => {
                DataType::Decimal(precission.as_ref().map(|str| str.parse().unwrap()))
            }
            parser::DataType::Array(sub_type) => {
                DataType::Array(Box::new(sub_type.lint(lsp, file)))
            }
            parser::DataType::Complex(group, name) => {
                match lsp.find_class_from_file(file, Some(group), &name) {
                    Some(class) => DataType::Class(class),
                    None => DataType::Unknown,
                }
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
                _ => match lsp.find_class_from_file(file, None, &id) {
                    Some(class) => DataType::Class(class),
                    None => DataType::Unknown,
                },
            },
        }
    }
}

#[derive(Default, Clone, Debug)]
struct Usage {
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

struct LintData {
    current_class: Rc<RefCell<Class>>,
    variables: Vec<Rc<RefCell<Variable>>>,
    // TODO stack<loop, throw, ...>
    return_type: DataType,
}

impl LintData {
    // fn find_class(&self, group: Option<&String>, name: &String) -> Option<Rc<RefCell<Class>>> {
    //     match group {
    //         Some(group) => {
    //             // let within = self.find_class(None, group);
    //             // within.
    //             todo!()
    //         }
    //         None => todo!(),
    //     }
    // }
}

trait Lintable {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>, state: &mut LintData) -> DataType;
}

impl Lintable for parser::VariableAccess {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>, state: &mut LintData) -> DataType {
        let class = state.current_class.borrow();

        match state
            .variables
            .iter()
            .find(|var| var.borrow().parsed().name.eq_ignore_ascii_case(&self.name))
            .cloned()
            .or_else(|| {
                class
                    .find_variable(self, &tokens::AccessType::PRIVATE)
                    .or_else(|| {
                        let var = file
                            .borrow_mut()
                            .find_variable(self, &tokens::AccessType::PRIVATE);
                        var.or_else(|| lsp.find_global_variable(&self.name))
                    })
            }) {
            Some(var) => {
                var.borrow_mut().uses.push(self.range);
                var.borrow_mut().data_type.clone()
            }
            None => {
                file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>, state: &mut LintData) -> DataType {
        match &self.lvalue_type {
            parser::LValueType::This => DataType::Class(state.current_class.clone()),
            parser::LValueType::Super => state.current_class.borrow().base.clone(),
            parser::LValueType::Parent => match state.current_class.borrow().within.clone() {
                Some(within) => DataType::Class(within),
                None => {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Class does not have a Parent".into(),
                        range: self.range,
                    });

                    DataType::Unknown
                }
            },

            parser::LValueType::Variable(variable) => variable.lint(lsp, file, state),
            parser::LValueType::Function(call) => {
                let types = call
                    .arguments
                    .iter()
                    .map(|expression| expression.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                let ret = if call.event {
                    match state
                        .current_class
                        .borrow_mut()
                        .find_callable_event(&call.name, &types)
                    {
                        Some(event) => {
                            event.borrow_mut().uses.push(call.range.clone());
                            event.borrow().returns.clone()
                        }
                        None => {
                            if !call.dynamic {
                                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Event not found".into(),
                                    range: self.range,
                                });
                            }
                            DataType::Unknown
                        }
                    }
                } else {
                    let func = state.current_class.borrow_mut().find_callable_function(
                        &call.name,
                        &types,
                        &tokens::AccessType::PRIVATE,
                    );

                    match func.or_else(|| lsp.find_global_function(&call.name, &types)) {
                        Some(func) => {
                            func.borrow_mut().uses.push(call.range.clone());
                            func.borrow().returns.clone()
                        }
                        None => {
                            if !call.dynamic {
                                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Function not found".into(),
                                    range: self.range,
                                });
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
                let class = lvalue.lint(lsp, file, state);

                let types = call
                    .arguments
                    .iter()
                    .map(|expression| expression.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                match class {
                    DataType::Class(class_rc) => {
                        let mut class = class_rc.borrow_mut();

                        let ret = if call.event {
                            match class.find_callable_event(&call.name, &types) {
                                Some(event) => {
                                    event.borrow_mut().uses.push(call.range);
                                    event.borrow().returns.clone()
                                }
                                None => {
                                    if !call.dynamic {
                                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Error,
                                            message: "Event not found".into(),
                                            range: self.range,
                                        });
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
                                    func.borrow_mut().uses.push(call.range);
                                    func.borrow().returns.clone()
                                }
                                None => {
                                    if !call.dynamic {
                                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Error,
                                            message: "Method not found".into(),
                                            range: self.range,
                                        });
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
                    DataType::Any | DataType::Unknown => DataType::Unknown,
                    _ => {
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Cannot call a method of a non Complex type".into(),
                            range: self.range,
                        });
                        DataType::Unknown
                    }
                }
            }
            parser::LValueType::Member(lvalue, member) => {
                let class = lvalue.lint(lsp, file, state);

                match class {
                    DataType::Class(class_rc) => {
                        let mut class = class_rc.borrow_mut();
                        let var = class.find_variable(&member, &tokens::AccessType::PUBLIC); // TODO correct type thing

                        match var {
                            Some(var) => {
                                var.borrow_mut().uses.push(member.range);
                                var.borrow_mut().data_type.clone()
                            }
                            None => {
                                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Member not found".into(),
                                    range: self.range,
                                });
                                DataType::Unknown
                            }
                        }
                    }
                    DataType::Any | DataType::Unknown => DataType::Unknown,
                    _ => {
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Cannot call a method of a non Complex type".into(),
                            range: self.range,
                        });
                        DataType::Unknown
                    }
                }
            }
            parser::LValueType::Index(array, index) => {
                let array_type = array.lint(lsp, file, state);
                let index_type = index.lint(lsp, file, state);

                if !index_type.is_numeric() {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Index for subscript operator must be numerical".into(),
                        range: index.range,
                    });
                }

                match array_type {
                    DataType::Array(sub_type) => *sub_type,
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>, state: &mut LintData) -> DataType {
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
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for Operation, expected booleans".into(),
                                range: self.range,
                            });
                        }

                        DataType::Boolean
                    }
                    tokens::Operator::EQ | tokens::Operator::GTLT => {
                        if !(left_type.is_convertible(&right_type)
                            || right_type.is_convertible(&left_type))
                        {
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Types do not match".into(),
                                range: self.range,
                            });
                        }

                        DataType::Boolean
                    }
                    tokens::Operator::GT
                    | tokens::Operator::GTE
                    | tokens::Operator::LT
                    | tokens::Operator::LTE => {
                        if !(left_type.is_numeric() || right_type.is_numeric()) {
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Invalid types for Operation, expected numeric".into(),
                                range: self.range,
                            });
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
                                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Invalid types for Operation".into(),
                                    range: self.range,
                                });
                                DataType::Unknown
                            }
                        }
                    }
                }
            }
            parser::ExpressionType::PreMinusPlus(_operator, expression) => {
                let data_type = expression.lint(lsp, file, state);

                if !data_type.is_numeric() {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected number".into(),
                        range: expression.range,
                    });

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
                let expression_type = expression.lint(lsp, file, state);

                if !expression_type.is_convertible(&DataType::Boolean) {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".into(),
                        range: expression.range,
                    });
                }

                DataType::Boolean
            }
            parser::ExpressionType::Parenthesized(expression) => expression.lint(lsp, file, state),
            parser::ExpressionType::Create(class) => {
                match lsp.find_class_from_file(file, None, class) {
                    Some(class) => DataType::Class(class),
                    None => {
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Class not found".into(),
                            range: self.range,
                        });
                        DataType::Unknown
                    }
                }
            }
            parser::ExpressionType::CreateUsing(class) => {
                let class_type = class.lint(lsp, file, state);

                if !class_type.is_convertible(&DataType::String) {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected string".into(),
                        range: self.range,
                    });
                }

                DataType::Unknown
            }
            parser::ExpressionType::LValue(lvalue) => lvalue.lint(lsp, file, state),
            parser::ExpressionType::IncrementDecrement(lvalue, _) => {
                let lvalue_type = lvalue.lint(lsp, file, state);

                if !lvalue_type.is_numeric() {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>, state: &mut LintData) -> DataType {
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
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Can only destroy Objects".into(),
                            range: object.range,
                        });
                    }
                }
            }
            parser::StatementType::Declaration(var) => {
                let data_type = var.variable.data_type.lint(lsp, file);

                if let DataType::Unknown = data_type {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Type not found".into(),
                        range: var.variable.range,
                    });
                }

                if let Some(initial_value) = &var.variable.initial_value {
                    let initial_type = initial_value.lint(lsp, file, state);
                    if !initial_type.is_convertible(&data_type) {
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Type's are not convertible".into(),
                            range: initial_value.range,
                        });
                    }
                }

                state.variables.push(Rc::new(RefCell::new(Variable {
                    variable_type: VariableType::Local(var.variable.clone()),
                    data_type,
                    uses: Vec::new(),
                })));
            }
            parser::StatementType::Assignment(lvalue, expression) => {
                let lvalue_type = lvalue.lint(lsp, file, state);
                let expression_type = expression.lint(lsp, file, state);

                if !expression_type.is_convertible(&lvalue_type) {
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                        file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                            file.borrow_mut().diagnostics.push(parser::Diagnostic {
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
                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Wrong return Type".into(),
                        range: self.range,
                    });
                }
            }
            parser::StatementType::Call(parser::CallStatement {
                call_type,
                function,
            }) => {
                let types = function
                    .arguments
                    .iter()
                    .map(|expression| expression.lint(lsp, file, state))
                    .collect::<Vec<_>>();

                let base = match call_type {
                    parser::CallType::Super => match state.current_class.borrow().base.clone() {
                        DataType::Class(base) => Some(base),
                        _ => unreachable!(),
                    },
                    parser::CallType::Ancestor(group, name) => {
                        let mut curr = state.current_class.clone();
                        loop {
                            let new_curr = match &curr.borrow().base {
                                DataType::Class(base) => {
                                    if base.borrow().name.eq_ignore_ascii_case(name) {
                                        match (group, &base.borrow().within) {
                                            (Some(group), Some(within))
                                                if within
                                                    .borrow()
                                                    .name
                                                    .eq_ignore_ascii_case(group) =>
                                            {
                                                break Some(base.clone());
                                            }
                                            (None, _) => break Some(base.clone()),
                                            _ => {}
                                        }
                                    }
                                    base.clone()
                                }
                                _ => {
                                    file.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Ancestor not found".into(),
                                        range: self.range.clone(),
                                    });
                                    break None;
                                }
                            };
                            curr = new_curr;
                        }
                    }
                };

                if let Some(base) = base {
                    match base.borrow_mut().find_callable_function(
                        &function.name,
                        &types,
                        &tokens::AccessType::PROTECTED,
                    ) {
                        Some(_) => todo!(),
                        None => todo!(),
                    }
                }
            }
            parser::StatementType::Exit => {} // TODO stack?
            parser::StatementType::Continue => {}
            parser::StatementType::Error => {}
            parser::StatementType::Empty => {}
            parser::StatementType::SQL => {}
        }

        DataType::Void
    }
}

trait LintableNoState {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>) -> DataType;
}

impl LintableNoState for parser::DatatypeDecl {
    fn lint(&self, lsp: &mut LSP, file: &Rc<RefCell<File>>) -> DataType {
        let within = self.class.within.as_ref().and_then(|within_name| {
            let within = file.borrow().find_class(None, within_name);

            if within.is_none() {
                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                    severity: parser::Severity::Error,
                    message: "Within Class not found".into(),
                    range: self.range,
                });
            }

            within
        });

        // lsp.find_class(group, name, include_local)
        let mut base = lsp
            .find_class_from_file(file, None, &self.class.base)
            .map(DataType::Class);
        if base.is_none() {
            base = lsp.find_enum(&self.class.base).map(DataType::Enum);

            if base.is_none() {
                file.borrow_mut().diagnostics.push(parser::Diagnostic {
                    severity: parser::Severity::Error,
                    message: "Base Class not found".into(),
                    range: self.range,
                });
            }
        }
        let mut new_class = Class::new(
            self.class.name.clone(),
            base.unwrap_or(DataType::Unknown),
            within,
            matches!(self.class.scope, Some(tokens::ScopeModif::GLOBAL)),
        );

        for var in &self.variables {
            new_class
                .instance_variables
                .push(Rc::new(RefCell::new(Variable {
                    variable_type: VariableType::Instance(var.clone()),
                    data_type: var.variable.data_type.lint(lsp, file),
                    uses: Vec::new(),
                })));
        }

        for event in &self.events {
            new_class.events.push(Rc::new(RefCell::new(Event::new(
                lsp,
                file,
                event.clone(),
                Some(event.range),
                None,
            ))));
        }

        DataType::Class(Rc::new(RefCell::new(new_class)))
    }
}

#[derive(Clone, Debug)]
enum VariableType {
    Local(parser::Variable),
    Argument(parser::Argument),
    Instance(parser::InstanceVariable),
}

#[derive(Clone, Debug)]
struct Variable {
    variable_type: VariableType,

    data_type: DataType,
    uses: Vec<Range>,
}

impl Variable {
    fn parsed(&self) -> &parser::Variable {
        match &self.variable_type {
            VariableType::Local(local) => &local,
            VariableType::Argument(arg) => &arg.variable,
            VariableType::Instance(instance) => &instance.variable,
        }
    }
}

#[derive(Clone, Debug)]
struct Event {
    parsed: parser::Event,

    returns: DataType,
    arguments: Vec<Variable>,
    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

impl Event {
    fn new(
        lsp: &mut LSP,
        file: &Rc<RefCell<File>>,
        parsed: parser::Event,
        declaration: Option<Range>,
        definition: Option<Range>,
    ) -> Self {
        let returns;
        let arguments;
        match &parsed.event_type {
            parser::EventType::User(ret, args) => {
                arguments = args
                    .iter()
                    .map(|arg| Variable {
                        variable_type: VariableType::Argument(arg.clone()),
                        data_type: arg.variable.data_type.lint(lsp, file),
                        uses: Vec::new(),
                    })
                    .collect();
                returns = ret
                    .as_ref()
                    .map_or(DataType::Void, |ret| ret.lint(lsp, file));
            }
            parser::EventType::System(name) => {
                arguments = Vec::new();
                returns = DataType::Void;
                // TODO get system events from where?
            }
            parser::EventType::Predefined => {
                arguments = Vec::new();
                returns = DataType::Void;
                // TODO get arguments and return value from base class using parsed.name
            }
        }

        Event {
            declaration,
            definition,
            uses: Vec::new(),

            parsed,
            returns,
            arguments,
        }
    }

    fn equals(&self, other: &Event) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    fn conflicts(&self, other: &Event) -> bool {
        self.parsed.name.eq_ignore_ascii_case(&other.parsed.name)
            && self
                .arguments
                .iter()
                .zip(other.arguments.iter())
                .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
    }

    pub fn is_callable(&self, arguments: &Vec<DataType>) -> bool {
        self.arguments
            .iter()
            .zip(arguments.iter())
            .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
            && self.arguments.len() == arguments.len()
    }
}

#[derive(Clone, Debug)]
struct Function {
    parsed: parser::Function,

    returns: DataType,
    arguments: Vec<Variable>,
    help: Option<String>,

    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

impl Function {
    fn new(
        lsp: &mut LSP,
        file: &Rc<RefCell<File>>,
        parsed: parser::Function,
        declaration: Option<Range>,
        definition: Option<Range>,
    ) -> Self {
        Function {
            declaration,
            definition,
            uses: Vec::new(),

            help: None,

            arguments: parsed
                .arguments
                .iter()
                .map(|arg| Variable {
                    variable_type: VariableType::Argument(arg.clone()),
                    data_type: arg.variable.data_type.lint(lsp, file),
                    uses: Vec::new(),
                })
                .collect(),
            returns: parsed
                .returns
                .as_ref()
                .map_or(DataType::Void, |ret| ret.lint(lsp, file)),

            parsed,
        }
    }

    fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    fn conflicts(&self, other: &Function) -> bool {
        self.parsed.name.eq_ignore_ascii_case(&other.parsed.name)
            && self
                .arguments
                .iter()
                .zip(other.arguments.iter())
                .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
    }

    pub fn is_callable(&self, arguments: &Vec<DataType>, min_access: &tokens::AccessType) -> bool {
        min_access.strictness()
            >= self
                .parsed
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && self
                .arguments
                .iter()
                .zip(arguments)
                .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
            && (self.arguments.len() == arguments.len()
                || (self.arguments.len() < arguments.len() && self.parsed.has_vararg))
    }
}

#[derive(Debug)]
struct Enum {
    name: String,
    help: Option<String>,
    values: Vec<String>,
}

#[derive(Debug)]
struct Class {
    // file: Option<Rc<RefCell<File>>>,
    name: String,
    base: DataType,
    within: Option<Rc<RefCell<Class>>>,
    help: Option<String>,

    is_global: bool,
    usage: Usage,

    ons: Vec<Rc<RefCell<Function>>>,
    events: Vec<Rc<RefCell<Event>>>,
    functions: Vec<Rc<RefCell<Function>>>,
    external_functions: Vec<Rc<RefCell<Function>>>,

    instance_variables: Vec<Rc<RefCell<Variable>>>,
}

impl Class {
    fn new(
        // file: Option<Rc<RefCell<File>>>,
        name: String,
        base: DataType,
        within: Option<Rc<RefCell<Class>>>,
        is_global: bool,
    ) -> Class {
        Class {
            // file,
            name,
            base,
            within,
            help: None,

            is_global,
            usage: Usage {
                declaration: None,
                definition: None,
                uses: Vec::new(),
            },

            events: Vec::new(),
            instance_variables: Vec::new(),
            functions: Vec::new(),
            ons: Vec::new(),
            external_functions: Vec::new(),
        }
    }

    fn inherits_from(&self, datatype: &DataType) -> bool {
        match datatype {
            DataType::Class(class) => match &self.base {
                DataType::Class(base) => {
                    Rc::ptr_eq(base, &class) || base.borrow().inherits_from(datatype)
                }
                _ => false,
            },
            DataType::Enum(enumerated) => match &self.base {
                DataType::Class(base) => base.borrow().inherits_from(datatype),
                DataType::Enum(base) => Rc::ptr_eq(base, enumerated),
                _ => unreachable!(),
            },
            _ => false,
        }
    }

    fn find_variable(
        &self,
        variable: &parser::VariableAccess,
        min_access: &tokens::AccessType,
    ) -> Option<Rc<RefCell<Variable>>> {
        self.instance_variables
            .iter()
            .find(|var| {
                let var = var.borrow();
                if let VariableType::Instance(var) = &var.variable_type {
                    var.variable.name.eq_ignore_ascii_case(&variable.name)
                        && min_access.strictness()
                            >= var
                                .access
                                .read
                                .map(|access| access.strictness())
                                .unwrap_or(0)
                } else {
                    unreachable!();
                }
            })
            .cloned()
    }

    fn find_exact_event(&mut self, event: &Event) -> Option<Rc<RefCell<Event>>> {
        self.events
            .iter()
            .find(|func| func.borrow().equals(event))
            .cloned()
    }

    fn find_conflicting_event(&mut self, event: &Event) -> Option<Rc<RefCell<Event>>> {
        self.events
            .iter()
            .find(|func| func.borrow().conflicts(event))
            .cloned()
    }

    fn find_callable_event(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Rc<RefCell<Event>>> {
        self.events
            .iter()
            .find(|event| {
                event.borrow().parsed.name.eq_ignore_ascii_case(name)
                    && event.borrow().is_callable(arguments)
            })
            .cloned()
    }

    fn find_exact_function(&mut self, function: &Function) -> Option<Rc<RefCell<Function>>> {
        self.functions
            .iter()
            .find(|func| func.borrow().equals(function))
            .or_else(|| {
                self.external_functions
                    .iter()
                    .find(|func| func.borrow().equals(function))
            })
            .cloned()
    }

    fn find_conflicting_function(&mut self, function: &Function) -> Option<Rc<RefCell<Function>>> {
        self.functions
            .iter()
            .find(|func| func.borrow().conflicts(function))
            .or_else(|| {
                self.external_functions
                    .iter()
                    .find(|func| func.borrow().conflicts(function))
            })
            .cloned()
    }

    fn find_callable_function(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> Option<Rc<RefCell<Function>>> {
        self.functions
            .iter()
            .find(|func| {
                func.borrow().parsed.name.eq_ignore_ascii_case(name)
                    && func.borrow().is_callable(arguments, min_access)
            })
            .or_else(|| {
                self.external_functions.iter().find(|func| {
                    func.borrow().parsed.name.eq_ignore_ascii_case(name)
                        && func.borrow().is_callable(arguments, min_access)
                })
            })
            .cloned()
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum LintState {
    None,
    OnlyTypes,
    Shallow,
    Complete,
}

impl LintState {
    fn next(&self) -> Option<LintState> {
        match self {
            LintState::None => Some(Self::OnlyTypes),
            LintState::OnlyTypes => Some(Self::Shallow),
            LintState::Shallow => Some(Self::Complete),
            LintState::Complete => None,
        }
    }
}

#[derive(Debug)]
struct File {
    classes: Vec<Rc<RefCell<Class>>>,
    // Shared with all instances
    shared_variables: Vec<Rc<RefCell<Variable>>>,
    global_variables: Vec<Rc<RefCell<Variable>>>,

    diagnostics: Vec<parser::Diagnostic>,

    top_levels: Vec<parser::TopLevel>,
    path: PathBuf,
    lint_state: LintState,
}

impl File {
    fn new(path: PathBuf) -> anyhow::Result<File> {
        Ok(File {
            classes: Vec::new(),
            diagnostics: Vec::new(),
            shared_variables: Vec::new(),
            global_variables: Vec::new(),

            top_levels: tokenize_file(&path)?.parse_tokens(),
            path,
            lint_state: LintState::None,
        })
    }

    fn find_variable(
        &mut self,
        variable: &parser::VariableAccess,
        min_access: &tokens::AccessType,
    ) -> Option<Rc<RefCell<Variable>>> {
        self.shared_variables
            .iter()
            .find(|var| {
                let var = var.borrow();
                if let VariableType::Instance(var) = &var.variable_type {
                    var.variable.name.eq_ignore_ascii_case(&variable.name)
                        && min_access.strictness()
                            >= var
                                .access
                                .read
                                .map(|access| access.strictness())
                                .unwrap_or(0)
                } else {
                    unreachable!();
                }
            })
            .cloned()
    }

    fn find_class(&self, group: Option<&String>, name: &String) -> Option<Rc<RefCell<Class>>> {
        self.classes.iter().find_map(|class| {
            if class.borrow().name.eq_ignore_ascii_case(name) {
                match (group, &class.borrow().within) {
                    (Some(group), Some(within))
                        if within.borrow().name.eq_ignore_ascii_case(group) =>
                    {
                        Some(class.clone())
                    }
                    (None, _) => Some(class.clone()),
                    _ => None,
                }
            } else {
                None
            }
        })
    }

    fn require_class(
        file: &Rc<RefCell<File>>,
        current_class: &Weak<RefCell<Class>>,
        top_level_type: String,
        range: Range,
    ) -> Option<Rc<RefCell<Class>>> {
        let class = current_class.upgrade();
        match class {
            Some(_) => {}
            None => file.borrow_mut().diagnostics.push(parser::Diagnostic {
                severity: parser::Severity::Error,
                message: top_level_type
                    + " have to come after the Type Definition that they refer to",
                range,
            }),
        }
        class
    }
}

trait LintableFile {
    fn lint(&self, lsp: &mut LSP, lin_state: &LintState);
}

impl LintableFile for Rc<RefCell<File>> {
    fn lint(&self, lsp: &mut LSP, lint_state: &LintState) {
        let mut current_class = Weak::<RefCell<Class>>::new();

        let mut top_levels = Vec::new();
        swap(&mut self.borrow_mut().top_levels, &mut top_levels);
        for top_level in &top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::ForwardDecl(types) => {
                    if matches!(lint_state, LintState::OnlyTypes) {
                        for datatype in types {
                            let mut new_class = Class::new(
                                // self,
                                datatype.class.name.clone(),
                                DataType::Unknown,
                                None,
                                matches!(datatype.class.scope, Some(tokens::ScopeModif::GLOBAL)),
                            );
                            new_class.usage.declaration = Some(datatype.range);
                            self.borrow_mut()
                                .classes
                                .push(Rc::new(RefCell::new(new_class)));
                        }
                    }
                }

                // #region LintState::Shallow
                parser::TopLevelType::DatatypeDecl(datatype)
                    if matches!(lint_state, LintState::Shallow) =>
                {
                    let new_class = match datatype.lint(lsp, self) {
                        DataType::Class(class) => class,
                        _ => unreachable!(),
                    };

                    match self.borrow().find_class(None, &datatype.class.name) {
                        Some(class_ref) => {
                            current_class = Rc::downgrade(&class_ref);
                            let mut class = class_ref.borrow_mut();
                            match class.usage.definition {
                                Some(def) => {
                                    self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Type already defined".into(),
                                        range: datatype.range,
                                    });
                                    self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Type already defined here".into(),
                                        range: def,
                                    });
                                }
                                None => {
                                    let mut new_class = new_class.borrow_mut();
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
                            current_class = Rc::downgrade(&new_class);
                            if new_class.borrow().is_global {
                                self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Warning,
                                        message: "Global Classes should be Forward Declared, otherwise they might not be seen by other Files".into(),
                                        range: datatype.range,
                                    });
                            }
                            self.borrow_mut().classes.push(new_class);
                        }
                    }
                }
                parser::TopLevelType::TypeVariablesDecl(vars) => {
                    if matches!(lint_state, LintState::Shallow) {
                        if let Some(class) = File::require_class(
                            self,
                            &current_class,
                            "Type Variables".into(),
                            top_level.range,
                        ) {
                            for var in vars {
                                let data_type = var.variable.data_type.lint(lsp, self);

                                class
                                    .borrow_mut()
                                    .instance_variables
                                    .push(Rc::new(RefCell::new(Variable {
                                        variable_type: VariableType::Instance(var.clone()),
                                        data_type,
                                        uses: Vec::new(),
                                    })));
                            }
                        }
                    }
                }
                parser::TopLevelType::ScopedVariablesDecl(_) => {
                    if matches!(lint_state, LintState::Shallow) { /* TODO shared + global */ }
                }
                parser::TopLevelType::GlobalVariableDecl(_) => {
                    if matches!(lint_state, LintState::Shallow) { /* TODO implicitly shared + global + shared scope keyword invalid */
                    }
                }
                parser::TopLevelType::ConstantDecl => {
                    if matches!(lint_state, LintState::Shallow) { /* TODO */ }
                }
                parser::TopLevelType::FunctionForwardDecl => {
                    if matches!(lint_state, LintState::Shallow) { /* TODO */ }
                }
                parser::TopLevelType::FunctionsForwardDecl(functions) => {
                    if matches!(lint_state, LintState::Shallow) {
                        if let Some(class) = File::require_class(
                            self,
                            &current_class,
                            "Function Forward Declarations".into(),
                            top_level.range,
                        ) {
                            for function in functions {
                                let mut new_func =
                                    Function::new(lsp, self, function.clone(), None, None);

                                let mut borrow = class.borrow_mut();
                                match borrow.find_conflicting_function(&new_func) {
                                    Some(func) => {
                                        let mut func = func.borrow_mut();
                                        if func.returns
                                            != function
                                                .returns
                                                .as_ref()
                                                .map_or(DataType::Void, |ret| ret.lint(lsp, self))
                                        {
                                            self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                                severity: parser::Severity::Error,
                                                message: "Same function with different return type already exists".into(),
                                                range: function.range,
                                            });
                                            if let Some(declaration) = func.declaration {
                                                self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                                severity: parser::Severity::Hint,
                                                message: "Function with different return type declared here".into(),
                                                range: declaration,
                                            });
                                            }
                                        }

                                        if let Some(declaration) = func.declaration {
                                            self.borrow_mut().diagnostics.push(
                                                parser::Diagnostic {
                                                    severity: parser::Severity::Error,
                                                    message: "Function already forward declared"
                                                        .into(),
                                                    range: function.range,
                                                },
                                            );
                                            self.borrow_mut().diagnostics.push(
                                                parser::Diagnostic {
                                                    severity: parser::Severity::Hint,
                                                    message: "Already forward declared here".into(),
                                                    range: declaration,
                                                },
                                            );
                                        } else {
                                            func.declaration = Some(function.range);
                                        }
                                    }
                                    None => {
                                        new_func.declaration = Some(function.range);
                                        borrow.functions.push(Rc::new(RefCell::new(new_func)));
                                    }
                                }
                            }
                        }
                    }
                }
                parser::TopLevelType::ExternalFunctions(functions) => {
                    if matches!(lint_state, LintState::Shallow) {
                        if let Some(class) = File::require_class(
                            self,
                            &current_class,
                            "External Functions".into(),
                            top_level.range,
                        ) {
                            for function in functions {
                                let mut new_func =
                                    Function::new(lsp, self, function.clone(), None, None);

                                let mut borrow = class.borrow_mut();
                                match borrow.find_exact_function(&new_func) {
                                    Some(func) => {
                                        let mut func = func.borrow_mut();
                                        if let Some(declaration) = func.declaration {
                                            self.borrow_mut().diagnostics.push(
                                                parser::Diagnostic {
                                                    severity: parser::Severity::Error,
                                                    message: "Function already forward declared"
                                                        .into(),
                                                    range: function.range,
                                                },
                                            );
                                            self.borrow_mut().diagnostics.push(
                                                parser::Diagnostic {
                                                    severity: parser::Severity::Hint,
                                                    message: "Already forward declared here".into(),
                                                    range: declaration,
                                                },
                                            );
                                        } else {
                                            func.declaration = Some(function.range);
                                        }
                                    }
                                    None => {
                                        new_func.declaration = Some(function.range);
                                        borrow
                                            .external_functions
                                            .push(Rc::new(RefCell::new(new_func)));
                                    }
                                }
                            }
                        }
                    }
                }
                // #endregion

                // #region LintState::Complete
                parser::TopLevelType::DatatypeDecl(parser::DatatypeDecl { class, .. })
                    if matches!(lint_state, LintState::Complete) =>
                {
                    current_class =
                        match self.borrow().find_class(class.within.as_ref(), &class.name) {
                            Some(class) => Rc::downgrade(&class),
                            None => Rc::downgrade(&lsp.find_builtin_class("powerobject")),
                        }
                }
                parser::TopLevelType::FunctionBody(function, statements) => {
                    if matches!(lint_state, LintState::Complete) {
                        if let Some(class) = File::require_class(
                            self,
                            &current_class,
                            "Function Bodies".into(),
                            top_level.range,
                        ) {
                            let new_func = Function::new(
                                lsp,
                                self,
                                function.clone(),
                                Some(function.range),
                                None,
                            );

                            let mut state = LintData {
                                current_class: class.clone(),
                                variables: new_func
                                    .arguments
                                    .iter()
                                    .map(|var| Rc::new(RefCell::new(var.clone())))
                                    .collect(),
                                return_type: new_func.returns.clone(),
                            };

                            statements.iter().for_each(|statement| {
                                statement.lint(lsp, self, &mut state);
                            });

                            let mut borrow = class.borrow_mut();
                            match borrow.find_exact_function(&new_func) {
                                Some(existing) => {
                                    let mut existing = existing.borrow_mut();
                                    if let Some(definition) = existing.definition {
                                        self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Error,
                                            message: "Function already defined".into(),
                                            range: function.range,
                                        });
                                        self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Hint,
                                            message: "Already defined here".into(),
                                            range: definition,
                                        });
                                    } else {
                                        existing.definition = Some(function.range);
                                    }
                                }
                                None => {
                                    self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Function is missing a Forward Declaration".into(),
                                        range: function.range,
                                    });

                                    borrow.functions.push(Rc::new(RefCell::new(new_func)));
                                }
                            }
                        }
                    }
                }
                parser::TopLevelType::OnBody(on, statements) => {
                    if matches!(lint_state, LintState::Complete) {
                        if let Some(class) = self.borrow().find_class(None, &on.class) {
                            let mut state = LintData {
                                current_class: class,
                                variables: Vec::new(),
                                return_type: DataType::Void,
                            };

                            statements.iter().for_each(|statement| {
                                statement.lint(lsp, self, &mut state);
                            });
                        } else {
                            self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "On Body for non Existing Class".into(),
                                range: top_level.range.clone(),
                            });
                        }
                    }
                }
                parser::TopLevelType::EventBody(event, statements) => {
                    if matches!(lint_state, LintState::Complete) {
                        if let Some(class) = File::require_class(
                            self,
                            &current_class,
                            "Event Bodies".into(),
                            top_level.range,
                        ) {
                            let new_event =
                                Event::new(lsp, self, event.clone(), Some(event.range), None);

                            let mut state = LintData {
                                current_class: class.clone(),
                                variables: new_event
                                    .arguments
                                    .iter()
                                    .map(|var| Rc::new(RefCell::new(var.clone())))
                                    .collect(),
                                return_type: new_event.returns.clone(),
                            };

                            statements.iter().for_each(|statement| {
                                statement.lint(lsp, self, &mut state);
                            });

                            let mut borrow = class.borrow_mut();
                            match borrow.find_exact_event(&new_event) {
                                Some(existing) => {
                                    let mut existing = existing.borrow_mut();
                                    if let Some(definition) = existing.definition {
                                        self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Error,
                                            message: "Event already defined".into(),
                                            range: event.range,
                                        });
                                        self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                            severity: parser::Severity::Hint,
                                            message: "Already defined here".into(),
                                            range: definition,
                                        });
                                    } else {
                                        existing.definition = Some(event.range);
                                    }
                                }
                                None => {
                                    self.borrow_mut().diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Event is missing a Forward Declaration".into(),
                                        range: event.range,
                                    });

                                    borrow.events.push(Rc::new(RefCell::new(new_event)));
                                }
                            }
                        }
                    }
                } // #endregion

                parser::TopLevelType::DatatypeDecl(..) => {
                    if !matches!(lint_state, LintState::Shallow | LintState::Complete) {}
                }
            }
        }

        swap(&mut self.borrow_mut().top_levels, &mut top_levels);
        self.borrow_mut().lint_state = lint_state.clone();
    }
}

pub struct LSPOptions {
    root: PathBuf,
}

pub struct LSP {
    files: Vec<Rc<RefCell<File>>>,
    placeholder_file: Rc<RefCell<File>>,

    builtin_enums: Vec<Rc<RefCell<Enum>>>,
    builtin_functions: Vec<Rc<RefCell<Function>>>,
    builtin_classes: Vec<Rc<RefCell<Class>>>,

    options: LSPOptions,
}

impl LSP {
    fn find_class_from_file(
        &mut self,
        file: &Rc<RefCell<File>>,
        group: Option<&String>,
        name: &String,
    ) -> Option<Rc<RefCell<Class>>> {
        file.borrow()
            .find_class(group, name)
            .or_else(|| self.find_class(group, name, false))
    }

    fn find_class(
        &mut self,
        group: Option<&String>,
        name: &String,
        include_local: bool,
    ) -> Option<Rc<RefCell<Class>>> {
        self.files
            .iter()
            .find_map(|file| {
                file.borrow().find_class(group, name).and_then(|class| {
                    (include_local || class.borrow().is_global).then(|| (file.clone(), class))
                })
            })
            .and_then(|(file, class)| {
                if file.borrow().lint_state < LintState::Shallow {
                    file.lint(self, &LintState::Shallow);
                }
                Some(class)
            })
            .or_else(|| {
                self.builtin_classes
                    .iter()
                    .find(|builtin| builtin.borrow().name.eq_ignore_ascii_case(name))
                    .cloned()
            })
    }

    fn find_enum(&self, name: &String) -> Option<Rc<RefCell<Enum>>> {
        self.builtin_enums
            .iter()
            .find(|enumerated| enumerated.borrow().name.eq_ignore_ascii_case(name))
            .cloned()
    }

    fn find_builtin_class(&self, name: &'static str) -> Rc<RefCell<Class>> {
        self.builtin_classes
            .iter()
            .find(|class| class.borrow().name.eq_ignore_ascii_case(name))
            .expect(format!("Builtin Class not found: {}", name).as_str())
            .clone()
    }

    fn find_global_variable(&mut self, name: &String) -> Option<Rc<RefCell<Variable>>> {
        self.files.iter().find_map(|file| {
            file.borrow()
                .global_variables
                .iter()
                .find(|var| var.borrow().parsed().name.eq_ignore_ascii_case(name))
                .cloned()
        })
    }

    fn find_global_function(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Rc<RefCell<Function>>> {
        let func_class = DataType::Class(self.find_builtin_class("function_object"));
        self.files
            .iter()
            .find_map(|file| {
                file.borrow().find_class(None, name).and_then(|class| {
                    if class.borrow().inherits_from(&func_class) {
                        class.borrow_mut().find_callable_function(
                            name,
                            arguments,
                            &tokens::AccessType::PUBLIC,
                        )
                    } else {
                        None
                    }
                })
            })
            .or_else(|| {
                self.builtin_functions
                    .iter()
                    .find(|global| {
                        global.borrow().parsed.name.eq_ignore_ascii_case(name)
                            && global
                                .borrow()
                                .is_callable(arguments, &tokens::AccessType::PUBLIC)
                    })
                    .cloned()
            })
    }

    pub fn new() -> anyhow::Result<LSP> {
        Ok(LSP {
            files: Vec::new(),
            placeholder_file: Rc::new(RefCell::new(File {
                classes: vec![],
                shared_variables: vec![],
                global_variables: vec![],
                diagnostics: vec![],
                top_levels: vec![],

                path: "".into(),
                lint_state: LintState::Complete,
            })),

            builtin_enums: Vec::new(),
            builtin_functions: Vec::new(),
            builtin_classes: Vec::new(),

            options: LSPOptions {
                root: current_dir()?,
            },
        })
    }

    pub fn add_file(&mut self, path: PathBuf, lint_state: LintState) -> anyhow::Result<()> {
        let file = match self.files.iter().find(|file| file.borrow().path == path) {
            Some(file) => file.clone(),
            None => {
                let file = Rc::new(RefCell::new(File::new(path)?));
                self.files.push(file.clone());
                file
            }
        };

        loop {
            if file.borrow().lint_state >= lint_state {
                break;
            }

            let next = file.borrow().lint_state.next();
            match next {
                Some(state) => file.lint(self, &state),
                None => break,
            }
        }

        for diagnostic in &file.borrow().diagnostics {
            println!("{} - {}", diagnostic.range, diagnostic.message)
        }

        Ok(())
    }

    fn load_proto_function(
        &mut self,
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

    pub fn load_enums(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let enums = powerbuilder_proto::Enums::decode(buf)?;

        self.builtin_enums
            .extend(enums.r#enum.into_iter().map(|en| {
                Rc::new(RefCell::new(Enum {
                    name: en.name,
                    help: en.help,
                    values: en.value,
                }))
            }));

        Ok(())
    }

    pub fn load_builtin_classes(&mut self, path: PathBuf) -> anyhow::Result<()> {
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
                .iter()
                .find(|base| base.borrow().name == class.base)
                .cloned()
                .map(DataType::Class)
                .or_else(|| self.find_enum(&class.base).map(DataType::Enum))
            {
                Some(base) => {
                    let mut new_class = Class::new(class.name, base.clone(), None, true);

                    for var in class.variable {
                        let parsed = parser::Variable {
                            constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32 > 0,
                            data_type: tokenize(&var.r#type.unwrap())?.parse_type()?,
                            name: var.name.unwrap(),
                            initial_value: None,
                            range: Default::default(),
                        };
                        new_class
                            .instance_variables
                            .push(Rc::new(RefCell::new(Variable {
                                data_type: parsed
                                    .data_type
                                    .lint(self, &self.placeholder_file.clone()),
                                variable_type: VariableType::Instance(parser::InstanceVariable {
                                    variable: parsed,
                                    access: parser::Access {
                                        read: None,
                                        write: None,
                                    },
                                }),
                                uses: Vec::new(),
                            })))
                    }

                    // let mut parsed_event = parser::Event {
                    //     name: func.name,
                    //     range: Default::default(),
                    //     event_type: parser::EventType::User((), ()),
                    // };
                    // Ok(Rc::new(RefCell::new(Function::new(
                    //     self, &mut file, parsed, None, None,
                    // ))))

                    for func in class.function {
                        let (returns, arguments, has_vararg) = self.load_proto_function(&func)?;
                        new_class.functions.push(Rc::new(RefCell::new(Function::new(
                            self,
                            &self.placeholder_file.clone(),
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
                        ))));
                    }

                    for event in class.event {
                        let (returns, arguments, has_vararg) = self.load_proto_function(&event)?;
                        if has_vararg {
                            todo!();
                        }
                        new_class.events.push(Rc::new(RefCell::new(Event::new(
                            self,
                            &self.placeholder_file.clone(),
                            parser::Event {
                                name: event.name,
                                range: Default::default(),
                                event_type: parser::EventType::User(returns, arguments),
                            },
                            None,
                            None,
                        ))));
                    }

                    self.builtin_classes.push(Rc::new(RefCell::new(new_class)))
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
            let (returns, arguments, has_vararg) = self.load_proto_function(&func)?;
            let new_func = Rc::new(RefCell::new(Function::new(
                self,
                &self.placeholder_file.clone(),
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
            )));
            self.builtin_functions.push(new_func);
        }

        Ok(())
    }
}
