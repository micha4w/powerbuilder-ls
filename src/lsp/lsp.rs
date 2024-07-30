use std::{
    cell::RefCell,
    env::current_dir,
    f32::consts::E,
    path::PathBuf,
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
            // TODO class
            _ => self == other || (self.is_numeric() && other.is_numeric()),
        }
    }
}

trait LintableDataType {
    fn lint(&self, file: &mut FileData) -> DataType;
}

impl LintableDataType for parser::DataType {
    fn lint(&self, file: &mut FileData) -> DataType {
        match self {
            parser::DataType::Decimal(precission) => {
                DataType::Decimal(precission.as_ref().map(|str| str.parse().unwrap()))
            }
            parser::DataType::Array(sub_type) => DataType::Array(Box::new(sub_type.lint(file))),
            parser::DataType::Complex(group, name) => match file.find_class(Some(group), &name) {
                Some(class) => DataType::Class(class),
                None => DataType::Unknown,
            },
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
                _ => match file.find_class(None, &id) {
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

struct LintState {
    current_class: Rc<RefCell<Class>>,
    variables: Vec<Variable>,
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
            .find(|var| var.parsed().name.eq_ignore_ascii_case(&self.name))
            .or_else(|| {
                class
                    .find_variable(self, &tokens::AccessType::PRIVATE)
                    .or_else(|| {
                        file.find_variable(self, &tokens::AccessType::PRIVATE)
                            .or_else(|| lsp.find_global_variable(&self.name))
                    })
            }) {
            Some(var) => {
                var.uses.push(self.range);
                var.data_type.clone()
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
            parser::LValueType::This => DataType::Class(state.current_class.clone()),
            parser::LValueType::Super => state
                .current_class
                .borrow()
                .base
                .clone()
                .map_or(DataType::Unknown, DataType::Class),
            parser::LValueType::Parent => match state.current_class.borrow().within.clone() {
                Some(within) => DataType::Class(within),
                None => {
                    file.diagnostics.push(parser::Diagnostic {
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

                match state
                    .current_class
                    .borrow_mut()
                    .find_callable_function(&call.name, &types, &tokens::AccessType::PRIVATE)
                    .or_else(|| lsp.find_global_function(&call.name, &types))
                {
                    Some(func) => func.returns.clone(),
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
                                func.returns.clone()
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
                    DataType::Any | DataType::Unknown => DataType::Unknown,
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
            parser::LValueType::Member(lvalue, member) => {
                let class = lvalue.lint(lsp, file, state);

                match class {
                    DataType::Class(class_rc) => {
                        let mut class = class_rc.borrow_mut();
                        let var = class.find_variable(&member, &tokens::AccessType::PUBLIC); // TODO correct type thing

                        match var {
                            Some(var) => {
                                var.uses.push(member.range);
                                var.data_type.clone()
                            }
                            None => {
                                file.diagnostics.push(parser::Diagnostic {
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
                        file.diagnostics.push(parser::Diagnostic {
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
                            file.diagnostics.push(parser::Diagnostic {
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
                            file.diagnostics.push(parser::Diagnostic {
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
                                file.diagnostics.push(parser::Diagnostic {
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
                    file.diagnostics.push(parser::Diagnostic {
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
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Invalid type, expected boolean".into(),
                        range: expression.range,
                    });
                }

                DataType::Boolean
            }
            parser::ExpressionType::Parenthesized(expression) => expression.lint(lsp, file, state),
            parser::ExpressionType::Create(class) => {
                match file
                    .find_class(None, class)
                    .or_else(|| lsp.find_class(None, class, false))
                {
                    Some(class) => DataType::Class(class),
                    None => {
                        file.diagnostics.push(parser::Diagnostic {
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
                    file.diagnostics.push(parser::Diagnostic {
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
                let data_type = var.variable.data_type.lint(file);

                if let DataType::Unknown = data_type {
                    file.diagnostics.push(parser::Diagnostic {
                        severity: parser::Severity::Error,
                        message: "Type not found".into(),
                        range: var.variable.range,
                    });
                }

                if let Some(initial_value) = &var.variable.initial_value {
                    match initial_value.expression_type {
                        parser::ExpressionType::Literal(literal) => {
                            if !DataType::from(&literal).is_convertible(&data_type) {
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

                state.variables.push(Variable {
                    variable_type: VariableType::Local(var.variable.clone()),
                    data_type,
                    uses: Vec::new(),
                });
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
                    parser::CallType::Super => state.current_class.borrow().base.clone(),
                    parser::CallType::Ancestor(group, name) => {
                        let mut curr = state.current_class.clone();
                        loop {
                            let new_curr = match &curr.borrow().base {
                                Some(base) => {
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
                                None => {
                                    file.diagnostics.push(parser::Diagnostic {
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
        file: &mut FileData,
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
                        data_type: arg.variable.data_type.lint(file),
                        uses: Vec::new(),
                    })
                    .collect();
                returns = ret.as_ref().map_or(DataType::Void, |ret| ret.lint(file));
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
            },
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
            .zip(arguments)
            .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
    }
}

#[derive(Clone, Debug)]
struct Function {
    parsed: parser::Function,

    returns: DataType,
    arguments: Vec<Variable>,

    declaration: Option<Range>,
    definition: Option<Range>,
    uses: Vec<Range>,
}

impl Function {
    fn new(
        file: &mut FileData,
        parsed: parser::Function,
        declaration: Option<Range>,
        definition: Option<Range>,
    ) -> Self {
        Function {
            declaration,
            definition,
            uses: Vec::new(),

            arguments: parsed
                .arguments
                .iter()
                .map(|arg| Variable {
                    variable_type: VariableType::Argument(arg.clone()),
                    data_type: arg.variable.data_type.lint(file),
                    uses: Vec::new(),
                })
                .collect(),
            returns: parsed
                .returns
                .as_ref()
                .map_or(DataType::Void, |ret| ret.lint(file)),

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
    }
}

#[derive(Clone, Debug)]
struct Class {
    name: String,
    base: Option<Rc<RefCell<Class>>>,
    within: Option<Rc<RefCell<Class>>>,

    is_global: bool,

    ons: Vec<Function>,
    events: Vec<Event>,
    functions: Vec<Function>,
    external_functions: Vec<Function>,

    instance_variables: Vec<Variable>,
}

impl Class {
    fn new(
        name: String,
        base: Option<Rc<RefCell<Class>>>,
        within: Option<Rc<RefCell<Class>>>,
        is_global: bool,
    ) -> Class {
        Class {
            name,
            base,
            within,
            is_global,

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
    ) -> Option<&mut Variable> {
        self.instance_variables.iter_mut().find(|var| {
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
    }

    fn find_exact_event(&mut self, event: &Event) -> Option<&mut Event> {
        self.events.iter_mut().find(|func| func.equals(event))
    }

    fn find_conflicting_event(&mut self, event: &Event) -> Option<&mut Event> {
        self.events.iter_mut().find(|func| func.conflicts(event))
    }

    fn find_callable_event(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<&mut Event> {
        self.events.iter_mut().find(|event| {
            event.parsed.name.eq_ignore_ascii_case(name) && event.is_callable(arguments)
        })
    }

    fn find_exact_function(&mut self, function: &Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.equals(function))
            .or_else(|| {
                self.external_functions
                    .iter_mut()
                    .find(|func| func.equals(function))
            })
    }

    fn find_conflicting_function(&mut self, function: &Function) -> Option<&mut Function> {
        self.functions
            .iter_mut()
            .find(|func| func.conflicts(function))
            .or_else(|| {
                self.external_functions
                    .iter_mut()
                    .find(|func| func.conflicts(function))
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
            .find(|func| {
                func.parsed.name.eq_ignore_ascii_case(name)
                    && func.is_callable(arguments, min_access)
            })
            .or_else(|| {
                self.external_functions.iter_mut().find(|func| {
                    func.parsed.name.eq_ignore_ascii_case(name)
                        && func.is_callable(arguments, min_access)
                })
            })
    }
}

struct FileData {
    classes: Vec<Rc<RefCell<Class>>>,
    // Shared with all isntances
    shared_variables: Vec<Variable>,

    diagnostics: Vec<parser::Diagnostic>,
}

impl FileData {
    fn find_variable(
        &mut self,
        variable: &parser::VariableAccess,
        min_access: &tokens::AccessType,
    ) -> Option<&mut Variable> {
        self.shared_variables.iter_mut().find(|var| {
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
    }

    fn find_class(&mut self, group: Option<&String>, name: &String) -> Option<Rc<RefCell<Class>>> {
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
        let mut current_class = Weak::<RefCell<Class>>::new();

        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(class, ..) => {
                    current_class = match self.data.find_class(class.within.as_ref(), &class.name) {
                        Some(class) => Rc::downgrade(&class),
                        None => Rc::downgrade(&lsp.find_builtin_class("powerobject")),
                    }
                }
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
                        let new_func = Function::new(
                            &mut self.data,
                            function.clone(),
                            Some(function.range),
                            None,
                        );

                        let mut state = LintState {
                            current_class: class.clone(),
                            variables: new_func.arguments.clone(),
                            return_type: new_func.returns.clone(),
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });

                        let mut borrow = class.borrow_mut();
                        match borrow.find_exact_function(&new_func) {
                            Some(existing) => {
                                if let Some(definition) = existing.definition {
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
                                } else {
                                    existing.definition = Some(function.range);
                                }
                            }
                            None => {
                                self.data.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Function is missing a Forward Declaration".into(),
                                    range: function.range,
                                });

                                borrow.functions.push(new_func);
                            }
                        }
                    }
                }
                parser::TopLevelType::OnBody(on, statements) => {
                    if let Some(class) = self.data.find_class(None, &on.class) {
                        let mut state = LintState {
                            current_class: class,
                            variables: Vec::new(),
                            return_type: DataType::Void,
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });
                    } else {
                        self.data.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "On Body for non Existing Class".into(),
                            range: top_level.range.clone(),
                        });
                    }
                }
                parser::TopLevelType::EventBody(event, statements) => {
                    if let Some(class) = Self::require_class(
                        &mut self.data,
                        &current_class,
                        "Event Bodies".into(),
                        top_level.range,
                    ) {
                        let new_event =
                            Event::new(&mut self.data, event.clone(), Some(event.range), None);

                        let mut state = LintState {
                            current_class: class.clone(),
                            variables: new_event.arguments.clone(),
                            return_type: new_event.returns.clone(),
                        };

                        statements.iter().for_each(|statement| {
                            statement.lint(lsp, &mut self.data, &mut state);
                        });

                        let mut borrow = class.borrow_mut();
                        match borrow.find_exact_event(&new_event) {
                            Some(existing) => {
                                if let Some(definition) = existing.definition {
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Error,
                                        message: "Event already defined".into(),
                                        range: event.range,
                                    });
                                    self.data.diagnostics.push(parser::Diagnostic {
                                        severity: parser::Severity::Hint,
                                        message: "Already defined here".into(),
                                        range: definition,
                                    });
                                } else {
                                    existing.definition = Some(event.range);
                                }
                            }
                            None => {
                                self.data.diagnostics.push(parser::Diagnostic {
                                    severity: parser::Severity::Error,
                                    message: "Event is missing a Forward Declaration".into(),
                                    range: event.range,
                                });

                                borrow.events.push(new_event);
                            }
                        }
                    }
                }
            }
        }
    }

    fn lint_shallow(&mut self, lsp: &mut LSP) {
        let mut current_class = Weak::<RefCell<Class>>::new();

        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::DatatypeDecl(class, vars, events) => {
                    let within = class.within.as_ref().and_then(|within_name| {
                        let within = self.data.find_class(None, within_name);

                        if within.is_none() {
                            self.data.diagnostics.push(parser::Diagnostic {
                                severity: parser::Severity::Error,
                                message: "Within Class not found".into(),
                                range: top_level.range,
                            });
                        }

                        within
                    });

                    let base = self.data.find_class(None, &class.base);
                    if base.is_none() {
                        self.data.diagnostics.push(parser::Diagnostic {
                            severity: parser::Severity::Error,
                            message: "Base Class not found".into(),
                            range: top_level.range,
                        });
                    }
                    let mut new_class = Class::new(
                        class.name.clone(),
                        base,
                        within,
                        matches!(class.scope, Some(tokens::ScopeModif::GLOBAL)),
                    );

                    for var in vars {
                        new_class.instance_variables.push(Variable {
                            variable_type: VariableType::Instance(var.clone()),
                            data_type: var.variable.data_type.lint(&mut self.data),
                            uses: Vec::new(),
                        });
                    }

                    for event in events {
                        new_class.events.push(Event::new(
                            &mut self.data,
                            event.clone(),
                            Some(event.range),
                            None,
                        ));
                    }

                    let class_rc = Rc::new(RefCell::new(new_class));
                    current_class = Rc::downgrade(&class_rc);
                    self.data.classes.push(class_rc);
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
                            let data_type = var.variable.data_type.lint(&mut self.data);

                            class.borrow_mut().instance_variables.push(Variable {
                                variable_type: VariableType::Instance(var.clone()),
                                data_type,
                                uses: Vec::new(),
                            });
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
                        "Function Forward Declarations".into(),
                        top_level.range,
                    ) {
                        for function in functions {
                            let mut new_func =
                                Function::new(&mut self.data, function.clone(), None, None);

                            let mut borrow = class.borrow_mut();
                            match borrow.find_conflicting_function(&new_func) {
                                Some(func) => {
                                    if func.returns
                                        != function
                                            .returns
                                            .as_ref()
                                            .map_or(DataType::Void, |ret| ret.lint(&mut self.data))
                                    {
                                        self.data.diagnostics.push(parser::Diagnostic {
                                                severity: parser::Severity::Error,
                                                message: "Same function with different return type already exists".into(),
                                                range: function.range,
                                            });
                                        if let Some(declaration) = func.declaration {
                                            self.data.diagnostics.push(parser::Diagnostic {
                                                severity: parser::Severity::Hint,
                                                message: "Function with different return type declared here".into(),
                                                range: declaration,
                                            });
                                        }
                                    }

                                    if let Some(declaration) = func.declaration {
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
                                None => {
                                    new_func.declaration = Some(function.range);
                                    borrow.functions.push(new_func);
                                }
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
                            let mut new_func =
                                Function::new(&mut self.data, function.clone(), None, None);

                            let mut borrow = class.borrow_mut();
                            match borrow.find_exact_function(&new_func) {
                                Some(func) => {
                                    if let Some(declaration) = func.declaration {
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
                                None => {
                                    new_func.declaration = Some(function.range);
                                    borrow.external_functions.push(new_func);
                                }
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
    global_variables: Vec<Variable>,
    global_functions: Vec<Function>,

    builtin_classes: Vec<Rc<RefCell<Class>>>,

    options: LSPOptions,
}

impl LSP {
    fn find_class(
        &mut self,
        group: Option<&String>,
        name: &String,
        include_hidden: bool,
    ) -> Option<Rc<RefCell<Class>>> {
        self.files
            .iter_mut()
            .find_map(|file| {
                file.data
                    .find_class(group, name)
                    .and_then(|class| (include_hidden || class.borrow().is_global).then_some(class))
            })
            .or_else(|| {
                self.builtin_classes
                    .iter()
                    .find(|builtin| builtin.borrow().name.eq_ignore_ascii_case(name))
                    .cloned()
            })
    }

    fn find_builtin_class(&mut self, name: &'static str) -> Rc<RefCell<Class>> {
        // TODO, yay!
        self.builtin_classes[0].clone()
    }

    fn find_global_variable(&mut self, name: &String) -> Option<&mut Variable> {
        self.global_variables
            .iter_mut()
            .find(|var| var.parsed().name.eq_ignore_ascii_case(name))
    }

    fn find_global_function(
        &mut self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<&mut Function> {
        // No need to look for instance functions
        self.global_functions.iter_mut().find(|global| {
            global.parsed.name.eq_ignore_ascii_case(name)
                && global.is_callable(arguments, &tokens::AccessType::PUBLIC)
        })
    }

    pub fn new() -> anyhow::Result<LSP> {
        Ok(LSP {
            files: Vec::new(),
            global_variables: Vec::new(),
            global_functions: Vec::new(),

            builtin_classes: vec![Rc::new(RefCell::new(Class::new(
                "powerobject".into(),
                None,
                None,
                true,
            )))],

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
