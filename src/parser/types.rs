use std::fmt;

use anyhow::anyhow;

use crate::{
    tokenizer::{self, Token},
    types::*,
};

pub enum ParseError {
    UnexpectedToken,
}

impl From<ParseError> for anyhow::Error {
    fn from(value: ParseError) -> Self {
        let err = match value {
            ParseError::UnexpectedToken => "Parser: Unexpected Token",
        };
        anyhow!(err)
    }
}

pub type ParseResult<T> = Result<T, (ParseError, Option<T>)>;
pub type EOFOr<T> = Option<T>;
pub type EOFOrParserResult<T> = EOFOr<ParseResult<T>>;

pub trait ParseResultT<T> {
    fn new(t: T, err: Option<ParseError>) -> Self;

    fn value(self) -> Option<T>;
    fn split(self) -> (Option<T>, Option<ParseError>);
}

impl<T> ParseResultT<T> for ParseResult<T> {
    fn new(t: T, err: Option<ParseError>) -> Self {
        match err {
            Some(err) => Err((err, Some(t))),
            None => Ok(t),
        }
    }

    fn value(self) -> Option<T> {
        match self {
            Ok(t) => Some(t),
            Err((_, t)) => t,
        }
    }

    fn split(self) -> (Option<T>, Option<ParseError>) {
        match self {
            Ok(t) => (Some(t), None),
            Err((err, t)) => (t, Some(err)),
        }
    }
}

// pub trait KeepEOF<T> {
//     fn split_eof(self) -> ParseResult<ParseResult<T>>;
// }

// impl<T> KeepEOF<T> for ParseResult<T> {
//     fn split_eof(self) -> ParseResult<()> {
//         match self {
//             Some(eof @ ParseError::EOF) => Err(eof),
//             rest => Ok(()),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedName {
    pub group: Option<String>,
    pub name: String,
}

impl From<(Option<String>, String)> for GroupedName {
    fn from((group, name): (Option<String>, String)) -> Self {
        Self { group, name }
    }
}

impl From<&(Option<Token>, Token)> for GroupedName {
    fn from((group, name): &(Option<Token>, Token)) -> Self {
        (group, name).into()
    }
}

impl From<(&Option<Token>, &Token)> for GroupedName {
    fn from((group, name): (&Option<Token>, &Token)) -> Self {
        Self::new(
            group.as_ref().map(|g| g.content.clone()),
            name.content.clone(),
        )
    }
}

impl GroupedName {
    pub fn new(group: Option<String>, name: String) -> Self {
        Self { group, name }
    }

    pub fn combine(&self) -> String {
        match &self.group {
            Some(g) => g.clone() + "`" + &self.name,
            None => self.name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeType {
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
    Complex(GroupedName),
    Array(Box<Self>),

    Any,
    Unknown,
    Void,
}

impl DataTypeType {
    pub fn is_numeric(&self) -> bool {
        self.numeric_precedence().is_some()
    }

    pub fn numeric_precedence(&self) -> Option<u8> {
        match self {
            Self::Int => Some(0),
            Self::Uint => Some(1),
            Self::Long => Some(2),
            Self::Ulong => Some(3),
            Self::Longlong => Some(4),
            Self::Longptr => Some(5),
            Self::Real => Some(6),
            Self::Double => Some(7),
            Self::Decimal(_) => Some(8),
            Self::Any => Some(9),
            Self::Unknown => Some(10),

            Self::Blob
            | Self::Boolean
            | Self::Byte
            | Self::Char
            | Self::Date
            | Self::Datetime
            | Self::String
            | Self::Time
            | Self::Complex(_)
            | Self::Array(_)
            | Self::Void => None,
        }
    }
}

impl fmt::Display for DataTypeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Byte => "byte".into(),
            Self::Char => "char".into(),
            Self::String => "string".into(),
            Self::Blob => "blob".into(),

            Self::Date => "date".into(),
            Self::Time => "time".into(),
            Self::Datetime => "datetime".into(),

            Self::Boolean => "boolean".into(),
            Self::Int => "int".into(),
            Self::Uint => "unsigned int".into(),
            Self::Long => "long".into(),
            Self::Ulong => "unsigned long".into(),
            Self::Longlong => "longlong".into(),
            Self::Longptr => "longptr".into(),
            Self::Real => "real".into(),
            Self::Double => "double".into(),
            Self::Decimal(precision) => {
                if let Some(prec) = precision {
                    format!("decimal {{{}}}", prec)
                } else {
                    "decimal".into()
                }
            }
            Self::Complex(grouped_name) => grouped_name.combine(),
            Self::Array(data_type) => data_type.to_string() + "[]",

            Self::Any => "any".into(),
            Self::Unknown => "<Error>".into(),
            Self::Void => "void".into(),
        };

        f.write_str(name.as_str())
    }
}

impl From<&Option<DataType>> for DataTypeType {
    fn from(value: &Option<DataType>) -> Self {
        match value {
            Some(val) => val.data_type_type.clone(),
            None => Self::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType {
    pub data_type_type: DataTypeType,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct Access {
    pub read: Option<tokenizer::AccessType>,
    pub write: Option<tokenizer::AccessType>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub constant: bool,
    pub data_type: DataType,
    pub name: Token,
    pub initial_value: Option<Expression>,

    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct InstanceVariable {
    pub access: Access,
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct ScopedVariable {
    pub scope: tokenizer::ScopeModif,
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub is_ref: bool,
    pub variable: Variable,
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.variable.constant {
            write!(f, "readonly ")?;
        }
        if self.is_ref {
            write!(f, "ref ")?;
        }
        write!(
            f,
            "{} {}",
            self.variable.data_type.data_type_type, self.variable.name.content
        )
    }
}

// #[derive(Debug, Clone)]
// pub enum AccessType {
//     Read,
//     Write,
//     ReadWrite,
// }

#[derive(Debug, Clone)]
pub struct VariableAccess {
    pub name: Token,
    pub is_write: bool,
    // pub access_type: AccessType,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub returns: Option<DataType>,
    pub scope_modif: Option<tokenizer::ScopeModif>,
    pub access: Option<tokenizer::AccessType>,
    pub name: Token,
    pub arguments: Vec<Argument>,
    pub vararg: Option<Token>,

    pub range: Range,
}

#[derive(Debug, Clone)]
pub enum EventType {
    User(Option<DataType>, Vec<Argument>),
    Predefined,
    System(String),
}

#[derive(Debug, Clone)]
pub struct Event {
    pub name: Token,
    pub event_type: EventType,

    pub range: Range,
}

impl Event {
    pub fn get_arguments(&self) -> Option<&Vec<Argument>> {
        match &self.event_type {
            EventType::User(_, args) => Some(&args),
            EventType::Predefined => None,
            EventType::System(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct On {
    pub class: Token,
    pub name: Token,
}

// impl Function {
//     pub fn equals(&self, other: &Function) -> bool {
//         self.returns == other.returns && self.conflicts(other)
//     }

//     pub fn conflicts(&self, other: &Function) -> bool {
//         self.name == other.name
//             && self
//                 .arguments
//                 .iter()
//                 .zip(other.arguments.iter())
//                 .all(|(self_arg, other_arg)| {
//                     self_arg.variable.data_type == other_arg.variable.data_type
//                 })
//     }
// }

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: Token,
    pub arguments: Vec<Expression>,
    pub dynamic: Option<Token>,
    pub event: Option<Token>,
    pub post: Option<Token>,

    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub scope: Option<tokenizer::ScopeModif>,
    pub name: Token,
    pub base: (Option<Token>, Token),
    pub within: Option<(Option<Token>, Token)>,
}

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

#[derive(Debug, Clone)]
pub enum LValueType {
    This,
    Super,
    Parent,
    Variable(VariableAccess),
    Function(FunctionCall),

    Method(Box<LValue>, FunctionCall),
    Member(Box<LValue>, VariableAccess),
    Index(Box<LValue>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub lvalue_type: LValueType,
    pub range: Range,
}
impl LValue {
    fn get_expression_at(&self, pos: &Position) -> Option<EitherOr<&Expression, &LValue>> {
        if !self.range.contains(pos) {
            return None;
        }

        macro_rules! ret_if_contains {
            ( $val:expr ) => {
                if let Some(val) = $val.get_expression_at(pos) {
                    return Some(val);
                }
            };
        }
        macro_rules! ret_if_one_contains {
            ( $vec:expr ) => {
                for val in $vec {
                    ret_if_contains!(val);
                }
            };
        }

        match &self.lvalue_type {
            LValueType::This | LValueType::Super | LValueType::Parent | LValueType::Variable(_) => {
            }

            LValueType::Function(call) => ret_if_one_contains!(&call.arguments),
            LValueType::Method(lvalue, call) => {
                ret_if_contains!(lvalue);
                ret_if_one_contains!(&call.arguments);
            }
            LValueType::Member(lvalue, _) => {
                ret_if_contains!(lvalue);
            }
            LValueType::Index(lvalue, expression) => {
                ret_if_contains!(lvalue);
                ret_if_contains!(expression);
            }
        }

        Some(EitherOr::Right(self))
    }

    fn get_variable_at(&self, pos: &Position) -> Option<&VariableAccess> {
        match &self.lvalue_type {
            LValueType::This | LValueType::Super | LValueType::Parent => {}
            LValueType::Function(..)
            | LValueType::Method(..)
            | LValueType::Member(..)
            | LValueType::Index(..) => {}

            LValueType::Variable(access) => {
                if self.range.contains(pos) {
                    return Some(access);
                };
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Literal(Literal),
    ArrayLiteral(Vec<Expression>),
    Operation(Box<Expression>, tokenizer::Operator, Box<Expression>),
    UnaryOperation(tokenizer::Operator, Box<Expression>),
    IncrementDecrement(Box<Expression>, tokenizer::Symbol),
    BooleanNot(Box<Expression>),
    Parenthesized(Box<Expression>),
    Create(DataType),
    CreateUsing(Box<Expression>),
    LValue(LValue),
    Error,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub range: Range,
}

impl Expression {
    pub fn get_expression_at(&self, pos: &Position) -> Option<EitherOr<&Expression, &LValue>> {
        if !self.range.contains(pos) {
            return None;
        }

        macro_rules! ret_if_contains {
            ( $val:expr ) => {
                if let Some(val) = $val.get_expression_at(pos) {
                    return Some(val);
                }
            };
        }
        macro_rules! ret_if_one_contains {
            ( $vec:expr ) => {
                for val in $vec {
                    ret_if_contains!(val);
                }
            };
        }

        match &self.expression_type {
            ExpressionType::Create(..) | ExpressionType::Literal(..) => {}
            ExpressionType::ArrayLiteral(expressions) => ret_if_one_contains!(expressions),
            ExpressionType::Operation(left, _operator, right) => {
                ret_if_contains!(left);
                ret_if_contains!(right);
            }
            ExpressionType::UnaryOperation(_, expression)
            | ExpressionType::IncrementDecrement(expression, _)
            | ExpressionType::BooleanNot(expression)
            | ExpressionType::Parenthesized(expression)
            | ExpressionType::CreateUsing(expression) => ret_if_contains!(expression),

            ExpressionType::LValue(lvalue) => ret_if_contains!(lvalue),
            ExpressionType::Error => {}
        };

        Some(EitherOr::Left(self))
    }

    fn get_variable_at(&self, pos: &Position) -> Option<&VariableAccess> {
        if let Some(EitherOr::Right(lvalue)) = self.get_expression_at(pos) {
            lvalue.get_variable_at(pos)
        } else {
            None
        }
    }

    // pub fn is_consteval()

    // TODO cache type??
    // pub fn get_type(&self) -> DataType {
    //     match &self.expression_type {
    //         ExpressionType::ArrayLiteral(arr) => {
    //             if arr.is_empty() {
    //                 DataType::Unknown
    //             } else {
    //                 arr[0].get_type()
    //             }
    //         }
    //         ExpressionType::Operation(first, operator, _) => match operator {
    //             tokenizer::Operator::EQ
    //             | tokenizer::Operator::GT
    //             | tokenizer::Operator::GTE
    //             | tokenizer::Operator::LT
    //             | tokenizer::Operator::LTE
    //             | tokenizer::Operator::GTLT
    //             | tokenizer::Operator::OR
    //             | tokenizer::Operator::AND => DataType::Primitive(tokenizer::Type::BOOLEAN),
    //             tokenizer::Operator::PLUS
    //                 if first.get_type() == DataType::Primitive(tokenizer::Type::STRING) =>
    //             {
    //                 DataType::Primitive(tokenizer::Type::STRING)
    //             }
    //             _ => first.get_type(),
    //         },
    //         ExpressionType::Create(class) => DataType::Complex(None, class.clone()),
    //         ExpressionType::CreateUsing(expr) => match expr.expression_type {
    //             ExpressionType::Literal(tokenizer::Literal::STRING) => {
    //                 todo!("Read out the variable")
    //             }
    //             _ => DataType::Complex(None, "powerobject".to_owned()),
    //         },
    //         ExpressionType::BooleanNot(_) => DataType::Primitive(tokenizer::Type::BOOLEAN),
    //         ExpressionType::Parenthesized(paren) => paren.get_type(),
    //         ExpressionType::LValue(val) => val.get_type(),
    //         ExpressionType::Literal(literal) => match literal {
    //             tokenizer::Literal::NUMBER => DataType::Primitive(tokenizer::Type::DECIMAL),
    //             tokenizer::Literal::DATE => DataType::Primitive(tokenizer::Type::DATE),
    //             tokenizer::Literal::TIME => DataType::Primitive(tokenizer::Type::TIME),
    //             tokenizer::Literal::STRING => DataType::Primitive(tokenizer::Type::STRING),
    //             tokenizer::Literal::BOOLEAN => DataType::Primitive(tokenizer::Type::BOOLEAN),
    //             tokenizer::Literal::ENUM => todo!(),
    //         },
    //         ExpressionType::IncrementDecrement(lvalue, _) => lvalue.get_type(),
    //     }
    // }
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub elseif_statements: Vec<(Expression, Vec<Statement>)>,
    pub else_statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct TryCatchStatement {
    pub statements: Vec<Statement>,
    pub catches: Vec<(Statement, Vec<Statement>)>,
    pub finally: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub struct ForLoopStatement {
    pub start: Expression,
    pub stop: Expression,
    pub step: Option<Expression>,
    pub variable: VariableAccess,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct WhileLoopStatement {
    pub condition: Expression,
    pub is_inversed: bool,
    pub is_until: bool,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Literal {
    pub literal_type: tokenizer::Literal,
    pub content: String,
    pub range: Range,
}

#[derive(Debug)]
pub enum CaseSpecifierType {
    Literals(Literal),
    To(Literal, Literal),
    Is(tokenizer::Operator, Literal),
    Else,
}

#[derive(Debug)]
pub struct CaseSpecifier {
    pub specifier_type: CaseSpecifierType,
    pub range: Range,
}

#[derive(Debug)]
pub struct ChooseCaseStatement {
    pub choose: Expression,
    pub cases: Vec<(Vec<CaseSpecifier>, Vec<Statement>)>,
}

#[derive(Debug)]
pub enum CallType {
    Super,
    Ancestor(Option<Token>, Token),
}

#[derive(Debug)]
pub struct CallStatement {
    pub call_type: CallType,
    pub function: FunctionCall,
}

#[derive(Debug)]
pub enum StatementType {
    Expression(Expression),
    If(IfStatement),
    Throw(Expression),
    Destroy(Expression),
    Assignment(LValue, Option<tokenizer::SpecialAssignment>, Expression),
    TryCatch(TryCatchStatement),
    Declaration(InstanceVariable),
    ForLoop(ForLoopStatement),
    WhileLoop(WhileLoopStatement),
    Choose(ChooseCaseStatement),
    Return(Option<Expression>),
    Call(CallStatement),
    Exit,
    Continue,
    SQL,
    Error,
}

#[derive(Debug)]
pub struct Statement {
    pub statement_type: StatementType,
    pub range: Range,
}

impl Statement {
    pub fn get_statement_at(&self, pos: &Position) -> Option<&Statement> {
        if !self.range.contains(pos) {
            return None;
        }

        macro_rules! ret_if_contains {
            ( $val:expr ) => {
                if let Some(val) = $val.get_statement_at(pos) {
                    return Some(val);
                }
            };
        }
        macro_rules! ret_if_one_contains {
            ( $vec:expr ) => {
                for val in $vec {
                    ret_if_contains!(val);
                }
            };
        }

        match &self.statement_type {
            StatementType::Expression(..)
            | StatementType::Throw(..)
            | StatementType::Destroy(..)
            | StatementType::Assignment(..)
            | StatementType::Declaration(..)
            | StatementType::Return(..)
            | StatementType::Call(..)
            | StatementType::Exit
            | StatementType::Continue
            | StatementType::SQL
            | StatementType::Error => {}

            StatementType::If(if_statement) => {
                ret_if_one_contains!(&if_statement.statements);
                ret_if_one_contains!(&if_statement.else_statements);

                for (_expression, statements) in &if_statement.elseif_statements {
                    ret_if_one_contains!(statements);
                }
            }
            StatementType::TryCatch(try_catch_statement) => {
                ret_if_one_contains!(&try_catch_statement.statements);
                if let Some(statements) = &try_catch_statement.finally {
                    ret_if_one_contains!(statements);
                };

                for (variable, statements) in &try_catch_statement.catches {
                    ret_if_contains!(variable);
                    ret_if_one_contains!(statements);
                }
            }
            StatementType::Choose(choose_case_statement) => {
                for (_, statements) in &choose_case_statement.cases {
                    ret_if_one_contains!(statements);
                }
            }
            StatementType::ForLoop(for_loop) => {
                ret_if_one_contains!(&for_loop.statements)
            }
            StatementType::WhileLoop(while_loop) => {
                ret_if_one_contains!(&while_loop.statements)
            }
        };

        Some(self)
    }

    pub fn get_expression_at(&self, pos: &Position) -> Option<EitherOr<&Expression, &LValue>> {
        macro_rules! ret_if_contains {
            ( $val:expr ) => {
                if let Some(expression) = $val.get_expression_at(pos) {
                    return Some(expression);
                }
            };
        }

        let statement = self.get_statement_at(pos)?;

        match &statement.statement_type {
            StatementType::Expression(expression) |
            StatementType::Throw(expression) |
            StatementType::Destroy(expression) |
            StatementType::Return(Some(expression)) => {
                ret_if_contains!(expression);
            }
            StatementType::Assignment(lvalue, _, expression) => {
                ret_if_contains!(lvalue);
                ret_if_contains!(expression);
            }
            StatementType::Declaration(var) => {
                if let Some(expression) = &var.variable.initial_value {
                    ret_if_contains!(expression);
                }
            }
            StatementType::TryCatch(..) |
            StatementType::Return(None) |
            StatementType::Call(..) | // TODO
            StatementType::Exit |
            StatementType::Continue |
            StatementType::SQL |
            StatementType::Error => {},

            StatementType::If(if_statement) => {
                ret_if_contains!(&if_statement.condition);
                for (expression, _) in &if_statement.elseif_statements {
                    ret_if_contains!(expression);
                }
            },
            StatementType::ForLoop(for_loop) => {
                ret_if_contains!(&for_loop.start);
                ret_if_contains!(&for_loop.stop);
                if let Some(step) = &for_loop.step {
                    ret_if_contains!(step);
                };
            },
            StatementType::WhileLoop(while_loop) => {
                ret_if_contains!(&while_loop.condition);
            },
            StatementType::Choose(choose_case) => {
                ret_if_contains!(&choose_case.choose);
            },
        };

        None
    }

    pub fn get_variable_at(
        &self,
        pos: &Position,
    ) -> Option<EitherOr<&InstanceVariable, &VariableAccess>> {
        let statement = self.get_statement_at(pos)?;
        if let Some(EitherOr::Right(lvalue)) = statement.get_expression_at(pos) {
            return lvalue.get_variable_at(pos).map(EitherOr::Right);
        }

        match &statement.statement_type {
            StatementType::Declaration(var) => {
                if var.variable.name.range.contains(pos) {
                    return Some(EitherOr::Left(var));
                }
            }
            StatementType::Expression(_expression) => {}
            StatementType::Throw(_expression) => {}
            StatementType::Destroy(_expression) => {}
            StatementType::Assignment(_lvalue, _assignment, _expression) => {}
            StatementType::If(_if_statement) => {}
            StatementType::TryCatch(_try_catch) => {}
            StatementType::ForLoop(for_loop) => {
                if for_loop.variable.name.range.contains(pos) {
                    return Some(EitherOr::Right(&for_loop.variable));
                }
            }
            StatementType::WhileLoop(_while_loop) => {}
            StatementType::Choose(_choose_case) => {}
            StatementType::Return(_expression) => {}
            StatementType::Call(_call) => todo!(),
            StatementType::Exit => {}
            StatementType::Continue => {}
            StatementType::SQL => {}
            StatementType::Error => {}
        };
        None
    }
}

#[derive(Debug)]
pub struct DatatypeDecl {
    pub class: Class,
    pub variables: Vec<InstanceVariable>,
    pub events: Vec<Event>,

    pub range: Range,
}

#[derive(Debug)]
pub enum TopLevelType {
    ForwardDecl(Vec<DatatypeDecl>),

    ScopedVariableDecl(ScopedVariable),
    ScopedVariablesDecl(Vec<ScopedVariable>),

    DatatypeDecl(DatatypeDecl),
    TypeVariablesDecl(Vec<InstanceVariable>),
    FunctionsForwardDecl(Vec<Function>),
    ExternalFunctions(Vec<Function>),

    FunctionBody(Function, Vec<Statement>),
    EventBody(Event, Vec<Statement>),
    OnBody(On, Vec<Statement>),
}

#[derive(Debug)]
pub struct TopLevel {
    pub top_level_type: TopLevelType,
    pub range: Range,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub range: Range,
}
