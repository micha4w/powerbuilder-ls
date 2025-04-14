use std::fmt::{self, Debug};

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
    pub fn simple(name: String) -> Self {
        Self { group: None, name }
    }
}

impl fmt::Display for GroupedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(g) = &self.group {
            write!(f, "{}`", g)?;
        }
        write!(f, "{}", self.name)
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

    pub fn grouped_name(&self) -> GroupedName {
        match self {
            Self::Complex(grouped_name) => grouped_name.clone(),
            _ => GroupedName::simple(self.to_string()),
        }
    }

    pub fn wrap_variable(&self, variable: &String) -> String {
        match self {
            Self::Array(data_type) => format!("{} {}[]", data_type, variable),
            _ => format!("{} {}", self, variable),
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
            Self::Uint => "unsignedint".into(),
            Self::Long => "long".into(),
            Self::Ulong => "unsignedlong".into(),
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
            Self::Complex(grouped_name) => grouped_name.to_string(),
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
    pub access: VariableAccess,
    pub initial_value: Option<Expression>,

    pub range: Range,
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.constant {
            write!(f, "constant ")?;
        }
        write!(
            f,
            "{}",
            self.data_type
                .data_type_type
                .wrap_variable(&self.access.name.content)
        )?;
        if let Some(init) = &self.initial_value {
            write!(f, " = {}", init.expression_type)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct InstanceVariable {
    pub access: Access,
    pub variable: Variable,
}

impl fmt::Display for InstanceVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.access.read, &self.access.write) {
            (Some(read), Some(write)) => {
                if read == write {
                    write!(f, "{} ", read)?;
                } else if write.is_general() {
                    write!(f, "{} {} ", write, read)?;
                } else {
                    write!(f, "{} {} ", read, write)?;
                }
            }
            (Some(acc), None) | (None, Some(acc)) => {
                write!(f, "{} ", acc)?;
            }
            (None, None) => {}
        };
        write!(f, "{}", self.variable)
    }
}

#[derive(Debug, Clone)]
pub struct ScopedVariable {
    pub scope: tokenizer::ScopeModif,
    pub variable: Variable,
}

impl fmt::Display for ScopedVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.scope, self.variable)
    }
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
            self.variable.data_type.data_type_type, self.variable.access.name.content
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(scope) = &self.scope_modif {
            write!(f, "{} ", scope)?;
        };
        if let Some(access) = &self.access {
            write!(f, "{} ", access)?;
        };
        if let Some(ret) = &self.returns {
            write!(f, "function {} ", ret.data_type_type)?;
        } else {
            write!(f, "subroutine ")?;
        }

        write!(
            f,
            "{} ({}",
            self.name.content,
            self.arguments
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if self.vararg.is_some() {
            write!(f, ", ...")?;
        }
        write!(f, ")")
    }
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

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event ")?;

        match &self.event_type {
            EventType::User(returns, args) => {
                if let Some(ret) = &returns {
                    write!(f, "type {} ", ret.data_type_type)?;
                }
                write!(f, "{} ", self.name.content)?;
                if !args.is_empty() {
                    write!(
                        f,
                        "({})",
                        args.iter()
                            .map(|arg| arg.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                } else {
                    write!(f, "( )")?;
                }
                Ok(())
            }
            EventType::System(event) => write!(f, "{} {}", self.name.content, event),
            EventType::Predefined => write!(f, "{}", self.name.content),
        }
    }
}

#[derive(Debug)]
pub struct On {
    pub class: Token,
    pub name: Token,
}

impl fmt::Display for On {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "on {}.{}", self.class.content, self.name.content)
    }
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
    pub name: DataType,
    pub base: DataType,
    pub within: Option<DataType>,
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
    SQLAccess(Token, Box<LValue>),
}

fn join_expressions(vec: &Vec<Expression>) -> String {
    vec.iter()
        .map(|expr| expr.expression_type.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

impl fmt::Display for LValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValueType::This => write!(f, "this"),
            LValueType::Super => write!(f, "super"),
            LValueType::Parent => write!(f, "parent"),
            LValueType::Variable(variable_access) => write!(f, "{}", variable_access.name.content),
            LValueType::Function(function_call) => write!(
                f,
                "{}({})",
                function_call.name.content,
                join_expressions(&function_call.arguments)
            ),
            LValueType::Method(lvalue, function_call) => {
                write!(
                    f,
                    "{}.{}({})",
                    lvalue.lvalue_type,
                    function_call.name.content,
                    join_expressions(&function_call.arguments)
                )
            }
            LValueType::Member(lvalue, variable_access) => {
                write!(f, "{}.{}", lvalue.lvalue_type, variable_access.name.content)
            }
            LValueType::Index(lvalue, expression) => {
                write!(f, "{}[{}]", lvalue.lvalue_type, expression.expression_type)
            }
            LValueType::SQLAccess(_, lvalue) => {
                write!(f, ":{}", lvalue.lvalue_type)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub lvalue_type: LValueType,
    pub range: Range,
}

impl LValue {
    pub fn set_write(&mut self) -> Option<(String, Range)> {
        match &mut self.lvalue_type {
            LValueType::Variable(ref mut access) => {
                access.is_write = true;
                None
            }
            LValueType::Member(lvalue, ref mut access) => {
                lvalue.set_write();
                access.is_write = true;
                None
            }
            LValueType::SQLAccess(_, lvalue) => lvalue.set_write(),
            _ => Some((
                "Can only assign to LValue of type Variable or Member".into(),
                self.range,
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Literal(Literal),
    ArrayLiteral(Vec<Expression>),
    Operation(Box<Expression>, tokenizer::Operator, Box<Expression>),
    UnaryOperation(tokenizer::Operator, Box<Expression>),
    IncrementDecrement(Box<Expression>, tokenizer::IncrDecrOperator),
    BooleanNot(Box<Expression>),
    Parenthesized(Box<Expression>),
    Create(DataType),
    CreateUsing(Box<Expression>),
    LValue(LValue),
    Error,
}

impl fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Literal(literal) => {
                write!(f, "{}({})", literal.literal_type, literal.content)
            }
            Self::ArrayLiteral(expressions) => write!(f, "{{ {} }}", join_expressions(expressions)),
            Self::Operation(left, operator, right) => write!(
                f,
                "{} {} {}",
                left.expression_type, operator, right.expression_type
            ),
            Self::UnaryOperation(operator, expression) => {
                write!(f, "{} {}", operator, expression.expression_type)
            }
            Self::IncrementDecrement(expression, operator) => write!(
                f,
                "{} {}",
                expression.expression_type,
                match operator {
                    tokenizer::IncrDecrOperator::PLUSPLUS => "++",
                    tokenizer::IncrDecrOperator::MINUSMINUS => "--",
                }
            ),
            Self::BooleanNot(expression) => write!(f, "not {}", expression.expression_type),
            Self::Parenthesized(expression) => write!(f, "({})", expression.expression_type),
            Self::Create(data_type) => write!(f, "create {}", data_type.data_type_type),
            Self::CreateUsing(expression) => {
                write!(f, "create using {}", expression.expression_type)
            }
            Self::LValue(lvalue) => write!(f, "{}", lvalue.lvalue_type),
            Self::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub range: Range,
}

impl Expression {

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

    pub fn set_write(&mut self) -> Option<(String, Range)> {
        match self.expression_type {
            ExpressionType::LValue(ref mut lvalue) => lvalue.set_write(),
            _ => Some(("Cannot assign to non-LValue".into(), self.range)),
        }
    }
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

pub type UsingTranscation = Option<Token>;

#[derive(Debug)]
pub struct SQLDeclareProcedureStatement {
    pub procedure_name: Token,
    pub stored_procedure_name: Token,
    pub params: Vec<(Token, Expression)>,
    pub transaction: Option<Token>,
}

#[derive(Debug)]
pub struct SQLSelectStatement {
    pub is_blob: bool,
    pub fields: Vec<Expression>,
    pub intos: Vec<LValue>,
    pub table: Token,
    pub clause: Option<Expression>,
    pub transaction: UsingTranscation,
}

#[derive(Debug)]
pub struct SQLInsertStatement {
    pub table: Token,
    pub fields: Vec<LValue>,
    pub values: Vec<Vec<Expression>>,
    pub transaction: UsingTranscation,
}

#[derive(Debug)]
pub struct SQLUpdateStatement {
    pub is_blob: bool,
    pub table: Token,
    pub set: Expression,
    pub clause: EitherOr<(Expression, UsingTranscation), Token>,
}

#[derive(Debug)]
pub enum SQLStatement {
    OPEN(Token),
    CLOSE(Token),
    CONNECT(UsingTranscation),
    DISCONNECT(UsingTranscation),

    COMMIT(UsingTranscation),
    DECLARE_CURSOR(Token, SQLSelectStatement),
    DECLARE_PROCEDURE(SQLDeclareProcedureStatement),
    EXECUTE(Token),
    FETCH(Token, Vec<LValue>),
    ROLLBACK(UsingTranscation),

    DELETE(Token, Expression, UsingTranscation),
    DELETE_OF_CURSOR(Token, Token),
    INSERT(SQLInsertStatement),
    SELECT(SQLSelectStatement),
    UPDATE(SQLUpdateStatement),
}

#[derive(Debug)]
pub enum StatementType {
    SQL(SQLStatement),
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
    Error,
}

#[derive(Debug)]
pub struct Statement {
    pub statement_type: StatementType,
    pub range: Range,
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
