use multipeek::{multipeek, MultiPeek};

use super::tokenizer_types::{self as tokens, Range};

pub enum ParseError {
    EOF,
    UnexpectedToken,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub trait KeepEOF<T> {
    fn split_eof(self) -> ParseResult<ParseResult<T>>;
}

impl<T> KeepEOF<T> for ParseResult<T> {
    fn split_eof(self) -> ParseResult<ParseResult<T>> {
        match self {
            Err(eof @ ParseError::EOF) => Err(eof),
            rest => Ok(rest),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Decimal(Option<String>),
    Array(Box<DataType>),
    Complex(String, String),
    ID(String),
}

#[derive(Debug, Clone)]
pub struct Access {
    pub read: Option<tokens::AccessType>,
    pub write: Option<tokens::AccessType>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub constant: bool,
    pub data_type: DataType,
    pub name: String,
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
    pub scope: tokens::ScopeModif,
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub is_ref: bool,
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct VariableAccess {
    pub name: String,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub returns: Option<DataType>,
    pub scope_modif: Option<tokens::ScopeModif>,
    pub access: Option<tokens::AccessType>,
    pub name: String,
    pub arguments: Vec<Argument>,

    pub range: Range,
}

#[derive(Debug, Clone)]
pub enum EventType {
    User(Option<DataType>, Vec<Argument>),
    System(String),
}

#[derive(Debug, Clone)]
pub struct Event {
    pub name: String,
    pub event_type: EventType,

    pub range: Range,
}

#[derive(Debug)]
pub struct On {
    pub class: String,
    pub name: String,
}

impl Function {
    pub fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    pub fn conflicts(&self, other: &Function) -> bool {
        self.name == other.name
            && self
                .arguments
                .iter()
                .zip(other.arguments.iter())
                .all(|(self_arg, other_arg)| {
                    self_arg.variable.data_type == other_arg.variable.data_type
                })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
    pub range: Range,
    pub dynamic: bool,
    pub event: bool,
    pub post: bool,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub scope: Option<tokens::ScopeModif>,
    pub name: String,
    pub base: String,
    pub within: Option<String>,
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
    // pub fn get_type(&self) -> DataType {
    //     match &self.lvalue_type {
    //         LValueType::Super => DataType::Unknown,
    //         LValueType::Variable(var) => var.data_type.clone(),
    //         LValueType::Function(func, _) => func.returns.clone(),
    //         LValueType::Method(_, func, _) => func.returns.clone(),
    //         LValueType::Member(_, var) => var.data_type.clone(),
    //         LValueType::Index(var, _) => match var.get_type() {
    //             DataType::Array(data_type) => *data_type,
    //             _ => DataType::Unknown,
    //         },
    //     }
    // }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Literal(tokens::Literal),
    ArrayLiteral(Vec<Expression>),
    Operation(Box<Expression>, tokens::Operator, Box<Expression>),
    IncrementDecrement(Box<Expression>, tokens::Symbol),
    BooleanNot(Box<Expression>),
    Parenthesized(Box<Expression>),
    Create(String),
    CreateUsing(Box<Expression>),
    LValue(LValue),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub range: Range,
}

impl Expression {
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
    //             tokens::Operator::EQ
    //             | tokens::Operator::GT
    //             | tokens::Operator::GTE
    //             | tokens::Operator::LT
    //             | tokens::Operator::LTE
    //             | tokens::Operator::GTLT
    //             | tokens::Operator::OR
    //             | tokens::Operator::AND => DataType::Primitive(tokens::Type::BOOLEAN),
    //             tokens::Operator::PLUS
    //                 if first.get_type() == DataType::Primitive(tokens::Type::STRING) =>
    //             {
    //                 DataType::Primitive(tokens::Type::STRING)
    //             }
    //             _ => first.get_type(),
    //         },
    //         ExpressionType::Create(class) => DataType::Complex(None, class.clone()),
    //         ExpressionType::CreateUsing(expr) => match expr.expression_type {
    //             ExpressionType::Literal(tokens::Literal::STRING) => {
    //                 todo!("Read out the variable")
    //             }
    //             _ => DataType::Complex(None, "powerobject".to_owned()),
    //         },
    //         ExpressionType::BooleanNot(_) => DataType::Primitive(tokens::Type::BOOLEAN),
    //         ExpressionType::Parenthesized(paren) => paren.get_type(),
    //         ExpressionType::LValue(val) => val.get_type(),
    //         ExpressionType::Literal(literal) => match literal {
    //             tokens::Literal::NUMBER => DataType::Primitive(tokens::Type::DECIMAL),
    //             tokens::Literal::DATE => DataType::Primitive(tokens::Type::DATE),
    //             tokens::Literal::TIME => DataType::Primitive(tokens::Type::TIME),
    //             tokens::Literal::STRING => DataType::Primitive(tokens::Type::STRING),
    //             tokens::Literal::BOOLEAN => DataType::Primitive(tokens::Type::BOOLEAN),
    //             tokens::Literal::ENUM => todo!(),
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
    pub catches: Vec<(DataType, String, Vec<Statement>)>,
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

#[derive(Debug)]
pub enum CaseSpecifierType {
    Literals(tokens::Literal),
    To(tokens::Literal, tokens::Literal),
    Is(tokens::Operator, tokens::Literal),
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
    Ancestor(Option<String>, String),
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
    Assignment(LValue, Expression),
    TryCatch(TryCatchStatement),
    Declaration(InstanceVariable),
    ForLoop(ForLoopStatement),
    WhileLoop(WhileLoopStatement),
    Choose(ChooseCaseStatement),
    Return(Option<Expression>),
    Call(CallStatement),
    Exit,
    Continue,
    Empty,
    Error,
}

#[derive(Debug)]
pub struct Statement {
    pub statement_type: StatementType,
    pub range: Range,
}

#[derive(Debug)]
pub enum TopLevelType {
    DatatypeDecl(Class, Vec<InstanceVariable>, Vec<Event>),
    ForwardDecl,
    TypeVariablesDecl(Vec<InstanceVariable>),
    ScopedVariablesDecl(Vec<ScopedVariable>),
    GlobalVariableDecl(Variable),
    ConstantDecl,
    FunctionForwardDecl,
    ExternalFunctions(Vec<Function>),
    FunctionsForwardDecl(Vec<Function>),
    FunctionBody(Function, Vec<Statement>),
    OnBody(On, Vec<Statement>),
    EventBody(Event, Vec<Statement>),
}

#[derive(Debug)]
pub struct TopLevel {
    pub top_level_type: TopLevelType,
    pub range: Range,
}

pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub range: Range,
}
