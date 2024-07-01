use std::rc::Rc;

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

#[derive(Clone, Debug, PartialEq)]
pub enum DataTypeType {
    Primitive(tokens::Type),
    Complex(Option<String>, String),
    Enum(String), // TODO
    Array(Box<DataType>),
    Void,
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataType {
    pub data_type: DataTypeType,
    pub range: Range,
}

impl DataTypeType {
    pub fn is_numeric(&self) -> bool {
        match self {
            DataTypeType::Primitive(
                tokens::Type::ANY
                | tokens::Type::INT
                | tokens::Type::UINT
                | tokens::Type::LONG
                | tokens::Type::ULONG
                | tokens::Type::LONGLONG
                | tokens::Type::LONGPTR
                | tokens::Type::REAL
                | tokens::Type::DOUBLE
                | tokens::Type::DECIMAL,
            ) => true,
            DataTypeType::Unknown => true,
            _ => false,
        }
    }

    pub fn is_convertible(&self, other: &DataTypeType) -> bool {
        match (self, other) {
            (DataTypeType::Unknown, _) | (_, DataTypeType::Unknown) => true,
            (DataTypeType::Array(self_type), DataTypeType::Array(other_type)) => {
                self_type.data_type.is_convertible(&other_type.data_type)
            }
            _ => self == other || (self.is_numeric() && other.is_numeric()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Access {
    pub read: Option<tokens::AccessType>,
    pub write: Option<tokens::AccessType>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub constant: bool,
    pub access: Access,
    pub data_type: DataType,
    pub name: String,
    pub initial_value: Option<Expression>,

    pub range: Range,
    // declaration: Option<Range>,
    // uses: Vec<Range>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub returns: DataType,
    pub scope_modif: Option<tokens::ScopeModif>,
    pub access: Option<tokens::AccessType>,
    pub name: String,
    pub arguments: Vec<(DataType, String)>,

    pub range: Range,
}

impl Function {
    pub fn is_callable(
        &self,
        other: &Function,
        min_access: &tokens::AccessType,
    ) -> bool {
        self.name == other.name
            && min_access.strictness() >= self.access.map(|access| access.strictness()).unwrap_or(0)
            && self
                .returns
                .data_type
                .is_convertible(&other.returns.data_type)
            && self.arguments.iter().zip(other.arguments.iter()).all(
                |((self_type, _), (other_type, _))| {
                    other_type
                        .data_type
                        .is_convertible(&self_type.data_type)
                },
            )
    }
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
    Super,
    Variable(Rc<Variable>),
    Function(Rc<Function>, Vec<Expression>),

    Method(Box<LValue>, Rc<Function>, Vec<Expression>),
    Member(Box<LValue>, Rc<Variable>),
    Index(Box<LValue>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub lvalue_type: LValueType,
    pub range: Range,
}

impl LValue {
    pub fn get_type(&self) -> DataTypeType {
        match &self.lvalue_type {
            LValueType::Super => DataTypeType::Unknown,
            LValueType::Variable(var) => var.data_type.data_type.clone(),
            LValueType::Function(func, _) => func.returns.data_type.clone(),
            LValueType::Method(_, func, _) => func.returns.data_type.clone(),
            LValueType::Member(_, var) => var.data_type.data_type.clone(),
            LValueType::Index(var, _) => match var.get_type() {
                DataTypeType::Array(data_type) => data_type.data_type,
                _ => DataTypeType::Unknown,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Literal(tokens::Literal),
    ArrayLiteral(Vec<Expression>),
    Operation(Box<Expression>, tokens::Operator, Box<Expression>),
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
    pub fn get_type(&self) -> DataTypeType {
        match &self.expression_type {
            ExpressionType::ArrayLiteral(arr) => {
                if arr.is_empty() {
                    DataTypeType::Unknown
                } else {
                    arr[0].get_type()
                }
            }
            ExpressionType::Operation(first, operator, _) => match operator {
                tokens::Operator::EQ
                | tokens::Operator::GT
                | tokens::Operator::GTE
                | tokens::Operator::LT
                | tokens::Operator::LTE
                | tokens::Operator::GTLT
                | tokens::Operator::OR
                | tokens::Operator::AND => DataTypeType::Primitive(tokens::Type::BOOLEAN),
                tokens::Operator::PLUS
                    if first.get_type() == DataTypeType::Primitive(tokens::Type::STRING) =>
                {
                    DataTypeType::Primitive(tokens::Type::STRING)
                }
                _ => first.get_type(),
            },
            ExpressionType::Create(class) => DataTypeType::Complex(None, class.clone()),
            ExpressionType::CreateUsing(expr) => match expr.expression_type {
                ExpressionType::Literal(tokens::Literal::STRING) => {
                    todo!("Read out the variable")
                }
                _ => DataTypeType::Complex(None, "powerobject".to_owned()),
            },
            ExpressionType::BooleanNot(_) => DataTypeType::Primitive(tokens::Type::BOOLEAN),
            ExpressionType::Parenthesized(paren) => paren.get_type(),
            ExpressionType::LValue(val) => val.get_type(),
            ExpressionType::Literal(literal) => match literal {
                tokens::Literal::NUMBER => DataTypeType::Primitive(tokens::Type::DECIMAL),
                tokens::Literal::DATE => DataTypeType::Primitive(tokens::Type::DATE),
                tokens::Literal::TIME => DataTypeType::Primitive(tokens::Type::TIME),
                tokens::Literal::STRING => DataTypeType::Primitive(tokens::Type::STRING),
                tokens::Literal::BOOLEAN => DataTypeType::Primitive(tokens::Type::BOOLEAN),
                tokens::Literal::ENUM => todo!(),
            },
        }
    }
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub else_statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct IncrementDecrementStatement {
    pub value: LValue,
    pub operator: tokens::Symbol,
}

#[derive(Debug)]
pub struct TryCatchStatement {
    pub statements: Vec<Statement>,
    pub catches: Vec<(Variable, Vec<Statement>)>,
}

#[derive(Debug)]
pub struct ForLoopStatement {
    pub start: Expression,
    pub stop: Expression,
    pub step: Option<Expression>,
    pub variable: Variable,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct WhileLoopStatement {
    pub condition: Expression,
    pub is_do_while: bool,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ChooseCaseStatement {
    pub choose: Expression,
    pub cases: Vec<(tokens::Literal, Vec<Statement>)>,
}
#[derive(Debug)]
pub enum StatementType {
    IncrementDecrement(IncrementDecrementStatement),
    Expression(Expression),
    If(IfStatement),
    Throw(Expression),
    Assignment(LValue, Expression),
    TryCatch(TryCatchStatement),
    Declaration(Variable),
    ForLoop(ForLoopStatement),
    WhileLoop(WhileLoopStatement),
    Choose(ChooseCaseStatement),
    Return(Expression),
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
    DatatypeDecl(Option<tokens::ScopeModif>, Vec<Rc<Variable>>),
    ForwardDecl,
    TypeVariablesDecl(Vec<Rc<Variable>>),
    GlobalVariablesDecl(Rc<Variable>),
    VariableDecl(Rc<Variable>),
    ConstantDecl,
    FunctionForwardDecl,
    FunctionsForwardDecl(Vec<Rc<Function>>),
    FunctionBody(Rc<Function>, Vec<Statement>),
    OnBody(String, Vec<Statement>),
    EventBody(Rc<Function>, Vec<Statement>),
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
