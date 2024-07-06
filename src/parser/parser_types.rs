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
pub enum DataType {
    Primitive(tokens::Type),
    Complex(Option<String>, String),
    Enum(String), // TODO
    Array(Box<DataType>),
    Void,
    Unknown,
}

impl DataType {
    pub fn is_numeric(&self) -> bool {
        match self {
            DataType::Primitive(
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
            DataType::Unknown => true,
            _ => false,
        }
    }

    pub fn numeric_precedence(&self) -> Option<u8> {
        match self {
            DataType::Primitive(primitive) => match primitive {
                tokens::Type::INT => Some(0),
                tokens::Type::UINT => Some(1),
                tokens::Type::LONG => Some(2),
                tokens::Type::ULONG => Some(3),
                tokens::Type::LONGLONG => Some(4),
                tokens::Type::LONGPTR => Some(5),
                tokens::Type::REAL => Some(6),
                tokens::Type::DOUBLE => Some(7),
                tokens::Type::DECIMAL => Some(8),
                tokens::Type::ANY => Some(9),
                tokens::Type::BLOB
                | tokens::Type::BOOLEAN
                | tokens::Type::BYTE
                | tokens::Type::CHAR
                | tokens::Type::DATE
                | tokens::Type::DATETIME
                | tokens::Type::STRING
                | tokens::Type::TIME => None,
            },
            DataType::Unknown => Some(10),
            _ => None,
        }
    }

    pub fn is_convertible(&self, other: &DataType) -> bool {
        match (self, other) {
            (DataType::Unknown, _) | (_, DataType::Unknown) => true,
            (DataType::Array(self_type), DataType::Array(other_type)) => {
                self_type.is_convertible(&other_type)
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
    pub fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    pub fn conflicts(&self, other: &Function) -> bool {
        self.name == other.name
            && self
                .arguments
                .iter()
                .zip(other.arguments.iter())
                .all(|((self_type, _), (other_type, _))| other_type == self_type)
    }

    pub fn is_callable(&self, other: &Function, min_access: &tokens::AccessType) -> bool {
        self.name == other.name
            && min_access.strictness() >= self.access.map(|access| access.strictness()).unwrap_or(0)
            && self.returns.is_convertible(&other.returns)
            && self
                .arguments
                .iter()
                .zip(other.arguments.iter())
                .all(|((self_type, _), (other_type, _))| other_type.is_convertible(&self_type))
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
    pub catches: Vec<(Variable, Vec<Statement>)>,
}

#[derive(Debug)]
pub struct ForLoopStatement {
    pub start: Expression,
    pub stop: Expression,
    pub step: Option<Expression>,
    pub variable: Rc<Variable>,
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
pub enum StatementType {
    Expression(Expression),
    If(IfStatement),
    Throw(Expression),
    Assignment(LValue, Expression),
    TryCatch(TryCatchStatement),
    Declaration(Rc<Variable>),
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
    ExternalFunctions(Vec<Rc<Function>>),
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
