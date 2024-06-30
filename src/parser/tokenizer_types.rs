use std::fmt;

use strum_macros::EnumString;

use super::parser_types::{DataType, DataTypeType};

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} - {})", self.start, self.end)
    }
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Type {
    ANY,
    BLOB,
    BOOLEAN,
    BYTE,
    CHAR,
    DATE,
    DATETIME,
    DECIMAL,
    DOUBLE,
    INT,
    LONG,
    LONGLONG,
    LONGPTR,
    REAL,
    STRING,
    TIME,
    UINT,
    ULONG,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Literal {
    NUMBER,
    DATE,
    TIME,
    STRING,
    ENUM,
    BOOLEAN,
}

impl Literal {
    pub fn is_type(&self, data_type: &DataTypeType) -> bool {
        match self {
            Literal::NUMBER => data_type.is_numeric(),
            Literal::DATE => DataTypeType::Primitive(Type::DATE).is_convertible(data_type),
            Literal::TIME => DataTypeType::Primitive(Type::TIME).is_convertible(data_type),
            Literal::STRING => DataTypeType::Primitive(Type::STRING).is_convertible(data_type),
            Literal::BOOLEAN => DataTypeType::Primitive(Type::BOOLEAN).is_convertible(data_type),
            Literal::ENUM => false, // TODO scrape https://docs.appeon.com/pb2022/powerscript_reference
        }
    }
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum ScopeModif {
    GLOBAL,
    SHARED,
    LOCAL,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum AccessType {
    PUBLIC,
    PRIVATE,
    PROTECTED,
    PRIVATEWRITE,
    PRIVATEREAD,
    PROTECTEDREAD,
    PROTECTEDWRITE,
    SYSTEMREAD,
    SYSTEMWRITE,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Keyword {
    NOT,

    // Variable Stuff
    // WINDOW,
    INDIRECT,
    VARIABLES,

    FORWARD,
    TYPE,
    PROTOTYPES,
    WITHIN,
    AUTOINSTANTIATE,
    ALIAS,
    LIBRARY,
    SYSTEM,
    RPCFUNC,

    READONLY,
    REF,
    CONSTANT,

    THIS,
    SUPER,
    PARENT,
    SQLCA,

    // Control flow
    FUNCTION,
    SUBROUTINE,
    ON,
    EVENT,

    CALL,
    POST,
    TRIGGER,

    IF,
    THEN,
    ELSEIF,

    CASE,
    CHOOSE,

    EXIT,
    CONTINUE,
    RETURN,
    THROWS,

    FOR,
    TO,
    STEP,
    NEXT,

    DO,
    WHILE,
    LOOP,
    UNTIL,

    TRY,
    CATCH,
    FINALLY,

    THROW,
    RELEASE,

    GOTO,
    HALT,

    END,

    // SQL
    // Mixed SQL
    XOR,
    CLOSE,
    OPEN,

    COMMIT,
    CONNECT,
    DECLARE,
    DELETE,
    DESCRIBE,
    DISCONNECT,
    EXECUTE,
    FETCH,
    INSERT,
    PREPARE,
    ROLLBACK,
    SELECT,
    SELECTBLOB,
    UPDATE,
    UPDATEBLOB,
    SET,
    CURRENT,
    IS,
    USING,
    NULL,
    FROM,
    INTO,
    VALUES,
    WHERE,
    FIRST,
    PRIOR,
    LAST,
    IMMEDIATE,
    DESCRIPTOR,
    CURSOR,
    PROCEDURE,
    OF,

    CREATE,
    DESTROY,

    DYNAMIC,

    // Reserved
    NAMESPACE,
    INTRINSIC,
    WITH,
    _DEBUG,
    ENUMERATED,
    EXTERNAL,
    NATIVE
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Operator {
    PLUS,
    MINUS,
    MULT,
    DIV,
    CARAT,

    EQ,
    GT,
    GTE,
    LT,
    LTE,
    GTLT,
    OR,
    AND,
}

impl Operator {
    pub fn precedence(&self) -> u8 {
        match self {
            Operator::CARAT => 1,

            Operator::MULT => 2,
            Operator::DIV => 2,

            Operator::PLUS => 3,
            Operator::MINUS => 3,

            Operator::EQ => 4,
            Operator::GT => 4,
            Operator::GTE => 4,
            Operator::LT => 4,
            Operator::LTE => 4,
            Operator::GTLT => 4,

            // NOT => 5,
            Operator::AND => 6,

            Operator::OR => 7,
        }
    }
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum SpecialAssignment {
    PLUSEQ,
    MINUSEQ,
    MULTEQ,
    DIVEQ,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Symbol {
    AT,
    COLON,
    COLONCOLON,

    LCURLY,
    RCURLY,
    LBRACE,
    RBRACE,
    LPAREN,
    RPAREN,

    TICK,
    COMMA,
    SEMI,

    DOTDOTDOT,
    DOT,

    PLUSPLUS,
    MINUSMINUS,
}