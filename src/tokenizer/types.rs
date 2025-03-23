use std::fmt;

use strum::Display;
use strum_macros::EnumString;

#[derive(Clone, Copy, EnumString, Debug, PartialEq, Display)]
pub enum Literal {
    NUMBER,
    DATE,
    TIME,
    STRING,
    ENUM,
    BOOLEAN,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq, Display)]
// #[strum(serialize_all = "lowercase")]
pub enum ScopeModif {
    GLOBAL,
    SHARED,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq, Display)]
// #[strum(serialize_all = "lowercase")]
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

impl AccessType {
    pub fn is_read(&self) -> bool {
        match self {
            AccessType::PUBLIC
            | AccessType::PRIVATE
            | AccessType::PROTECTED
            | AccessType::PRIVATEWRITE
            | AccessType::PROTECTEDWRITE
            | AccessType::SYSTEMWRITE => false,

            AccessType::SYSTEMREAD | AccessType::PRIVATEREAD | AccessType::PROTECTEDREAD => true,
        }
    }

    pub fn is_write(&self) -> bool {
        match self {
            AccessType::PUBLIC
            | AccessType::PRIVATE
            | AccessType::PROTECTED
            | AccessType::PRIVATEREAD
            | AccessType::PROTECTEDREAD
            | AccessType::SYSTEMREAD => false,

            AccessType::SYSTEMWRITE | AccessType::PRIVATEWRITE | AccessType::PROTECTEDWRITE => true,
        }
    }

    pub fn is_general(&self) -> bool {
        !self.is_write() && !self.is_read()
    }

    pub fn strictness(&self) -> u8 {
        match self {
            AccessType::PUBLIC => 0,

            AccessType::PROTECTED | AccessType::PROTECTEDREAD | AccessType::PROTECTEDWRITE => 1,

            AccessType::PRIVATE | AccessType::PRIVATEWRITE | AccessType::PRIVATEREAD => 2,

            AccessType::SYSTEMREAD | AccessType::SYSTEMWRITE => 3,
        }
    }
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
    // SQLCA,

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
    ELSE,

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
    //  Non-reserved keywords
    CLOSE,
    OPEN,
    UPDATE,

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
    STATIC,

    // Reserved
    NAMESPACE,
    INTRINSIC,
    WITH,
    _DEBUG,
    ENUMERATED,
    EXTERNAL,
    NATIVE,
    XOR,
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::PLUS => "+",
            Self::MINUS => "-",
            Self::MULT => "*",
            Self::DIV => "/",
            Self::CARAT => "^",

            Self::EQ => "=",
            Self::GT => ">",
            Self::GTE => ">=",
            Self::LT => "<",
            Self::LTE => "<=",
            Self::GTLT => "<>",

            Self::OR => "or",
            Self::AND => "and",
        })
    }
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum IncrDecrOperator {
    PLUSPLUS,
    MINUSMINUS,
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
    SEMICOLON,

    LCURLY,
    RCURLY,
    LBRACE,
    RBRACE,
    LPAREN,
    RPAREN,

    TICK,
    COMMA,
    // SEMI,
    DOTDOTDOT,
    DOT,
}
