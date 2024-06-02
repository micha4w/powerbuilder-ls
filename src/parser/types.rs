use std::fmt;

use strum_macros::EnumString;

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, Default, PartialEq)]
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
    VARIABLE,
    BOOLEAN,
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum ScopeModif {
    GLOBAL,
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
}

#[derive(Clone, Copy, EnumString, Debug, PartialEq)]
pub enum Keyword {
    NOT,

    // Variable Stuff
    SHARED,

    WINDOW,
    INDIRECT,
    VARIABLES,

    FORWARD,

    READONLY,
    REF,

    // Control flow
    FUNCTION,
    SUBROUTINE,

    IF,
    THEN,
    ELSEIF,

    CASE,
    CHOOSE,

    EXIT,
    CONTINUE,
    RETURN,

    FOR,
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

    END,

    // SQL
    IS,
    CLOSE,
    CREATE,
    DESTROY,
    USING,
    SELECT,
    DELETE,
    INSERT,
    DESCRIBE,

    // Stuff
    POST,
    TRIGGER,
    PROTOTYPES,
    TYPE,
    ON,
    TO,
    FROM,
    NULL,
    UPDATE,
    DYNAMIC,
    WITHIN,
    EVENT,
    OPEN,
    GOTO,

    CALL,
    HALT,
    SUPER,
    LIBRARY,
    SYSTEM,
    RPCFUNC,
    ALIAS,
    THROWS,
    AUTOINSTANTIATE,
    DESCRIPTOR,
    SQLCA,
    IMMEDIATE,
    EXECUTE,
    DECLARE,
    PROCEDURE,
    INTO,
    VALUES,
    WHERE,
    COMMIT,
    CURSOR,
    PREPARE,
    FETCH,
    SET,
    CONNECT,
    DISCONNECT,
    CONSTANT,
    SELECTBLOB,
    UPDATEBLOB,
    ROLLBACK,
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    Type(Type),
    ScopeModif(ScopeModif),
    AccessType(AccessType),
    Keyword(Keyword),

    Literal(Literal),
    Symbol(Symbol),
    Operator(Operator),
    SpecialAssignment(SpecialAssignment),

    COMMENT,
    NEWLINE,
    SPACE,
    INVALID,
}

pub struct Token {
    pub token_type: TokenType,
    pub content: String,
    pub range: Range,
    pub error: Option<String>,
}
