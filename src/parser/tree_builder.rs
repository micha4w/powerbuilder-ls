use super::types::*;
use defer::defer;
use std::{iter::Peekable, os::linux::raw::stat, rc::Rc};

pub enum TopLevel {
    DatatypeDecl,
    AccessModif,
    ForwardDecl,
    TypeVariablesDecl,
    GlobalVariablesDecl,
    VariableDecl,
    ConstantDecl,
    FunctionForwardDecl,
    FunctionsForwardDecl,
    FunctionBody,
    OnBody,
    EventBody,
}

#[derive(PartialEq)]
enum DataType {
    Primitive(Type),
    Complex(String),
    Unknown,
}

struct Variable {
    data_type: DataType,
    name: String,
    constant: bool,

    declaration: Range,
    definition: Range,
}

struct Function {
    returns: Option<DataType>,
    scope_modif: Option<ScopeModif>,
    access: Option<AccessType>,
    name: String,
    arguments: Vec<(DataType, String)>,
    declaration: Range,
    definition: Range,

    variables: Vec<Rc<Variable>>,
    statements: Vec<Statement>,
}

enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

enum SubField {
    Function(Rc<Function>),
    Variable(Rc<Variable>),
}

struct Field {
    sub_fields: Vec<SubField>,
}

impl Field {
    fn get_type(&self) -> Option<DataType> {
        self.sub_fields
            .last()
            .map(|field| match field {
                SubField::Function(func) => func.returns,
                SubField::Variable(var) => Some(var.data_type),
            })
            .flatten()
    }
}

struct ArrayDefinition {
    data_type: DataType,
}

enum ExpressionType {
    Literal(Literal),
    ArrayDefinition(ArrayDefinition),
    FunctionCall(Field),
    Operation(Box<Expression>, Operator, Box<Expression>), // TODO boolean not
    Parenthesized(Box<Expression>),
}

struct Expression {
    expression_type: ExpressionType,
    range: Range,
}

struct IfStatement {
    condition: Expression,
    statements: Vec<Statement>,
    else_statements: Vec<Statement>,
}

struct IncrementDecrementStatement {
    variable: Rc<Variable>,
    operator: Operator,
}

struct TryCatchStatement {
    statements: Vec<Statement>,
    catches: Vec<(Rc<Variable>, Statement)>,
}

struct ForLoopStatement {
    start: usize,
    stop: usize,
    step: Option<usize>,
    variable: Rc<Variable>,
    statements: Vec<Statement>,
}

struct WhileLoopStatement {
    condition: Expression,
    is_do_while: bool,
    statements: Vec<Statement>,
}

struct ChooseCaseStatement {
    field: Field,
    statements: Vec<(Literal, Statement)>,
}
enum StatementType {
    IncrementDecrement(IncrementDecrementStatement),
    Field(Field),
    SimpleIf(Expression, Box<Statement>),
    If(IfStatement),
    Throw(Expression),
    Assignment(Field, Expression),
    TryCatch(TryCatchStatement),
    Declaration(Rc<Variable>),
    ForLoop(ForLoopStatement),
    WhileLoop(WhileLoopStatement),
    Choose(ChooseCaseStatement),
    Return(Expression),
    Exit,
    Continue,
    Empty,
    // CALL
}

struct Statement {
    statement_type: StatementType,
    range: Range,
}

struct Diagnostic {
    severity: Severity,
    message: String,
    range: Range,
}

struct File {
    // types
    instance_variables: Vec<Rc<Variable>>,
    functions: Vec<Rc<Function>>,
    diagnostics: Vec<Diagnostic>,
}

pub struct TreeBuilder<'a> {
    tokens: Peekable<&'a mut dyn Iterator<Item = Token>>,
    file: File,
    function: Option<Rc<Function>>,
}

impl TreeBuilder<'_> {
    pub fn new<'a>(tokens: &'a mut dyn Iterator<Item = Token>) -> TreeBuilder<'a> {
        TreeBuilder {
            tokens: tokens.peekable(),
            file: File {
                instance_variables: Vec::new(),
                functions: Vec::new(),
                diagnostics: Vec::new(),
            },
            function: None,
        }
    }

    fn consume_line(&mut self) -> Option<()> {
        loop {
            match self.tokens.next()?.token_type {
                TokenType::NEWLINE => break Some(()),
                _ => {}
            }
        }
    }

    fn expect(&mut self, token_type: TokenType) -> Option<Token> {
        let token = self.tokens.next()?;
        if token.token_type == token_type {
            Some(token)
        } else {
            self.file.diagnostics.push(Diagnostic {
                severity: Severity::Error,
                message: format!("[Parser] Expected {:?}", token_type),
                range: token.range,
            });
            if token.token_type != TokenType::NEWLINE {
                self.consume_line();
            }
            None
        }
    }

    fn find_variable(&self, name: &String) -> Option<Rc<Variable>> {
        if let Some(function) = self.function {
            let local = function.variables.iter().find(|var| &var.name == name);
            if let Some(_) = local {
                return local.cloned();
            }
        }

        let instance = self
            .file
            .instance_variables
            .iter()
            .find(|var| &var.name == name);
        if let Some(_) = instance {
            return instance.cloned();
        }
        None
    }

    fn find_function(
        &self,
        name: &String,
        returns: &Option<DataType>,
        arguments: &Vec<DataType>,
    ) -> Option<Rc<Function>> {
        // TODO global functions
        let instance = self.file.functions.iter().find(|func| {
            &func.name == name
                && (returns == &Some(DataType::Unknown) || returns == &func.returns)
                && func.arguments.iter().zip(arguments.iter()).all(
                    |((existing_type, _), function_type)| {
                        function_type == &DataType::Unknown || function_type == existing_type
                    },
                )
        });
        if let Some(_) = instance {
            return instance.cloned();
        }
        None
    }

    fn parse_type(&mut self) -> Option<DataType> {
        let token = self.tokens.next()?;
        match token.token_type {
            TokenType::Type(data_type) => Some(DataType::Primitive(data_type)),
            TokenType::Literal(Literal::VARIABLE) => Some(DataType::Complex(token.content)),
            _ => todo!("Expected a Type"),
        }
    }

    fn parse_field(&mut self) -> Option<Field> {
        let mut field = Field {
            sub_fields: Vec::new(),
        };

        let mut first = true;
        loop {
            if !first {
                match self.tokens.peek()?.token_type {
                    TokenType::Symbol(Symbol::DOT) => {}
                    _ => break,
                }
            }

            let name = self.expect(TokenType::Literal(Literal::VARIABLE))?;

            match self.tokens.peek()?.token_type {
                TokenType::Symbol(Symbol::LPAREN) => {
                    let mut arguments = Vec::new();
                    loop {
                        match self.tokens.peek()?.token_type {
                            TokenType::Symbol(Symbol::RPAREN) => {
                                self.tokens.next();
                            }
                            _ => {}
                        };

                        let mut sub_field = self.parse_field()?;

                        // TODO error when returns void
                        arguments.push(sub_field.get_type().unwrap_or(DataType::Unknown));

                        match self.tokens.peek()?.token_type {
                            TokenType::Symbol(Symbol::COMMA) => {
                                self.tokens.next();
                            }
                            TokenType::Symbol(Symbol::RPAREN) => {
                                self.tokens.next();
                                break;
                            }
                            _ => todo!("Expected ',' or ')'"),
                        }
                    }

                    // TODO if another class is used, parse the forward definitions of that file
                    // TODO do the find_function on another file
                    match self.find_function(&name.content, &Some(DataType::Unknown), &arguments) {
                        Some(func) => field.sub_fields.push(SubField::Function(func)),
                        None => field.sub_fields.push(SubField::Function(Rc::new(Function {
                            name: name.content,
                            declaration: Range::default(),
                            definition: Range::default(),
                            returns: Some(DataType::Unknown),
                            scope_modif: None,
                            access: None,
                            arguments: arguments.iter().map(|arg| (*arg, "".to_owned())).collect(),
                            variables: Vec::new(),
                            statements: Vec::new(),
                        }))),
                    };
                }
                _ => match self.find_variable(&name.content) {
                    Some(var) => {
                        field.sub_fields.push(SubField::Variable(var));
                    }
                    None => {
                        field.sub_fields.push(SubField::Variable(Rc::new(Variable {
                            data_type: DataType::Unknown,
                            name: name.content,
                            constant: false,
                            declaration: Range::default(),
                            definition: Range::default(),
                        })));
                    }
                },
            }
        }

        Some(field)
    }

    fn parse_function_header(&mut self) -> Option<Function> {
        let mut returns = None;
        let mut scope_modif = None;
        let mut access = None;
        let mut arguments = Vec::new();
        let name;
        let start = self.tokens.peek()?.range.start;

        if let TokenType::AccessType(_) = &self.tokens.peek()?.token_type {
            match self.tokens.next()?.token_type {
                TokenType::AccessType(_access) => access = Some(_access),
                _ => {}
            };
        }

        if let TokenType::ScopeModif(_) = &self.tokens.peek()?.token_type {
            match self.tokens.next()?.token_type {
                TokenType::ScopeModif(_scope) => scope_modif = Some(_scope),
                _ => {}
            };
        }

        match self.tokens.peek()?.token_type {
            TokenType::Keyword(Keyword::FUNCTION) => {
                returns = self.parse_type();
                self.tokens.next();
            }
            TokenType::Keyword(Keyword::SUBROUTINE) => {
                self.tokens.next();
            }
            _ => todo!("Expected either Fucntion or Subroutine"),
        }

        name = self.expect(TokenType::Literal(Literal::VARIABLE))?.content;
        self.expect(TokenType::Symbol(Symbol::LPAREN))?;
        loop {
            match self.tokens.peek()?.token_type {
                TokenType::Symbol(Symbol::RPAREN) => {
                    self.tokens.next();
                }
                TokenType::Symbol(Symbol::DOTDOTDOT) => {
                    self.tokens.next();
                    self.expect(TokenType::Symbol(Symbol::RPAREN))?;
                }
                _ => {}
            };

            let data_type = self.parse_type()?;
            let name = self.expect(TokenType::Literal(Literal::VARIABLE))?.content;
            arguments.push((data_type, name));

            match self.tokens.peek()?.token_type {
                TokenType::Symbol(Symbol::COMMA) => {
                    self.tokens.next();
                }
                TokenType::Symbol(Symbol::RPAREN) => {
                    self.tokens.next();
                    break;
                }
                _ => todo!("Expected ',' or ')'"),
            }
        }
        self.expect(TokenType::NEWLINE);

        let end = self.tokens.peek()?.range.start;

        Some(Function {
            returns,
            scope_modif,
            access,
            name,
            arguments,
            declaration: Range { start, end },
            definition: Range::default(),
            variables: Vec::new(),
            statements: Vec::new(),
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let mut start;
        let mut statement = loop {
            start = self.tokens.peek()?.range;
            match self.tokens.peek()?.token_type {
                TokenType::Keyword(Keyword::CONSTANT)
                | TokenType::Type(_)
                | TokenType::Literal(Literal::VARIABLE) => {
                    let constant;
                    match self.tokens.peek()?.token_type {
                        TokenType::Keyword(Keyword::CONSTANT) => {
                            constant = true;
                            self.tokens.next()?;
                        }
                        _ => {
                            constant = false;
                        }
                    }
                    let token = self.tokens.peek()?;
                    let data_type;
                    match token {
                        Token {
                            token_type: TokenType::Type(primitive_type),
                            ..
                        } => {
                            data_type = DataType::Primitive(*primitive_type);
                            self.tokens.next()?;
                        }
                        token @ Token {
                            token_type: TokenType::Literal(Literal::VARIABLE),
                            ..
                        } => {
                            let field = self.parse_field()?;
                            data_type = DataType::Complex(token.content.to_lowercase());
                        }
                        _ => todo!("Expected a type"),
                    }

                    let name = self.expect(TokenType::Literal(Literal::VARIABLE))?;

                    break Statement {
                        statement_type: StatementType::Declaration(Rc::new(Variable {
                            data_type,
                            name: name.content,
                            constant,
                            declaration: Range {
                                start: start.start,
                                end: name.range.end,
                            },
                            definition: Range::default(),
                        })),
                        range: Range::default(),
                    };
                }
                TokenType::Keyword(_) => todo!("if while and stuff"),
                TokenType::COMMENT => {
                    self.tokens.next()?;
                    continue;
                }

                TokenType::NEWLINE => {
                    break Statement {
                        statement_type: StatementType::Empty,
                        range: Range::default(),
                    }
                }
                _ => {
                    let token = self.tokens.next()?;
                    self.file.diagnostics.push(Diagnostic {
                        severity: Severity::Error,
                        message: format!("[Parser] Unexpected {:?}", token.token_type),
                        range: token.range,
                    });
                    break Statement {
                        statement_type: StatementType::Empty,
                        range: Range::default(),
                    };
                }
            }
        };
        let new_line = self.tokens.next()?;
        statement.range = Range {
            start: start.start,
            end: new_line.range.end,
        };
        Some(statement)
    }

    fn parse_function(&mut self) -> Option<Rc<Function>> {
        let mut function = Rc::new(self.parse_function_header()?);
        self.function = Some(function);
        defer!(self.function = None);

        loop {
            let token_type = self.tokens.peek()?.token_type.clone();
            match token_type {
                TokenType::Keyword(Keyword::FORWARD) => {
                    self.expect(TokenType::Keyword(Keyword::END))?;
                    self.expect(TokenType::NEWLINE)?;
                    break;
                }
                _ => {
                    let statement = self.parse_statement()?;
                    if let Statement{statement_type: StatementType::Declaration(var), ..} = statement {
                        function.variables.push(var);
                    }
                    function.statements.push(statement);
                }
            };
        }

        Some(function)
    }

    fn parse_top_level(&mut self) -> Option<()> {
        match self.tokens.peek()?.token_type {
            TokenType::Keyword(Keyword::FORWARD) => {
                self.tokens.next();
                match self.tokens.next()?.token_type {
                    TokenType::Keyword(Keyword::PROTOTYPES) => loop {
                        self.expect(TokenType::NEWLINE);

                        match self.tokens.peek()?.token_type {
                            _ => {
                                let function = self.parse_function_header()?;
                                match self.file.functions.iter().find(|existing| {
                                    existing.name == function.name
                                        && existing.returns == function.returns
                                        && existing
                                            .arguments
                                            .iter()
                                            .zip(function.arguments.iter())
                                            .all(|((existing_type, _), (function_type, _))| {
                                                existing_type == function_type
                                            })
                                }) {
                                    Some(existing) => {
                                        self.file.diagnostics.push(Diagnostic {
                                            severity: Severity::Warning,
                                            message: format!(
                                                "Function already declared at {}",
                                                existing.declaration.start
                                            ),
                                            range: function.declaration,
                                        });
                                        self.file.diagnostics.push(Diagnostic {
                                            severity: Severity::Hint,
                                            message: "Function already declared here".to_owned(),
                                            range: existing.declaration,
                                        });
                                    }
                                    None => {
                                        self.file.functions.push(function);
                                    }
                                }
                            }

                            TokenType::Keyword(Keyword::PROTOTYPES) => {
                                if self.expect(TokenType::Keyword(Keyword::END)).is_some() {
                                    self.expect(TokenType::NEWLINE);
                                }
                            }
                        };
                    },
                    TokenType::NEWLINE => {
                        match self.tokens.peek()?.token_type {
                            TokenType::Keyword(Keyword::FORWARD) => {
                                self.expect(TokenType::Keyword(Keyword::END))?;
                            }
                            _ => self.consume_line()?,
                        };
                    }
                    _ => todo!("INVALID"),
                }
            }
            TokenType::AccessType(_) => {
                let function = self.parse_function()?;

                match self.file.functions.iter_mut().find(|func| {
                    func.name == function.name
                        && func.returns == function.returns
                        && func.arguments == function.arguments
                }) {
                    Some(existing) => {
                        if existing.definition != Range::default() {
                            existing.definition = function.definition;
                        } else {
                            self.file.diagnostics.push(Diagnostic {
                                severity: Severity::Warning,
                                message: format!(
                                    "Function already defined at {}",
                                    existing.declaration.start
                                ),
                                range: function.declaration,
                            });
                            self.file.diagnostics.push(Diagnostic {
                                severity: Severity::Hint,
                                message: "Function already defined here".to_owned(),
                                range: existing.declaration,
                            });
                        }
                    }
                    None => {
                        self.file.diagnostics.push(Diagnostic {
                            severity: Severity::Error,
                            message: "Function missing declaration".to_owned(),
                            range: function.definition,
                        });
                        self.file.functions.push(function);
                    }
                }
            }
            TokenType::ScopeModif(_) | TokenType::Keyword(Keyword::TYPE) => {
                if let TokenType::ScopeModif(_) = self.tokens.next()?.token_type {
                    self.expect(TokenType::Keyword(Keyword::TYPE))?;
                }
                // TODO do something with this
                loop {
                    match self.tokens.peek()?.token_type {
                        TokenType::Keyword(Keyword::FORWARD) => {
                            self.expect(TokenType::Keyword(Keyword::END))?;
                        }
                        _ => self.consume_line()?,
                    };
                }
            }
            _ => {
                todo!("INVALID")
            }
        }
        Some(())
    }

    pub fn parse_tokens(&mut self) {
        loop {
            if self.parse_top_level() == None {
                break;
            }
        }
    }
}
