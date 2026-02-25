use super::types::*;
use crate::{parser, tokenizer, types::*};

use std::fmt;

#[derive(Clone, Copy, Debug)]
pub enum Node<'a> {
    DataType(&'a parser::DataType),
    VariableDeclaration(&'a parser::Variable),
    ScopedVariableDeclaration(&'a parser::ScopedVariable),
    InstanceVariableDeclaration(&'a parser::InstanceVariable),
    Label(&'a tokenizer::Token),
    FunctionDeclaration(&'a parser::Function),
    EventDeclaration(&'a parser::Event),
    LValue(&'a parser::LValue),
    Expression(&'a parser::Expression),
    Statement(&'a parser::Statement),
}

impl<'a> Node<'a> {
    pub fn get_range(&self) -> &Range {
        match self {
            Node::DataType(data_type) => &data_type.range,
            Node::ScopedVariableDeclaration(decl) => &decl.variable.range,
            Node::InstanceVariableDeclaration(decl) => &decl.variable.range,
            Node::VariableDeclaration(variable) => &variable.range,
            Node::Label(token) => &token.range,
            Node::FunctionDeclaration(function) => &function.range,
            Node::EventDeclaration(event) => &event.range,
            Node::LValue(lvalue) => &lvalue.range,
            Node::Expression(expression) => &expression.range,
            Node::Statement(statement) => &statement.range,
        }
    }
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::DataType(data_type) => fmt::Display::fmt(&data_type, f),
            Node::ScopedVariableDeclaration(decl) => fmt::Display::fmt(&decl.variable, f),
            Node::InstanceVariableDeclaration(decl) => fmt::Display::fmt(&decl.variable, f),
            Node::VariableDeclaration(variable) => write!(f, "{}", variable),
            Node::Label(token) => write!(f, "{}:", token.content),
            Node::FunctionDeclaration(function) => write!(f, "{}", function),
            Node::EventDeclaration(event) => write!(f, "{}", event),
            Node::LValue(lvalue) => fmt::Display::fmt(&lvalue.lvalue_type, f),
            Node::Expression(expression) => fmt::Display::fmt(&expression.expression_type, f),
            Node::Statement(_) => write!(f, "<TODO statement>"),
        }
    }
}

trait NodeSearcher {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()>;
}

impl<T: NodeSearcher> NodeSearcher for Option<T> {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if let Some(val) = self {
            val.search(pos, nodes)?;
        }

        Found::No
    }
}

impl<T: NodeSearcher> NodeSearcher for Vec<T> {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        for val in self {
            val.search(pos, nodes)?;
        }

        Found::No
    }
}

impl NodeSearcher for parser::DataType {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::DataType(self));
        Found::Yes(())
    }
}

impl NodeSearcher for parser::Variable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::VariableDeclaration(self));
        self.data_type.search(pos, nodes)?;
        self.initial_value.search(pos, nodes)?;

        Found::Yes(())
    }
}

impl NodeSearcher for parser::InstanceVariable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.variable.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::InstanceVariableDeclaration(self));
        self.variable.search(pos, nodes)?;
        Found::Yes(())
    }
}

impl NodeSearcher for parser::ScopedVariable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.variable.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::ScopedVariableDeclaration(self));
        self.variable.search(pos, nodes)?;
        Found::Yes(())
    }
}

impl NodeSearcher for parser::LValue {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::LValue(self));

        match &self.lvalue_type {
            // TODO make these be variable accesses?
            parser::LValueType::This | parser::LValueType::Super | parser::LValueType::Parent => {}
            parser::LValueType::Variable(_) => {},
            parser::LValueType::Function(call) => {
                call.arguments.search(pos, nodes)?;
            }
            parser::LValueType::Method(lvalue, call) => {
                lvalue.search(pos, nodes)?;
                call.arguments.search(pos, nodes)?;
            }
            parser::LValueType::Member(lvalue, _) => {
                lvalue.search(pos, nodes)?;
            }
            parser::LValueType::Index(lvalue, expression) => {
                lvalue.search(pos, nodes)?;
                expression.search(pos, nodes)?;
            }
            parser::LValueType::SQLAccess(_, lvalue) => lvalue.search(pos, nodes)?,
        }

        Found::Yes(())
    }
}

impl NodeSearcher for parser::Expression {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::Expression(self));

        match &self.expression_type {
            parser::ExpressionType::Create(data_type) => {
                data_type.search(pos, nodes)?;
            }
            parser::ExpressionType::Literal(..) => {}
            parser::ExpressionType::ArrayLiteral(expressions) => {
                expressions.search(pos, nodes)?;
            }
            parser::ExpressionType::Operation(left, _operator, right) => {
                left.search(pos, nodes)?;
                right.search(pos, nodes)?;
            }
            parser::ExpressionType::UnaryOperation(_, expression)
            | parser::ExpressionType::IncrementDecrement(expression, _)
            | parser::ExpressionType::BooleanNot(expression)
            | parser::ExpressionType::Parenthesized(expression)
            | parser::ExpressionType::CreateUsing(expression) => expression.search(pos, nodes)?,

            parser::ExpressionType::LValue(lvalue) => lvalue.search(pos, nodes)?,
            parser::ExpressionType::Error => {}
        };

        Found::Yes(())
    }
}

fn search_lvalue_sql<'a, T: NodeSearcher>(
    t: &'a T,
    pos: &Position,
    nodes: &mut Vec<Node<'a>>,
) -> Found<()> {
    let mut new_nodes = Vec::new();
    if let Found::Yes(()) = t.search(pos, &mut new_nodes) {
        if new_nodes.iter().any(|node| {
            matches!(
                node,
                Node::LValue(parser::LValue {
                    lvalue_type: parser::LValueType::SQLAccess(..),
                    ..
                })
            )
        }) {
            nodes.extend(new_nodes);
            return Found::Yes(());
        }
    }
    Found::No
}

impl NodeSearcher for parser::Statement {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::Statement(self));

        match &self.statement_type {
            parser::StatementType::Expression(expression) |
            parser::StatementType::Throw(expression) |
            parser::StatementType::Destroy(expression) |
            parser::StatementType::Return(Some(expression)) => {
                    expression.search(pos, nodes)?;
            }
            parser::StatementType::Assignment(lvalue, _, expression) => {
                    lvalue.search(pos, nodes)?;
                    expression.search(pos, nodes)?;
            }
            parser::StatementType::Declaration(var) => {
                var.search(pos, nodes)?;
            }

            parser::StatementType::Goto(label) | parser::StatementType::Label(label) => if label.range.contains(pos) {
                nodes.push(Node::Label(label));
            },

            parser::StatementType::Return(None) |
            parser::StatementType::Call(..) | // TODO(call): ...
            parser::StatementType::Exit |
            parser::StatementType::Continue |
            parser::StatementType::Error => {},
            parser::StatementType::SQL(sql) => {
                sql.get_transaction().search(pos, nodes)?;

                // TODO(sql): ...
                match sql {
                    parser::SQLStatement::OPEN(_token) => {}
                    parser::SQLStatement::CLOSE(_token) => {}
                    parser::SQLStatement::CONNECT(_) => {}
                    parser::SQLStatement::DISCONNECT(_) => {}
                    parser::SQLStatement::COMMIT(_) => { }
                    parser::SQLStatement::DECLARE_CURSOR(_cursor, select) => {
                        search_lvalue_sql(&select.fields, pos, nodes)?;
                        search_lvalue_sql(&select.intos, pos, nodes)?;
                        search_lvalue_sql(&select.clause, pos, nodes)?;
                    }
                    parser::SQLStatement::DECLARE_PROCEDURE(_decl) => {},
                    parser::SQLStatement::EXECUTE(_procedure) => {},
                    parser::SQLStatement::FETCH(_cursor_or_procedure, intos) => intos.search(pos, nodes)?,
                    parser::SQLStatement::ROLLBACK(_) => {},
                    parser::SQLStatement::DELETE(_, criteria, _) => {
                        search_lvalue_sql(criteria, pos, nodes)?
                    }
                    parser::SQLStatement::DELETE_OF_CURSOR(_, _cursor) => {}
                    parser::SQLStatement::INSERT(insert) => {
                        search_lvalue_sql(&insert.fields, pos, nodes)?;
                        search_lvalue_sql(&insert.values, pos, nodes)?;
                    }
                    parser::SQLStatement::SELECT(select) => {
                        search_lvalue_sql(&select.fields, pos, nodes)?;
                        search_lvalue_sql(&select.intos, pos, nodes)?;
                        search_lvalue_sql(&select.clause, pos, nodes)?;
                    }
                    parser::SQLStatement::UPDATE(update) => {
                        search_lvalue_sql(&update.set, pos, nodes)?;
                        search_lvalue_sql(&update.clause, pos, nodes)?;
                    }
                    parser::SQLStatement::UPDATE_OF_CURSOR(update) => {
                        search_lvalue_sql(&update.set, pos, nodes)?;
                    }
                }
            }
            parser::StatementType::If(if_statement) => {
                if_statement.condition.search(pos, nodes)?;
                if_statement.statements.search(pos, nodes)?;
                for (expression, statements) in &if_statement.elseif_statements {
                    expression.search(pos, nodes)?;
                    statements.search(pos, nodes)?;
                }
                if_statement.else_statements.search(pos, nodes)?;
            },
            parser::StatementType::TryCatch(try_catch_statement) => {
                try_catch_statement.statements.search(pos, nodes)?;

                for (variable, statements) in &try_catch_statement.catches {
                    variable.search(pos, nodes)?;
                    statements.search(pos, nodes)?;

                }

                if let Some(statements) = &try_catch_statement.finally {
                    statements.search(pos, nodes)?;
                };
            }
            parser::StatementType::ForLoop(for_loop) => {
                for_loop.variable.search(pos, nodes)?;

                for_loop.start.search(pos, nodes)?;
                for_loop.stop.search(pos, nodes)?;
                for_loop.step.search(pos, nodes)?;

                for_loop.statements.search(pos, nodes)?;
            },
            parser::StatementType::WhileLoop(while_loop) => {
                while_loop.condition.search(pos, nodes)?;
                while_loop.statements.search(pos, nodes)?;
            },
            parser::StatementType::Choose(choose_case) => {
                choose_case.choose.search(pos, nodes)?;

                for (specifiers, statements) in &choose_case.cases {
                    for specifier in specifiers {
                        for expr in specifier.get_expressions() {
                            expr.search(pos, nodes)?;
                        }
                    }

                    statements.search(pos, nodes)?;
                }
            },
        };

        Found::Yes(())
    }
}

impl NodeSearcher for parser::Function {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::FunctionDeclaration(self));

        self.returns.search(pos, nodes)?;
        for arg in &self.arguments {
            arg.variable.search(pos, nodes)?;
        }
        self.throws.search(pos, nodes)?;

        Found::Yes(())
    }
}

impl NodeSearcher for parser::Event {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        if !self.range.contains(pos) {
            return Found::No;
        }

        nodes.push(Node::EventDeclaration(self));

        match &self.event_type {
            parser::EventType::User(returns, arguments) => {
                returns.search(pos, nodes)?;

                for arg in arguments {
                    arg.variable.search(pos, nodes)?;
                }
            }
            parser::EventType::Predefined => todo!(),
            parser::EventType::System(_) => todo!(),
        }

        Found::Yes(())
    }
}

impl NodeSearcher for parser::Class {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Found<()> {
        self.name.search(pos, nodes)?;
        self.base.search(pos, nodes)?;
        self.within.search(pos, nodes)?;

        Found::No
    }
}

pub trait NodeGetter {
    fn get_nodes_at(&self, pos: &Position) -> Vec<Node<'_>>;
}

impl NodeGetter for TopLevelType {
    // TODO: make this return not only the parsed but also the built values
    fn get_nodes_at(&self, pos: &Position) -> Vec<Node<'_>> {
        let mut nodes = vec![];

        match &self {
            TopLevelType::ForwardDecl(decl, _) => {
                decl.variables.search(pos, &mut nodes);
                for datatype in &decl.classes {
                    datatype.class.search(pos, &mut nodes);
                    datatype.events.search(pos, &mut nodes);
                    datatype.functions.search(pos, &mut nodes);
                    datatype.variables.search(pos, &mut nodes);
                }
            }
            TopLevelType::TypeVariablesDecl(vars) => {
                for var in vars {
                    var.parsed().search(pos, &mut nodes);
                }
            }
            TopLevelType::FunctionsForwardDecl(functions)
            | TopLevelType::ExternalFunctions(functions) => {
                for func in functions {
                    func.header.parsed.search(pos, &mut nodes);
                }
            }

            TopLevelType::FunctionBody(body) => {
                body.parsed.header.search(pos, &mut nodes);
                body.parsed.statements.search(pos, &mut nodes);
            }
            TopLevelType::EventBody(body) => {
                body.parsed.header.search(pos, &mut nodes);
                body.parsed.statements.search(pos, &mut nodes);
            }
            TopLevelType::OnBody(body) => {
                body.statements.search(pos, &mut nodes);
            }

            TopLevelType::ScopedVariableDecl(vars) | TopLevelType::ScopedVariablesDecl(vars) => {
                for var in vars {
                    var.parsed().search(pos, &mut nodes);
                }
            }
            TopLevelType::DatatypeDecl(decl) => {
                decl.class.parsed.class.search(pos, &mut nodes);

                decl.class.parsed.events.search(pos, &mut nodes);
                decl.class.parsed.functions.search(pos, &mut nodes);
                decl.class.parsed.variables.search(pos, &mut nodes);
            }
        }

        nodes
    }
}