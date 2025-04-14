use core::panic;
use std::{
    collections::HashMap,
    fmt::{self},
    sync::{Arc, Weak},
};

use futures::future::{self};
use tokio::sync::Mutex;

use super::Linter;
use crate::{
    parser::{self, GroupedName},
    tokenizer::{self},
    types::*,
};

#[derive(Clone, Debug)]
pub enum VariableType {
    Local(parser::Variable),
    Scoped(parser::ScopedVariable),
    Argument(parser::Argument),
    Instance((Weak<Mutex<Class>>, parser::InstanceVariable)),
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub variable_type: VariableType,

    pub data_type: parser::DataTypeType,
    // pub uses: Vec<Range>,
}

impl Variable {
    pub fn parsed(&self) -> &parser::Variable {
        match &self.variable_type {
            VariableType::Local(local) => &local,
            VariableType::Scoped(scoped) => &scoped.variable,
            VariableType::Argument(arg) => &arg.variable,
            VariableType::Instance((_, instance)) => &instance.variable,
        }
    }

    pub fn unwrap_local(&self) -> &parser::Variable {
        match &self.variable_type {
            VariableType::Local(var) => &var,
            _ => panic!("unwrap_local failed"),
        }
    }

    pub fn unwrap_scoped(&self) -> &parser::ScopedVariable {
        match &self.variable_type {
            VariableType::Scoped(var) => &var,
            _ => panic!("unwrap_scoped failed"),
        }
    }

    pub fn unwrap_argument(&self) -> &parser::Argument {
        match &self.variable_type {
            VariableType::Argument(var) => &var,
            _ => panic!("unwrap_argument failed"),
        }
    }

    pub fn unwrap_instance(&self) -> (&Weak<Mutex<Class>>, &parser::InstanceVariable) {
        match &self.variable_type {
            VariableType::Instance((class, var)) => (&class, &var),
            _ => panic!("unwrap_instance failed"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Event {
    pub parsed: parser::Event,

    pub returns: parser::DataTypeType,
    pub arguments: Vec<Arc<Mutex<Variable>>>,
    pub declaration: Option<Range>,
    pub definition: Option<FunctionDefinition>,
    pub uses: Vec<Range>,
}

impl Event {
    pub fn new(
        parsed: parser::Event,
        declaration: Option<Range>,
        definition: Option<FunctionDefinition>,
    ) -> Self {
        let returns;
        let arguments;
        match &parsed.event_type {
            parser::EventType::User(ret, args) => {
                arguments = args
                    .iter()
                    .map(|arg| {
                        Mutex::new(Variable {
                            variable_type: VariableType::Argument(arg.clone()),
                            data_type: arg.variable.data_type.data_type_type.clone(),
                            // uses: Vec::new(),
                        })
                        .into()
                    })
                    .collect();
                returns = ret.clone()
            }
            parser::EventType::System(name) => {
                arguments = Vec::new();
                returns = None;
                // TODO get system events from where?
            }
            parser::EventType::Predefined => {
                arguments = Vec::new();
                returns = None;
                // TODO get arguments and return value from base class using parsed.name
            }
        }

        Event {
            declaration,
            definition,
            uses: Vec::new(),

            parsed,
            returns: returns.map_or(parser::DataTypeType::Void, |dt| dt.data_type_type),
            arguments,
        }
    }

    pub async fn equals(&self, other: &Event) -> bool {
        self.returns == other.returns && self.conflicts(other).await
    }

    pub async fn conflicts(&self, other: &Event) -> bool {
        self.arguments.len() == other.arguments.len() && {
            let self_args = future::join_all(self.arguments.iter().map(|arg| arg.lock())).await;
            let other_args = future::join_all(other.arguments.iter().map(|arg| arg.lock())).await;

            self_args
                .iter()
                .zip(other_args)
                .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,
    pub range: Range,
}

impl FunctionDefinition {
    pub fn new(args: &Vec<parser::Argument>, range: Range) -> Self {
        FunctionDefinition {
            range,

            variables: args
                .iter()
                .map(|arg| {
                    (
                        (&arg.variable.access.name.content).into(),
                        Mutex::new(Variable {
                            variable_type: VariableType::Argument(arg.clone()),
                            data_type: arg.variable.data_type.data_type_type.clone(),
                            // uses: Vec::new(),
                        })
                        .into(),
                    )
                })
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parsed: parser::Function,

    pub returns: parser::DataTypeType,
    pub help: Option<String>,

    pub declaration: Option<Range>,
    pub definition: Option<FunctionDefinition>,
    // pub uses: Vec<Range>,
}

impl Function {
    pub fn new(
        parsed: parser::Function,
        declaration: Option<Range>,
        definition: Option<Range>,
        help: Option<String>,
    ) -> Self {
        Function {
            declaration,
            definition: definition.map(|range| FunctionDefinition::new(&parsed.arguments, range)),

            // uses: Vec::new(),
            help,

            returns: (&parsed.returns).into(),
            parsed,
        }
    }

    pub async fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other).await
    }

    pub async fn conflicts(&self, other: &Function) -> bool {
        self.parsed.arguments.len() == other.parsed.arguments.len()
            && self.parsed.vararg.is_some() == other.parsed.vararg.is_some()
            && {
                self.parsed
                    .arguments
                    .iter()
                    .zip(&other.parsed.arguments)
                    .all(|(self_arg, other_arg)| {
                        self_arg.variable.data_type == other_arg.variable.data_type
                    })
            }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}({})",
            &self.returns,
            &self.parsed.name.content,
            &self
                .parsed
                .arguments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub help: Option<String>,
    pub values: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub base: GroupedName,
    pub within: Option<GroupedName>,
    pub help: Option<String>,

    pub is_global: bool,
    pub usage: Usage,

    pub ons: HashMap<IString, Arc<Mutex<Function>>>,
    pub events: HashMap<IString, Arc<Mutex<Event>>>,
    pub functions: HashMap<IString, Vec<Arc<Mutex<Function>>>>,
    pub external_functions: HashMap<IString, Vec<Arc<Mutex<Function>>>>,

    pub instance_variables: HashMap<IString, Arc<Mutex<Variable>>>,
}

impl Class {
    pub fn new(
        name: String,
        base: GroupedName,
        within: Option<GroupedName>,
        is_global: bool,
    ) -> Class {
        Class {
            // file,
            name,
            base,
            within,
            help: None,

            is_global,
            usage: Usage {
                declaration: None,
                definition: None,
                // uses: Vec::new(),
            },

            events: HashMap::new(),
            instance_variables: HashMap::new(),
            functions: HashMap::new(),
            ons: HashMap::new(),
            external_functions: HashMap::new(),
        }
    }

    pub async fn find_variable(
        &self,
        linter: &Linter<'_>,
        variable: &parser::VariableAccess,
        access: &tokenizer::AccessType,
        write: bool,
    ) -> Option<Arc<Mutex<Variable>>> {
        let mut class_arc_holder;
        let mut class_holder = None;
        let mut class = self;
        let mut strictness = access.strictness();
        loop {
            if let Some(found) = class
                .instance_variables
                .get(&(&variable.name.content).into())
            {
                let access = &found.lock().await.unwrap_instance().1.access.clone();
                if write
                    .then_some(&access.write)
                    .unwrap_or(&access.read)
                    .map_or(0, |acc| acc.strictness())
                    <= strictness
                {
                    return Some(found.clone());
                }
            }

            match linter.find_class(&class.base).await {
                Some(Complex::Class(cls)) => {
                    drop(class_holder);
                    class_arc_holder = cls.clone();
                    class_holder = Some(class_arc_holder.lock().await);
                    class = class_holder.as_ref().unwrap();
                }
                Some(Complex::Enum(_)) | None => break,
            }

            if strictness < tokenizer::AccessType::PRIVATE.strictness() {
                strictness = tokenizer::AccessType::PROTECTED.strictness();
            }
        }

        None
    }

    // fn find_exact_event(&self, event: &Event) -> Option<&Event> {
    //     self.events
    //         .iter()
    //         .find(|func| func.borrow().equals(event))
    //         .cloned()
    // }

    pub async fn find_conflicting_event(&self, event: &Event) -> Option<Arc<Mutex<Event>>> {
        if let Some(ev) = self.events.get(&(&event.parsed.name.content).into()) {
            ev.lock().await.conflicts(event).await.then_some(ev.clone())
        } else {
            None
        }
    }
    // fn find_exact_function(&self, function: &Function) -> Option<&Function> {
    //     for functions in [&self.functions, &self.external_functions] {
    //         if let Some(func) = functions
    //             .get(&function.parsed.name.into())
    //             .map(|funcs| funcs.iter().find(|func| func.equals(function)))
    //         {
    //             return Some(func);
    //         }
    //     }

    //     None
    // }

    pub async fn find_conflicting_function(
        &self,
        function: &Function,
    ) -> Option<Arc<Mutex<Function>>> {
        for functions in [&self.functions, &self.external_functions] {
            if let Some(funcs) = functions.get(&(&function.parsed.name.content).into()) {
                for func in funcs {
                    if func.lock().await.conflicts(function).await {
                        return Some(func.clone());
                    }
                }
            }
        }

        None
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_global {
            write!(f, "global ")?;
        }
        write!(f, "type {} from {}", &self.name, &self.base)?;
        if let Some(within) = &self.within {
            write!(f, " within {}", within)?;
        };

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Complex {
    Class(Arc<Mutex<Class>>),
    Enum(Arc<Mutex<Enum>>),
}

impl Complex {
    pub async fn name(&self) -> String {
        match self {
            Complex::Class(class) => class.lock().await.name.clone(),
            Complex::Enum(r#enum) => r#enum.lock().await.name.clone(),
        }
    }

    pub async fn help(&self) -> Option<String> {
        match self {
            Complex::Class(class) => class.lock().await.help.clone(),
            Complex::Enum(r#enum) => r#enum.lock().await.help.clone(),
        }
    }

    pub fn unwrap_class(&self) -> &Arc<Mutex<Class>> {
        match self {
            Complex::Class(class) => class,
            Complex::Enum(_) => panic!("unwrap_class failed"),
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct Usage {
    pub declaration: Option<Range>,
    pub definition: Option<Range>,
    // uses: Vec<Range>,
}

pub trait IndexableProgress<Idx> {
    type T: std::fmt::Debug;
}

#[derive(Debug)]
pub struct LintProgressOnlyTypes;
impl<B: std::fmt::Debug, C, D> IndexableProgress<LintProgressOnlyTypes> for (B, C, D) {
    type T = B;
}
#[derive(Debug)]
pub struct LintProgressShallow;
impl<B, C: std::fmt::Debug, D> IndexableProgress<LintProgressShallow> for (B, C, D) {
    type T = C;
}
#[derive(Debug)]
pub struct LintProgressComplete;
impl<B, C, D: std::fmt::Debug> IndexableProgress<LintProgressComplete> for (B, C, D) {
    type T = D;
}

pub type LintedInOnlyTypes<A, B> = ((A, B), (A, B), (A, B));
pub type LintedInShallow<A, B> = (A, (A, B), (A, B));
pub type LintedInComplete<A, B> = (A, A, (A, B));

pub type ForwardDecl =
    LintedInOnlyTypes<Vec<parser::DatatypeDecl>, HashMap<IString, Arc<Mutex<Class>>>>;
pub type ScopedVariableDecl = LintedInOnlyTypes<parser::ScopedVariable, Arc<Mutex<Variable>>>;
pub type ScopedVariablesDecl =
    LintedInOnlyTypes<Vec<parser::ScopedVariable>, HashMap<IString, Arc<Mutex<Variable>>>>;

pub type DatatypeDecl = LintedInShallow<parser::DatatypeDecl, Arc<Mutex<Class>>>;
pub type TypeVariablesDecl = LintedInShallow<
    Vec<parser::InstanceVariable>,
    (
        Option<Arc<Mutex<Class>>>,
        HashMap<IString, Arc<Mutex<Variable>>>,
    ),
>;
pub type FunctionsForwardDecl = LintedInShallow<
    Vec<parser::Function>,
    (
        Option<Arc<Mutex<Class>>>,
        HashMap<IString, Arc<Mutex<Function>>>,
    ),
>;
pub type ExternalFunctions = LintedInShallow<
    Vec<parser::Function>,
    (
        Option<Arc<Mutex<Class>>>,
        HashMap<IString, Arc<Mutex<Function>>>,
    ),
>;

pub type FunctionBody = LintedInComplete<
    (parser::Function, Vec<parser::Statement>),
    (Option<Arc<Mutex<Class>>>, Arc<Mutex<Function>>),
>;
pub type EventBody = LintedInComplete<
    (parser::Event, Vec<parser::Statement>),
    (Option<Arc<Mutex<Class>>>, Arc<Mutex<Event>>),
>;
pub type OnBody =
    LintedInComplete<(parser::On, Vec<parser::Statement>), (Option<Arc<Mutex<Class>>>,)>;

#[derive(Debug)]
pub enum TopLevelType<Idx>
where
    ForwardDecl: IndexableProgress<Idx>,
    ScopedVariableDecl: IndexableProgress<Idx>,
    ScopedVariablesDecl: IndexableProgress<Idx>,
    DatatypeDecl: IndexableProgress<Idx>,
    FunctionsForwardDecl: IndexableProgress<Idx>,
    ExternalFunctions: IndexableProgress<Idx>,
    TypeVariablesDecl: IndexableProgress<Idx>,
    FunctionBody: IndexableProgress<Idx>,
    EventBody: IndexableProgress<Idx>,
    OnBody: IndexableProgress<Idx>,
{
    ForwardDecl(<ForwardDecl as IndexableProgress<Idx>>::T),

    ScopedVariableDecl(<ScopedVariableDecl as IndexableProgress<Idx>>::T),
    ScopedVariablesDecl(<ScopedVariablesDecl as IndexableProgress<Idx>>::T),

    DatatypeDecl(<DatatypeDecl as IndexableProgress<Idx>>::T),
    TypeVariablesDecl(<TypeVariablesDecl as IndexableProgress<Idx>>::T),
    FunctionsForwardDecl(<FunctionsForwardDecl as IndexableProgress<Idx>>::T),
    ExternalFunctions(<ExternalFunctions as IndexableProgress<Idx>>::T),

    FunctionBody(<FunctionBody as IndexableProgress<Idx>>::T),
    EventBody(<EventBody as IndexableProgress<Idx>>::T),
    OnBody(<OnBody as IndexableProgress<Idx>>::T),
}

impl TopLevelType<LintProgressComplete> {
    pub fn get_class(&self) -> Option<&Arc<Mutex<Class>>> {
        match self {
            TopLevelType::ForwardDecl(_)
            | TopLevelType::ScopedVariableDecl(_)
            | TopLevelType::ScopedVariablesDecl(_) => None,

            TopLevelType::DatatypeDecl((_, class)) => Some(&class),
            TopLevelType::TypeVariablesDecl((_, (class, _)))
            | TopLevelType::FunctionsForwardDecl((_, (class, _)))
            | TopLevelType::ExternalFunctions((_, (class, _)))
            | TopLevelType::FunctionBody((_, (class, _)))
            | TopLevelType::EventBody((_, (class, _)))
            | TopLevelType::OnBody((_, (class,))) => class.as_ref(),
        }
    }

    pub async fn get_variables(&self) -> Option<HashMap<IString, Arc<Mutex<Variable>>>> {
        match self {
            TopLevelType::ForwardDecl(_) => None,
            TopLevelType::ScopedVariableDecl(_) => None,
            TopLevelType::DatatypeDecl(_) => None,
            TopLevelType::FunctionsForwardDecl(_) => None,
            TopLevelType::ExternalFunctions(_) => None,

            TopLevelType::OnBody(_) => None, // TODO
            TopLevelType::TypeVariablesDecl((_, (_, vars)))
            | TopLevelType::ScopedVariablesDecl((_, vars)) => Some(vars.clone()),
            TopLevelType::FunctionBody((_, (_, func))) => func
                .lock()
                .await
                .definition
                .as_ref()
                .map(|def| def.variables.clone()),
            TopLevelType::EventBody((_, (_, event))) => event
                .lock()
                .await
                .definition
                .as_ref()
                .map(|def| def.variables.clone()),
        }
    }

    pub fn get_nodes_at(&self, pos: &Position) -> Vec<Node> {
        let mut nodes = vec![];

        match self {
            TopLevelType::ForwardDecl((forwards, _)) => {
                for forward in forwards {
                    forward.class.search(pos, &mut nodes);
                    forward.events.search(pos, &mut nodes);
                    forward.variables.search(pos, &mut nodes);
                }
            }
            TopLevelType::TypeVariablesDecl((vars, _)) => {
                vars.search(pos, &mut nodes);
            }
            TopLevelType::FunctionsForwardDecl((functions, _))
            | TopLevelType::ExternalFunctions((functions, _)) => {
                functions.search(pos, &mut nodes);
            }

            TopLevelType::FunctionBody(((function, statements), _)) => {
                function.search(pos, &mut nodes);
                statements.search(pos, &mut nodes);
            }
            TopLevelType::EventBody(((event, statements), _)) => {
                event.search(pos, &mut nodes);
                statements.search(pos, &mut nodes);
            }
            TopLevelType::OnBody(((_, statements), _)) => {
                statements.search(pos, &mut nodes);
            }

            TopLevelType::ScopedVariableDecl((var, _)) => {
                var.variable.search(pos, &mut nodes);
            }
            TopLevelType::ScopedVariablesDecl((vars, _)) => {
                for var in vars {
                    var.variable.search(pos, &mut nodes);
                }
            }
            TopLevelType::DatatypeDecl((decl, _)) => {
                decl.class.search(pos, &mut nodes);

                decl.events.search(pos, &mut nodes);
                decl.variables.search(pos, &mut nodes);
            }
        }

        nodes
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum LintProgress {
    None,
    OnlyTypes, // Only parse the first types block
    Shallow,   // Only parses the type and prototype blocks
    Complete,  // Parse all the Statements
}

impl LintProgress {
    pub fn next(&self) -> Option<LintProgress> {
        match self {
            LintProgress::None => Some(Self::OnlyTypes),
            LintProgress::OnlyTypes => Some(Self::Shallow),
            LintProgress::Shallow => Some(Self::Complete),
            LintProgress::Complete => None,
        }
    }
}

#[derive(Debug)]
pub enum ProgressedTopLevels {
    None(Vec<parser::TopLevel>),
    OnlyTypes(Vec<(Range, TopLevelType<LintProgressOnlyTypes>)>),
    Shallow(Vec<(Range, TopLevelType<LintProgressShallow>)>),
    Complete(Vec<(Range, TopLevelType<LintProgressComplete>)>),
}
impl ProgressedTopLevels {
    pub fn get_progress(&self) -> LintProgress {
        match self {
            ProgressedTopLevels::None(_) => LintProgress::None,
            ProgressedTopLevels::OnlyTypes(_) => LintProgress::OnlyTypes,
            ProgressedTopLevels::Shallow(_) => LintProgress::Shallow,
            ProgressedTopLevels::Complete(_) => LintProgress::Complete,
        }
    }
}

pub enum Node<'a> {
    DataType(&'a parser::DataType),
    VariableDeclaration(&'a parser::Variable),
    ScopedVariableDeclaration(&'a parser::ScopedVariable),
    InstanceVariableDeclaration(&'a parser::InstanceVariable),
    FunctionDeclaration(&'a parser::Function),
    EventDeclaration(&'a parser::Event),
    VariableAccess(&'a parser::VariableAccess),
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
            Node::VariableAccess(var) => &var.name.range,
            Node::VariableDeclaration(variable) => &variable.range,
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
            Node::DataType(data_type) => fmt::Display::fmt(&data_type.data_type_type, f),
            Node::ScopedVariableDeclaration(decl) => fmt::Display::fmt(&decl.variable, f),
            Node::InstanceVariableDeclaration(decl) => fmt::Display::fmt(&decl.variable, f),
            Node::VariableAccess(var) => write!(f, "{}", var.name.content),
            Node::VariableDeclaration(variable) => write!(f, "{}", variable),
            Node::FunctionDeclaration(function) => write!(f, "{}", function),
            Node::EventDeclaration(event) => write!(f, "{}", event),
            Node::LValue(lvalue) => fmt::Display::fmt(&lvalue.lvalue_type, f),
            Node::Expression(expression) => fmt::Display::fmt(&expression.expression_type, f),
            Node::Statement(_) => write!(f, "<TODO statement>"),
        }
    }
}

trait NodeSearcher {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()>;
}

impl<T: NodeSearcher> NodeSearcher for Option<T> {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if let Some(val) = self {
            val.search(pos, nodes)?;
        }

        Some(())
    }
}

impl<T: NodeSearcher> NodeSearcher for Vec<T> {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        for val in self {
            val.search(pos, nodes)?;
        }

        Some(())
    }
}

impl NodeSearcher for parser::DataType {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::DataType(self));
        None
    }
}

impl NodeSearcher for parser::VariableAccess {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.name.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::VariableAccess(self));
        None
    }
}

impl NodeSearcher for parser::Variable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::VariableDeclaration(self));
        self.access.search(pos, nodes)?;
        self.data_type.search(pos, nodes)?;
        self.initial_value.search(pos, nodes)?;

        None
    }
}

impl NodeSearcher for parser::InstanceVariable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.variable.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::InstanceVariableDeclaration(self));
        self.variable.search(pos, nodes)?;
        None
    }
}

impl NodeSearcher for parser::ScopedVariable {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.variable.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::ScopedVariableDeclaration(self));
        self.variable.search(pos, nodes)?;
        None
    }
}

impl NodeSearcher for parser::LValue {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::LValue(self));

        match &self.lvalue_type {
            // TODO make these be variable accesses?
            parser::LValueType::This | parser::LValueType::Super | parser::LValueType::Parent => {}
            parser::LValueType::Variable(access) => access.search(pos, nodes)?,
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

        None
    }
}

impl NodeSearcher for parser::Expression {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
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

        None
    }
}

impl NodeSearcher for parser::Statement {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
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
            parser::StatementType::Return(None) |
            parser::StatementType::Call(..) | // TODO
            parser::StatementType::Exit |
            parser::StatementType::Continue |
            parser::StatementType::SQL(..) | // TODO
            parser::StatementType::Error => {},

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

                for (_specifiers, statements) in &choose_case.cases {
                    // for specifier in specifiers {
                    //     match specifier.specifier_type {
                    //         parser::CaseSpecifierType::Literals(literal) => todo!(),
                    //         parser::CaseSpecifierType::To(literal, literal) => todo!(),
                    //         parser::CaseSpecifierType::Is(operator, literal) => todo!(),
                    //         parser::CaseSpecifierType::Else => todo!(),
                    //     }
                    // }

                    statements.search(pos, nodes)?;
                }
            },
        };

        None
    }
}

impl NodeSearcher for parser::Function {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
        }

        nodes.push(Node::FunctionDeclaration(self));

        self.returns.search(pos, nodes)?;

        for arg in &self.arguments {
            arg.variable.search(pos, nodes)?;
        }

        None
    }
}

impl NodeSearcher for parser::Event {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        if !self.range.contains(pos) {
            return Some(());
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

        None
    }
}

impl NodeSearcher for parser::Class {
    fn search<'a>(&'a self, pos: &Position, nodes: &mut Vec<Node<'a>>) -> Option<()> {
        self.name.search(pos, nodes)?;
        self.base.search(pos, nodes)?;
        self.within.search(pos, nodes)?;

        Some(())
    }
}
