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
    parser::{self, Expression, GroupedName, Statement},
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
                        (&arg.variable.name.content).into(),
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

    pub fn get_statement_at(&self, pos: &Position) -> Option<&Statement> {
        match self {
            TopLevelType::ForwardDecl(_)
            | TopLevelType::ScopedVariableDecl(_)
            | TopLevelType::ScopedVariablesDecl(_)
            | TopLevelType::DatatypeDecl(_)
            | TopLevelType::TypeVariablesDecl(_)
            | TopLevelType::FunctionsForwardDecl(_)
            | TopLevelType::ExternalFunctions(_) => None,

            TopLevelType::FunctionBody(((_, statements), _))
            | TopLevelType::EventBody(((_, statements), _))
            | TopLevelType::OnBody(((_, statements), _)) => statements
                .iter()
                .find_map(|statement| statement.get_statement_at(pos)),
        }
    }

    pub fn get_expression_at(
        &self,
        pos: &Position,
    ) -> Option<EitherOr<&Expression, &parser::LValue>> {
        macro_rules! ret_if_contains {
            ( $val:expr ) => {
                if let Some(val) = $val.get_expression_at(pos) {
                    return Some(val);
                }
            };
        }
        macro_rules! ret_if_one_contains {
            ( $vec:expr ) => {
                for val in $vec {
                    ret_if_contains!(val);
                }
            };
        }

        match &self {
            TopLevelType::ForwardDecl(..)
            | TopLevelType::TypeVariablesDecl(..)
            | TopLevelType::FunctionsForwardDecl(..)
            | TopLevelType::ExternalFunctions(..)
            | TopLevelType::FunctionBody(..)
            | TopLevelType::EventBody(..)
            | TopLevelType::OnBody(..) => {}

            TopLevelType::ScopedVariableDecl((var, _)) => {
                ret_if_one_contains!(var.variable.initial_value.iter());
            }
            TopLevelType::ScopedVariablesDecl((vars, _)) => ret_if_one_contains!(vars
                .iter()
                .filter_map(|var| var.variable.initial_value.as_ref())),
            TopLevelType::DatatypeDecl((decl, _)) => ret_if_one_contains!(decl
                .variables
                .iter()
                .filter_map(|var| var.variable.initial_value.as_ref())),
        }

        if let Some(statement) = &self.get_statement_at(pos) {
            if let Some(expression) = statement.get_expression_at(pos) {
                return Some(expression);
            };
        };

        None
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
