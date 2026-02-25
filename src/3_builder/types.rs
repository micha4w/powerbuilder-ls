use core::panic;
use std::{
    collections::{hash_map, HashMap},
    fmt,
    sync::Arc,
};

use crate::{
    parser::{self, SQLDeclareProcedureStatement, SQLSelectStatement},
    tokenizer::{self, Token},
    types::*,
};

pub type TopLevelIndex = usize;

#[derive(Debug, Clone, Copy)]
pub enum BaseType {
    Blob(Option<usize>),
    Boolean,
    Byte,
    Char,
    Date,
    Datetime,
    Double,
    Int,
    Long,
    Longlong,
    Longptr,
    Real,
    String,
    Time,
    Uint,
    Ulong,
    Decimal(Option<usize>),
    Any,
}

impl BaseType {
    pub fn new(data_type: &parser::DataType) -> Option<BaseType> {
        if data_type.group.is_some() {
            return None;
        }

        let curly_num = data_type
            .curly_number
            .as_ref()
            .map(|num| num.content.parse().expect("checked in parser"));

        match data_type.name.content.to_lowercase().as_str() {
            "any" => Some(BaseType::Any),
            "blob" => Some(BaseType::Blob(curly_num)),
            "boolean" => Some(BaseType::Boolean),
            "byte" => Some(BaseType::Byte),
            "char" => Some(BaseType::Char),
            "date" => Some(BaseType::Date),
            "datetime" => Some(BaseType::Datetime),
            "dec" | "decimal" => Some(BaseType::Decimal(curly_num)),
            "double" => Some(BaseType::Double),
            "integer" | "int" => Some(BaseType::Int),
            "long" => Some(BaseType::Long),
            "longlong" => Some(BaseType::Longlong),
            "longptr" => Some(BaseType::Longptr),
            "real" => Some(BaseType::Real),
            "string" => Some(BaseType::String),
            "time" => Some(BaseType::Time),
            "unsignedinteger" | "unsignedint" | "uint" => Some(BaseType::Uint),
            "unsignedlong" | "ulong" => Some(BaseType::Ulong),
            _ => None,
        }
    }
}

impl std::fmt::Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Blob(_) => write!(f, "blob"),
            BaseType::Boolean => write!(f, "boolean"),
            BaseType::Byte => write!(f, "byte"),
            BaseType::Char => write!(f, "char"),
            BaseType::Date => write!(f, "date"),
            BaseType::Datetime => write!(f, "datetime"),
            BaseType::Double => write!(f, "double"),
            BaseType::Int => write!(f, "int"),
            BaseType::Long => write!(f, "long"),
            BaseType::Longlong => write!(f, "longlong"),
            BaseType::Longptr => write!(f, "longptr"),
            BaseType::Real => write!(f, "real"),
            BaseType::String => write!(f, "string"),
            BaseType::Time => write!(f, "time"),
            BaseType::Uint => write!(f, "uint"),
            BaseType::Ulong => write!(f, "ulong"),
            BaseType::Decimal(_) => write!(f, "decimal"),
            BaseType::Any => write!(f, "any"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PowerScriptType {
    Base(BaseType),
    Complex(IString),
    Array(Box<Self>), // TODO: fixed size arrays
}

impl fmt::Display for PowerScriptType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PowerScriptType::Base(base) => write!(f, "{}", base),
            PowerScriptType::Complex(name) => write!(f, "{}", name),
            PowerScriptType::Array(inner) => write!(f, "{}[]", inner),
        }
    }
}

impl PowerScriptType {
    pub fn new(data_type: &parser::DataType) -> PowerScriptType {
        let base = BaseType::new(data_type);

        let typ = match base {
            Some(base) => PowerScriptType::Base(base),
            None => PowerScriptType::Complex((&data_type.name.content).into()), // TODO(groups)
        };

        if data_type.array_bounds.is_some() {
            PowerScriptType::Array(Box::new(typ))
        } else {
            typ
        }
    }

    // TODO: remove this
    pub fn simple_eq(a: &Self, b: &Self) -> bool {
        match (a, b) {
            (PowerScriptType::Array(nested_a), PowerScriptType::Array(nested_b)) => {
                Self::simple_eq(&nested_a, &nested_b)
            }
            (PowerScriptType::Complex(a_name), PowerScriptType::Complex(b_name)) => {
                a_name == b_name
            }
            (PowerScriptType::Base(a_base), PowerScriptType::Base(b_base)) => {
                match (a_base, b_base) {
                    (BaseType::Decimal(a_num), BaseType::Decimal(b_num)) => a_num == b_num,
                    (BaseType::Blob(a_num), BaseType::Blob(b_num)) => a_num == b_num,
                    _ => std::mem::discriminant(a_base) == std::mem::discriminant(b_base),
                }
            }
            _ => std::mem::discriminant(a) == std::mem::discriminant(b),
        }
    }
}

// TODO(cleanup): remove this?
#[derive(Debug, Clone)]
pub struct DataType {
    pub parsed: parser::DataType,
    pub powerscript_type: PowerScriptType,
}

impl DataType {
    pub fn new(data_type: parser::DataType) -> DataType {
        DataType {
            powerscript_type: PowerScriptType::new(&data_type),
            parsed: data_type,
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.parsed.wrap_variable("").to_lowercase().trim_end())
    }
}

#[derive(Clone, Debug)]
pub enum VariableType {
    Local(parser::Variable),
    Scoped(parser::ScopedVariable),
    Argument(parser::Argument),
    Instance(parser::InstanceVariable),
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub variable_type: VariableType,
    pub data_type: DataType,
}

impl Variable {
    pub fn new_local(var: parser::Variable) -> Variable {
        Variable {
            data_type: DataType::new(var.data_type.clone()),
            variable_type: VariableType::Local(var),
        }
    }

    pub fn new_scoped(var: parser::ScopedVariable) -> Variable {
        Variable {
            data_type: DataType::new(var.variable.data_type.clone()),
            variable_type: VariableType::Scoped(var.clone()),
        }
    }

    pub fn new_instance(instance: parser::InstanceVariable) -> Variable {
        Variable {
            data_type: DataType::new(instance.variable.data_type.clone()),
            variable_type: VariableType::Instance(instance),
        }
    }

    pub fn new_argument(arg: parser::Argument) -> Variable {
        Variable {
            data_type: DataType::new(arg.variable.data_type.clone()),
            variable_type: VariableType::Argument(arg),
        }
    }

    pub fn parsed(&self) -> &parser::Variable {
        match &self.variable_type {
            VariableType::Local(local) => &local,
            VariableType::Scoped(scoped) => &scoped.variable,
            VariableType::Argument(arg) => &arg.variable,
            VariableType::Instance(instance) => &instance.variable,
        }
    }

    pub fn iname(&self) -> IString {
        (&self.parsed().access.name.content).into()
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

    pub fn unwrap_instance(&self) -> &parser::InstanceVariable {
        match &self.variable_type {
            VariableType::Instance(var) => &var,
            _ => panic!("unwrap_instance failed"),
        }
    }
}

#[derive(Debug)]
pub struct SQLCursor {
    pub definitions: Vec<(Token, SQLSelectStatement)>,
}

#[derive(Debug)]
pub struct SQLProcedure {
    pub definitions: Vec<SQLDeclareProcedureStatement>,
}

#[derive(Clone, Debug)]
pub struct Body {
    /// Does not include arguments
    pub variables: HashMap<IString, Arc<Variable>>,
    pub labels: HashMap<IString, tokenizer::Token>,
}

impl Body {
    pub fn new(vars: impl Iterator<Item = Variable>) -> Self {
        Body {
            variables: vars
                .into_iter()
                .map(|var| ((&var.parsed().access.name.content).into(), Arc::new(var)))
                .collect(),
            labels: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Body {
            variables: HashMap::new(),
            labels: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct EventHeader {
    pub parsed: parser::Event,

    pub returns: Option<DataType>,
    pub arguments: Vec<Arc<Variable>>,
}

impl EventHeader {
    pub fn new(parsed: parser::Event) -> EventHeader {
        let (ret, args) = parsed.get_types();
        EventHeader {
            returns: ret
                .as_ref()
                .map(|data_type| DataType::new(data_type.clone())),
            arguments: args
                .iter()
                .map(|arg| Arc::new(Variable::new_argument(arg.clone())))
                .collect(),

            parsed,
        }
    }

    pub fn iname(&self) -> IString {
        (&self.parsed.name.content).into()
    }
}

#[derive(Clone, Debug)]
pub struct EventDeclaration {
    pub header: EventHeader,
}

#[derive(Clone, Debug)]
pub struct EventDefinition {
    pub parsed: parser::EventBody,

    pub header: EventHeader,
    pub body: Body,
}

pub type Event = DefinitionDeclaration<Arc<EventDefinition>, Arc<EventDeclaration>>;

impl Event {
    pub fn header(&self) -> &EventHeader {
        match self.get_any() {
            DefinitionDeclarationEnum::Definition(def) => &def.header,
            DefinitionDeclarationEnum::Declaration(decl) => &decl.header,
        }
    }

    pub fn help(&self) -> Option<&String> {
        self.map(
            |def| def.header.parsed.help.as_ref(),
            |decl| decl.header.parsed.help.as_ref(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct FunctionHeader {
    pub parsed: parser::Function,

    pub returns: Option<DataType>,
    pub arguments: Vec<Arc<Variable>>,
    pub throws: Vec<DataType>,
}

impl FunctionHeader {
    pub fn new(parsed: parser::Function) -> FunctionHeader {
        FunctionHeader {
            returns: parsed
                .returns
                .as_ref()
                .map(|data_type| DataType::new(data_type.clone())),
            arguments: parsed
                .arguments
                .iter()
                .map(|arg| Arc::new(Variable::new_argument(arg.clone())))
                .collect(),
            throws: parsed
                .throws
                .iter()
                .map(|data_type| DataType::new(data_type.clone()))
                .collect(),

            parsed,
        }
    }

    pub fn iname(&self) -> IString {
        (&self.parsed.name.content).into()
    }

    pub fn signature(&self) -> IString {
        // TODO use pbsig syntax
        (&self
            .arguments
            .iter()
            .map(|arg| arg.data_type.to_string())
            .collect::<Vec<_>>()
            .join(", "))
            .into()
    }

    pub fn types_conflict(&self, other: &FunctionHeader) -> bool {
        self.arguments.len() == other.arguments.len()
            && self.arguments.iter().zip(&other.arguments).all(|(a, b)| {
                PowerScriptType::simple_eq(
                    &a.data_type.powerscript_type,
                    &b.data_type.powerscript_type,
                )
            })
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub header: FunctionHeader,
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub parsed: parser::FunctionBody,

    pub header: FunctionHeader,
    pub body: Body,
}

pub type Function = DefinitionDeclaration<Arc<FunctionDefinition>, Arc<FunctionDeclaration>>;

impl Function {
    pub fn header(&self) -> &FunctionHeader {
        match self.get_any() {
            DefinitionDeclarationEnum::Definition(def) => &def.header,
            DefinitionDeclarationEnum::Declaration(decl) => &decl.header,
        }
    }

    pub fn help(&self) -> Option<&String> {
        self.map(
            |def| def.header.parsed.help.as_ref(),
            |decl| decl.header.parsed.help.as_ref(),
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
    pub parsed: parser::DatatypeDecl,

    pub help: Option<String>,
    pub is_global: bool,

    pub instance_variables: HashMap<IString, Arc<Variable>>,
    pub events: HashMap<IString, Event>,
    pub functions: HashMap<IString, HashMap<IString, Function>>,
    pub external_functions: HashMap<IString, HashMap<IString, Function>>,
    pub ons: HashMap<IString, ()>,
}

impl Class {
    pub fn new(parsed: parser::DatatypeDecl) -> Class {
        Class {
            is_global: matches!(parsed.class.scope, Some(tokenizer::ScopeModif::GLOBAL)),

            parsed,
            help: None,

            instance_variables: HashMap::new(),
            events: HashMap::new(),
            functions: HashMap::new(),
            external_functions: HashMap::new(),
            ons: HashMap::new(),
        }
    }

    pub fn name(&self) -> &String {
        &self.parsed.class.name.name.content
    }

    pub fn iname(&self) -> IString {
        self.name().into()
    }

    pub fn base(&self) -> &String {
        &self.parsed.class.base.name.content
    }

    pub fn within(&self) -> Option<&String> {
        self.parsed.class.within.as_ref().map(|g| &g.name.content)
    }

    pub fn function_entry(
        &mut self,
        name: IString,
        sig: IString,
        is_external: bool,
    ) -> hash_map::Entry<'_, IString, Function> {
        let funcs = if is_external {
            &mut self.external_functions
        } else {
            &mut self.functions
        };
        funcs.entry(name).or_insert_with(HashMap::new).entry(sig)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_global {
            write!(f, "global ")?;
        }
        write!(f, "type {} from {}", &self.name(), &self.base())?;
        if let Some(within) = &self.within() {
            write!(f, " within {}", within)?;
        };

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct DatatypeDecl {
    pub class: Arc<Class>,

    pub variables: Vec<Arc<Variable>>,
    pub events: Vec<Arc<EventDeclaration>>,
    pub functions: Vec<Arc<FunctionDeclaration>>,
}

#[derive(Debug)]
pub enum TopLevelType {
    ForwardDecl(parser::ForwardDecl, Vec<Arc<Variable>>),

    ScopedVariableDecl(Vec<Arc<Variable>>),
    ScopedVariablesDecl(Vec<Arc<Variable>>),

    DatatypeDecl(DatatypeDecl),
    TypeVariablesDecl(Vec<Arc<Variable>>),
    FunctionsForwardDecl(Vec<Arc<FunctionDeclaration>>),
    ExternalFunctions(Vec<Arc<FunctionDeclaration>>),

    FunctionBody(Arc<FunctionDefinition>),
    EventBody(Arc<EventDefinition>),
    OnBody(parser::OnBody),
}

#[derive(Debug)]
pub struct TopLevel {
    pub range: Range,
    pub top_level_type: TopLevelType,
}
