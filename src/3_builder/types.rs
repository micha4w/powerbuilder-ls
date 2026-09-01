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
            "char" | "character" => Some(BaseType::Char),
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
pub struct DataType<'pars> {
    pub parsed: &'pars parser::DataType,
    pub powerscript_type: PowerScriptType,
}

impl DataType<'_> {
    pub fn new(data_type: &parser::DataType) -> DataType<'_> {
        DataType {
            powerscript_type: PowerScriptType::new(&data_type),
            parsed: data_type,
        }
    }
}

impl fmt::Display for DataType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.parsed.wrap_variable("").to_lowercase().trim_end())
    }
}

#[derive(Clone, Debug)]
pub enum VariableType<'pars> {
    Local(&'pars parser::Variable),
    Scoped(&'pars parser::ScopedVariable),
    Argument(&'pars parser::Argument),
    Instance(&'pars parser::InstanceVariable),
}

#[derive(Clone, Debug)]
pub struct Variable<'pars> {
    pub variable_type: VariableType<'pars>,
    pub data_type: DataType<'pars>,
}

impl Variable<'_> {
    pub fn new_local(var: &parser::Variable) -> Variable<'_> {
        Variable {
            data_type: DataType::new(&var.data_type),
            variable_type: VariableType::Local(var),
        }
    }

    pub fn new_scoped(var: &parser::ScopedVariable) -> Variable<'_> {
        Variable {
            data_type: DataType::new(&var.variable.data_type),
            variable_type: VariableType::Scoped(var),
        }
    }

    pub fn new_instance(instance: &parser::InstanceVariable) -> Variable<'_> {
        Variable {
            data_type: DataType::new(&instance.variable.data_type),
            variable_type: VariableType::Instance(instance),
        }
    }

    pub fn new_argument(arg: &parser::Argument) -> Variable<'_> {
        Variable {
            data_type: DataType::new(&arg.variable.data_type),
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
pub struct Body<'pars> {
    /// Does not include arguments
    pub variables: HashMap<IString, Arc<Variable<'pars>>>,
    pub labels: HashMap<IString, &'pars tokenizer::Token>,
}

impl<'pars> Body<'pars> {
    pub fn new(vars: impl Iterator<Item = Variable<'pars>>) -> Self {
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
pub struct EventHeader<'pars> {
    pub parsed: &'pars parser::Event,

    pub returns: Option<DataType<'pars>>,
    pub arguments: Vec<Arc<Variable<'pars>>>,
}

impl<'pars> EventHeader<'pars> {
    pub fn new(parsed: &'pars parser::Event) -> EventHeader<'pars> {
        let (ret, args) = parsed.get_types();
        EventHeader {
            returns: ret.as_ref().map(DataType::new),
            arguments: args
                .iter()
                .map(|arg| Arc::new(Variable::new_argument(arg)))
                .collect(),

            parsed,
        }
    }

    pub fn iname(&self) -> IString {
        (&self.parsed.name.content).into()
    }
}

#[derive(Clone, Debug)]
pub struct EventDeclaration<'pars> {
    pub header: EventHeader<'pars>,
}

#[derive(Clone, Debug)]
pub struct EventDefinition<'pars> {
    pub parsed: &'pars parser::EventBody,

    pub header: EventHeader<'pars>,
    pub body: Body<'pars>,
}

pub type Event<'pars> = DefinitionDeclaration<Arc<EventDefinition<'pars>>, Arc<EventDeclaration<'pars>>>;

impl<'pars> Event<'pars> {
    pub fn header(&self) -> &EventHeader<'pars> {
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
pub struct FunctionHeader<'pars> {
    pub parsed: &'pars parser::Function,

    pub returns: Option<DataType<'pars>>,
    pub arguments: Vec<Arc<Variable<'pars>>>,
    pub throws: Vec<DataType<'pars>>,
}

impl FunctionHeader<'_> {
    pub fn new(parsed: &parser::Function) -> FunctionHeader<'_> {
        FunctionHeader {
            returns: parsed
                .returns
                .as_ref()
                .map(DataType::new),
            arguments: parsed
                .arguments
                .iter()
                .map(|arg| Arc::new(Variable::new_argument(arg)))
                .collect(),
            throws: parsed
                .throws
                .iter()
                .map(DataType::new)
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
pub struct FunctionDeclaration<'pars> {
    pub header: FunctionHeader<'pars>,
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition<'pars> {
    pub parsed: &'pars parser::FunctionBody,

    pub header: FunctionHeader<'pars>,
    pub body: Body<'pars>,
}

pub type Function<'pars> =
    DefinitionDeclaration<Arc<FunctionDefinition<'pars>>, Arc<FunctionDeclaration<'pars>>>;

impl Function<'_> {
    pub fn header(&self) -> &FunctionHeader<'_> {
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
pub struct Class<'pars> {
    pub parsed: &'pars parser::DatatypeDecl,

    pub help: Option<&'pars String>,
    pub is_global: bool,

    pub instance_variables: HashMap<IString, Arc<Variable<'pars>>>,
    pub events: HashMap<IString, Event<'pars>>,
    pub functions: HashMap<IString, HashMap<IString, Function<'pars>>>,
    pub external_functions: HashMap<IString, HashMap<IString, Function<'pars>>>,
    pub ons: HashMap<IString, ()>,
}

impl<'pars> Class<'pars> {
    pub fn new(parsed: &'pars parser::DatatypeDecl) -> Class<'pars>{
        Class {
            is_global: matches!(parsed.class.scope, Some(tokenizer::ScopeModif::GLOBAL)),

            parsed,
            help: parsed.help.as_ref(),

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

    pub fn function_entry<'a>(
        &'a mut self,
        name: IString,
        sig: IString,
        is_external: bool,
    ) -> hash_map::Entry<'a, IString, Function<'pars>> {
        let funcs = if is_external {
            &mut self.external_functions
        } else {
            &mut self.functions
        };
        funcs.entry(name).or_insert_with(HashMap::new).entry(sig)
    }
}

impl fmt::Display for Class<'_> {
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
pub struct DatatypeDecl<'pars> {
    pub class: Arc<Class<'pars>>,

    pub variables: Vec<Arc<Variable<'pars>>>,
    pub events: Vec<Arc<EventDeclaration<'pars>>>,
    pub functions: Vec<Arc<FunctionDeclaration<'pars>>>,
}

#[derive(Debug)]
pub enum TopLevelType<'pars> {
    ForwardDecl(&'pars parser::ForwardDecl, Vec<Arc<Variable<'pars>>>),

    ScopedVariableDecl(Vec<Arc<Variable<'pars>>>),
    ScopedVariablesDecl(Vec<Arc<Variable<'pars>>>),

    DatatypeDecl(DatatypeDecl<'pars>),
    TypeVariablesDecl(Vec<Arc<Variable<'pars>>>),
    FunctionsForwardDecl(Vec<Arc<FunctionDeclaration<'pars>>>),
    ExternalFunctions(Vec<Arc<FunctionDeclaration<'pars>>>),

    FunctionBody(Arc<FunctionDefinition<'pars>>),
    EventBody(Arc<EventDefinition<'pars>>),
    OnBody(&'pars parser::OnBody),
}

#[derive(Debug)]
pub struct TopLevel<'pars> {
    pub range: &'pars Range,
    pub top_level_type: TopLevelType<'pars>,
}
