use core::panic;
use std::{
    backtrace::Backtrace,
    collections::HashMap,
    mem,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{Arc, Weak},
};

use dashmap::mapref::one::Ref;
use futures::{
    future::{self, BoxFuture, Either},
    FutureExt,
};
use tokio::sync::{Mutex, RwLock};

use crate::{
    ls::add_file,
    parser::{
        parser_types::{self as parser, EitherOr, Expression, Statement},
        tokenize_file,
        tokenizer::Token,
        tokenizer_types::{self as tokens, Position, Range},
    },
};

pub enum MaybeRef<'a, T> {
    Ref(&'a T),
    No(T),
}

impl<T> AsRef<T> for MaybeRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            MaybeRef::Ref(t) => t,
            MaybeRef::No(t) => t,
        }
    }
}

pub enum MaybeMut<'a, T> {
    Mut(&'a mut T),
    No(&'a T),
}

impl<T> MaybeMut<'_, T> {
    pub fn unwrap_mut(&mut self) -> &mut T {
        match self {
            MaybeMut::Mut(t) => t,
            MaybeMut::No(_) => panic!("MaybeMut unwrapped when it wasn't Mutable"),
        }
    }
}

impl<T> AsRef<T> for MaybeMut<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            MaybeMut::Mut(t) => t,
            MaybeMut::No(t) => t,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct IString {
    _str: String,
}

impl From<&String> for IString {
    fn from(value: &String) -> Self {
        Self {
            _str: value.to_lowercase(),
        }
    }
}

impl Deref for IString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self._str
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedName {
    pub group: Option<String>,
    pub name: String,
}

impl From<(Option<String>, String)> for GroupedName {
    fn from((group, name): (Option<String>, String)) -> Self {
        Self { group, name }
    }
}

impl From<&(Option<Token>, Token)> for GroupedName {
    fn from((group, name): &(Option<Token>, Token)) -> Self {
        (group, name).into()
    }
}

impl From<(&Option<Token>, &Token)> for GroupedName {
    fn from((group, name): (&Option<Token>, &Token)) -> Self {
        Self::new(
            group.as_ref().map(|g| g.content.clone()),
            name.content.clone(),
        )
    }
}

impl GroupedName {
    pub fn new(group: Option<String>, name: String) -> Self {
        Self { group, name }
    }

    pub fn combine(&self) -> String {
        match &self.group {
            Some(g) => g.clone() + "`" + &self.name,
            None => self.name.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Blob,
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
    Complex(GroupedName),
    Array(Box<DataType>),

    Any,
    Unknown,
    Void,
}

// impl PartialEq for DataType {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Complex((lg, lc)), Self::Complex((rg, rc))) => lg == rg && lc == rc,
//             (Self::Decimal(l), Self::Decimal(r)) => l == r,
//             (Self::Array(l), Self::Array(r)) => l == r,
//             _ => core::mem::discriminant(self) == core::mem::discriminant(other),
//         }
//     }
// }
impl From<Option<&parser::DataType>> for DataType {
    fn from(src: Option<&parser::DataType>) -> DataType {
        src.map_or(DataType::Void, |ret| ret.into())
    }
}

impl From<&parser::DataType> for DataType {
    fn from(src: &parser::DataType) -> DataType {
        (&src.data_type_type).into()
    }
}

impl From<&parser::DataTypeType> for DataType {
    fn from(src: &parser::DataTypeType) -> DataType {
        match src {
            parser::DataTypeType::Decimal(precission) => {
                DataType::Decimal(precission.as_ref().map(|str| str.parse().unwrap()))
            }
            parser::DataTypeType::Array(sub_type) => {
                DataType::Array(Box::new((&**sub_type).into()))
            }
            parser::DataTypeType::Complex(group, name) => {
                DataType::Complex(GroupedName::new(Some(group.clone()), name.clone()))
            }
            parser::DataTypeType::ID(id) => match id.to_lowercase().as_str() {
                "any" => DataType::Any,
                "blob" => DataType::Blob,
                "boolean" => DataType::Boolean,
                "byte" => DataType::Byte,
                "char" => DataType::Char,
                "date" => DataType::Date,
                "datetime" => DataType::Datetime,
                "double" => DataType::Double,
                "integer" | "int" => DataType::Int,
                "long" => DataType::Long,
                "longlong" => DataType::Longlong,
                "longptr" => DataType::Longptr,
                "real" => DataType::Real,
                "string" => DataType::String,
                "time" => DataType::Time,
                "unsignedinteger" | "uint" => DataType::Uint,
                "unsignedlong" | "ulong" => DataType::Ulong,
                _ => DataType::Complex(GroupedName::new(None, id.clone())),
            },
        }
    }
}

// impl From<&tokens::Literal> for DataType {
//     fn from(src: &tokens::Literal) -> DataType {
//         match src {
//             tokens::Literal::NUMBER => DataType::Int,
//             tokens::Literal::DATE => DataType::Date,
//             tokens::Literal::TIME => DataType::Time,
//             tokens::Literal::STRING => DataType::String,
//             tokens::Literal::BOOLEAN => DataType::Boolean,
//             tokens::Literal::ENUM => DataType::Any, // TODO scrape https://docs.appeon.com/pb2022/powerscript_reference
//         }
//     }
// }

impl ToString for DataType {
    fn to_string(&self) -> String {
        match self {
            DataType::Byte => "byte".into(),
            DataType::Char => "char".into(),
            DataType::String => "string".into(),
            DataType::Blob => "blob".into(),

            DataType::Date => "date".into(),
            DataType::Time => "time".into(),
            DataType::Datetime => "datetime".into(),

            DataType::Boolean => "boolean".into(),
            DataType::Int => "int".into(),
            DataType::Uint => "unsigned int".into(),
            DataType::Long => "long".into(),
            DataType::Ulong => "unsigned long".into(),
            DataType::Longlong => "longlong".into(),
            DataType::Longptr => "longptr".into(),
            DataType::Real => "real".into(),
            DataType::Double => "double".into(),
            DataType::Decimal(precission) => {
                if let Some(prec) = precission {
                    format!("decimal {{{}}}", prec)
                } else {
                    "decimal".into()
                }
            }
            DataType::Complex(grouped_name) => grouped_name.combine(),
            DataType::Array(data_type) => data_type.to_string() + "[]",

            DataType::Any => "any".into(),
            DataType::Unknown => "<Error>".into(),
            DataType::Void => "void".into(),
        }
    }
}

impl DataType {
    pub fn is_numeric(&self) -> bool {
        self.numeric_precedence().is_some()
    }

    pub fn numeric_precedence(&self) -> Option<u8> {
        match self {
            DataType::Int => Some(0),
            DataType::Uint => Some(1),
            DataType::Long => Some(2),
            DataType::Ulong => Some(3),
            DataType::Longlong => Some(4),
            DataType::Longptr => Some(5),
            DataType::Real => Some(6),
            DataType::Double => Some(7),
            DataType::Decimal(_) => Some(8),
            DataType::Any => Some(9),
            DataType::Unknown => Some(10),

            DataType::Blob
            | DataType::Boolean
            | DataType::Byte
            | DataType::Char
            | DataType::Date
            | DataType::Datetime
            | DataType::String
            | DataType::Time
            | DataType::Complex(_)
            | DataType::Array(_)
            | DataType::Void => None,
        }
    }
}

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

    pub data_type: DataType,
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

    pub returns: DataType,
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
                            data_type: (&arg.variable.data_type).into(),
                            // uses: Vec::new(),
                        })
                        .into()
                    })
                    .collect();
                returns = ret.as_ref().into()
            }
            parser::EventType::System(name) => {
                arguments = Vec::new();
                returns = DataType::Void;
                // TODO get system events from where?
            }
            parser::EventType::Predefined => {
                arguments = Vec::new();
                returns = DataType::Void;
                // TODO get arguments and return value from base class using parsed.name
            }
        }

        Event {
            declaration,
            definition,
            uses: Vec::new(),

            parsed,
            returns,
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
                            data_type: (&arg.variable.data_type).into(),
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

    pub returns: DataType,
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

            returns: parsed.returns.as_ref().into(),
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
        // file: Option<Rc<RefCell<File>>>,
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

    pub async fn get_accessible_variables(
        &self,
        state: &LintState<'_>,
        access: &tokens::AccessType,
        write: bool,
    ) -> (
        Vec<(IString, Arc<Mutex<Variable>>)>,
        Vec<(IString, Arc<Mutex<Variable>>, &'static str)>,
    ) {
        let mut variables = Vec::new();
        let mut err_variables = Vec::new();

        let mut class_arc_holder;
        let mut class_holder = None;
        let mut class = self;
        let mut strictness = access.strictness();
        loop {
            for (name, var) in &class.instance_variables {
                let access = var.lock().await.unwrap_instance().1.access.clone();
                if write
                    .then_some(&access.write)
                    .unwrap_or(&access.read)
                    .map_or(0, |acc| acc.strictness())
                    <= strictness
                {
                    variables.push((name.clone(), var.clone()));
                } else {
                    err_variables.push((
                        name.clone(),
                        var.clone(),
                        "Cannot access Private Variable",
                    ));
                }
            }

            match state.find_class(&class.base).await {
                Some(Complex::Class(cls)) => {
                    drop(class_holder);
                    class_arc_holder = cls.clone();
                    class_holder = Some(class_arc_holder.lock().await);
                    class = class_holder.as_ref().unwrap();
                }
                Some(Complex::Enum(_)) | None => break,
            }

            if strictness < tokens::AccessType::PRIVATE.strictness() {
                strictness = tokens::AccessType::PROTECTED.strictness();
            }
        }

        (variables, err_variables)
    }

    pub async fn find_variable(
        &self,
        state: &LintState<'_>,
        variable: &parser::VariableAccess,
        access: &tokens::AccessType,
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

            match state.find_class(&class.base).await {
                Some(Complex::Class(cls)) => {
                    drop(class_holder);
                    class_arc_holder = cls.clone();
                    class_holder = Some(class_arc_holder.lock().await);
                    class = class_holder.as_ref().unwrap();
                }
                Some(Complex::Enum(_)) | None => break,
            }

            if strictness < tokens::AccessType::PRIVATE.strictness() {
                strictness = tokens::AccessType::PROTECTED.strictness();
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

pub struct LintState<'a> {
    pub proj: Arc<RwLock<Project>>,
    pub file: MaybeMut<'a, File>,
    pub class: Option<Arc<Mutex<Class>>>,

    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,
    pub return_type: DataType,
}

impl<'a> LintState<'a> {
    pub fn new(proj: Arc<RwLock<Project>>, file: MaybeMut<'a, File>) -> Self {
        Self {
            proj,
            file,
            class: None,
            variables: HashMap::new(),
            return_type: DataType::Void,
        }
    }

    pub fn push_diagnostic(&mut self, mut diagnostic: parser::Diagnostic) {
        if let MaybeMut::Mut(file) = &mut self.file {
            diagnostic.message += format!("\n {}", Backtrace::capture()).as_str();
            file.diagnostics.push(diagnostic);
        }
    }

    pub fn diagnostic_error(&mut self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Error,
            message,
            range,
        });
    }
    pub fn diagnostic_warning(&mut self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Warning,
            message,
            range,
        });
    }
    pub fn diagnostic_info(&mut self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Info,
            message,
            range,
        });
    }
    pub fn diagnostic_hint(&mut self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Hint,
            message,
            range,
        });
    }

    // pub async fn get_current_class(&self) -> Option<Arc<Class>> {
    //     match &self.class {
    //         Some(current) => Some(
    //             self.find_class(&GroupedName::new(None, current.name.clone()))
    //                 .await
    //                 .unwrap()
    //                 .unwrap_class()
    //                 .clone(),
    //         ),
    //         None => None,
    //     }
    // }

    pub async fn get_accessible_data_types(
        &self,
    ) -> (Vec<DataType>, Vec<(DataType, &'static str)>) {
        let mut err_data_types = Vec::new();
        let mut data_types = vec![
            DataType::Any,
            DataType::Blob,
            DataType::Boolean,
            DataType::Byte,
            DataType::Char,
            DataType::Date,
            DataType::Datetime,
            DataType::Decimal(None),
            DataType::Double,
            DataType::Int,
            DataType::Long,
            DataType::Longlong,
            DataType::Longptr,
            DataType::Real,
            DataType::String,
            DataType::Time,
            DataType::Uint,
            DataType::Ulong,
        ];
        // .into_iter()
        // .map(|dt| Arc::new(Mutex::new(dt)))
        // .collect::<Vec<_>>();

        // Complex(GroupedName),

        for class in self.file.as_ref().classes.values() {
            data_types.push(DataType::Complex(GroupedName::new(
                None,
                class.lock().await.name.clone(),
            )));
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            if self.file.as_ref().path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            for class_mut in file.classes.values() {
                let class = class_mut.lock().await;
                let complex = DataType::Complex(GroupedName::new(None, class.name.clone()));
                if class.is_global {
                    data_types.push(complex);
                } else {
                    err_data_types.push((complex, "Cannot access non Global Class"));
                }
            }
        }

        // TODO arrays?

        (data_types, err_data_types)
    }

    pub async fn get_accessible_variables(
        &self,
        local_before: &Position,
        write: bool,
    ) -> (
        Vec<(IString, Arc<Mutex<Variable>>)>,
        Vec<(IString, Arc<Mutex<Variable>>, &'static str)>,
    ) {
        let mut variables = Vec::new();
        let mut err_variables = Vec::new();

        for (name, var) in &self.variables {
            if &var.lock().await.parsed().range.end < local_before {
                variables.push((name.clone(), var.clone()));
            } else {
                err_variables.push((
                    name.clone(),
                    var.clone(),
                    "Variable has not been defined yet",
                ));
            }
        }

        if let Some(current_class) = &self.class {
            let (vars, err_vars) = current_class
                .lock()
                .await
                .get_accessible_variables(self, &tokens::AccessType::PRIVATE, write)
                .await;

            variables.extend(vars);
            err_variables.extend(err_vars);
        }

        for (name, var) in &self.file.as_ref().variables {
            variables.push((name.clone(), var.clone()));
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            if self.file.as_ref().path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            for (name, var) in &file.variables {
                match var.lock().await.unwrap_scoped().scope {
                    tokens::ScopeModif::GLOBAL => variables.push((name.clone(), var.clone())),
                    tokens::ScopeModif::SHARED => err_variables.push((
                        name.clone(),
                        var.clone(),
                        "Variable is not defined as Global",
                    )),
                }
            }
        }

        (variables, err_variables)
    }

    pub async fn find_variable(
        &self,
        variable: &parser::VariableAccess,
        write: bool,
    ) -> Option<Arc<Mutex<Variable>>> {
        let iname = (&variable.name.content).into();
        if let Some(var) = self.variables.get(&iname) {
            return Some(var.clone());
        };

        if let Some(current_class) = &self.class {
            if let Some(found) = current_class
                .lock()
                .await
                .find_variable(&self, variable, &tokens::AccessType::PRIVATE, write)
                .await
            {
                return Some(found);
            }
        }

        if let Some(found) = self
            .file
            .as_ref()
            .variables
            .get(&(&variable.name.content).into())
        {
            return Some(found.clone());
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            if self.file.as_ref().path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            if let Some(found) = file.variables.get(&(&variable.name.content).into()) {
                // TODO: do error handling if not is_global
                return (found.lock().await.unwrap_scoped().scope == tokens::ScopeModif::GLOBAL)
                    .then_some(found.clone());
            }
        }

        None
    }

    pub async fn find_function(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Arc<Mutex<Function>>> {
        if let Some(mut class_arc) = self.class.clone() {
            let mut first_iter = true;
            loop {
                if let Some(found) = self
                    .find_callable_function_in_class(
                        &class_arc,
                        name,
                        arguments,
                        first_iter
                            .then_some(&tokens::AccessType::PRIVATE)
                            .unwrap_or(&tokens::AccessType::PROTECTED),
                    )
                    .await
                {
                    return Some(found);
                }

                let complex = self.find_class(&class_arc.lock().await.base).await;
                match complex {
                    Some(Complex::Class(base)) => {
                        class_arc = base;
                    }
                    Some(Complex::Enum(_)) | None => break,
                }
                first_iter = false;
            }
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            if self.file.as_ref().path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            for (name, class) in &file.classes {
                if class.lock().await.base.combine().to_lowercase() == "function_object" {
                    if let Some(func) = self
                        .find_callable_function_in_class(
                            &class,
                            name,
                            arguments,
                            &tokens::AccessType::PUBLIC,
                        )
                        .await
                    {
                        return Some(func.clone());
                    }
                }
            }
        }

        if let Some(funcs) = proj.builtin_functions.get(&name.into()) {
            for func in funcs {
                let func_holder = func.lock().await;
                if self
                    .is_function_callable(&func_holder, arguments, &tokens::AccessType::PUBLIC)
                    .await
                {
                    return Some(func.clone());
                }
            }
        }

        None
    }

    pub async fn find_class(&self, grouped_name: &GroupedName) -> Option<Complex> {
        let GroupedName { group, name } = grouped_name;

        if let Some(class_arc) = self.file.as_ref().classes.get(&name.into()) {
            let class = class_arc.lock().await;
            if group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref() {
                return Some(Complex::Class(class_arc.clone()));
            }
        }

        let proj = self.proj.read().await;
        if group.is_none() {
            if let Some(found) = proj
                .builtin_classes
                .get(&name.into())
                .cloned()
                .map(Complex::Class)
            {
                return Some(found);
            }
            if let Some(found) = proj
                .builtin_enums
                .get(&name.into())
                .cloned()
                .map(Complex::Enum)
            {
                return Some(found);
            }
        }

        for (path, _file_lock) in &proj.files {
            if self.file.as_ref().path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            if let Some(class_arc) = file.classes.get(&name.into()).cloned() {
                let class = class_arc.lock().await;
                // TODO: do error handling if not is_global
                if class.is_global
                    && (group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref())
                {
                    if file.top_levels.get_progress() < LintProgress::Shallow {
                        drop(class);
                        drop(file);
                        // TODO call lint_file directly?
                        let err = add_file(self.proj.clone(), path, LintProgress::Shallow).await;
                    }
                    return Some(Complex::Class(class_arc.clone()));
                }
            }
        }

        // TODO scan more files?

        None
    }

    pub async fn is_event_callable(&self, event: &Event, arguments: &Vec<DataType>) -> bool {
        event.arguments.len() == arguments.len()
            && future::join_all(
                future::join_all(event.arguments.iter().map(|arg| arg.lock()))
                    .await
                    .iter()
                    .zip(arguments.iter())
                    .map(|(self_arg, call_arg)| {
                        self.is_convertible(&call_arg, &self_arg.data_type)
                    }),
            )
            .await
            .iter()
            .all(|x| *x)
    }
    pub async fn is_function_callable(
        &self,
        func: &Function,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> bool {
        min_access.strictness()
            >= func
                .parsed
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && (func.parsed.arguments.len() == arguments.len()
                || (func.parsed.arguments.len() < arguments.len() && func.parsed.vararg.is_some()))
            && future::join_all(
                func.parsed
                    .arguments
                    .iter()
                    .map(|arg| (&arg.variable.data_type).into())
                    .collect::<Vec<_>>()
                    .iter()
                    .zip(arguments)
                    .map(|(self_arg, call_arg)| self.is_convertible(call_arg, self_arg)),
            )
            .await
            .iter()
            .all(|x| *x)
    }

    pub fn find_callable_function_in_class<'b>(
        &'b self,
        class_mut: &'b Arc<Mutex<Class>>,
        name: &'b String,
        arguments: &'b Vec<DataType>,
        min_access: &'b tokens::AccessType,
    ) -> BoxFuture<Option<Arc<Mutex<Function>>>> {
        async move {
            let class = class_mut.lock().await;
            let all_functions = [&class.functions, &class.external_functions]
                .map(|funcs| funcs.get(&name.into()).cloned());
            drop(class);

            for functions in all_functions {
                let Some(functions) = functions else {
                    continue;
                };
                for func_mut in functions {
                    let func = func_mut.lock().await;
                    if self
                        .is_function_callable(&func, arguments, min_access)
                        .await
                    {
                        return Some(func_mut.clone());
                    }
                }
            }

            match self.find_class(&class_mut.lock().await.base).await {
                Some(Complex::Class(base)) => {
                    self.find_callable_function_in_class(&base, name, arguments, min_access)
                        .await
                }
                _ => None,
            }
        }
        .boxed()
    }

    pub fn find_callable_event_in_class<'b>(
        &'b self,
        class: &'b Arc<Mutex<Class>>,
        name: &'b String,
        arguments: &'b Vec<DataType>,
    ) -> BoxFuture<Option<Arc<Mutex<Event>>>> {
        async move {
            if let Some(event_mut) = class.lock().await.events.get(&name.into()).cloned() {
                let event = event_mut.lock().await;
                if self.is_event_callable(&event, arguments).await {
                    return Some(event_mut.clone());
                }
            }

            match self.find_class(&class.lock().await.base).await {
                Some(Complex::Class(base)) => {
                    self.find_callable_event_in_class(&base, name, arguments)
                        .await
                }
                _ => None,
            }
        }
        .boxed()
    }

    pub async fn inherits_from(
        &self,
        child_name: &GroupedName,
        base_name: &GroupedName,
    ) -> Option<bool> {
        if child_name == base_name {
            return Some(true);
        }

        let mut child_name = child_name.clone();
        match self.find_class(base_name).await? {
            Complex::Class(base) => loop {
                match self.find_class(&child_name).await? {
                    Complex::Class(child) => {
                        if Arc::ptr_eq(&base, &child) {
                            return Some(true);
                        }
                        child_name = child.lock().await.base.clone();
                    }
                    Complex::Enum(_) => return Some(false),
                }
            },
            Complex::Enum(base) => loop {
                match self.find_class(&child_name).await? {
                    Complex::Class(child) => child_name = child.lock().await.base.clone(),
                    Complex::Enum(child) => return Some(Arc::ptr_eq(&base, &child)),
                }
            },
        }
    }

    pub fn is_convertible<'b>(
        &'b self,
        from: &'b DataType,
        to: &'b DataType,
    ) -> BoxFuture<'b, bool> {
        async move {
            match (from, to) {
                (DataType::Unknown, _) | (_, DataType::Unknown) => true,
                (DataType::Any, _) | (_, DataType::Any) => true,
                (DataType::Array(nested_from), DataType::Array(nested_to)) => {
                    self.is_convertible(&nested_from, &nested_to).await
                }
                (DataType::Complex(from_name), DataType::Complex(to_name)) => {
                    self.inherits_from(from_name, to_name)
                        .await
                        .unwrap_or(false)
                        || self
                            .inherits_from(to_name, from_name)
                            .await
                            .unwrap_or(false)
                }
                _ => from == to || (from.is_numeric() && to.is_numeric()),
            }
        }
        .boxed()
    }
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

#[derive(Debug)]
pub struct File {
    pub classes: HashMap<IString, Arc<Mutex<Class>>>,
    // Shared and Global variables
    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,

    pub diagnostics: Vec<parser::Diagnostic>,

    pub top_levels: ProgressedTopLevels,
    pub path: PathBuf,
}

impl File {
    pub fn new(path: PathBuf) -> anyhow::Result<File> {
        let mut file = tokenize_file(&path)?;

        Ok(File {
            classes: HashMap::new(),
            variables: HashMap::new(),

            top_levels: ProgressedTopLevels::None(file.parse_tokens()),
            diagnostics: file.get_syntax_errors(),

            path,
        })
    }
}

#[derive(Debug)]
pub struct Project {
    // Keep the writing of RwLock to the minimal
    pub files: HashMap<PathBuf, RwLock<File>>,
    // placeholder_file: File,
    pub builtin_enums: HashMap<IString, Arc<Mutex<Enum>>>,
    pub builtin_functions: HashMap<IString, Vec<Arc<Mutex<Function>>>>,
    pub builtin_classes: HashMap<IString, Arc<Mutex<Class>>>,
}

// Use Arc<RwLock<Project>> and only write when necessary and only very short!!!

impl Project {
    pub async fn new(
        enums_file: PathBuf,
        classes_file: PathBuf,
        functions_file: PathBuf,
    ) -> anyhow::Result<Project> {
        let mut proj = Project {
            files: HashMap::new(),
            // placeholder_file: Rc::new(RefCell::new(File {
            //     classes: vec![],
            //     shared_variables: vec![],
            //     global_variables: vec![],
            //     diagnostics: vec![],
            //     top_levels: vec![],

            //     path: "".into(),
            //     lint_state: LintState::Complete,
            // })),
            builtin_enums: HashMap::new(),
            builtin_functions: HashMap::new(),
            builtin_classes: HashMap::new(),
        };

        proj.load_enums(enums_file)?;
        proj.load_builtin_classes(classes_file).await?;
        proj.load_builtin_functions(functions_file)?;

        Ok(proj)
    }
}
