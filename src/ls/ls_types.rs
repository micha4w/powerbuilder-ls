use std::{
    collections::HashMap,
    ops::Deref,
    path::{Path, PathBuf},
    sync::Arc,
};

use futures::future;
use tokio::sync::{Mutex, RwLock};

use crate::{
    ls::add_file,
    parser::{
        parser_types as parser, tokenize_file,
        tokenizer_types::{self as tokens, Range},
    },
};

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
    group: Option<String>,
    name: String,
}

impl From<(Option<String>, String)> for GroupedName {
    fn from((group, name): (Option<String>, String)) -> Self {
        Self { group, name }
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

impl From<&tokens::Literal> for DataType {
    fn from(src: &tokens::Literal) -> DataType {
        match src {
            tokens::Literal::NUMBER => DataType::Int,
            tokens::Literal::DATE => DataType::Date,
            tokens::Literal::TIME => DataType::Time,
            tokens::Literal::STRING => DataType::String,
            tokens::Literal::BOOLEAN => DataType::Boolean,
            tokens::Literal::ENUM => DataType::Any, // TODO scrape https://docs.appeon.com/pb2022/powerscript_reference
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

    pub fn is_convertible(&self, other: &DataType) -> bool {
        match (self, other) {
            (DataType::Unknown, _) | (_, DataType::Unknown) => true,
            (DataType::Any, _) | (_, DataType::Any) => true,
            (DataType::Array(self_type), DataType::Array(other_type)) => {
                self_type.is_convertible(&other_type)
            }
            (DataType::Complex(_), DataType::Complex(_)) => {
                todo!("Check if one class in ancestor of other");
            }
            _ => self == other || (self.is_numeric() && other.is_numeric()),
        }
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
    // pub uses: Vec<Range>,
}

impl Variable {
    pub fn parsed(&self) -> &parser::Variable {
        match &self.variable_type {
            VariableType::Local(local) => &local,
            VariableType::Scoped(scoped) => &scoped.variable,
            VariableType::Argument(arg) => &arg.variable,
            VariableType::Instance(instance) => &instance.variable,
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

    pub fn unwrap_instance(&self) -> &parser::InstanceVariable {
        match &self.variable_type {
            VariableType::Instance(var) => &var,
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
    pub definition: Option<Range>,
    pub uses: Vec<Range>,
}

impl Event {
    pub fn new(
        parsed: parser::Event,
        declaration: Option<Range>,
        definition: Option<Range>,
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

    pub async fn is_callable(&self, arguments: &Vec<DataType>) -> bool {
        self.arguments.len() == arguments.len()
            && future::join_all(self.arguments.iter().map(|arg| arg.lock()))
                .await
                .iter()
                .zip(arguments.iter())
                .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parsed: parser::Function,

    pub returns: DataType,
    pub arguments: Vec<Arc<Mutex<Variable>>>,
    pub help: Option<String>,

    pub declaration: Option<Range>,
    pub definition: Option<Range>,
    // pub uses: Vec<Range>,
}

impl Function {
    pub fn new(
        parsed: parser::Function,
        declaration: Option<Range>,
        definition: Option<Range>,
    ) -> Self {
        Function {
            declaration,
            definition,
            // uses: Vec::new(),
            help: None,

            arguments: parsed
                .arguments
                .iter()
                .map(|arg| {
                    Mutex::new(Variable {
                        variable_type: VariableType::Argument(arg.clone()),
                        data_type: (&arg.variable.data_type).into(),
                        // uses: Vec::new(),
                    })
                    .into()
                })
                .collect(),
            returns: parsed.returns.as_ref().into(),
            parsed,
        }
    }

    pub async fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other).await
    }

    pub async fn conflicts(&self, other: &Function) -> bool {
        self.arguments.len() == other.arguments.len() && {
            let self_args = future::join_all(self.arguments.iter().map(|arg| arg.lock())).await;
            let other_args = future::join_all(other.arguments.iter().map(|arg| arg.lock())).await;

            self_args
                .iter()
                .zip(other_args)
                .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
        }
    }

    pub async fn is_callable(
        &self,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> bool {
        min_access.strictness()
            >= self
                .parsed
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && (self.arguments.len() == arguments.len()
                || (self.arguments.len() < arguments.len() && self.parsed.has_vararg))
            // && self
            //     .arguments
            //     .iter()
            //     .zip(arguments)
            //     .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
            && future::join_all(self.arguments.iter().map(|arg| arg.lock()))
                .await
                .iter()
                .zip(arguments.iter())
                .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
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
            if let Some(found) = class.instance_variables.get(&(&variable.name).into()) {
                let access = &found.lock().await.unwrap_instance().access.clone();
                if write
                    .then_some(&access.write)
                    .unwrap_or(&access.read)
                    .map_or(0, |acc| acc.strictness())
                    > strictness
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
        if let Some(ev) = self.events.get(&(&event.parsed.name).into()) {
            ev.lock().await.conflicts(event).await.then_some(ev.clone())
        } else {
            None
        }
    }

    pub async fn find_callable_event(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Arc<Mutex<Event>>> {
        if let Some(ev) = self.events.get(&name.into()) {
            ev.lock()
                .await
                .is_callable(arguments)
                .await
                .then_some(ev.clone())
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
            if let Some(funcs) = functions.get(&(&function.parsed.name).into()) {
                for func in funcs {
                    if func.lock().await.conflicts(function).await {
                        return Some(func.clone());
                    }
                }
            }
        }

        None
    }

    pub async fn find_callable_function(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> Option<Arc<Mutex<Function>>> {
        for functions in [&self.functions, &self.external_functions] {
            if let Some(funcs) = functions.get(&name.into()) {
                for func in funcs {
                    if func.lock().await.is_callable(arguments, min_access).await {
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
    pub file: Option<&'a mut File>,
    pub class: Option<Arc<Mutex<Class>>>,

    pub variables: Vec<Arc<Mutex<Variable>>>,
    pub return_type: DataType,
}

impl<'a> LintState<'a> {
    pub fn new(proj: Arc<RwLock<Project>>, file: Option<&'a mut File>) -> Self {
        Self {
            proj,
            file,
            class: None,
            variables: Vec::new(),
            return_type: DataType::Void,
        }
    }

    pub fn push_diagnostic(&mut self, diagnostic: parser::Diagnostic) {
        if let Some(file) = &mut self.file {
            (*file).diagnostics.push(diagnostic);
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

    pub fn unwrap_file(&mut self) -> &mut File {
        match &mut self.file {
            Some(file) => *file,
            None => panic!("unwrap_file failed"),
        }
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

    pub async fn find_variable(
        &self,
        variable: &parser::VariableAccess,
        write: bool,
    ) -> Option<Arc<Mutex<Variable>>> {
        for var in &self.variables {
            if var.lock().await.parsed().name == variable.name {
                return Some(var.clone());
            }
        }

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

        if let Some(current_file) = &self.file {
            if let Some(found) = current_file.variables.get(&(&variable.name).into()) {
                return Some(found.clone());
            }
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            match &self.file {
                Some(file) if file.path == *path => continue,
                _ => {}
            }

            let file = _file_lock.read().await;

            if let Some(found) = file.variables.get(&(&variable.name).into()) {
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
                let class = class_arc.lock().await;
                if let Some(found) = class
                    .find_callable_function(
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

                match self.find_class(&class.base).await {
                    Some(Complex::Class(base)) => {
                        drop(class);
                        class_arc = base;
                    }
                    Some(Complex::Enum(_)) | None => break,
                }
                first_iter = false;
            }
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            match &self.file {
                Some(file) if file.path == *path => continue,
                _ => {}
            }

            let file = _file_lock.read().await;

            for (name, class) in &file.classes {
                let class = class.lock().await;
                if class.base.combine().to_lowercase() == "function_object" {
                    if let Some(func) = class
                        .find_callable_function(name, arguments, &tokens::AccessType::PUBLIC)
                        .await
                    {
                        return Some(func.clone());
                    }
                }
            }
        }

        if let Some(funcs) = proj.builtin_functions.get(&name.into()) {
            for func in funcs {
                if func
                    .lock()
                    .await
                    .is_callable(arguments, &tokens::AccessType::PUBLIC)
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

        if let Some(current_file) = &self.file {
            if let Some(class_arc) = current_file.classes.get(&name.into()) {
                let class = class_arc.lock().await;
                if group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref() {
                    return Some(Complex::Class(class_arc.clone()));
                }
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
            match &self.file {
                Some(file) if file.path == *path => continue,
                _ => {}
            }

            let file = _file_lock.read().await;

            if let Some(class_arc) = file.classes.get(&name.into()).cloned() {
                let class = class_arc.lock().await;
                // TODO: do error handling if not is_global
                if class.is_global
                    && (group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref())
                {
                    if file.lint_progress < LintProgress::Shallow {
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
pub struct File {
    pub classes: HashMap<IString, Arc<Mutex<Class>>>,
    // Shared with all instances
    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,

    pub diagnostics: Vec<parser::Diagnostic>,

    pub top_levels: Vec<parser::TopLevel>,
    pub path: PathBuf,
    pub lint_progress: LintProgress,
}

impl File {
    pub fn new(path: PathBuf) -> anyhow::Result<File> {
        let mut file = tokenize_file(&path)?;

        Ok(File {
            classes: HashMap::new(),
            variables: HashMap::new(),

            top_levels: file.parse_tokens(),
            diagnostics: file.get_syntax_errors(),

            path,
            lint_progress: LintProgress::None,
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
    pub fn new(
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
        proj.load_builtin_classes(classes_file)?;
        proj.load_builtin_functions(functions_file)?;

        let x = Some(19);
            
        Ok(proj)
    }
}
