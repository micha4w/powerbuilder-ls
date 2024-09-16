use std::{collections::HashMap, ops::Deref, path::PathBuf, sync::Arc};

use tokio::sync::RwLock;

use crate::{
    lsp::lsp::add_file,
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
        match self.group {
            Some(g) => g + "`" + &self.name,
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
        match src {
            parser::DataType::Decimal(precission) => {
                DataType::Decimal(precission.as_ref().map(|str| str.parse().unwrap()))
            }
            parser::DataType::Array(sub_type) => DataType::Array(Box::new((&**sub_type).into())),
            parser::DataType::Complex(group, name) => {
                DataType::Complex(GroupedName::new(Some(group.clone()), name.clone()))
            }
            parser::DataType::ID(id) => match id.to_lowercase().as_str() {
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
        match self.variable_type {
            VariableType::Local(var) => &var,
            _ => panic!("unwrap_local failed"),
        }
    }

    pub fn unwrap_scoped(&self) -> &parser::ScopedVariable {
        match self.variable_type {
            VariableType::Scoped(var) => &var,
            _ => panic!("unwrap_scoped failed"),
        }
    }

    pub fn unwrap_argument(&self) -> &parser::Argument {
        match self.variable_type {
            VariableType::Argument(var) => &var,
            _ => panic!("unwrap_argument failed"),
        }
    }

    pub fn unwrap_instance(&self) -> &parser::InstanceVariable {
        match self.variable_type {
            VariableType::Instance(var) => &var,
            _ => panic!("unwrap_instance failed"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Event {
    pub parsed: parser::Event,

    pub returns: DataType,
    pub arguments: Vec<Arc<Variable>>,
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
                    .map(|arg| Variable {
                        variable_type: VariableType::Argument(arg.clone()),
                        data_type: (&arg.variable.data_type).into(),
                        // uses: Vec::new(),
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

    pub fn equals(&self, other: &Event) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    pub fn conflicts(&self, other: &Event) -> bool {
        self.arguments
            .iter()
            .zip(other.arguments.iter())
            .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
    }

    pub fn is_callable(&self, arguments: &Vec<DataType>) -> bool {
        self.arguments
            .iter()
            .zip(arguments.iter())
            .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
            && self.arguments.len() == arguments.len()
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parsed: parser::Function,

    pub returns: DataType,
    pub arguments: Vec<Arc<Variable>>,
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
                    Arc::new(Variable {
                        variable_type: VariableType::Argument(arg.clone()),
                        data_type: (&arg.variable.data_type).into(),
                        // uses: Vec::new(),
                    })
                })
                .collect(),
            returns: parsed.returns.as_ref().into(),
            parsed,
        }
    }

    pub fn equals(&self, other: &Function) -> bool {
        self.returns == other.returns && self.conflicts(other)
    }

    pub fn conflicts(&self, other: &Function) -> bool {
        self.arguments
            .iter()
            .zip(other.arguments.iter())
            .all(|(self_arg, other_arg)| self_arg.data_type == other_arg.data_type)
    }

    pub fn is_callable(&self, arguments: &Vec<DataType>, min_access: &tokens::AccessType) -> bool {
        min_access.strictness()
            >= self
                .parsed
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && self
                .arguments
                .iter()
                .zip(arguments)
                .all(|(self_arg, call_arg)| call_arg.is_convertible(&self_arg.data_type))
            && (self.arguments.len() == arguments.len()
                || (self.arguments.len() < arguments.len() && self.parsed.has_vararg))
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

    pub ons: HashMap<IString, Arc<Function>>,
    pub events: HashMap<IString, Arc<Event>>,
    pub functions: HashMap<IString, Vec<Arc<Function>>>,
    pub external_functions: HashMap<IString, Vec<Arc<Function>>>,

    pub instance_variables: HashMap<IString, Arc<Variable>>,
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
    ) -> Option<Arc<Variable>> {
        let mut class_holder;
        let mut class = self;
        let mut strictness = access.strictness();
        loop {
            if let Some(found) = class
                .instance_variables
                .get(&(&variable.name).into())
                .and_then(|var| {
                    let access = &var.unwrap_instance().access;
                    (write
                        .then_some(&access.write)
                        .unwrap_or(&access.read)
                        .map_or(0, |acc| acc.strictness())
                        > strictness)
                        .then_some(var)
                })
            {
                return Some(found.clone());
            }

            match state.find_class(&class.base).await {
                Some(Complex::Class(cls)) => {
                    class_holder = cls.clone();
                    class = &class_holder;
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

    pub fn find_conflicting_event(&self, event: &Event) -> Option<Arc<Event>> {
        self.events
            .get(&(&event.parsed.name).into())
            .and_then(|ev| ev.conflicts(event).then_some(ev))
            .cloned()
    }

    pub fn find_callable_event(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Arc<Event>> {
        self.events
            .get(&name.into())
            .and_then(|ev| ev.is_callable(arguments).then_some(ev))
            .cloned()
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

    pub fn find_conflicting_function(&self, function: &Function) -> Option<Arc<Function>> {
        for functions in [&self.functions, &self.external_functions] {
            if let Some(func) = functions
                .get(&(&function.parsed.name).into())
                .and_then(|funcs| funcs.iter().find(|func| func.conflicts(function)))
            {
                return Some(func.clone());
            }
        }

        None
    }

    pub fn find_callable_function(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
        min_access: &tokens::AccessType,
    ) -> Option<Arc<Function>> {
        for functions in [&self.functions, &self.external_functions] {
            if let Some(func) = functions.get(&name.into()).and_then(|funcs| {
                funcs
                    .iter()
                    .find(|func| func.is_callable(arguments, min_access))
            }) {
                return Some(func.clone());
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub enum Complex {
    Class(Arc<Class>),
    Enum(Arc<Enum>),
}

impl Complex {
    pub fn name(&self) -> &String {
        match self {
            Complex::Class(class) => &class.name,
            Complex::Enum(r#enum) => &r#enum.name,
        }
    }

    pub fn help(&self) -> &Option<String> {
        match self {
            Complex::Class(class) => &class.help,
            Complex::Enum(r#enum) => &r#enum.help,
        }
    }

    pub fn unwrap_class(&self) -> &Arc<Class> {
        match self {
            Complex::Class(class) => class,
            Complex::Enum(_) => panic!("unwrap_class failed"),
        }
    }
}

#[derive(Default, Clone, Debug)]
struct Usage {
    pub declaration: Option<Range>,
    pub definition: Option<Range>,
    // uses: Vec<Range>,
}

pub struct LintState<'a> {
    pub proj: Arc<RwLock<Project>>,
    pub file: Option<&'a mut File>,
    pub class: Option<Arc<Class>>,

    pub variables: Vec<Arc<Variable>>,
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

    pub fn push_diagnostic(&self, diagnostic: parser::Diagnostic) {
        if let Some(file) = &mut self.file {
            (*file).diagnostics.push(diagnostic);
        }
    }

    pub fn diagnostic_error(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Error,
            message,
            range,
        });
    }
    pub fn diagnostic_warning(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Warning,
            message,
            range,
        });
    }
    pub fn diagnostic_info(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Info,
            message,
            range,
        });
    }
    pub fn diagnostic_hint(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Hint,
            message,
            range,
        });
    }

    pub fn unwrap_file(&self) -> &mut File {
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
    ) -> Option<Arc<Variable>> {
        if let Some(found) = self
            .variables
            .iter()
            .find(|var| var.parsed().name == variable.name)
        {
            return Some(found.clone());
        }

        if let Some(current_file) = &self.file {
            if let Some(current_class) = self.get_current_class().await {
                if let Some(found) = current_class
                    .find_variable(&self, variable, &tokens::AccessType::PRIVATE, write)
                    .await
                {
                    return Some(found);
                }
            }

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

            if let Some(found) = file
                .variables
                .get(&(&variable.name).into())
                // TODO: do error handling if not is_global
                .and_then(|var| {
                    (var.unwrap_scoped().scope == tokens::ScopeModif::GLOBAL).then_some(var)
                })
            {
                return Some(found.clone());
            }
        }

        None
    }

    pub async fn find_function(
        &self,
        name: &String,
        arguments: &Vec<DataType>,
    ) -> Option<Arc<Function>> {
        if let Some(current_file) = &self.file {
            if let Some(mut current_class) = self.class.clone() {
                let mut first_iter = true;
                loop {
                    match self.find_class(&current_class).await {
                        Some(Complex::Class(cls)) => {
                            if let Some(found) = cls.find_callable_function(
                                name,
                                arguments,
                                first_iter
                                    .then_some(&tokens::AccessType::PRIVATE)
                                    .unwrap_or(&tokens::AccessType::PROTECTED),
                            ) {
                                return Some(found);
                            }
                            current_class = cls.base.clone();
                            first_iter = false;
                        }
                        Some(Complex::Enum(_)) | None => break,
                    }
                }
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
                if class.base.combine().to_lowercase() == "function_object" {
                    if let Some(func) =
                        class.find_callable_function(name, arguments, &tokens::AccessType::PUBLIC)
                    {
                        return Some(func.clone());
                    }
                }
            }
        }

        None
    }

    pub async fn find_class(&self, grouped_name: &GroupedName) -> Option<Complex> {
        let GroupedName { group, name } = grouped_name;

        if let Some(current_file) = &self.file {
            if let Some(found) = current_file.classes.get(&name.into()).and_then(|class| {
                (group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref())
                    .then(|| Complex::Class(class.clone()))
            }) {
                return Some(found);
            }
        }

        let proj = self.proj.read().await;
        for (path, _file_lock) in &proj.files {
            match &self.file {
                Some(file) if file.path == *path => continue,
                _ => {}
            }

            let file = _file_lock.read().await;

            if let Some(found) = file
                .classes
                .get(&name.into())
                // TODO: do error handling if not is_global
                .and_then(|class| {
                    (class.is_global
                        && (group.is_none()
                            || class.within.as_ref().map(|w| &w.name) == group.as_ref()))
                    .then(|| Complex::Class(class.clone()))
                })
            {
                if file.lint_progress < LintProgress::Shallow {
                    drop(file);
                    // TODO call lint_file directly?
                    add_file(self.proj.clone(), path.clone(), LintProgress::Shallow).await;
                    todo!("Lint the File!");
                }
                return Some(found);
            }
        }

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
                        child_name = child.base.clone();
                    }
                    Complex::Enum(_) => return Some(false),
                }
            },
            Complex::Enum(base) => loop {
                match self.find_class(&child_name).await? {
                    Complex::Class(child) => child_name = child.base.clone(),
                    Complex::Enum(child) => return Some(Arc::ptr_eq(&base, &child)),
                }
            },
        }
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum LintProgress {
    None,
    OnlyTypes,
    Shallow,
    Complete,
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

pub struct File {
    pub classes: HashMap<IString, Arc<Class>>,
    // Shared with all instances
    pub variables: HashMap<IString, Arc<Variable>>,

    pub diagnostics: Vec<parser::Diagnostic>,

    pub top_levels: Vec<parser::TopLevel>,
    pub path: PathBuf,
    pub lint_progress: LintProgress,
}

impl File {
    pub fn new(path: PathBuf) -> anyhow::Result<File> {
        Ok(File {
            classes: HashMap::new(),
            variables: HashMap::new(),
            diagnostics: Vec::new(),

            top_levels: tokenize_file(&path)?.parse_tokens(),
            path,
            lint_progress: LintProgress::None,
        })
    }
}

pub struct Project {
    // Keep the writing of RwLock to the minimal
    pub files: HashMap<PathBuf, RwLock<File>>,
    // placeholder_file: File,
    pub builtin_enums: HashMap<IString, Arc<Enum>>,
    pub builtin_functions: HashMap<IString, Vec<Arc<Function>>>,
    pub builtin_classes: HashMap<IString, Arc<Class>>,
}

// Use Arc<RwLock<Project>> and only write when necessary and only very short!!!

impl Project {
    pub fn new() -> anyhow::Result<Project> {
        Ok(Project {
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
        })
    }
}
