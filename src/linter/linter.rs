use std::{backtrace::Backtrace, collections::HashMap, sync::Arc};

use futures::{
    future::{self, BoxFuture},
    FutureExt,
};

use super::{
    project::{File, Project},
    types::*,
};
use crate::{
    parser::{self, GroupedName},
    tokenizer,
    types::*,
};

pub struct Linter<'a> {
    pub proj: &'a Project,
    pub file: &'a RwLock<File>,
    pub diagnose: bool,

    pub class: Option<Arc<Mutex<Class>>>,

    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,
    pub return_type: parser::DataTypeType,
}

impl<'a> Linter<'a> {
    pub fn new(proj: &'a Project, file: &'a RwLock<File>, diagnose: bool) -> Self {
        Self {
            proj,
            file,
            diagnose,
            class: None,
            variables: HashMap::new(),
            return_type: parser::DataTypeType::Void,
        }
    }

    pub async fn push_diagnostic(&self, mut diagnostic: parser::Diagnostic) {
        if self.diagnose {
            if cfg!(debug_assertions) {
                diagnostic.message += "\n";
                diagnostic.message += Backtrace::capture().to_string().as_str();
            }

            self.file.write().await.diagnostics.push(diagnostic);
        }
    }

    pub async fn diagnostic_error(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Error,
            message,
            range,
        })
        .await
    }
    pub async fn diagnostic_warning(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Warning,
            message,
            range,
        })
        .await
    }
    pub async fn diagnostic_info(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Info,
            message,
            range,
        })
        .await
    }
    pub async fn diagnostic_hint(&self, message: String, range: Range) {
        self.push_diagnostic(parser::Diagnostic {
            severity: parser::Severity::Hint,
            message,
            range,
        })
        .await
    }

    pub async fn get_accessible_data_types(
        &self,
    ) -> (
        Vec<parser::DataTypeType>,
        Vec<(parser::DataTypeType, &'static str)>,
    ) {
        let mut err_data_types = Vec::new();
        let mut data_types = vec![
            parser::DataTypeType::Any,
            parser::DataTypeType::Blob,
            parser::DataTypeType::Boolean,
            parser::DataTypeType::Byte,
            parser::DataTypeType::Char,
            parser::DataTypeType::Date,
            parser::DataTypeType::Datetime,
            parser::DataTypeType::Decimal(None),
            parser::DataTypeType::Double,
            parser::DataTypeType::Int,
            parser::DataTypeType::Long,
            parser::DataTypeType::Longlong,
            parser::DataTypeType::Longptr,
            parser::DataTypeType::Real,
            parser::DataTypeType::String,
            parser::DataTypeType::Time,
            parser::DataTypeType::Uint,
            parser::DataTypeType::Ulong,
        ];

        for class in self.file.read().await.classes.values() {
            data_types.push(parser::DataTypeType::Complex(GroupedName::new(
                None,
                class.lock().await.name.clone(),
            )));
        }

        let current_path = self.file.read().await.uri.clone();
        for (path, _file_lock) in &self.proj.files {
            if current_path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            for class_mut in file.classes.values() {
                let class = class_mut.lock().await;
                let complex =
                    parser::DataTypeType::Complex(GroupedName::new(None, class.name.clone()));
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

    async fn get_variables_helper<'b>(
        &'b self,
        local_before: &'b Position,
        write: bool,
        accessible: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>, Option<String>)> {
        let mut out = Vec::new();

        for (name, var) in &self.variables {
            let is_accessible = &var.lock().await.parsed().range.end <= local_before;

            if is_accessible == accessible {
                let err = (!accessible).then(|| "Variable has not been defined yet".to_string());
                out.push((name.clone(), var.clone(), err));
            }
        }

        if let Some(current_class) = &self.class {
            out.extend(
                self.get_variables_in_class_helper(
                    current_class.clone(),
                    &tokenizer::AccessType::PRIVATE,
                    write,
                    accessible,
                )
                .await,
            );
        };

        for (name, var) in &self.file.read().await.variables {
            out.push((name.clone(), var.clone(), None));
        }

        let current_path = self.file.read().await.uri.clone();
        for (path, file) in &self.proj.files {
            if current_path == *path {
                continue;
            }

            for (name, var) in file.read().await.variables.clone() {
                let is_accessible = match var.lock().await.unwrap_scoped().scope {
                    tokenizer::ScopeModif::GLOBAL => true,
                    tokenizer::ScopeModif::SHARED => false,
                };

                if is_accessible == accessible {
                    let err =
                        (!accessible).then(|| "Variable is not defined as Global".to_string());
                    out.push((name.clone(), var.clone(), err));
                }
            }
        }

        out
    }

    pub async fn get_variables<'b>(
        &'b self,
        local_before: &'b Position,
        write: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>)> {
        self.get_variables_helper(local_before, write, true)
            .await
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect()
    }

    pub async fn get_inaccessible_variables<'b>(
        &'b self,
        local_before: &'b Position,
        write: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>, String)> {
        self.get_variables_helper(local_before, write, true)
            .await
            .into_iter()
            .map(|(name, var, err)| (name, var, err.unwrap()))
            .collect()
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
                .find_variable(&self, variable, &tokenizer::AccessType::PRIVATE, write)
                .await
            {
                return Some(found);
            }
        }

        if let Some(found) = self
            .file
            .read()
            .await
            .variables
            .get(&(&variable.name.content).into())
        {
            return Some(found.clone());
        }

        let current_path = self.file.read().await.uri.clone();
        for (path, _file_lock) in &self.proj.files {
            if current_path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            if let Some(found) = file.variables.get(&(&variable.name.content).into()) {
                // TODO: do error handling if not is_global
                return (found.lock().await.unwrap_scoped().scope == tokenizer::ScopeModif::GLOBAL)
                    .then_some(found.clone());
            }
        }

        None
    }

    pub async fn find_function(
        &self,
        name: &String,
        arguments: &Vec<parser::DataTypeType>,
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
                            .then_some(&tokenizer::AccessType::PRIVATE)
                            .unwrap_or(&tokenizer::AccessType::PROTECTED),
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

        let current_path = self.file.read().await.uri.clone();
        for (path, _file_lock) in &self.proj.files {
            if current_path == *path {
                continue;
            }

            let file = _file_lock.read().await;

            for (name, class) in &file.classes {
                if class.lock().await.base.to_string().to_lowercase() == "function_object" {
                    if let Some(func) = self
                        .find_callable_function_in_class(
                            &class,
                            name,
                            arguments,
                            &tokenizer::AccessType::PUBLIC,
                        )
                        .await
                    {
                        return Some(func.clone());
                    }
                }
            }
        }

        if let Some(funcs) = self.proj.builtin_functions.get(&name.into()) {
            for func in funcs {
                let func_holder = func.lock().await;
                if self
                    .is_function_callable(&func_holder, arguments, &tokenizer::AccessType::PUBLIC)
                    .await
                {
                    return Some(func.clone());
                }
            }
        }

        None
    }

    pub async fn get_functions_in_class_helper(
        &self,
        mut class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
        accessible: bool,
    ) -> Vec<(IString, Arc<Mutex<Function>>, Option<String>)> {
        let mut strictness = access.strictness();
        let mut out = Vec::new();

        loop {
            let class = class_mut.lock().await;
            for functions in [&class.functions, &class.external_functions] {
                for (name, funcs) in functions {
                    for func_mut in funcs {
                        let is_accessible = func_mut
                            .lock()
                            .await
                            .parsed()
                            .access
                            .map_or(0, |acc| acc.strictness())
                            <= strictness;

                        if is_accessible == accessible {
                            let err =
                                (!accessible).then(|| "Cannot access Private Function".to_string());
                            out.push((name.clone(), func_mut.clone(), err));
                        }
                    }
                }
            }

            let complex = self.find_class(&class.base).await;
            drop(class);

            class_mut = match complex {
                Some(Complex::Class(cls)) => cls.clone(),
                Some(Complex::Enum(_)) | None => break,
            };

            if strictness < tokenizer::AccessType::PRIVATE.strictness() {
                strictness = tokenizer::AccessType::PROTECTED.strictness();
            }
        }

        out
    }

    pub async fn get_functions_in_class(
        &self,
        class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
    ) -> Vec<(IString, Arc<Mutex<Function>>)> {
        self.get_functions_in_class_helper(class_mut, access, true)
            .await
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect()
    }

    pub async fn get_inaccessible_functions_in_class(
        &self,
        class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
    ) -> Vec<(IString, Arc<Mutex<Function>>, String)> {
        self.get_functions_in_class_helper(class_mut, access, false)
            .await
            .into_iter()
            .map(|(name, var, err)| (name, var, err.unwrap()))
            .collect()
    }

    pub async fn get_functions_helper(
        &self,
        accessible: bool,
    ) -> Vec<(IString, Arc<Mutex<Function>>, Option<String>)> {
        let mut out = Vec::new();

        if let Some(current_class) = &self.class {
            out = self
                .get_functions_in_class_helper(
                    current_class.clone(),
                    &tokenizer::AccessType::PRIVATE,
                    accessible,
                )
                .await;
        }

        let current_path = self.file.read().await.uri.clone();
        for (path, file) in &self.proj.files {
            if &current_path == path {
                continue;
            }

            for (name, class_mut) in file.read().await.classes.clone() {
                let class = class_mut.lock().await;
                if class.base.to_string().to_lowercase() == "function_object" {
                    if let Some(funcs) = class.functions.get(&name) {
                        for func in funcs {
                            out.push((name.clone(), func.clone(), None));
                        }
                    }
                }
            }
        }

        for (name, funcs) in &self.proj.builtin_functions {
            for func in funcs {
                out.push((name.clone(), func.clone(), None));
            }
        }

        out
    }

    pub async fn get_functions(&self) -> Vec<(IString, Arc<Mutex<Function>>)> {
        self.get_functions_helper(true)
            .await
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect()
    }

    pub async fn get_inaccessible_functions(&self) -> Vec<(IString, Arc<Mutex<Function>>, String)> {
        self.get_functions_helper(false)
            .await
            .into_iter()
            .map(|(name, var, err)| (name, var, err.unwrap()))
            .collect()
    }

    pub async fn find_class_dt(&self, data_type: &parser::DataType) -> Option<Complex> {
        match &data_type.data_type_type {
            parser::DataTypeType::Complex(grouped_name) => self.find_class(grouped_name).await,
            _ => None,
        }
    }

    pub async fn find_class(&self, grouped_name: &GroupedName) -> Option<Complex> {
        let GroupedName { group, name } = grouped_name;

        if let Some(class_arc) = self.file.read().await.classes.get(&name.into()) {
            let class = class_arc.lock().await;
            if group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref() {
                return Some(Complex::Class(class_arc.clone()));
            }
        }

        if group.is_none() {
            if let Some(found) = self
                .proj
                .builtin_classes
                .get(&name.into())
                .cloned()
                .map(Complex::Class)
            {
                return Some(found);
            }
            if let Some(found) = self
                .proj
                .builtin_enums
                .get(&name.into())
                .cloned()
                .map(Complex::Enum)
            {
                return Some(found);
            }
        }

        let current_path = self.file.read().await.uri.clone();
        for (path, file_lock) in &self.proj.files {
            if &current_path == path {
                continue;
            }

            let file = file_lock.read().await;

            if let Some(class_arc) = file.classes.get(&name.into()).cloned() {
                let class = class_arc.lock().await;
                // TODO: do error handling if not is_global
                if class.is_global
                    && (group.is_none() || class.within.as_ref().map(|w| &w.name) == group.as_ref())
                {
                    if file.top_levels.get_progress() < LintProgress::Shallow {
                        drop(class);
                        drop(file);
                        self.proj
                            .lint_file_to(file_lock, LintProgress::Shallow)
                            .await;
                    }
                    return Some(Complex::Class(class_arc.clone()));
                }
            }
        }

        // TODO scan more files?

        None
    }

    pub async fn is_event_callable(
        &self,
        event: &Event,
        arguments: &Vec<parser::DataTypeType>,
    ) -> bool {
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
        arguments: &Vec<parser::DataTypeType>,
        min_access: &tokenizer::AccessType,
    ) -> bool {
        min_access.strictness()
            >= func
                .parsed()
                .access
                .map(|access| access.strictness())
                .unwrap_or(0)
            && (func.parsed().arguments.len() == arguments.len()
                || (func.parsed().arguments.len() < arguments.len()
                    && func.parsed().vararg.is_some()))
            && future::join_all(
                func.parsed()
                    .arguments
                    .iter()
                    .map(|arg| &arg.variable.data_type.data_type_type)
                    .zip(arguments)
                    .map(|(self_arg, call_arg)| self.is_convertible(call_arg, self_arg)),
            )
            .await
            .iter()
            .all(|x| *x)
    }

    async fn get_variables_in_class_helper<'b>(
        &'b self,
        mut class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
        accessible: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>, Option<String>)> {
        let mut out = Vec::new();

        let mut strictness = access.strictness();

        loop {
            let class = class_mut.lock().await;

            for (name, var) in &class.instance_variables {
                let access = var.lock().await.unwrap_instance().1.access.clone();
                let is_accessible = write
                    .then_some(&access.write)
                    .unwrap_or(&access.read)
                    .map_or(0, |acc| acc.strictness())
                    <= strictness;

                if is_accessible == accessible {
                    let err = (!accessible).then(|| "Cannot access Private Variable".to_string());
                    out.push((name.clone(), var.clone(), err));
                }
            }

            let complex = self.find_class(&class.base).await;
            drop(class);

            match complex {
                Some(Complex::Class(cls)) => {
                    class_mut = cls.clone();
                }
                Some(Complex::Enum(_)) | None => break,
            }

            if strictness < tokenizer::AccessType::PRIVATE.strictness() {
                strictness = tokenizer::AccessType::PROTECTED.strictness();
            }
        }

        out
    }

    pub async fn get_variables_in_class<'b>(
        &'b self,
        class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>)> {
        self.get_variables_in_class_helper(class_mut, access, write, true)
            .await
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect()
    }

    pub async fn get_inaccessible_variables_in_class<'b>(
        &'b self,
        class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
    ) -> Vec<(IString, Arc<Mutex<Variable>>, String)> {
        self.get_variables_in_class_helper(class_mut, access, write, true)
            .await
            .into_iter()
            .map(|(name, var, err)| (name, var, err.unwrap()))
            .collect()
    }

    pub fn find_callable_function_in_class<'b>(
        &'b self,
        class_mut: &'b Arc<Mutex<Class>>,
        name: &'b String,
        arguments: &'b Vec<parser::DataTypeType>,
        min_access: &'b tokenizer::AccessType,
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
        arguments: &'b Vec<parser::DataTypeType>,
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
        from: &'b parser::DataTypeType,
        to: &'b parser::DataTypeType,
    ) -> BoxFuture<'b, bool> {
        async move {
            match (from, to) {
                (parser::DataTypeType::Unknown, _) | (_, parser::DataTypeType::Unknown) => true,
                (parser::DataTypeType::Any, _) | (_, parser::DataTypeType::Any) => true,
                (
                    parser::DataTypeType::Array(nested_from),
                    parser::DataTypeType::Array(nested_to),
                ) => self.is_convertible(&nested_from, &nested_to).await,
                (
                    parser::DataTypeType::Complex(from_name),
                    parser::DataTypeType::Complex(to_name),
                ) => {
                    self.inherits_from(from_name, to_name)
                        .await
                        .unwrap_or(false)
                        || self
                            .inherits_from(to_name, from_name)
                            .await
                            .unwrap_or(false)
                }
                (parser::DataTypeType::String, parser::DataTypeType::Char)
                | (parser::DataTypeType::Char, parser::DataTypeType::String) => true,
                _ => from == to || (from.is_numeric() && to.is_numeric()),
            }
        }
        .boxed()
    }
}
