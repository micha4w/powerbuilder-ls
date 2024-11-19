use std::{backtrace::Backtrace, collections::HashMap, pin::Pin, sync::Arc};

use futures::{
    future::{self, BoxFuture},
    stream, FutureExt, Stream, StreamExt,
};
use tokio::sync::Mutex;

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
    pub file: MaybeMut<'a, File>,
    pub class: Option<Arc<Mutex<Class>>>,

    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,
    pub return_type: parser::DataTypeType,
}

impl<'a> Linter<'a> {
    pub fn new(proj: &'a Project, file: MaybeMut<'a, File>) -> Self {
        Self {
            proj,
            file,
            class: None,
            variables: HashMap::new(),
            return_type: parser::DataTypeType::Void,
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

        for class in self.file.as_ref().classes.values() {
            data_types.push(parser::DataTypeType::Complex(GroupedName::new(
                None,
                class.lock().await.name.clone(),
            )));
        }

        for (path, _file_lock) in &self.proj.files {
            if self.file.as_ref().path == *path {
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
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>, Option<String>)> + Send + 'b {
        let wants_accessible = accessible;
        let variables = stream::iter(&self.variables).filter_map(move |(name, var)| async move {
            let is_accessible = &var.lock().await.parsed().range.end < local_before;

            if is_accessible == wants_accessible {
                let err = (!accessible).then(|| "Variable has not been defined yet".to_string());
                Some((name.clone(), var.clone(), err))
            } else {
                None
            }
        });

        let variables: Pin<Box<dyn Stream<Item = _> + Send>> =
            if let Some(current_class) = &self.class {
                Box::pin(
                    variables.chain(
                        self.get_variables_in_class_helper(
                            current_class.clone(),
                            &tokenizer::AccessType::PRIVATE,
                            write,
                            accessible,
                        )
                        .await,
                    ),
                )
            } else {
                Box::pin(variables)
            };

        let variables = variables.chain(
            stream::iter(&self.file.as_ref().variables)
                .map(|(name, var)| (name.clone(), var.clone(), None)),
        );

        let current_path = self.file.as_ref().path.clone();
        let variables = variables.chain(
            stream::once(async move {
                let mut variables: Pin<Box<dyn Stream<Item = _> + Send>> =
                    Box::pin(stream::empty());

                for (path, file) in &self.proj.files {
                    if &current_path == path {
                        continue;
                    }

                    let accessible = accessible;
                    variables = Box::pin(variables.chain(
                        stream::iter(file.read().await.variables.clone()).filter_map(
                            move |(name, var)| async move {
                                let is_accessible = match var.lock().await.unwrap_scoped().scope {
                                    tokenizer::ScopeModif::GLOBAL => true,
                                    tokenizer::ScopeModif::SHARED => false,
                                };

                                if is_accessible == accessible {
                                    let err = (!accessible)
                                        .then(|| "Variable is not defined as Global".to_string());
                                    Some((name.clone(), var.clone(), err))
                                } else {
                                    None
                                }
                            },
                        ),
                    ));
                }

                variables
            })
            .flatten(),
        );

        variables
    }

    pub async fn get_variables<'b>(
        &'b self,
        local_before: &'b Position,
        write: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>)> + Send + 'b {
        self.get_variables_helper(local_before, write, true)
            .await
            .map(|(name, var, _)| (name, var))
    }

    pub async fn get_inaccessible_variables<'b>(
        &'b self,
        local_before: &'b Position,
        write: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>, String)> + Send + 'b {
        self.get_variables_helper(local_before, write, true)
            .await
            .map(|(name, var, err)| (name, var, err.unwrap()))
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
            .as_ref()
            .variables
            .get(&(&variable.name.content).into())
        {
            return Some(found.clone());
        }

        for (path, _file_lock) in &self.proj.files {
            if self.file.as_ref().path == *path {
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

        for (path, _file_lock) in &self.proj.files {
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
        class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
        accessible: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>, Option<String>)> + Send + '_ {
        let funcs_iter = class_mut.lock().await.functions.clone().into_iter();

        stream::unfold(
            (class_mut, funcs_iter, None, false, access.strictness()),
            move |(
                mut class_mut,
                mut funcs_iter,
                mut func_iter,
                mut is_external,
                mut strictness,
            )| async move {
                loop {
                    loop {
                        let (name, mut iter) = match func_iter {
                            Some(func_iter) => func_iter,
                            None => {
                                if let Some((name, functions)) = funcs_iter.next() {
                                    func_iter = Some((name, functions.into_iter()));
                                    func_iter.unwrap()
                                } else {
                                    break;
                                }
                            }
                        };

                        while let Some(func_mut) = iter.next() {
                            let is_accessible = func_mut
                                .lock()
                                .await
                                .parsed
                                .access
                                .map_or(0, |acc| acc.strictness())
                                <= strictness;

                            if is_accessible == accessible {
                                let err = (!accessible)
                                    .then(|| "Cannot access Private Function".to_string());
                                return Some((
                                    (name.clone(), func_mut, err),
                                    (
                                        class_mut,
                                        funcs_iter,
                                        Some((name, iter)),
                                        is_external,
                                        strictness,
                                    ),
                                ));
                            }
                        }
                        func_iter = None;
                    }

                    func_iter = None;
                    is_external = !is_external;

                    if is_external {
                        funcs_iter = class_mut
                            .lock()
                            .await
                            .external_functions
                            .clone()
                            .into_iter();
                        continue;
                    }

                    let complex = self.find_class(&class_mut.lock().await.base).await;
                    class_mut = match complex {
                        Some(Complex::Class(cls)) => cls.clone(),
                        Some(Complex::Enum(_)) | None => break,
                    };
                    funcs_iter = class_mut.lock().await.functions.clone().into_iter();

                    if strictness < tokenizer::AccessType::PRIVATE.strictness() {
                        strictness = tokenizer::AccessType::PROTECTED.strictness();
                    }
                }

                None
            },
        )
    }

    pub async fn get_functions_in_class(
        &self,
        class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>)> + Send + '_ {
        self.get_functions_in_class_helper(class_mut, access, true)
            .await
            .map(|(name, var, _)| (name, var))
    }

    pub async fn get_inaccessible_functions_in_class(
        &self,
        class_mut: Arc<Mutex<Class>>,
        access: &tokenizer::AccessType,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>, String)> + Send + '_ {
        self.get_functions_in_class_helper(class_mut, access, false)
            .await
            .map(|(name, var, err)| (name, var, err.unwrap()))
    }

    pub async fn get_functions_helper(
        &self,
        accessible: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>, Option<String>)> + Send + '_ {
        let functions: Pin<Box<dyn Stream<Item = _> + Send>> =
            if let Some(current_class) = &self.class {
                Box::pin(
                    self.get_functions_in_class_helper(
                        current_class.clone(),
                        &tokenizer::AccessType::PRIVATE,
                        accessible,
                    )
                    .await,
                )
            } else {
                Box::pin(stream::empty())
            };

        let current_path = self.file.as_ref().path.clone();
        let functions = functions.chain(
            stream::once(async move {
                let mut functions: Pin<Box<dyn Stream<Item = _> + Send>> =
                    Box::pin(stream::empty());

                for (path, file) in &self.proj.files {
                    if &current_path == path {
                        continue;
                    }

                    functions = Box::pin(
                        functions.chain(
                            stream::iter(file.read().await.classes.clone())
                                .filter_map(move |(name, class_mut)| async move {
                                    let class = class_mut.lock().await;
                                    if class.base.combine().to_lowercase() == "function_object" {
                                        if let Some(funcs) = class.functions.get(&name) {
                                            return Some(
                                                stream::iter(funcs.clone())
                                                    .map(move |func| (name.clone(), func, None)),
                                            );
                                        }
                                    }
                                    None
                                })
                                .flatten(),
                        ),
                    );
                }

                functions.chain(stream::iter(self.proj.builtin_functions.clone()).flat_map(
                    |(name, functions)| {
                        stream::iter(functions).map(move |func| (name.clone(), func, None))
                    },
                ))
            })
            .flatten(),
        );

        functions
    }

    pub async fn get_functions(
        &self,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>)> + Send + '_ {
        self.get_functions_helper(true)
            .await
            .map(|(name, var, _)| (name, var))
    }

    pub async fn get_inaccessible_functions(
        &self,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Function>>, String)> + Send + '_ {
        self.get_functions_helper(false)
            .await
            .map(|(name, var, err)| (name, var, err.unwrap()))
    }

    pub async fn find_class(&self, grouped_name: &GroupedName) -> Option<Complex> {
        let GroupedName { group, name } = grouped_name;

        if let Some(class_arc) = self.file.as_ref().classes.get(&name.into()) {
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

        for (path, file_lock) in &self.proj.files {
            if self.file.as_ref().path == *path {
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
        class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
        accessible: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>, Option<String>)> + Send + 'b {
        let var_iter = class_mut
            .lock()
            .await
            .instance_variables
            .clone()
            .into_iter();

        stream::unfold(
            (class_mut, var_iter, access.strictness()),
            move |(mut class_mut, mut var_iter, mut strictness)| async move {
                loop {
                    while let Some((name, var)) = var_iter.next() {
                        let access = var.lock().await.unwrap_instance().1.access.clone();
                        let is_accessible = write
                            .then_some(&access.write)
                            .unwrap_or(&access.read)
                            .map_or(0, |acc| acc.strictness())
                            <= strictness;

                        if is_accessible == accessible {
                            let err =
                                (!accessible).then(|| "Cannot access Private Variable".to_string());
                            return Some(((name, var, err), (class_mut, var_iter, strictness)));
                        }
                    }

                    let complex = self.find_class(&class_mut.lock().await.base).await;
                    match complex {
                        Some(Complex::Class(cls)) => {
                            var_iter = cls.lock().await.instance_variables.clone().into_iter();
                            class_mut = cls.clone();
                        }
                        Some(Complex::Enum(_)) | None => break,
                    }

                    if strictness < tokenizer::AccessType::PRIVATE.strictness() {
                        strictness = tokenizer::AccessType::PROTECTED.strictness();
                    }
                }

                None
            },
        )
    }

    pub async fn get_variables_in_class<'b>(
        &'b self,
        class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>)> + Send + 'b {
        self.get_variables_in_class_helper(class_mut, access, write, true)
            .await
            .map(|(name, var, _)| (name, var))
    }

    pub async fn get_inaccessible_variables_in_class<'b>(
        &'b self,
        class_mut: Arc<Mutex<Class>>,
        access: &'b tokenizer::AccessType,
        write: bool,
    ) -> impl Stream<Item = (IString, Arc<Mutex<Variable>>, String)> + Send + 'b {
        self.get_variables_in_class_helper(class_mut, access, write, true)
            .await
            .map(|(name, var, err)| (name, var, err.unwrap()))
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
