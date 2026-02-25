use std::{iter, ptr, sync::Arc};

use super::types::*;
use crate::{
    builder::{self, BuiltFile, File},
    project::{self, Project},
    tokenizer,
    types::*,
};

impl<'proj> Project {
    pub fn to_resolved_type(
        &'proj self,
        current_file: Option<&'proj BuiltFile>,
        ps_type: &builder::PowerScriptType,
    ) -> ResolvedType<'proj> {
        match ps_type {
            builder::PowerScriptType::Base(base) => ResolvedType::Base(base.clone()),
            builder::PowerScriptType::Complex(name) => match self.find_class(current_file, name) {
                Found::Yes(class) => ResolvedType::Complex(class),
                Found::No => ResolvedType::Unknown,
            },
            builder::PowerScriptType::Array(inner) => {
                ResolvedType::Array(Box::new(self.to_resolved_type(current_file, inner)))
            }
        }
    }

    pub fn classes(
        &'proj self,
        current_file: Option<&'proj BuiltFile>,
        name: &'proj IString,
    ) -> impl Iterator<Item = ListResult<project::Complex<'proj>>> + 'proj {
        iter::from_coroutine(
            #[coroutine]
            move || {
                if let Some(file) = current_file {
                    for class in file.classes.values() {
                        yield Ok(project::Complex::Class(project::ClassRef::new(file, class)));
                    }
                }

                for file in self.files.values() {
                    if let File::Built(built) = file {
                        if current_file.is_some_and(|f| ptr::eq(f, built)) {
                            continue;
                        }

                        for class in built.classes.values() {
                            yield Ok(project::Complex::Class(project::ClassRef::new(
                                built, class,
                            )));
                        }
                    };
                }

                if let Some(en) = self.builtins.enums.get(name) {
                    yield Ok(project::Complex::Enum(en));
                }

                if let Some(class) = self.builtins.classes.get(name) {
                    yield Ok(project::Complex::Class(project::ClassRef::builtin(class)));
                }
            },
        )
    }

    pub fn global_variables(
        &'proj self,
        filter: VariableFilter<'proj>,
    ) -> impl Iterator<Item = ListResult<&'proj Arc<builder::Variable>>> + 'proj {
        iter::from_coroutine(
            #[coroutine]
            move || {
                match filter {
                    VariableFilter::All => {
                        if let Some(File::Built(file)) = self
                            .application
                            .as_ref()
                            .and_then(|uri| self.files.get(uri))
                        {
                            for var in file.variables.values() {
                                yield Ok(var);
                            }
                        }

                        for file in self.files.values() {
                            // TODO(load): also make sure global variable dependents are loaded when loading?
                            if let File::Built(built) = file {
                                for var in built.variables.values() {
                                    yield Ok(var);
                                }
                            };
                        }
                    }
                    VariableFilter::ForAccess(variable, _) => {
                        let iname = (&variable.name.content).into();

                        if let Some(File::Built(file)) = self
                            .application
                            .as_ref()
                            .and_then(|uri| self.files.get(uri))
                        {
                            // TODO: ensure the application is always built
                            if let Some(var) = file.variables.get(&iname) {
                                yield Ok(var);
                            }
                        }

                        for (uri, file) in &self.files {
                            if let Some(prefix) = uri
                                .to_file_path()
                                .ok()
                                .as_ref()
                                .and_then(|p| p.file_prefix())
                            {
                                if variable
                                    .name
                                    .content
                                    .eq_ignore_ascii_case(&prefix.to_string_lossy())
                                {
                                    // TODO(load): also make sure global variable dependents are loaded when loading?
                                    if let File::Built(built) = file {
                                        if let Some(var) = built.variables.get(&iname) {
                                            if var.unwrap_scoped().scope
                                                == tokenizer::ScopeModif::GLOBAL
                                            {
                                                yield Ok(var);
                                            }
                                        }
                                    }
                                }
                            };
                        }
                    }
                }
            },
        )
    }

    pub fn global_functions<'a>(
        &'proj self,
        caller_file: Option<&'proj BuiltFile>,
        filter: FunctionFilter<'proj, 'a>,
    ) -> impl Iterator<Item = ListResult<(project::ClassRef<'proj>, &'proj builder::Function)>> + 'a
    {
        iter::from_coroutine(
            #[coroutine]
            move || {
                match filter {
                    FunctionFilter::All => {
                        // TODO: find all global functions (store all srf?)
                    }
                    FunctionFilter::ForCall(name, arguments, _) => {
                        if let Found::Yes(complex @ project::Complex::Class(class)) =
                            self.find_class(caller_file, &name)
                        {
                            // TODO(load): also ensure global functions are loaded when loading?
                            // TODO: also check file extension (.srf)?
                            let is_function = self
                                .inherits_from(
                                    &complex,
                                    &project::Complex::Class(self.builtin_class("function_object")),
                                )
                                .unwrap_or(false);

                            for func in self.functions_in_class(
                                class,
                                FunctionFilter::ForCall(
                                    name,
                                    arguments,
                                    tokenizer::AccessType::PUBLIC,
                                ),
                            ) {
                                yield match func {
                                    Ok(func) if !is_function => {
                                        Err((ListError::NotAFunctionObject, func))
                                    }
                                    res => res,
                                }
                            }
                        }
                    }
                }
            },
        )
    }

    // TODO: why does this not return a Found<bool>
    // This is symmetric
    pub fn is_convertible(&self, a: &ResolvedType<'_>, b: &ResolvedType<'_>) -> bool {
        match (a, b) {
            (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => true,
            (ResolvedType::Base(builder::BaseType::Any), _)
            | (_, ResolvedType::Base(builder::BaseType::Any)) => true,

            (ResolvedType::Array(nested_from), ResolvedType::Array(nested_to)) => {
                self.is_convertible(&nested_from, &nested_to)
            }
            (ResolvedType::Complex(from), ResolvedType::Complex(to)) => {
                self.inherits_from(&from, &to).unwrap_or(false)
                    || self.inherits_from(&to, &from).unwrap_or(false)
            }
            (
                ResolvedType::Base(builder::BaseType::String),
                ResolvedType::Base(builder::BaseType::Char),
            )
            | (
                ResolvedType::Base(builder::BaseType::Char),
                ResolvedType::Base(builder::BaseType::String),
            ) => true,
            (ResolvedType::Base(from_base), ResolvedType::Base(to_base)) => {
                std::mem::discriminant(from_base) == std::mem::discriminant(to_base)
                    || (a.is_numeric() && b.is_numeric())
            }
            (_, _) => false,
        }
    }

    pub fn is_event_callable(
        &self,
        ev_file: Option<&BuiltFile>,
        ev: &builder::EventHeader,
        arguments: &Vec<ResolvedType<'_>>,
    ) -> bool {
        ev.arguments.len() != arguments.len()
            && ev
                .arguments
                .iter()
                .zip(arguments)
                .all(|(self_arg, call_arg)| {
                    let res_self_arg =
                        self.to_resolved_type(ev_file, &self_arg.data_type.powerscript_type);
                    self.is_convertible(call_arg, &res_self_arg)
                })
    }

    pub fn is_function_callable(
        &self,
        func_file: Option<&BuiltFile>,
        func: &builder::FunctionHeader,
        arguments: &Vec<&ResolvedType<'_>>,
    ) -> bool {
        if func.parsed.vararg.is_some() {
            if func.arguments.len() < arguments.len() {
                return false;
            }
        } else if func.arguments.len() != arguments.len() {
            return false;
        }

        func.arguments
            .iter()
            .zip(arguments)
            .all(|(self_arg, call_arg)| {
                let res_self_arg =
                    self.to_resolved_type(func_file, &self_arg.data_type.powerscript_type);
                self.is_convertible(call_arg, &res_self_arg)
            })
    }

    pub fn variables_in_class(
        &'proj self,
        class_ref: project::ClassRef<'proj>,
        filter: VariableFilter<'proj>,
    ) -> impl Iterator<Item = ListResult<(project::ClassRef<'proj>, &'proj Arc<builder::Variable>)>>
           + 'proj {
        iter::from_coroutine(
            #[coroutine]
            move || {
                let next_filter = match filter {
                    VariableFilter::All => {
                        for var in class_ref.class.instance_variables.values() {
                            yield Ok((class_ref, var));
                        }

                        filter
                    }
                    VariableFilter::ForAccess(variable, access) => {
                        let iname = (&variable.name.content).into();

                        if let Some(var) = class_ref.class.instance_variables.get(&iname) {
                            let var_protection = &var.unwrap_instance().access;

                            if variable
                                .is_write
                                .then_some(&var_protection.write)
                                .unwrap_or(&var_protection.read)
                                .map_or(0, |acc| acc.strictness())
                                <= access.strictness()
                            {
                                yield Ok((class_ref, var));
                            } else {
                                yield Err((ListError::NotAccessible, (class_ref, var)));
                            }
                        };

                        let next_access =
                            if access.strictness() <= tokenizer::AccessType::PRIVATE.strictness() {
                                tokenizer::AccessType::PROTECTED
                            } else {
                                access
                            };

                        VariableFilter::ForAccess(variable, next_access)
                    }
                };

                if let Found::Yes(project::Complex::Class(base)) =
                    self.find_class(class_ref.file, &class_ref.class.base().into())
                {
                    for var in Box::new(self.variables_in_class(base, next_filter)) {
                        yield var
                    }
                };
            },
        )
    }

    pub fn events_in_class<'a>(
        &'proj self,
        class_ref: project::ClassRef<'proj>,
        filter: EventFilter<'a>,
    ) -> impl Iterator<Item = (project::ClassRef<'proj>, &'proj builder::Event)> + 'a
    where
        'proj: 'a,
    {
        iter::from_coroutine(
            #[coroutine]
            move || {
                match filter {
                    EventFilter::All => {
                        for ev in class_ref.class.events.values() {
                            yield (class_ref, ev);
                        }
                    }
                    EventFilter::WithName(name) => {
                        if let Some(ev) = class_ref.class.events.get(&name) {
                            yield (class_ref, ev);
                        }
                    }
                };

                if let Found::Yes(project::Complex::Class(base)) =
                    self.find_class(class_ref.file, &class_ref.class.base().into())
                {
                    for ev in Box::new(self.events_in_class(base, filter)) {
                        yield ev;
                    }
                }
            },
        )
    }

    pub fn functions_in_class<'a>(
        &'proj self,
        class_ref: project::ClassRef<'proj>,
        filter: FunctionFilter<'proj, 'a>,
    ) -> impl Iterator<Item = ListResult<(project::ClassRef<'proj>, &'proj builder::Function)>> + 'a
    {
        iter::from_coroutine(
            #[coroutine]
            move || {
                for functions in vec![
                    &class_ref.class.functions,
                    &class_ref.class.external_functions,
                ] {
                    match filter {
                        FunctionFilter::All => {
                            for overloads in functions.values() {
                                for overload in overloads.values() {
                                    yield Ok((class_ref, overload));
                                }
                            }
                        }
                        FunctionFilter::ForCall(iname, arguments, access) => {
                            if let Some(overloads) = functions.get(&iname) {
                                for overload in overloads.values() {
                                    let accessible = overload
                                        .header()
                                        .parsed
                                        .access
                                        .map_or(0, |acc| acc.strictness())
                                        <= access.strictness();

                                    let callable = self.is_function_callable(
                                        class_ref.file,
                                        overload.header(),
                                        &arguments,
                                    );

                                    if accessible && callable {
                                        yield Ok((class_ref, overload));
                                    } else if !accessible {
                                        yield Err((
                                            ListError::NotAccessible,
                                            (class_ref, overload),
                                        ));
                                    } else {
                                        yield Err((
                                            ListError::WrongArguments,
                                            (class_ref, overload),
                                        ));
                                    }
                                }
                            }
                        }
                    };
                }

                let next_filter = match filter {
                    FunctionFilter::All => filter,
                    FunctionFilter::ForCall(iname, arguments, access) => {
                        let next_access =
                            if access.strictness() <= tokenizer::AccessType::PRIVATE.strictness() {
                                tokenizer::AccessType::PROTECTED
                            } else {
                                access
                            };

                        FunctionFilter::ForCall(iname, arguments, next_access)
                    }
                };

                if let Found::Yes(project::Complex::Class(base)) =
                    self.find_class(class_ref.file, &class_ref.class.base().into())
                {
                    for func in Box::new(self.functions_in_class(base, next_filter)) {
                        yield func;
                    }
                }
            },
        )
    }
}
