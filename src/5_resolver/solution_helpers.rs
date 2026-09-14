use std::{iter, ptr, sync::Arc};

use super::types::*;
use crate::{
    builder::{self, BuiltFile},
    solution::{self, Solution},
    tokenizer,
    types::*,
};

impl<'sol> Solution {
    pub fn to_resolved_type(
        &'sol self,
        current_file: Option<&'sol BuiltFile>,
        ps_type: &builder::PowerScriptType,
    ) -> ResolvedType<'sol> {
        match ps_type {
            builder::PowerScriptType::Base(base) => ResolvedType::Base(base.clone()),
            builder::PowerScriptType::Complex(name) => {
                match self.find_class(current_file, &Grouped::simple(name.clone())) {
                    // TODO(perf): unnecessary clone
                    Found::Yes(class) => ResolvedType::Complex(class),
                    Found::No => ResolvedType::Unknown,
                }
            }
            builder::PowerScriptType::Array(inner) => {
                ResolvedType::Array(Box::new(self.to_resolved_type(current_file, inner)))
            }
        }
    }

    pub fn classes<'a>(
        &'sol self,
        current_file: Option<&'sol BuiltFile>,
        filter: ClassFilter<'a>,
    ) -> impl Iterator<Item = ListResult<solution::Complex<'sol>>> + 'a
    where
        'sol: 'a,
    {
        // TODO: return an error if the class is shadowed
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                // Do current file first because the order decides which one is shadowed
                if let Some(file) = current_file {
                    for (class, err) in filter
                        .name
                        .filter_hash_map(&file.inner().classes, ListError::WrongName)
                    {
                        yield (
                            solution::Complex::Class(solution::ClassRef::new(file, class)),
                            err,
                        );
                    }
                }

                for file in self.files_iter() {
                    if current_file.is_some_and(|f| ptr::eq(f, file)) {
                        continue;
                    }

                    for (class, err) in filter
                        .name
                        .filter_hash_map(&file.inner().classes, ListError::WrongName)
                    {
                        yield (
                            solution::Complex::Class(solution::ClassRef::new(file, class)),
                            err,
                        );
                    }
                }

                for (en, err) in filter.name.filter_hash_map(
                    &self.builtins.borrow_dependent().enums,
                    ListError::WrongName,
                ) {
                    yield (solution::Complex::Enum(en), err);
                }

                for (class, err) in filter.name.filter_hash_map(
                    &self.builtins.borrow_dependent().classes,
                    ListError::WrongName,
                ) {
                    yield (
                        solution::Complex::Class(solution::ClassRef::builtin(class)),
                        err,
                    );
                }
            },
        ))
    }

    pub fn global_variables<'a>(
        &'sol self,
        filter: GlobalVariableFilter<'a>,
    ) -> impl Iterator<Item = ListResult<&'sol Arc<builder::Variable<'sol>>>> + 'a
    where
        'sol: 'a,
    {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                if let Some(file) = self.application() {
                    for var in filter
                        .name
                        .filter_hash_map(&file.inner().variables, ListError::WrongName)
                    {
                        yield var;
                    }
                }

                let files = filter.name.filter(
                    self.files.iter(),
                    |(uri, _), want| {
                        uri.to_file_path()
                            .ok()
                            .as_ref()
                            .and_then(|p| p.file_prefix())
                            .map_or(false, |p| {
                                Into::<IString>::into(&*p.to_string_lossy()) == **want
                            })
                    },
                    ListError::WrongName,
                );

                for ((_, file), file_err) in files {
                    let file_vars = filter
                        .name
                        .filter_hash_map(&file.inner().variables, ListError::WrongName);

                    let global_vars = filter.accessible.filter_res(
                        file_vars,
                        |var, _| var.unwrap_scoped().scope == tokenizer::ScopeModif::GLOBAL,
                        ListError::NotAccessible,
                    );

                    for (var, var_err) in global_vars {
                        yield (var, file_err | var_err);
                    }
                }
            },
        ))
    }

    pub fn global_functions<'a>(
        &'sol self,
        caller_file: Option<&'sol BuiltFile>,
        filter: GlobalFunctionFilter<'sol, 'a>,
    ) -> impl Iterator<
        Item = ListResult<(
            Option<solution::ClassRef<'sol>>,
            &'sol builder::Function<'sol>,
        )>,
    > + 'a {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                let classes = self.classes(
                    caller_file,
                    ClassFilter {
                        name: filter.name.clone(),
                    },
                );
                for (complex, class_err) in classes {
                    let solution::Complex::Class(class) = complex else {
                        break;
                    };

                    let funcs = self.functions_in_class(
                        class,
                        FunctionFilter {
                            name: filter.name.clone(),
                            access: match filter.accessible {
                                SearchFilter::Filter { do_error, .. } => SearchFilter::Filter {
                                    want: tokenizer::AccessType::PUBLIC,
                                    do_error,
                                },
                                SearchFilter::All => SearchFilter::All,
                            },
                            arguments: filter.arguments.clone(),
                        },
                    );
                    let global_funcs = filter.accessible.filter_res(
                        funcs,
                        |(_, func), _| {
                            func.header().parsed.scope_modif == Some(tokenizer::ScopeModif::GLOBAL)
                        },
                        ListError::NotAccessible,
                    );

                    for ((class_ref, func), func_err) in global_funcs {
                        yield ((Some(class_ref), func), class_err | func_err);
                    }
                }

                let all_funcs = filter.name.filter_hash_map(
                    &self.builtins.borrow_dependent().functions,
                    ListError::WrongName,
                );
                let funcs_iter =
                    all_funcs.flat_map(|(f, err)| f.values().map(move |func| (func, err)));

                let funcs_iter = filter.arguments.filter_res(
                    funcs_iter,
                    |func, args| self.is_function_callable(caller_file, func.header(), &args),
                    ListError::WrongArguments,
                );

                for (func, err) in funcs_iter {
                    yield ((None, func), err);
                }
            },
        ))
    }

    // TODO: why does this not return a Found<bool>
    /// This is symmetric
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

    pub fn variables_in_class<'a>(
        &'sol self,
        class_ref: solution::ClassRef<'sol>,
        mut filter: VariableFilter<'a>,
    ) -> impl Iterator<
        Item = ListResult<(solution::ClassRef<'sol>, &'sol Arc<builder::Variable<'sol>>)>,
    > + 'a
    where
        'sol: 'a,
    {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                let variables: BoxIter<_> = match filter.name {
                    SearchFilter::Filter {
                        want,
                        do_error: false,
                    } => Box::new(class_ref.class.instance_variables.get(want).into_iter()),
                    _ => Box::new(class_ref.class.instance_variables.values()),
                };

                let variables = filter.name.filter(
                    variables,
                    |var, name| var.iname() == **name,
                    ListError::WrongName,
                );

                let variables = filter.access.filter_res(
                    variables,
                    |var, (access, is_write)| {
                        is_write
                            .then_some(&var.unwrap_instance().access.write)
                            .unwrap_or(&var.unwrap_instance().access.read)
                            .map_or(0, |acc| acc.strictness())
                            <= access.strictness()
                    },
                    ListError::NotAccessible,
                );

                for (var, err) in variables {
                    yield ((class_ref, var), err);
                }

                filter.access = filter.access.map(|(access, is_write)| {
                    (
                        if access.strictness() <= tokenizer::AccessType::PRIVATE.strictness() {
                            tokenizer::AccessType::PROTECTED
                        } else {
                            access
                        },
                        is_write,
                    )
                });

                // TODO(groups): correct order?
                // TODO(errors): can this lead to infinite recursion
                if let Some(within) = class_ref.class.within() {
                    if let Found::Yes(solution::Complex::Class(parent)) =
                        self.find_class(class_ref.file, &(&within).into())
                    {
                        for var in Box::new(self.variables_in_class(parent, filter.clone())) {
                            yield var;
                        }
                    }
                }

                if let Found::Yes(solution::Complex::Class(base)) =
                    self.find_class(class_ref.file, &(&class_ref.class.base()).into())
                {
                    for var in Box::new(self.variables_in_class(base, filter)) {
                        yield var
                    }
                }
            },
        ))
    }

    pub fn events_in_class<'a>(
        &'sol self,
        class_ref: solution::ClassRef<'sol>,
        filter: EventFilter<'a>,
    ) -> impl Iterator<Item = ListResult<(solution::ClassRef<'sol>, &'sol builder::Event<'sol>)>> + 'a
    where
        'sol: 'a,
    {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                let events = filter
                    .name
                    .filter_hash_map(&class_ref.class.events, ListError::WrongName);
                for (ev, err) in events {
                    yield ((class_ref, ev), err);
                }

                // TODO(groups): correct order?
                if let Some(within) = class_ref.class.within() {
                    if let Found::Yes(solution::Complex::Class(parent)) =
                        self.find_class(class_ref.file, &(&within).into())
                    {
                        for ev in Box::new(self.events_in_class(parent, filter.clone())) {
                            yield ev;
                        }
                    }
                }

                if let Found::Yes(solution::Complex::Class(base)) =
                    self.find_class(class_ref.file, &(&class_ref.class.base()).into())
                {
                    for ev in self.events_in_class(base, filter) {
                        yield ev;
                    }
                }
            },
        ))
    }

    pub fn functions_in_class<'a>(
        &'sol self,
        class_ref: solution::ClassRef<'sol>,
        mut filter: FunctionFilter<'sol, 'a>,
    ) -> impl Iterator<Item = ListResult<(solution::ClassRef<'sol>, &'sol builder::Function<'sol>)>> + 'a
    {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                for functions in vec![
                    &class_ref.class.functions,
                    &class_ref.class.external_functions,
                ] {
                    let all_funcs = filter
                        .name
                        .filter_hash_map(functions, ListError::WrongArguments);
                    let funcs_iter =
                        all_funcs.flat_map(|(f, err)| f.values().map(move |func| (func, err)));

                    // let funcs_iter: BoxIter<_> = match filter.signature {
                    //     SearchFilter::Filter {
                    //         want,
                    //         do_error: false,
                    //     } => Box::new(all_funcs.flat_map(|f| f.get(want).into_iter())),
                    //     _ => Box::new(all_funcs.flat_map(|f| f.values())),
                    // };

                    let funcs_iter = filter.arguments.filter_res(
                        funcs_iter,
                        |func, args| {
                            self.is_function_callable(class_ref.file, func.header(), &args)
                        },
                        ListError::WrongArguments,
                    );

                    let funcs_iter = filter.access.filter_res(
                        funcs_iter,
                        |func, access| {
                            func.header()
                                .parsed
                                .access
                                .map_or(0, |acc| acc.strictness())
                                <= access.strictness()
                        },
                        ListError::NotAccessible,
                    );

                    for (func, err) in funcs_iter {
                        yield ((class_ref, func), err);
                    }
                }

                filter.access = filter.access.map(|access| {
                    if access.strictness() <= tokenizer::AccessType::PRIVATE.strictness() {
                        tokenizer::AccessType::PROTECTED
                    } else {
                        access
                    }
                });

                // TODO(groups): correct order? (parent before base?)
                if let Some(within) = class_ref.class.within() {
                    if let Found::Yes(solution::Complex::Class(parent)) =
                        self.find_class(class_ref.file, &(&within).into())
                    {
                        for func in Box::new(self.functions_in_class(parent, filter.clone())) {
                            yield func;
                        }
                    }
                }

                if let Found::Yes(solution::Complex::Class(base)) =
                    self.find_class(class_ref.file, &(&class_ref.class.base()).into())
                {
                    for func in self.functions_in_class(base, filter) {
                        yield func;
                    }
                }
            },
        ))
    }
}
