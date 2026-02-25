use std::{
    collections::HashMap,
    iter::{self},
    sync::Arc,
};

use super::types::*;
use crate::{
    builder::{self, BuiltFile},
    project::{self, Project},
    tokenizer,
    types::*,
};

#[derive(Debug, Clone)]
pub struct FoundVariable<'proj> {
    /// None for builtins
    pub file: Option<&'proj BuiltFile>,
    /// None for locals/args and file scoped variables
    pub class: Option<&'proj Arc<builder::Class>>,

    pub variable: &'proj Arc<builder::Variable>,
}

impl<'proj> FoundVariable<'proj> {
    fn new(file: &'proj BuiltFile, variable: &'proj Arc<builder::Variable>) -> Self {
        FoundVariable {
            file: Some(file),
            class: None,
            variable,
        }
    }

    fn new_instance(
        class: project::ClassRef<'proj>,
        variable: &'proj Arc<builder::Variable>,
    ) -> Self {
        FoundVariable {
            file: class.file,
            class: Some(class.class),
            variable,
        }
    }

    pub fn class_ref(&self) -> Option<project::ClassRef<'proj>> {
        self.class.map(|c| project::ClassRef {
            file: self.file,
            class: c,
        })
    }
}

pub struct Context<'proj> {
    pub proj: &'proj Project,
    pub file: &'proj BuiltFile,

    pub class: Option<project::ClassRef<'proj>>,
    // pub top_level: Option<&'a TopLevel>,
    pub arguments: Option<HashMap<IString, &'proj Arc<builder::Variable>>>,
    pub body: Option<&'proj builder::Body>,
}

impl<'proj> Context<'proj> {
    pub fn new(
        proj: &'proj Project,
        file: &'proj BuiltFile,
        pos: &'proj Position,
    ) -> Context<'proj> {
        let mut ctx = Context {
            proj,
            file,

            class: None,
            // top_level: None,
            body: None,
            arguments: None,
        };

        for top_level in &file.top_levels {
            if let builder::TopLevelType::DatatypeDecl(decl) = &top_level.top_level_type {
                ctx.class = Some(project::ClassRef::new(file, &decl.class));
            }

            if top_level.range.contains(pos) {
                // ctx.top_level = Some(top_level);

                match &top_level.top_level_type {
                    builder::TopLevelType::FunctionBody(func) => {
                        if func.parsed.body_range.contains(pos) {
                            ctx.load_body(&func.body, &func.header.arguments)
                        }
                    }
                    builder::TopLevelType::EventBody(event) => {
                        if event.parsed.body_range.contains(pos) {
                            ctx.load_body(&event.body, &event.header.arguments);
                        }
                    }
                    builder::TopLevelType::OnBody(on) => {} // TODO(on): ...
                    _ => {}
                }
                break;
            }
        }

        ctx
    }

    pub fn new_for_body(
        proj: &'proj Project,
        file: &'proj BuiltFile,
        class: Option<project::ClassRef<'proj>>,
        // top_level: &'a TopLevel,
    ) -> Context<'proj> {
        let ctx = Context {
            proj,
            file,
            class,
            // top_level: Some(top_level),
            body: None,
            arguments: None,
        };

        // match &top_level.top_level_type {
        //     builder::TopLevelType::FunctionBody(func) => ctx.load_body(&func.body, &func.header.arguments),
        //     builder::TopLevelType::EventBody(event) => ctx.load_body(&event.body, &event.header.arguments),
        //     builder::TopLevelType::OnBody(on) => {} // TODO(on): ...
        //     _ => {}
        // }

        ctx
    }

    pub(super) fn load_body(
        &mut self,
        body: &'proj builder::Body,
        arguments: &'proj Vec<Arc<builder::Variable>>,
    ) {
        self.body = Some(body);
        self.arguments = Some(
            arguments
                .iter()
                .map(|arg| ((&arg.parsed().access.name.content).into(), arg))
                .collect(),
        );
    }

    pub fn find_class(&self, name: &IString) -> Found<project::Complex<'proj>> {
        self.proj.find_class(Some(self.file), name)
    }

    pub fn variables_in_class(
        &self,
        class: project::ClassRef<'proj>,
        filter: VariableFilter<'proj>,
    ) -> impl Iterator<Item = ListResult<(project::ClassRef<'proj>, &'proj Arc<builder::Variable>)>> + '_
    {
        let level = self
            .class
            .clone()
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.proj.get_access_for(current_class, class)
            });

        self.proj
            .variables_in_class(class, filter.with_access(level))
    }

    pub fn variables(
        &self,
        filter: VariableFilter<'proj>,
    ) -> impl Iterator<Item = ListResult<FoundVariable<'proj>>> + '_ {
        iter::from_coroutine(
            #[coroutine]
            move || {
                if let Some(body) = self.body {
                    match &filter {
                        VariableFilter::All => {
                            for var in body.variables.values() {
                                yield Ok(FoundVariable::new(self.file, var));
                            }
                        }
                        VariableFilter::ForAccess(variable, _) => {
                            if let Some(var) = body.variables.get(&(&variable.name.content).into())
                            {
                                yield Ok(FoundVariable::new(self.file, var));
                            }
                        }
                    }
                }

                if let Some(class) = self.class {
                    for var in self.proj.variables_in_class(
                        class,
                        filter.clone().with_access(tokenizer::AccessType::PRIVATE),
                    ) {
                        yield Self::map_res(var, |(class, t)| {
                            FoundVariable::new_instance(class, t)
                        });
                    }
                }

                match &filter {
                    VariableFilter::All => {
                        for var in self.file.variables.values() {
                            yield Ok(FoundVariable::new(self.file, var));
                        }
                    }
                    VariableFilter::ForAccess(variable, _) => {
                        if let Some(var) = self.file.variables.get(&(&variable.name.content).into())
                        {
                            yield Ok(FoundVariable::new(self.file, var));
                        }
                    }
                }

                for var in self.proj.global_variables(filter) {
                    yield Self::map_res(var, |var| FoundVariable::new(self.file, var));
                }
            },
        )
    }

    pub fn functions_in_class<'a>(
        &'a self,
        class: project::ClassRef<'proj>,
        filter: FunctionFilter<'proj, 'a>,
    ) -> impl Iterator<Item = ListResult<(project::ClassRef<'proj>, &'proj builder::Function)>> + 'a
    {
        let level = self
            .class
            .clone()
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.proj.get_access_for(current_class, class)
            });

        self.proj
            .functions_in_class(class, filter.with_access(level))
    }

    pub fn functions<'a>(
        &'a self,
        filter: FunctionFilter<'proj, 'a>,
    ) -> impl Iterator<Item = ListResult<(Option<project::ClassRef<'proj>>, &'proj builder::Function)>>
           + 'a {
        iter::from_coroutine(
            #[coroutine]
            move || {
                if let Some(class) = self.class {
                    for func in self.proj.functions_in_class(
                        class,
                        filter.clone().with_access(tokenizer::AccessType::PRIVATE),
                    ) {
                        yield Self::map_res(func, |(class, t)| (Some(class), t));
                    }
                }

                match filter {
                    FunctionFilter::ForCall(iname, arg_types, _) => {
                        for func in self.proj.global_functions(Some(self.file), filter) {
                            yield Self::map_res(func, |(class, t)| (Some(class), t));
                        }

                        if let Some(overloads) = self.proj.builtins.functions.get(&iname) {
                            for overload in overloads {
                                if self.proj.is_function_callable(
                                    Some(self.file),
                                    &overload.header(),
                                    &arg_types,
                                ) {
                                    yield Ok((None, overload));
                                } else {
                                    yield Err((ListError::WrongArguments, (None, overload)));
                                }
                            }
                        }
                    }
                    FunctionFilter::All => {
                        // TODO: find all global functions

                        for overloads in self.proj.builtins.functions.values() {
                            for overload in overloads {
                                yield Ok((None, overload));
                            }
                        }
                    }
                }
            },
        )
    }

    pub fn map_res<T, U>(res: ListResult<T>, f: impl FnOnce(T) -> U) -> ListResult<U> {
        match res {
            Ok(val) => Ok(f(val)),
            Err((err, val)) => Err((err, f(val))),
        }
    }

    // pub fn some_class<T>(
    //     res: ListResult<(project::ClassRef<'proj>, &'proj T)>,
    // ) -> ListResult<(Option<project::ClassRef<'proj>>, &'proj T)> {
    //     Self::map_res(res, |(class, t)| (Some(class), t))
    // }

    // pub fn no_class<T>(
    //     val: ListResult<&'proj T>,
    // ) -> ListResult<(Option<project::ClassRef<'proj>>, &'proj T)> {
    //     Self::map_res(val, |t| (None, t))
    // }

    pub fn gather<T>(iter: impl Iterator<Item = ListResult<T>>) -> Result<T, Vec<(ListError, T)>> {
        let mut errors = Vec::new();

        for item in iter {
            match item {
                Ok(ok) => return Ok(ok),
                Err(err) => errors.push(err),
            }
        }

        Err(errors)
    }

    pub fn first<T>(iter: impl Iterator<Item = ListResult<T>>) -> Found<(T, Option<ListError>)> {
        let mut fail = Found::No;

        for item in iter {
            match item {
                Ok(ok) => return Found::Yes((ok, None)),
                Err((err, t)) => fail = Found::Yes((t, Some(err))),
            }
        }

        fail
    }
}
