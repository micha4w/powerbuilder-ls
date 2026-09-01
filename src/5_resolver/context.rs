use std::{
    collections::HashMap,
    iter::{self},
    sync::Arc,
};

use super::types::*;
use crate::{
    builder::{self, BuiltFile},
    solution::{self, Solution},
    tokenizer,
    types::*,
};

#[derive(Debug, Clone)]
pub struct FoundVariable<'sol> {
    /// None for builtins
    pub file: Option<&'sol BuiltFile>,
    /// None for locals/args and file scoped variables
    pub class: Option<&'sol Arc<builder::Class<'sol>>>,

    pub variable: &'sol Arc<builder::Variable<'sol>>,
}

impl<'sol> FoundVariable<'sol> {
    fn new(file: &'sol BuiltFile, variable: &'sol Arc<builder::Variable<'sol>>) -> Self {
        FoundVariable {
            file: Some(file),
            class: None,
            variable,
        }
    }

    fn new_instance(
        class: solution::ClassRef<'sol>,
        variable: &'sol Arc<builder::Variable<'sol>>,
    ) -> Self {
        FoundVariable {
            file: class.file,
            class: Some(class.class),
            variable,
        }
    }

    pub fn class_ref(&self) -> Option<solution::ClassRef<'sol>> {
        self.class.map(|c| solution::ClassRef {
            file: self.file,
            class: c,
        })
    }
}

pub struct Context<'sol> {
    pub sol: &'sol Solution,
    pub file: &'sol BuiltFile,

    pub class: Option<solution::ClassRef<'sol>>,
    // pub top_level: Option<&'a TopLevel>,
    pub arguments: Option<HashMap<IString, &'sol Arc<builder::Variable<'sol>>>>,
    pub body: Option<&'sol builder::Body<'sol>>,
}

impl<'sol> Context<'sol> {
    pub fn new(sol: &'sol Solution, file: &'sol BuiltFile, pos: &'sol Position) -> Context<'sol> {
        let mut ctx = Context {
            sol,
            file,

            class: None,
            // top_level: None,
            body: None,
            arguments: None,
        };

        for top_level in &file.inner().top_levels {
            if let builder::TopLevelType::DatatypeDecl(decl) = &top_level.top_level_type {
                ctx.class = Some(solution::ClassRef::new(file, &decl.class));
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
        sol: &'sol Solution,
        file: &'sol BuiltFile,
        class: Option<solution::ClassRef<'sol>>,
        // top_level: &'a TopLevel,
    ) -> Context<'sol> {
        let ctx = Context {
            sol,
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
        body: &'sol builder::Body<'sol>,
        arguments: &'sol Vec<Arc<builder::Variable<'sol>>>,
    ) {
        self.body = Some(body);
        self.arguments = Some(
            arguments
                .iter()
                .map(|arg| ((&arg.parsed().access.name.content).into(), arg))
                .collect(),
        );
    }

    pub fn find_class(&self, name: &IString) -> Found<solution::Complex<'sol>> {
        self.sol.find_class(Some(self.file), name)
    }

    pub fn variables_in_class(
        &self,
        class: solution::ClassRef<'sol>,
        filter: VariableFilter<'sol>,
    ) -> impl Iterator<
        Item = ListResult<(solution::ClassRef<'sol>, &'sol Arc<builder::Variable<'sol>>)>,
    > + '_ {
        let level = self
            .class
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.sol.get_access_for(current_class, class)
            });

        self.sol
            .variables_in_class(class, filter.with_access(level))
    }

    pub fn variables(
        &self,
        filter: VariableFilter<'sol>,
    ) -> impl Iterator<Item = ListResult<FoundVariable<'sol>>> + '_ {
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

                if let Some(arguments) = &self.arguments {
                    match &filter {
                        VariableFilter::All => {
                            for var in arguments.values() {
                                yield Ok(FoundVariable::new(self.file, var));
                            }
                        }
                        VariableFilter::ForAccess(variable, _) => {
                            if let Some(var) = arguments.get(&(&variable.name.content).into()) {
                                yield Ok(FoundVariable::new(self.file, var));
                            }
                        }
                    }
                }

                if let Some(class) = self.class {
                    for var in self.sol.variables_in_class(
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
                        for var in self.file.inner().variables.values() {
                            yield Ok(FoundVariable::new(self.file, var));
                        }
                    }
                    VariableFilter::ForAccess(variable, _) => {
                        if let Some(var) = self
                            .file
                            .inner()
                            .variables
                            .get(&(&variable.name.content).into())
                        {
                            yield Ok(FoundVariable::new(self.file, var));
                        }
                    }
                }

                for var in self.sol.global_variables(filter) {
                    yield Self::map_res(var, |var| FoundVariable::new(self.file, var));
                }
            },
        )
    }

    pub fn functions_in_class<'a>(
        &'a self,
        class: solution::ClassRef<'sol>,
        filter: FunctionFilter<'sol, 'a>,
    ) -> impl Iterator<Item = ListResult<(solution::ClassRef<'sol>, &'sol builder::Function<'sol>)>> + 'a
    {
        let level = self
            .class
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.sol.get_access_for(current_class, class)
            });

        self.sol
            .functions_in_class(class, filter.with_access(level))
    }

    pub fn functions<'a>(
        &'a self,
        filter: FunctionFilter<'sol, 'a>,
    ) -> impl Iterator<
        Item = ListResult<(
            Option<solution::ClassRef<'sol>>,
            &'sol builder::Function<'sol>,
        )>,
    > + 'a {
        iter::from_coroutine(
            #[coroutine]
            move || {
                if let Some(class) = self.class {
                    for func in self.sol.functions_in_class(
                        class,
                        filter.clone().with_access(tokenizer::AccessType::PRIVATE),
                    ) {
                        yield Self::map_res(func, |(class, t)| (Some(class), t));
                    }
                }

                match filter {
                    FunctionFilter::ForCall(iname, arg_types, _) => {
                        for func in self.sol.global_functions(Some(self.file), filter) {
                            yield Self::map_res(func, |(class, t)| (Some(class), t));
                        }

                        if let Some(overloads) =
                            self.sol.builtins.borrow_dependent().functions.get(&iname)
                        {
                            for overload in overloads {
                                if self.sol.is_function_callable(
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

                        for overloads in self.sol.builtins.borrow_dependent().functions.values() {
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
    //     res: ListResult<(solution::ClassRef<'sol>, &'sol T)>,
    // ) -> ListResult<(Option<solution::ClassRef<'sol>>, &'sol T)> {
    //     Self::map_res(res, |(class, t)| (Some(class), t))
    // }

    // pub fn no_class<T>(
    //     val: ListResult<&'sol T>,
    // ) -> ListResult<(Option<solution::ClassRef<'sol>>, &'sol T)> {
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
