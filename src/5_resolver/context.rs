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
                    builder::TopLevelType::OnBody(_on) => {} // TODO(on): ...
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

    pub fn find_class(&self, name: &Grouped<IString>) -> Found<solution::Complex<'sol>> {
        self.sol.find_class(Some(self.file), name)
    }

    pub fn classes<'a>(
        &'a self,
        filter: ClassFilter<'sol>,
    ) -> impl Iterator<Item = ListResult<solution::Complex<'sol>>> + 'a
    where
        'sol: 'a,
    {
        self.sol.classes(Some(self.file), filter)
    }

    pub fn get_access_for(&self, class: solution::ClassRef<'sol>) -> tokenizer::AccessType {
        self.class
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.sol.get_access_for(current_class, class)
            })
    }

    pub fn variables_in_class<'a>(
        &self,
        class: solution::ClassRef<'sol>,
        mut filter: VariableFilter<'a>,
    ) -> impl Iterator<
        Item = ListResult<(solution::ClassRef<'sol>, &'sol Arc<builder::Variable<'sol>>)>,
    > + 'a
    where
        'sol: 'a,
    {
        let level = self.get_access_for(class);

        filter.access = filter
            .access
            .map(|(old_level, w)| (tokenizer::AccessType::stricter(level, old_level), w));

        self.sol.variables_in_class(class, filter)
    }

    pub fn variables<'a>(
        &'a self,
        filter: VariableFilter<'a>,
    ) -> impl Iterator<Item = ListResult<FoundVariable<'sol>>> + 'a {
        iter::from_coroutine(Box::pin(
            #[coroutine]
            static move || {
                if let Some(body) = self.body {
                    for (var, err) in filter
                        .name
                        .filter_hash_map(&body.variables, ListError::WrongName)
                    {
                        yield (FoundVariable::new(self.file, var), err);
                    }
                }

                if let Some(arguments) = &self.arguments {
                    for (var, err) in filter
                        .name
                        .filter_hash_map(&arguments, ListError::WrongName)
                    {
                        yield (FoundVariable::new(self.file, var), err);
                    }
                }

                if let Some(class) = self.class {
                    let mut filter = filter.clone();
                    filter.access = filter
                        .access
                        .map(|(_, is_write)| (tokenizer::AccessType::PRIVATE, is_write));

                    for ((class, var), err) in self.sol.variables_in_class(class, filter) {
                        yield (FoundVariable::new_instance(class, var), err);
                    }
                }

                for (var, err) in filter
                    .name
                    .filter_hash_map(&self.file.inner().variables, ListError::WrongName)
                {
                    yield (FoundVariable::new(self.file, var), err);
                }

                let globals = self.sol.global_variables(GlobalVariableFilter {
                    name: filter.name,
                    accessible: match filter.access {
                        SearchFilter::Filter { do_error: true, .. } => SearchFilter::Filter {
                            want: (),
                            do_error: true,
                        },
                        _ => SearchFilter::All,
                    },
                });
                for (var, err) in globals {
                    yield (FoundVariable::new(self.file, var), err);
                }
            },
        ))
    }

    pub fn functions_in_class<'a>(
        &'a self,
        class: solution::ClassRef<'sol>,
        mut filter: FunctionFilter<'sol, 'a>,
    ) -> impl Iterator<Item = ListResult<(solution::ClassRef<'sol>, &'sol builder::Function<'sol>)>> + 'a
    {
        let level = self.get_access_for(class);

        filter.access = filter
            .access
            .map(|old_level| tokenizer::AccessType::stricter(level, old_level));

        self.sol.functions_in_class(class, filter)
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
                    let mut filter = filter.clone();
                    filter.access = filter.access.map(|old_level| {
                        tokenizer::AccessType::stricter(tokenizer::AccessType::PRIVATE, old_level)
                    });

                    for ((class, func), err) in self.sol.functions_in_class(class, filter) {
                        yield ((Some(class), func), err);
                    }
                }

                for func in self.sol.global_functions(
                    Some(self.file),
                    GlobalFunctionFilter {
                        name: filter.name,
                        accessible: match filter.access {
                            SearchFilter::Filter { do_error: true, .. } => SearchFilter::Filter {
                                want: (),
                                do_error: true,
                            },
                            _ => SearchFilter::All,
                        },
                        arguments: filter.arguments,
                    },
                ) {
                    yield func;
                }
            },
        )
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

    pub fn gather<T>(iter: impl Iterator<Item = ListResult<T>>) -> Result<T, Vec<(T, ListError)>> {
        let mut errors = Vec::new();

        for (val, err) in iter {
            if err.is_empty() {
                return Ok(val);
            }
            errors.push((val, err));
        }

        Err(errors)
    }

    pub fn first<T>(iter: impl Iterator<Item = ListResult<T>>) -> Found<(T, ListError)> {
        let mut fail = Found::No;

        for item in iter {
            if item.1.is_empty() {
                return Found::Yes(item);
            }
            fail = Found::Yes(item);
        }

        fail
    }
}
