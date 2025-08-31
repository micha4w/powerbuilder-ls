use std::{
    collections::HashMap,
    mem::swap,
    sync::{Arc, Weak},
};

use futures::{future::BoxFuture, FutureExt};

use super::{linter::Linter, types::*};
use crate::{
    parser::{self, GroupedName},
    types::*,
};

impl<'a> Linter<'a> {
    async fn lint_datatype_decl(&mut self, decl: &parser::DatatypeDecl) -> Arc<Mutex<Class>> {
        if let Some(within) = &decl.class.within {
            match &within.data_type_type {
                parser::DataTypeType::Complex(within_name) => {
                    if self.find_class(&within_name).await.is_none() {
                        self.diagnostic_error(
                            "Parent Class not found".into(),
                            within.range.clone(),
                        )
                        .await;
                    }
                }
                _ => {
                    self.diagnostic_error(
                        "Parent Class has to be a Complex Type".into(),
                        within.range.clone(),
                    )
                    .await;
                }
            }
        };

        match &decl.class.base.data_type_type {
            parser::DataTypeType::Complex(base_name) => {
                if self.find_class(&base_name).await.is_none() {
                    self.diagnostic_error(
                        "Base Class not found".into(),
                        decl.class.base.range.clone(),
                    )
                    .await;
                }
            }
            _ => {
                self.diagnostic_error(
                    "Base Class has to be a Complex Type".into(),
                    decl.class.base.range.clone(),
                )
                .await;
            }
        }

        let new_class_mut = arc_mut(Class::new(decl.class.clone()));

        {
            let mut new_class = new_class_mut.lock().await;

            for var in &decl.variables {
                new_class.instance_variables.insert(
                    (&var.variable.access.name.content).into(),
                    arc_mut(Variable {
                        variable_type: VariableType::Instance((
                            Arc::downgrade(&new_class_mut),
                            var.clone(),
                        )),
                        data_type: var.variable.data_type.data_type_type.clone(),
                        // uses: Vec::new(),
                    }),
                );
            }

            for event in &decl.events {
                new_class.events.insert(
                    (&event.name.content).into(),
                    arc_mut(Event::new_declaration(event.clone(), None)),
                );
            }

            for function in &decl.functions {
                self.add_function_declaration(
                    &arc_mut(Function::new_declaration(function.clone(), None)),
                    &mut new_class,
                    false,
                )
                .await;
            }
        }

        new_class_mut
    }

    async fn require_class(
        &mut self,
        top_level_type: String,
        range: Range,
    ) -> Option<Arc<Mutex<Class>>> {
        if self.class.is_none() {
            self.diagnostic_error(
                top_level_type + " have to come after the Type Definition that they refer to",
                range,
            )
            .await;
        }
        self.class.clone()
    }

    async fn lint_scoped_vars(
        &mut self,
        vars: &Vec<parser::ScopedVariable>,
    ) -> HashMap<IString, Arc<Mutex<Variable>>> {
        let mut new_vars = HashMap::new();
        for var in vars {
            let new_var = arc_mut(Variable {
                data_type: var.variable.data_type.data_type_type.clone(),
                variable_type: VariableType::Scoped(var.clone()),
            });
            new_vars.insert((&var.variable.access.name.content).into(), new_var.clone());
        }

        self.file.write().await.variables.extend(new_vars.clone());
        new_vars
    }

    pub async fn lint_top_level_only_types(
        &mut self,
        top_level: parser::TopLevel,
    ) -> (Range, TopLevelType<LintProgressOnlyTypes>) {
        (
            top_level.range,
            match top_level.top_level_type {
                parser::TopLevelType::ForwardDecl(vars, types) => {
                    let new_vars = self.lint_scoped_vars(&vars).await;
                    let mut classes = HashMap::new();
                    for datatype in &types {
                        let mut new_class = Class::new(datatype.class.clone());
                        new_class.usage.declaration = Some(datatype.range.clone());

                        classes.insert(
                            (&datatype.class.name.data_type_type.to_string()).into(),
                            arc_mut(new_class),
                        );
                    }
                    self.file.write().await.classes.extend(classes.clone());
                    TopLevelType::ForwardDecl(((vars, types), (new_vars, classes)))
                }
                parser::TopLevelType::ScopedVariablesDecl(vars) => {
                    let new_vars = self.lint_scoped_vars(&vars).await;
                    TopLevelType::ScopedVariablesDecl((vars, new_vars))
                }
                parser::TopLevelType::ScopedVariableDecl(vars) => {
                    let new_vars = self.lint_scoped_vars(&vars).await;
                    TopLevelType::ScopedVariableDecl((vars, new_vars))
                }
                parser::TopLevelType::DatatypeDecl(datatype_decl) => {
                    TopLevelType::DatatypeDecl(datatype_decl)
                }
                parser::TopLevelType::TypeVariablesDecl(vec) => {
                    TopLevelType::TypeVariablesDecl(vec)
                }
                parser::TopLevelType::FunctionsForwardDecl(vec) => {
                    TopLevelType::FunctionsForwardDecl(vec)
                }
                parser::TopLevelType::ExternalFunctions(vec) => {
                    TopLevelType::ExternalFunctions(vec)
                }
                parser::TopLevelType::FunctionBody(function, vec) => {
                    TopLevelType::FunctionBody((function, vec))
                }
                parser::TopLevelType::EventBody(event, vec) => {
                    TopLevelType::EventBody((event, vec))
                }
                parser::TopLevelType::OnBody(on, vec) => TopLevelType::OnBody((on, vec)),
            },
        )
    }

    async fn add_function_declaration(
        &mut self,
        function_mut: &Arc<Mutex<Function>>,
        class: &mut Class,
        is_external: bool,
    ) {
        let new_func = function_mut.lock().await;

        match class.find_conflicting_function(&new_func).await {
            Some(func) => {
                let mut func = func.lock().await;
                if func.returns != (&new_func.parsed().returns).into() {
                    self.diagnostic_error(
                        "Same function with different return type already exists".into(),
                        new_func.parsed().range.clone(),
                    )
                    .await;
                    if let Some(declaration) = &func.declaration {
                        self.diagnostic_hint(
                            "Function with different return type declared here".into(),
                            declaration.range.clone(),
                        )
                        .await;
                    }
                }

                if let Some(declaration) = &func.declaration {
                    self.diagnostic_error(
                        "Function already forward declared".into(),
                        new_func.parsed().range.clone(),
                    )
                    .await;
                    self.diagnostic_hint(
                        "Already forward declared here".into(),
                        declaration.range.clone(),
                    )
                    .await;
                } else {
                    func.declaration = new_func.declaration.clone();
                }
            }
            None => {
                let functions = if is_external {
                    &mut class.external_functions
                } else {
                    &mut class.functions
                };

                let iname = IString::from(&new_func.parsed().name.content);
                match functions.get_mut(&iname) {
                    Some(funcs) => funcs.push(function_mut.clone()),
                    None => {
                        functions.insert(iname, vec![function_mut.clone()]);
                    }
                }
            }
        }
    }

    async fn add_function_declarations(
        &mut self,
        top_level_range: &Range,
        functions: &Vec<parser::Function>,
        is_external: bool,
    ) -> HashMap<IString, Vec<Arc<Mutex<Function>>>> {
        let mut new_functions = HashMap::new();
        for function in functions {
            let iname = (&function.name.content).into();
            let func_arc = arc_mut(Function::new_declaration(function.clone(), None));
            match new_functions.get_mut(&iname) {
                None => {
                    new_functions.insert(iname, vec![func_arc]);
                }
                Some(funcs) => funcs.push(func_arc),
            }
        }

        let top_level_name = if is_external {
            "Function Forward Declarations"
        } else {
            "External Functions"
        };
        if let Some(class_arc) = self
            .require_class(top_level_name.into(), top_level_range.clone())
            .await
        {
            let mut class = class_arc.lock().await;
            for (_, funcs) in &new_functions {
                for func in funcs {
                    self.add_function_declaration(func, &mut class, is_external)
                        .await;
                }
            }
        }

        new_functions
    }

    pub async fn lint_top_level_shallow(
        &mut self,
        range: &Range,
        top_level: TopLevelType<LintProgressOnlyTypes>,
    ) -> TopLevelType<LintProgressShallow> {
        match top_level {
            TopLevelType::DatatypeDecl(datatype) => {
                let new_class_mut = self.lint_datatype_decl(&datatype).await;

                let class_lock = self
                    .file
                    .read()
                    .await
                    .classes
                    .get(&(&datatype.class.name.data_type_type.to_string()).into())
                    .cloned();
                let class = match class_lock {
                    Some(class_arc) => {
                        let new_class = Arc::try_unwrap(new_class_mut).unwrap().into_inner();

                        self.class = Some(class_arc.clone());
                        let mut class = class_arc.lock().await;
                        match &class.usage.definition {
                            Some(def) => {
                                self.diagnostic_error(
                                    "Type already defined".into(),
                                    datatype.range.clone(),
                                )
                                .await;
                                self.diagnostic_hint(
                                    "Type already defined here".into(),
                                    def.clone(),
                                )
                                .await;
                            }
                            None => {
                                class.usage.definition = new_class.usage.definition;
                                // swap(&mut class.events, &mut new_class.events);
                                // swap(
                                //     &mut class.instance_variables,
                                //     &mut new_class.instance_variables,
                                // );
                                // swap(&mut class.base, &mut new_class.base);
                                // swap(&mut class.within, &mut new_class.within);
                            }
                        }

                        for (iname, var) in new_class.instance_variables {
                            if !class.instance_variables.contains_key(&iname) {
                                if let VariableType::Instance(inst) =
                                    &mut var.lock().await.variable_type
                                {
                                    inst.0 = Arc::downgrade(&class_arc);
                                }
                                class.instance_variables.insert(iname, var);
                            }
                        }
                        for (iname, event) in new_class.events {
                            if !class.events.contains_key(&iname) {
                                class.events.insert(iname, event);
                            }
                        }

                        class_arc.clone()
                    }
                    None => {
                        {
                            let new_class = new_class_mut.lock().await;
                            let iname = (&new_class.name).into();
                            self.class = Some(new_class_mut.clone());
                            self.file
                                .write()
                                .await
                                .classes
                                .insert(iname, new_class_mut.clone());

                            if new_class.is_global {
                                let name = new_class.name.clone();
                                drop(new_class);
                                if self
                                    .inherits_from(
                                        &GroupedName::simple(name),
                                        &GroupedName::simple("application".into()),
                                    )
                                    .await
                                    != Some(true)
                                {
                                    self.diagnostic_warning(
                                        "Global Classes should be ".to_owned()
                                            + "Forward Declared, otherwise they "
                                            + "might not be seen by other Files",
                                        datatype.range.clone(),
                                    )
                                    .await;
                                };
                            }
                        }

                        new_class_mut
                    }
                };

                TopLevelType::DatatypeDecl((datatype, class))
            }
            TopLevelType::TypeVariablesDecl(vars) => {
                {
                    let mut new_vars = HashMap::new();
                    for var in &vars {
                        let data_type = var.variable.data_type.data_type_type.clone();

                        if let parser::DataTypeType::Complex(name) = &data_type {
                            if self.find_class(&name).await.is_none() {
                                self.diagnostic_error(
                                    "Class not found".into(),
                                    var.variable.data_type.range.clone(),
                                )
                                .await;
                            }
                        }

                        new_vars.insert(
                            (&var.variable.access.name.content).into(),
                            arc_mut(Variable {
                                variable_type: VariableType::Instance((
                                    self.class
                                        .as_ref()
                                        .map_or(Weak::new(), |class| Arc::downgrade(&class)),
                                    var.clone(),
                                )),
                                data_type,
                                // uses: Vec::new(),
                            }),
                        );
                    }

                    if let Some(class) = self
                        .require_class("Type Variables".into(), range.clone())
                        .await
                    {
                        class
                            .lock()
                            .await
                            .instance_variables
                            .extend(new_vars.clone());
                    }

                    TopLevelType::TypeVariablesDecl((vars, (self.class.clone(), new_vars)))
                }
            }
            TopLevelType::FunctionsForwardDecl(functions) => {
                let new_functions = self
                    .add_function_declarations(&range, &functions, false)
                    .await;
                TopLevelType::FunctionsForwardDecl((functions, (self.class.clone(), new_functions)))
            }
            TopLevelType::ExternalFunctions(functions) => {
                let new_functions = self
                    .add_function_declarations(&range, &functions, true)
                    .await;
                TopLevelType::ExternalFunctions((functions, (self.class.clone(), new_functions)))
            }
            TopLevelType::ForwardDecl(prev) => TopLevelType::ForwardDecl(prev),
            TopLevelType::ScopedVariableDecl(prev) => TopLevelType::ScopedVariableDecl(prev),
            TopLevelType::ScopedVariablesDecl(prev) => TopLevelType::ScopedVariablesDecl(prev),
            TopLevelType::FunctionBody(prev) => TopLevelType::FunctionBody(prev),
            TopLevelType::EventBody(prev) => TopLevelType::EventBody(prev),
            TopLevelType::OnBody(prev) => TopLevelType::OnBody(prev),
        }
    }

    pub async fn lint_top_level_complete(
        &mut self,
        range: &Range,
        top_level: TopLevelType<LintProgressShallow>,
    ) -> TopLevelType<LintProgressComplete> {
        match top_level {
            TopLevelType::DatatypeDecl((class_decl, class)) => {
                self.class = Some(class.clone());
                TopLevelType::DatatypeDecl((class_decl, class))
            }
            TopLevelType::FunctionBody((function, statements)) => {
                let mut definition = FunctionDefinition::new(function.clone());

                swap(&mut self.variables, &mut definition.variables);
                self.return_type = (&function.returns).into();

                self.lint_statements(&statements).await;
                swap(&mut self.variables, &mut definition.variables);

                let mut new_func = Function {
                    returns: self.return_type.clone(),
                    help: None,
                    declaration: None,
                    definition: Some(definition),
                };

                let new_function = match self
                    .require_class("Function Bodies".into(), range.clone())
                    .await
                {
                    None => arc_mut(new_func),
                    Some(class_arc) => {
                        let mut class = class_arc.lock().await;
                        let new_func_mut = match class.find_conflicting_function(&new_func).await {
                            Some(existing_arc) => {
                                let mut existing = existing_arc.lock().await;
                                if let Some(definition) = &existing.definition {
                                    self.diagnostic_error(
                                        "Function already defined".into(),
                                        function.range.clone(),
                                    )
                                    .await;
                                    self.diagnostic_hint(
                                        "Already defined here".into(),
                                        definition.parsed.range.clone(),
                                    )
                                    .await;
                                    new_func.declaration = existing.declaration.clone();
                                    None
                                } else {
                                    existing.definition = new_func.definition.take();
                                    Some(existing_arc.clone())
                                }
                            }
                            None => {
                                self.diagnostic_error(
                                    "Function is missing a Forward Declaration".into(),
                                    function.range.clone(),
                                )
                                .await;
                                None
                            }
                        };

                        new_func_mut.unwrap_or_else(|| {
                            let iname = IString::from(&new_func.parsed().name.content);
                            let new_func_mut = arc_mut(new_func);
                            match class.functions.get_mut(&iname) {
                                Some(funcs) => funcs.push(new_func_mut.clone()),
                                None => {
                                    class.functions.insert(iname, vec![new_func_mut.clone()]);
                                }
                            }
                            new_func_mut
                        })
                    }
                };
                TopLevelType::FunctionBody((
                    (function, statements),
                    (self.class.clone(), new_function),
                ))
            }
            TopLevelType::EventBody((event, statements)) => {
                let mut new_event = Event::new_definition(event.clone());
                let definition = new_event.definition.as_mut().unwrap();

                swap(&mut self.variables, &mut definition.variables);
                self.return_type = new_event.returns.clone();
                self.lint_statements(&statements).await;
                swap(&mut self.variables, &mut definition.variables);

                let event_mut = match self
                    .require_class("Event Bodies".into(), range.clone())
                    .await
                {
                    None => arc_mut(new_event),
                    Some(class_arc) => {
                        let mut class = class_arc.lock().await;
                        let new_event_mut = match class.find_conflicting_event(&new_event).await {
                            Some(existing_arc) => {
                                let mut existing = existing_arc.lock().await;
                                if let Some(definition) = &existing.definition {
                                    self.diagnostic_error(
                                        "Event already defined".into(),
                                        event.range.clone(),
                                    )
                                    .await;
                                    self.diagnostic_hint(
                                        "Already defined here".into(),
                                        definition.parsed.range.clone(),
                                    )
                                    .await;
                                    new_event.declaration = existing.declaration.clone();
                                    None
                                } else {
                                    existing.definition = new_event.definition.take();
                                    Some(existing_arc.clone())
                                }
                            }
                            None => {
                                self.diagnostic_error(
                                    "Event is missing a Forward Declaration".into(),
                                    event.range.clone(),
                                )
                                .await;
                                None
                            }
                        };

                        new_event_mut.unwrap_or_else(|| {
                            let iname = IString::from(&new_event.parsed().name.content);
                            let new_event_mut = arc_mut(new_event);
                            class.events.insert(iname, new_event_mut.clone());
                            new_event_mut
                        })
                    }
                };

                TopLevelType::EventBody(((event, statements), (self.class.clone(), event_mut)))
            }
            TopLevelType::OnBody((on, statements)) => {
                self.class = match self
                    .find_class(&GroupedName::new(None, on.class.content.clone()))
                    .await
                {
                    Some(Complex::Class(class)) => Some(class),
                    _ => {
                        self.diagnostic_error(
                            "On Body for Enum or non Existing Class".into(),
                            range.clone(),
                        )
                        .await;
                        None
                    }
                };

                self.variables = HashMap::new();
                self.return_type = parser::DataTypeType::Void;

                self.lint_statements(&statements).await;

                TopLevelType::OnBody(((on, statements), (self.class.clone(),)))
                // TODO do something with the on?
            }
            TopLevelType::ForwardDecl(prev) => TopLevelType::ForwardDecl(prev),
            TopLevelType::ScopedVariableDecl(prev) => TopLevelType::ScopedVariableDecl(prev),
            TopLevelType::ScopedVariablesDecl(prev) => TopLevelType::ScopedVariablesDecl(prev),
            TopLevelType::TypeVariablesDecl(prev) => TopLevelType::TypeVariablesDecl(prev),
            TopLevelType::FunctionsForwardDecl(prev) => TopLevelType::FunctionsForwardDecl(prev),
            TopLevelType::ExternalFunctions(prev) => TopLevelType::ExternalFunctions(prev),
        }
    }

    pub fn lint_file(&mut self) -> BoxFuture<()> {
        async move {
            let mut top_levels = ProgressedTopLevels::None(Vec::new());
            swap(&mut self.file.write().await.top_levels, &mut top_levels);

            let new_top_levels = match top_levels {
                ProgressedTopLevels::None(prev_top_levels) => {
                    let mut new_vec = Vec::new();
                    for top_level in prev_top_levels {
                        new_vec.push(self.lint_top_level_only_types(top_level).await);
                    }
                    ProgressedTopLevels::OnlyTypes(new_vec)
                }
                ProgressedTopLevels::OnlyTypes(prev_top_levels) => {
                    let mut new_vec = Vec::new();
                    for (range, top_level) in prev_top_levels {
                        new_vec.push((
                            range.clone(),
                            self.lint_top_level_shallow(&range, top_level).await,
                        ))
                    }
                    ProgressedTopLevels::Shallow(new_vec)
                }
                ProgressedTopLevels::Shallow(prev_top_levels) => {
                    let mut new_vec = Vec::new();
                    for (range, top_level) in prev_top_levels {
                        new_vec.push((
                            range.clone(),
                            self.lint_top_level_complete(&range, top_level).await,
                        ));
                    }
                    ProgressedTopLevels::Complete(new_vec)
                }
                ProgressedTopLevels::Complete(prev_top_levels) => {
                    ProgressedTopLevels::Complete(prev_top_levels)
                }
            };

            self.file.write().await.top_levels = new_top_levels;
        }
        .boxed()
    }
}
