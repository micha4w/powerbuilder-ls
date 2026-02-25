use std::{collections::HashMap, ptr, sync::Arc};

use super::linter::{Linter, Scope};
use crate::{
    builder, project,
    resolver::{self, ResolvedType},
    types::*,
};

impl<'a> Linter<'a> {
    fn lint_variable_declaration(&self, var: &builder::Variable) {
        // TODO: merge with the thing in lint_statements?
        let parsed = var.parsed();

        self.lint_datatype(&parsed.data_type);
        if let Some(expr) = &parsed.initial_value {
            let mut scope = Scope {
                context: resolver::Context::new_for_body(self.proj, self.file, self.class),
                return_type: None,
            };
            if let builder::VariableType::Scoped(_) = &var.variable_type {
                scope.context.class = None;
            }

            self.lint_expression(expr, &scope);
        }

        match &var.variable_type {
            builder::VariableType::Scoped(_) => {
                let existing = self
                    .file
                    .variables
                    .get(&(&parsed.access.name.content).into())
                    .unwrap();
                if existing.parsed().range != parsed.range {
                    self.diagnostic_error(
                        "Scoped Variable is redeclared".into(),
                        parsed.range.clone(),
                    );
                    self.diagnostic_info(
                        "Scoped Variable redeclared here".into(),
                        existing.parsed().range.clone(),
                    );
                }
            }
            builder::VariableType::Instance(_) => {
                if let Some(class_ref) = self.class {
                    let existing = class_ref
                        .class
                        .instance_variables
                        .get(&(&parsed.access.name.content).into())
                        .unwrap();
                    if existing.parsed().range != parsed.range {
                        self.diagnostic_error(
                            "Instance Variable is redeclared".into(),
                            parsed.range.clone(),
                        );
                        self.diagnostic_info(
                            "Instance Variable redeclared here".into(),
                            existing.parsed().range.clone(),
                        );
                    }
                }
            }

            builder::VariableType::Argument(argument) => {}
            builder::VariableType::Local(variable) => {}
        }
    }

    fn lint_datatype_decl(&self, decl: &builder::DatatypeDecl, is_def: bool) {
        let parsed = &decl.class.parsed;

        if let Some(within) = &parsed.class.within {
            match self.lint_datatype(within) {
                ResolvedType::Complex(project::Complex::Class(fc)) => {
                    if fc.file.is_none_or(|f| !ptr::eq(f, self.file)) {
                        self.diagnostic_error(
                            "Parent Class has to be in this file".into(),
                            within.range.clone(),
                        );
                    }
                }
                ResolvedType::Complex(project::Complex::Enum(_)) => {
                    self.diagnostic_error(
                        "Parent Class cannot be an Enum".into(),
                        within.range.clone(),
                    );
                }
                ResolvedType::Unknown => {}
                _ => {
                    self.diagnostic_error(
                        "Parent Class has to be a Complex Type".into(),
                        within.range.clone(),
                    );
                }
            }
        };

        match self.lint_datatype(&parsed.class.base) {
            ResolvedType::Complex(_) | ResolvedType::Unknown => {}
            _ => {
                self.diagnostic_error(
                    "Base Class has to be a Complex Type".into(),
                    parsed.class.base.range.clone(),
                );
            }
        }

        let existing = self
            .file
            .classes
            .get(&(&parsed.class.name.name.content).into())
            .unwrap();

        if is_def {
            if !Arc::ptr_eq(existing, &decl.class) {
                self.diagnostic_error("Class was redefined".into(), parsed.range.clone());
                self.diagnostic_info("Class redefined here".into(), existing.parsed.range.clone());
            }

            // if new_class.is_global {
            //     let name = new_class.name.clone();
            //     drop(new_class);
            //     if self.inherits_from(
            //         &GroupedName::simple(name),
            //         &GroupedName::simple("application".into()),
            //     ) != Some(true)
            //     {
            //         self.diagnostic_warning(
            //             "Global Classes should be ".to_owned()
            //                 + "Forward Declared, otherwise they "
            //                 + "might not be seen by other Files",
            //             datatype.range.clone(),
            //         );
            //     };
            // }
            // if existing.def_decl.get_declaration().is_none() {
            //     self.diagnostic_warning("Class declaration not found".into(), parsed.range.clone());
            // }

            for var in &decl.variables {
                self.lint_variable_declaration(var);
            }
        } else {
            // if existing.def_decl.get_declaration().as_ref().unwrap().range != parsed.range {
            //     self.diagnostic_error("Class was redeclared".into(), parsed.range.clone());
            //     self.diagnostic_info(
            //         "Class redeclared here".into(),
            //         existing
            //             .def_decl
            //             .get_declaration()
            //             .as_ref()
            //             .unwrap()
            //             .range
            //             .clone(),
            //     );
            // }

            // if existing.def_decl.get_definition().is_none() {
            //     self.diagnostic_warning("Class definition not found".into(), parsed.range.clone());
            // }

            for var in &parsed.variables {
                self.diagnostic_info(
                    "Instance Variables in Forward Declaration are ignored".into(),
                    var.variable.range.clone(),
                );
            }
            for event in &parsed.events {
                self.diagnostic_info(
                    "Events in Forward Declaration are ignored".into(),
                    event.range.clone(),
                );
            }
            for func in &parsed.functions {
                self.diagnostic_info(
                    "Functions in Forward Declaration are ignored".into(),
                    func.range.clone(),
                );
            }
        }
    }

    fn require_class(&self, top_level_type: String, range: Range) -> Option<project::ClassRef<'_>> {
        if self.class.is_none() {
            self.diagnostic_error(
                top_level_type + " have to come after the Type Definition that they refer to",
                range,
            )
        }
        self.class.clone()
    }

    fn lint_function_declaration(
        &self,
        function: &builder::FunctionDeclaration,
        is_external: bool,
    ) {
        let parsed = &function.header.parsed;
        parsed.returns.as_ref().map(|ret| self.lint_datatype(ret));
        parsed.arguments.iter().for_each(|arg| {
            self.lint_datatype(&arg.variable.data_type);
        });

        if let Some(class_ref) = self.class {
            let funcs = if is_external {
                &class_ref.class.external_functions
            } else {
                &class_ref.class.functions
            };

            let overloads = funcs.get(&(&parsed.name.content).into()).unwrap();
            let existing = overloads
                .values()
                .find(|func| func.header().types_conflict(&function.header))
                .unwrap();

            let returns_eq = match (&existing.header().returns, &function.header.returns) {
                (Some(a), Some(b)) => {
                    builder::PowerScriptType::simple_eq(&a.powerscript_type, &b.powerscript_type)
                }
                (None, None) => true,
                _ => false,
            };

            let mut failed = false;
            if !returns_eq {
                failed = true;
                self.diagnostic_error(
                    "Same function with different return type already exists".into(),
                    parsed.range.clone(),
                );
                self.diagnostic_hint(
                    "Function with different return type declared here".into(),
                    existing.header().parsed.range.clone(),
                );
            }

            if let Some(decl) = &existing.get_declaration() {
                if !failed && !ptr::eq(function, &**decl) {
                    failed = true;
                    self.diagnostic_error(
                        "Function already forward declared".into(),
                        parsed.range.clone(),
                    );
                    self.diagnostic_hint(
                        "Already forward declared here".into(),
                        decl.header.parsed.range.clone(),
                    );
                }
            }

            if !failed && !is_external && existing.get_definition().is_none() {
                self.diagnostic_error(
                    "Function is missing a definition".into(),
                    parsed.range.clone(),
                );
            }
        }
    }

    pub fn lint_top_level(&mut self, top_level: &'a builder::TopLevel) {
        match &top_level.top_level_type {
            builder::TopLevelType::ForwardDecl(..) => {
                // TODO(forward): ...
                // self.lint_scoped_vars(&decl.variables);

                // for datatype in &decl.classes {
                //     self.lint_datatype_decl(&datatype, false);
                // }
            }

            builder::TopLevelType::ScopedVariableDecl(vars) => {
                for var in vars {
                    self.lint_variable_declaration(var);
                }
            }

            builder::TopLevelType::ScopedVariablesDecl(vars) => {
                for var in vars {
                    self.lint_variable_declaration(var);
                }
            }

            builder::TopLevelType::DatatypeDecl(decl) => {
                self.class = Some(project::ClassRef::new(self.file, &decl.class));
                self.lint_datatype_decl(&decl, true);
            }

            builder::TopLevelType::TypeVariablesDecl(vars) => {
                self.require_class("Instance Variables".into(), top_level.range.clone());

                for var in vars {
                    self.lint_variable_declaration(var);
                }
            }

            builder::TopLevelType::FunctionsForwardDecl(functions) => {
                self.require_class(
                    "Function Forward Declarations".into(),
                    top_level.range.clone(),
                );

                for func in functions {
                    self.lint_function_declaration(func, false);
                }
            }

            builder::TopLevelType::ExternalFunctions(functions) => {
                self.require_class("External Functions".into(), top_level.range.clone());

                for func in functions {
                    self.lint_function_declaration(func, true);
                }
            }

            builder::TopLevelType::FunctionBody(function) => {
                self.require_class("Function Body".into(), top_level.range.clone());

                self.lint_statements_in_block(
                    function.parsed.statements.iter(),
                    &function.header.returns,
                    function.header.arguments.iter(),
                    &function.body,
                );

                if let Some(class_ref) = self.class {
                    let overloads = class_ref
                        .class
                        .functions
                        .get(&function.header.iname())
                        .unwrap();
                    let existing = overloads
                        .values()
                        .find(|func| func.header().types_conflict(&function.header))
                        .unwrap();

                    if let Some(def) = existing.get_definition().as_ref() {
                        if !Arc::ptr_eq(def, function) {
                            self.diagnostic_error(
                                "Function redefined".into(),
                                function.header.parsed.range.clone(),
                            );
                            self.diagnostic_hint(
                                "Function redefined here".into(),
                                def.header.parsed.range.clone(),
                            );
                        }
                    }
                    if existing.get_declaration().is_none() {
                        self.diagnostic_error(
                            "Function is missing a declaration".into(),
                            function.header.parsed.range.clone(),
                        );
                    }
                }
            }

            builder::TopLevelType::EventBody(event) => {
                self.require_class("Event Body".into(), top_level.range.clone());

                self.lint_statements_in_block(
                    event.parsed.statements.iter(),
                    &event.header.returns,
                    event.header.arguments.iter(),
                    &event.body,
                );

                // TODO check whether the event has the correct arguments

                if let Some(class_ref) = self.class {
                    let existing = class_ref.class.events.get(&event.header.iname()).unwrap();

                    let def = existing.get_definition().as_ref().unwrap();
                    if !Arc::ptr_eq(def, event) {
                        self.diagnostic_error(
                            "Event redefined".into(),
                            event.header.parsed.range.clone(),
                        );
                        self.diagnostic_hint(
                            "Event redefined here".into(),
                            def.header.parsed.range.clone(),
                        );
                    }
                    // TODO: events need a declaration?
                    // if existing.def_decl.get_declaration().is_none() {
                    //     self.diagnostic_warning(
                    //         "Event is missing a declaration".into(),
                    //         event.range.clone(),
                    //     );
                    // }
                }
            }

            builder::TopLevelType::OnBody(on) => {
                let prev_class = self.class.take();
                self.class = match self
                    .file
                    .classes
                    .get(&(&on.header.class.name.content).into())
                {
                    Some(class) => Some(project::ClassRef::new(self.file, class)),
                    None => {
                        self.diagnostic_error(
                            "Class does not exist".into(),
                            on.header.class.range.clone(),
                        );
                        None
                    }
                };

                self.lint_statements_in_block(
                    on.statements.iter(),
                    &None,
                    std::iter::empty(),
                    &builder::Body::empty(),
                );
                self.class = prev_class;
            }
        }
    }

    pub fn lint_file(&mut self) {
        for top_level in &self.file.top_levels {
            self.lint_top_level(top_level);
        }
    }
}
