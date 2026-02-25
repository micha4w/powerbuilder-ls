use std::{
    collections::{
        hash_map::{self, Entry},
        HashMap,
    },
    mem::{replace, swap, take},
    sync::Arc,
};

use super::{types::*, Builder, BuiltFile, ParsedFile};
use crate::{parser, types::DefinitionDeclaration};

impl Builder {
    fn build_scoped_vars(&mut self, vars: &Vec<parser::ScopedVariable>) -> Vec<Arc<Variable>> {
        vars.iter()
            .map(|var| {
                let new_var = Variable::new_scoped(var.clone());

                self.request_class_load(&new_var.data_type.powerscript_type);

                Arc::new(new_var)
            })
            .collect()
    }

    fn build_callable(
        &mut self,
        ret: &Option<DataType>,
        args: &Vec<Arc<Variable>>,
        throws: &Vec<DataType>,
    ) {
        ret.as_ref()
            .map(|ret| self.request_class_load(&ret.powerscript_type));
        args.iter()
            .for_each(|arg| self.request_class_load(&arg.data_type.powerscript_type));
        throws
            .iter()
            .map(|throw| self.request_class_load(&throw.powerscript_type));
    }

    fn build_function_declaration(&mut self, f: parser::Function) -> Arc<FunctionDeclaration> {
        let func = FunctionDeclaration {
            header: FunctionHeader::new(f),
        };

        self.build_callable(
            &func.header.returns,
            &func.header.arguments,
            &func.header.throws,
        );

        Arc::new(func)
    }

    pub fn build_file_shallow(&mut self, parsed: ParsedFile) -> BuiltFile {
        let mut top_levels: Vec<TopLevel> = parsed
            .top_levels
            .into_iter()
            .map(|top_level| TopLevel {
                range: top_level.range,
                top_level_type: match top_level.top_level_type {
                    parser::TopLevelType::ForwardDecl(decl) => {
                        let new_vars = self.build_scoped_vars(&decl.variables);
                        // TODO(forward): classes
                        TopLevelType::ForwardDecl(decl, new_vars)
                    }
                    parser::TopLevelType::ScopedVariableDecl(vars) => {
                        let new_vars = self.build_scoped_vars(&vars);
                        TopLevelType::ScopedVariableDecl(new_vars)
                    }
                    parser::TopLevelType::ScopedVariablesDecl(vars) => {
                        let new_vars = self.build_scoped_vars(&vars);
                        TopLevelType::ScopedVariablesDecl(new_vars)
                    }

                    parser::TopLevelType::DatatypeDecl(datatype) => {
                        let variables = datatype
                            .variables
                            .iter()
                            .map(|v| {
                                let var = Arc::new(Variable::new_instance(v.clone()));
                                self.request_class_load(&var.data_type.powerscript_type);
                                var
                            })
                            .collect();

                        let events = datatype
                            .events
                            .iter()
                            .map(|e| {
                                let event = Arc::new(EventDeclaration {
                                    header: EventHeader::new(e.clone()),
                                });
                                self.build_callable(
                                    &event.header.returns,
                                    &event.header.arguments,
                                    &Vec::new(),
                                );
                                event
                            })
                            .collect();

                        let functions = datatype
                            .functions
                            .iter()
                            .map(|f| self.build_function_declaration(f.clone()))
                            .collect();

                        self.request_class_load(&PowerScriptType::new(&datatype.class.base));
                        datatype
                            .class
                            .within
                            .as_ref()
                            .map(|w| self.request_class_load(&PowerScriptType::new(w)));

                        let class = Arc::new(Class::new(datatype));

                        TopLevelType::DatatypeDecl(DatatypeDecl {
                            class,
                            variables,
                            events,
                            functions,
                        })
                    }
                    parser::TopLevelType::TypeVariablesDecl(parsed) => {
                        let variables = parsed
                            .into_iter()
                            .map(|var| {
                                let new_var = Variable::new_instance(var);
                                self.request_class_load(&new_var.data_type.powerscript_type);
                                Arc::new(new_var)
                            })
                            .collect();

                        TopLevelType::TypeVariablesDecl(variables)
                    }
                    parser::TopLevelType::FunctionsForwardDecl(parsed) => {
                        let functions = parsed
                            .into_iter()
                            .map(|f| self.build_function_declaration(f))
                            .collect();
                        TopLevelType::FunctionsForwardDecl(functions)
                    }
                    parser::TopLevelType::ExternalFunctions(parsed) => {
                        let functions = parsed
                            .into_iter()
                            .map(|f| self.build_function_declaration(f))
                            .collect();
                        TopLevelType::ExternalFunctions(functions)
                    }

                    parser::TopLevelType::FunctionBody(parsed) => {
                        let func = Arc::new(FunctionDefinition {
                            header: FunctionHeader::new(parsed.header.clone()),
                            body: Body::empty(),

                            parsed,
                        });

                        self.build_callable(
                            &func.header.returns,
                            &func.header.arguments,
                            &func.header.throws,
                        );

                        TopLevelType::FunctionBody(func)
                    }
                    parser::TopLevelType::EventBody(parsed) => {
                        let event = Arc::new(EventDefinition {
                            header: EventHeader::new(parsed.header.clone()),
                            body: Body::empty(),

                            parsed,
                        });

                        self.build_callable(
                            &event.header.returns,
                            &event.header.arguments,
                            &Vec::new(),
                        );

                        TopLevelType::EventBody(event)
                    }
                    parser::TopLevelType::OnBody(parsed) => {
                        let mut body = Body::empty();
                        self.build_statements(&parsed.statements, &mut body);

                        TopLevelType::OnBody(parsed)
                    }
                },
            })
            .collect();

        let mut class_ref: Option<&mut Arc<Class>> = None;
        for top_level in &mut top_levels {
            match &mut top_level.top_level_type {
                TopLevelType::ForwardDecl(..) => {}
                TopLevelType::ScopedVariableDecl(..) => {}
                TopLevelType::ScopedVariablesDecl(..) => {}
                TopLevelType::DatatypeDecl(decl) => {
                    let class = Arc::get_mut(&mut decl.class).unwrap();
                    decl.variables.iter().for_each(|var| {
                        class.instance_variables.insert(var.iname(), var.clone());
                    });
                    decl.events.iter().for_each(|event| {
                        class.events.insert(
                            event.header.iname(),
                            DefinitionDeclaration::declaration(event.clone()),
                        );
                    });
                    decl.functions.iter().for_each(|func| {
                        class
                            .function_entry(func.header.iname(), func.header.signature(), false)
                            .insert_entry(DefinitionDeclaration::declaration(func.clone()));
                    });

                    class_ref = Some(&mut decl.class);
                }
                TopLevelType::TypeVariablesDecl(vars) => {
                    if let Some(class) = &mut class_ref {
                        let class = Arc::get_mut(class).unwrap();
                        vars.iter().for_each(|var| {
                            class.instance_variables.insert(var.iname(), var.clone());
                        });
                    }
                }
                TopLevelType::FunctionsForwardDecl(functions) => {
                    if let Some(class) = &mut class_ref {
                        let class = Arc::get_mut(class).unwrap();
                        functions.iter().for_each(|func| {
                            class
                                .function_entry(func.header.iname(), func.header.signature(), false)
                                .and_modify(|f| *f.get_declaration_mut() = Some(func.clone()))
                                .or_insert_with(|| {
                                    DefinitionDeclaration::declaration(func.clone())
                                });
                        });
                    }
                }
                TopLevelType::ExternalFunctions(functions) => {
                    if let Some(class) = &mut class_ref {
                        let class = Arc::get_mut(class).unwrap();
                        functions.iter().for_each(|func| {
                            class
                                .function_entry(func.header.iname(), func.header.signature(), true)
                                .or_insert(DefinitionDeclaration::declaration(func.clone()));
                        });
                    }
                }
                TopLevelType::FunctionBody(function) => {
                    if let Some(class) = &mut class_ref {
                        let class = Arc::get_mut(class).unwrap();
                        class
                            .function_entry(
                                function.header.iname(),
                                function.header.signature(),
                                false,
                            )
                            .and_modify(|f| *f.get_definition_mut() = Some(function.clone()))
                            .or_insert_with(|| DefinitionDeclaration::definition(function.clone()));
                    }
                }
                TopLevelType::EventBody(event) => {
                    if let Some(class) = &mut class_ref {
                        let class = Arc::get_mut(class).unwrap();
                        class
                            .events
                            .entry(event.header.iname())
                            .and_modify(|e| *e.get_definition_mut() = Some(event.clone()))
                            .or_insert_with(|| DefinitionDeclaration::definition(event.clone()));
                    }
                }
                TopLevelType::OnBody(_) => {
                    // TODO(on): ...
                }
            }
        }

        let mut file = BuiltFile {
            meta: parsed.meta,

            bodies_processed: false,
            top_levels,

            classes: HashMap::new(),
            variables: HashMap::new(),
        };
        file.fill_caches();

        file
    }

    pub fn build_file_bodies(&mut self, file: &mut BuiltFile) {
        if file.bodies_processed {
            return;
        }

        // Remove classes Arcs so we can run get_mut
        file.classes.clear();

        let mut class_ref: Option<&mut Arc<Class>> = None;
        for top_level in &mut file.top_levels {
            match &mut top_level.top_level_type {
                TopLevelType::ForwardDecl(..) => {}
                TopLevelType::ScopedVariableDecl(..) => {}
                TopLevelType::ScopedVariablesDecl(..) => {}
                TopLevelType::DatatypeDecl(decl) => {
                    class_ref = Some(&mut decl.class);
                }
                TopLevelType::TypeVariablesDecl(..) => {}
                TopLevelType::FunctionsForwardDecl(..) => {}
                TopLevelType::ExternalFunctions(..) => {}
                TopLevelType::FunctionBody(def) => {
                    let entry = class_ref.as_mut().map(|class| {
                        let class = Arc::get_mut(class).unwrap();
                        class
                            .function_entry(def.header.iname(), def.header.signature(), false)
                            .and_modify(|f| *f.get_definition_mut() = None)
                    });

                    let def_inner = Arc::get_mut(def).unwrap();
                    self.build_statements(&def_inner.parsed.statements, &mut def_inner.body);

                    entry.map(|e| e.and_modify(|f| *f.get_definition_mut() = Some(def.clone())));
                }
                TopLevelType::EventBody(def) => {
                    let entry = class_ref.as_mut().map(|class| {
                        let class = Arc::get_mut(class).unwrap();
                        class
                            .events
                            .entry(def.header.iname())
                            .and_modify(|f| *f.get_definition_mut() = None)
                    });

                    let def_inner = Arc::get_mut(def).unwrap();
                    self.build_statements(&def_inner.parsed.statements, &mut def_inner.body);

                    entry.map(|e| e.and_modify(|f| *f.get_definition_mut() = Some(def.clone())));
                }
                TopLevelType::OnBody(parsed) => {
                    // TODO(on): ...
                    self.build_statements(&parsed.statements, &mut Body::empty());
                }
            }
        }

        file.fill_caches();
        file.bodies_processed = true;
    }
}
