use std::{
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use prost::{bytes::Bytes, Message as _};

use super::{
    powerbuilder_proto::{self, variable},
    types::*,
};
use crate::{
    builder,
    parser::{self, Parser, VariableAccess},
    tokenizer::{self, Token, TokenType},
    types::*,
};

pub static BUILTIN_URL: LazyLock<Arc<Url>> =
    LazyLock::new(|| Arc::new(Url::parse("powerbuilder-ls:///builtins.sru").unwrap()));

#[derive(Debug)]
pub struct Builtins {
    pub enums: HashMap<IString, builder::Enum>,
    pub enums_value_cache: HashMap<IString, IString>,
    pub functions: HashMap<
        IString,
        Vec<
            DefinitionDeclaration<
                Arc<builder::FunctionDefinition>,
                Arc<builder::FunctionDeclaration>,
            >,
        >,
    >,
    pub classes: HashMap<IString, Arc<builder::Class>>,
}

impl Builtins {
    pub fn new() -> Self {
        Builtins {
            enums: HashMap::new(),
            enums_value_cache: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    fn empty(&self) -> Range {
        Range::empty(BUILTIN_URL.clone())
    }

    fn parse_type(&self, mut name: String) -> parser::DataType {
        name += "\n\n";

        let mut parser = Parser::new(name.chars(), BUILTIN_URL.clone());
        if let Some(Ok(dt) | Err((_, Some(dt)))) = parser.parse_type() {
            return dt;
        }

        let errors = parser.get_syntax_errors();
        if errors.is_empty() {
            panic!(
                "Syntax error in builtin type ({}): {}",
                name,
                errors
                    .into_iter()
                    .map(|err| err.message)
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        } else {
            panic!("Unexpected Error while Parsing DataType")
        }
    }

    fn load_proto_callable(
        &self,
        ret: Option<String>,
        argument: Vec<powerbuilder_proto::Variable>,
    ) -> (Option<parser::DataType>, Vec<parser::Argument>, bool) {
        let mut has_vararg = false;
        let mut returns = None;
        let mut arguments = Vec::new();

        if let Some(ret) = ret {
            if ret != "\u{1}void" {
                returns = Some(self.parse_type(ret));
            }
        }

        for arg in argument {
            let flags = arg.flags.unwrap_or(0);

            if flags & variable::Flag::IsVarlist as u32 > 0 {
                has_vararg = true;
            } else {
                arguments.push(parser::Argument {
                    is_ref: flags & variable::Flag::IsRef as u32 > 0,
                    variable: parser::Variable {
                        help: None,
                        constant: flags & variable::Flag::NoWrite as u32 > 0,
                        data_type: self.parse_type(arg.r#type.unwrap()),
                        access: VariableAccess {
                            name: Token {
                                token_type: TokenType::ID,
                                content: arg.name.clone().unwrap(),
                                range: self.empty(),
                                error: None,
                            },
                            is_write: true,
                        },
                        initial_value: None,
                        range: self.empty(),
                    },
                })
            }
        }

        (returns, arguments, has_vararg)
    }

    fn load_return_and_arguments(
        &self,
        returns: Option<parser::DataType>,
        arguments: Vec<parser::Argument>,
    ) -> (Option<builder::DataType>, Vec<builder::Variable>) {
        (
            returns.map(builder::DataType::new),
            arguments
                .into_iter()
                .map(|arg| builder::Variable::new_argument(arg))
                .collect(),
        )
    }

    fn load_proto_function(
        &self,
        func: powerbuilder_proto::Function,
    ) -> builder::FunctionDeclaration {
        let (parser_returns, parser_arguments, has_vararg) =
            self.load_proto_callable(func.ret, func.argument);

        builder::FunctionDeclaration {
            header: builder::FunctionHeader::new(parser::Function {
                help: func.help,
                returns: parser_returns,
                scope_modif: None,
                access: None,
                name: Token {
                    token_type: TokenType::ID,
                    content: func.name,
                    range: self.empty(),
                    error: None,
                },
                arguments: parser_arguments,
                vararg: has_vararg.then(|| Token {
                    token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                    content: "...".into(),
                    range: self.empty(),
                    error: None,
                }),
                range: self.empty(),
                throws: Vec::new(),
            }),
        }
    }

    fn load_proto_event(&self, event: powerbuilder_proto::Function) -> builder::EventDeclaration {
        let (parsed_returns, parsed_arguments, has_vararg) =
            self.load_proto_callable(event.ret, event.argument);
        if has_vararg {
            todo!();
        }

        builder::EventDeclaration {
            header: builder::EventHeader::new(parser::Event {
                name: Token {
                    token_type: TokenType::ID,
                    content: event.name,
                    range: self.empty(),
                    error: None,
                },
                range: self.empty(),
                help: event.help,
                event_type: parser::EventType::User(parsed_returns, parsed_arguments),
            }),
        }
    }

    pub(super) fn load_enums(&mut self) {
        let enums = powerbuilder_proto::Enums::decode(Bytes::from_static(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/builtins/enums.pb"
        ))))
        .expect("Failed to load builtins");
        for mut en in enums.r#enum {
            let iname = IString::from(&en.name);

            en.value.iter_mut().for_each(|name| name.push('!'));
            for val in &en.value {
                self.enums_value_cache.insert(val.into(), iname.clone());
            }

            self.enums.insert(
                iname,
                builder::Enum {
                    name: en.name,
                    help: en.help,
                    values: en.value,
                },
            );
        }
    }

    pub(super) fn load_classes(&mut self) {
        let classes = powerbuilder_proto::Classes::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/builtins/classes.pb")
        )))
        .expect("Failed to load builtins");

        for class in classes.class.clone() {
            let iname = (&class.name).into();
            let mut new_class = builder::Class::new(parser::DatatypeDecl {
                class: parser::Class {
                    name: parser::DataType::simple(Token::fake_identifier(
                        class.name.clone(),
                        self.empty(),
                    )),
                    base: parser::DataType::simple(Token::fake_identifier(
                        class.base.clone(),
                        self.empty(),
                    )),
                    scope: Some(tokenizer::ScopeModif::GLOBAL),

                    within: None,
                    autoinstantiate: None,
                    native: None,
                },
                variables: Vec::new(),
                events: Vec::new(),
                functions: Vec::new(),
                range: self.empty(),
            });

            new_class.help = class.help;

            for var in class.variable {
                let iname = var.name.as_ref().unwrap().into();
                let parsed = parser::Variable {
                    help: None,
                    constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32 > 0,
                    data_type: self.parse_type(var.r#type.unwrap()),
                    access: VariableAccess {
                        name: Token::fake_identifier(var.name.unwrap(), self.empty()),
                        is_write: true,
                    },
                    initial_value: None,
                    range: self.empty(),
                };

                new_class.instance_variables.insert(
                    iname,
                    Arc::new(builder::Variable::new_instance(parser::InstanceVariable {
                        variable: parsed,
                        access: parser::Access {
                            read: None,
                            write: None,
                        },
                    })),
                );
            }

            for func in class.function {
                let new_func = self.load_proto_function(func);

                new_class
                    .functions
                    .entry(new_func.header.iname())
                    .or_default()
                    .insert(
                        new_func.header.signature(),
                        DefinitionDeclaration::declaration(Arc::new(new_func)),
                    );
            }

            for event in class.event {
                let new_event = self.load_proto_event(event);

                new_class.events.insert(
                    new_event.header.iname(),
                    DefinitionDeclaration::declaration(Arc::new(new_event)),
                );
            }

            self.classes.insert(iname, Arc::new(new_class));
        }
    }

    pub(super) fn load_functions(&mut self) {
        let funcs = powerbuilder_proto::Functions::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/builtins/functions.pb")
        )))
        .expect("Failed to load builtins");

        for func in funcs.function {
            let iname = (&func.name).into();
            let new_func = self.load_proto_function(func);

            self.functions
                .entry(iname)
                .or_default()
                .push(DefinitionDeclaration::declaration(Arc::new(new_func)));
        }
    }
}
