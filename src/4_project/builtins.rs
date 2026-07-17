use std::{
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use prost::{bytes::Bytes, Message as _};
use self_cell::self_cell;

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
pub struct BuiltinsParsed {
    pub functions: Vec<parser::Function>,
    pub classes: Vec<parser::DatatypeDecl>,
}

impl BuiltinsParsed {
    pub fn new() -> Self {
        let functions = powerbuilder_proto::Functions::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/builtins/functions.pb")
        )))
        .expect("Failed to load builtins");

        let classes = powerbuilder_proto::Classes::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/builtins/classes.pb")
        )))
        .expect("Failed to load builtins");

        BuiltinsParsed {
            functions: functions
                .function
                .into_iter()
                .map(|func| Self::parse_proto_function(func))
                .collect(),
            classes: classes
                .class
                .into_iter()
                .map(|class| Self::parse_proto_class(class))
                .collect(),
        }
    }

    fn empty() -> Range {
        Range::empty(BUILTIN_URL.clone())
    }

    fn parse_type(mut name: String) -> parser::DataType {
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

    fn parse_proto_callable(
        ret: Option<String>,
        argument: Vec<powerbuilder_proto::Variable>,
    ) -> (Option<parser::DataType>, Vec<parser::Argument>, bool) {
        let mut has_vararg = false;
        let mut returns = None;
        let mut arguments = Vec::new();

        if let Some(ret) = ret {
            if ret != "\u{1}void" {
                returns = Some(Self::parse_type(ret));
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
                        data_type: Self::parse_type(arg.r#type.unwrap()),
                        access: VariableAccess {
                            name: Token {
                                token_type: TokenType::ID,
                                content: arg.name.unwrap(),
                                range: Self::empty(),
                                error: None,
                            },
                            is_write: true,
                        },
                        initial_value: None,
                        range: Self::empty(),
                    },
                })
            }
        }

        (returns, arguments, has_vararg)
    }

    fn parse_proto_function(func: powerbuilder_proto::Function) -> parser::Function {
        let (parser_returns, parser_arguments, has_vararg) =
            Self::parse_proto_callable(func.ret, func.argument);

        parser::Function {
            help: func.help,
            returns: parser_returns,
            scope_modif: None,
            access: None,
            name: Token {
                token_type: TokenType::ID,
                content: func.name,
                range: Self::empty(),
                error: None,
            },
            arguments: parser_arguments,
            vararg: has_vararg.then(|| Token {
                token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                content: "...".into(),
                range: Self::empty(),
                error: None,
            }),
            range: Self::empty(),
            throws: Vec::new(),
        }
    }

    fn parse_proto_event(event: powerbuilder_proto::Function) -> parser::Event {
        let (parsed_returns, parsed_arguments, has_vararg) =
            Self::parse_proto_callable(event.ret, event.argument);
        assert!(!has_vararg, "Events cannot have varargs");

        parser::Event {
            name: Token {
                token_type: TokenType::ID,
                content: event.name,
                range: Self::empty(),
                error: None,
            },
            range: Self::empty(),
            help: event.help,
            event_type: parser::EventType::User(parsed_returns, parsed_arguments),
        }
    }

    fn parse_proto_variable(var: powerbuilder_proto::Variable) -> parser::InstanceVariable {
        parser::InstanceVariable {
            access: parser::Access {
                read: None,
                write: None,
            },
            variable: parser::Variable {
                help: None,
                constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32 > 0,
                data_type: Self::parse_type(var.r#type.unwrap()),
                access: VariableAccess {
                    name: Token::fake_identifier(var.name.unwrap(), Self::empty()),
                    is_write: true,
                },
                initial_value: None,
                range: Self::empty(),
            },
        }
    }

    pub(super) fn parse_proto_class(class: powerbuilder_proto::Class) -> parser::DatatypeDecl {
        parser::DatatypeDecl {
            class: parser::Class {
                name: parser::DataType::simple(Token::fake_identifier(class.name, Self::empty())),
                base: parser::DataType::simple(Token::fake_identifier(class.base, Self::empty())),
                scope: Some(tokenizer::ScopeModif::GLOBAL),

                within: None,
                autoinstantiate: None,
                native: None,
            },

            variables: class
                .variable
                .into_iter()
                .map(|var| Self::parse_proto_variable(var))
                .collect(),

            functions: class
                .function
                .into_iter()
                .map(|func| Self::parse_proto_function(func))
                .collect(),
            events: class
                .event
                .into_iter()
                .map(|event| Self::parse_proto_event(event))
                .collect(),

            help: class.help,

            range: Self::empty(),
        }
    }
}

#[derive(Debug)]
pub struct BuiltinsInner<'pars> {
    pub enums: HashMap<IString, builder::Enum>,
    pub enums_value_cache: HashMap<IString, IString>,
    pub functions: HashMap<
        IString,
        Vec<
            DefinitionDeclaration<
                Arc<builder::FunctionDefinition<'pars>>,
                Arc<builder::FunctionDeclaration<'pars>>,
            >,
        >,
    >,
    pub classes: HashMap<IString, Arc<builder::Class<'pars>>>,
}

impl<'pars> BuiltinsInner<'pars> {
    pub fn new() -> Self {
        BuiltinsInner {
            enums: HashMap::new(),
            enums_value_cache: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
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

    pub(super) fn load_classes(&mut self, parsed: &'pars BuiltinsParsed) {
        for dt in &parsed.classes {
            let mut new_class = builder::Class::new(dt);

            for var in &dt.variables {
                let new_var = builder::Variable::new_instance(var);
                new_class
                    .instance_variables
                    .insert(new_var.iname(), Arc::new(new_var));
            }

            for func in &dt.functions {
                let new_func = builder::FunctionDeclaration {
                    header: builder::FunctionHeader::new(func),
                };

                new_class
                    .functions
                    .entry(new_func.header.iname())
                    .or_default()
                    .insert(
                        new_func.header.signature(),
                        DefinitionDeclaration::declaration(Arc::new(new_func)),
                    );
            }

            for event in &dt.events {
                let new_event = builder::EventDeclaration {
                    header: builder::EventHeader::new(event),
                };

                new_class.events.insert(
                    new_event.header.iname(),
                    DefinitionDeclaration::declaration(Arc::new(new_event)),
                );
            }

            self.classes.insert(new_class.iname(), Arc::new(new_class));
        }
    }

    pub(super) fn load_functions(&mut self, parsed: &'pars BuiltinsParsed) {
        for func in &parsed.functions {
            let new_func = builder::FunctionDeclaration {
                header: builder::FunctionHeader::new(func),
            };

            self.functions
                .entry(new_func.header.iname())
                .or_default()
                .push(DefinitionDeclaration::declaration(Arc::new(new_func)));
        }
    }
}

self_cell!(
    pub struct Builtins {
        owner: BuiltinsParsed,

        #[covariant]
        dependent: BuiltinsInner,
    }

    impl {Debug}
);
