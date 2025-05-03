use std::{str::Chars, sync::Arc};

use anyhow::anyhow;
use prost::{bytes::Bytes, Message as _};
use tokio::sync::Mutex;

use super::{
    powerbuilder_proto::{self, variable},
    types::*,
    Project,
};
use crate::{
    parser::{self, GroupedName, Parser, VariableAccess},
    tokenizer::{self, Token, TokenType},
    types::*,
};

impl Project {
    fn builtin_empty(&self) -> Range {
        Range::empty(Arc::new(self.builtin_url.clone()))
    }

    fn parse_type(&self, mut name: String) -> anyhow::Result<parser::DataType> {
        name += "\n\n";

        let mut parser = Parser::new(name.chars(), self.builtin_url.clone());
        if let Some(Ok(dt) | Err((_, Some(dt)))) = parser.parse_type() {
            return Ok(dt);
        }

        let errors = parser.get_syntax_errors();
        if errors.is_empty() {
            Err(anyhow::Error::msg(
                errors
                    .into_iter()
                    .map(|err| err.message)
                    .collect::<Vec<_>>()
                    .join(", "),
            ))
        } else {
            Err(anyhow!("Unexpected Error while Parsing DataType"))
        }
    }

    fn parse_help(help: &Option<String>) -> anyhow::Result<Option<String>> {
        return Ok(None);

        // TODO actually parse the html goodly
        // let Some(help) = help else {
        //     return Ok(None);
        // };
        // let md = htmd::convert(help)?;
        // if md.trim().is_empty() {
        //     return Ok(None);
        // }

        // Ok(Some(md))
    }

    fn load_proto_function(
        &self,
        func: powerbuilder_proto::Function,
    ) -> anyhow::Result<(
        Option<parser::DataType>,
        Vec<parser::Argument>,
        bool,
        Option<String>,
    )> {
        let mut has_vararg = false;
        let mut returns = None;
        let mut arguments = Vec::new();

        if let Some(ret) = func.ret {
            if ret != "\u{1}void" {
                returns = Some(self.parse_type(ret)?);
            }
        }

        for arg in func.argument {
            let flags = arg.flags.unwrap_or(0);

            if flags & variable::Flag::IsVarlist as u32 > 0 {
                has_vararg = true;
            } else {
                arguments.push(parser::Argument {
                    is_ref: flags & variable::Flag::IsRef as u32 > 0,
                    variable: parser::Variable {
                        constant: flags & variable::Flag::NoWrite as u32 > 0,
                        data_type: self.parse_type(arg.r#type.unwrap())?,
                        access: VariableAccess {
                            name: Token {
                                token_type: TokenType::ID,
                                content: arg.name.clone().unwrap(),
                                range: self.builtin_empty(),
                                error: None,
                            },
                            is_write: true,
                        },
                        initial_value: None,
                        range: self.builtin_empty(),
                    },
                })
            }
        }

        let help = Project::parse_help(&func.help)?;

        Ok((returns, arguments, has_vararg, help))
    }

    pub(super) async fn load_enums(&mut self) -> anyhow::Result<()> {
        let enums = powerbuilder_proto::Enums::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/system/enums.pb")
        )))?;
        for mut en in enums.r#enum {
            let iname = (&en.name).into();
            let enum_lock = arc_mut(Enum {
                name: en.name,
                help: Project::parse_help(&en.help)?,
                values: Vec::new(),
            });

            en.value.iter_mut().for_each(|name| name.push('!'));
            for val in &en.value {
                self.builtin_enums_value_cache
                    .insert(val.into(), enum_lock.clone());
            }

            enum_lock.lock().await.values = en.value;
            self.builtin_enums.insert(iname, enum_lock);
        }

        Ok(())
    }

    pub(super) async fn load_builtin_classes(&mut self) -> anyhow::Result<()> {
        let classes = powerbuilder_proto::Classes::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/system/classes.pb")
        )))?;

        for class in classes.class {
            let iname = (&class.name).into();
            let new_class_mut = arc_mut(Class::new(parser::Class {
                scope: None,
                name: parser::DataType {
                    data_type_type: parser::DataTypeType::Complex(GroupedName::simple(class.name)),
                    range: self.builtin_empty(),
                },
                base: parser::DataType {
                    data_type_type: parser::DataTypeType::Complex(GroupedName::simple(class.base)),
                    range: self.builtin_empty(),
                },
                within: None,
                autoinstantiate: None,
            }));

            {
                let mut new_class = new_class_mut.lock().await;
                new_class.help = Project::parse_help(&class.help)?;

                for var in class.variable {
                    let iname = var.name.as_ref().unwrap().into();
                    let parsed = parser::Variable {
                        constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32 > 0,
                        data_type: self.parse_type(var.r#type.unwrap())?,
                        access: VariableAccess {
                            name: Token {
                                token_type: TokenType::ID,
                                content: var.name.unwrap(),
                                range: self.builtin_empty(),
                                error: None,
                            },
                            is_write: true,
                        },
                        initial_value: None,
                        range: self.builtin_empty(),
                    };
                    new_class.instance_variables.insert(
                        iname,
                        arc_mut(Variable {
                            data_type: parsed.data_type.data_type_type.clone(),
                            variable_type: VariableType::Instance((
                                Arc::downgrade(&new_class_mut),
                                parser::InstanceVariable {
                                    variable: parsed,
                                    access: parser::Access {
                                        read: None,
                                        write: None,
                                    },
                                },
                            )),
                        }),
                    );
                }

                for func in class.function {
                    let name = func.name.clone();
                    let iname = (&func.name).into();
                    let (returns, arguments, has_vararg, help) = self.load_proto_function(func)?;

                    let new_func = arc_mut(Function::new_declaration(
                        parser::Function {
                            returns,
                            scope_modif: None,
                            access: None,
                            name: Token {
                                token_type: TokenType::ID,
                                content: name,
                                range: self.builtin_empty(),
                                error: None,
                            },
                            arguments,
                            vararg: has_vararg.then(|| Token {
                                token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                                content: "...".into(),
                                range: self.builtin_empty(),
                                error: None,
                            }),
                            range: self.builtin_empty(),
                        },
                        help,
                    ));

                    match new_class.functions.get_mut(&iname) {
                        Some(funcs) => funcs.push(new_func),
                        None => {
                            new_class.functions.insert(iname, vec![new_func]);
                        }
                    };
                }

                for event in class.event {
                    let name = event.name.clone();
                    let (returns, arguments, has_vararg, help) = self.load_proto_function(event)?;
                    if has_vararg {
                        todo!();
                    }
                    new_class.events.insert(
                        (&name).into(),
                        arc_mut(Event::new_declaration(
                            parser::Event {
                                name: Token {
                                    token_type: TokenType::ID,
                                    content: name,
                                    range: self.builtin_empty(),
                                    error: None,
                                },
                                range: self.builtin_empty(),
                                event_type: parser::EventType::User(returns, arguments),
                            },
                            help,
                        )),
                    );
                }
            }

            self.builtin_classes.insert(iname, new_class_mut);
        }

        Ok(())
    }

    pub(super) fn load_builtin_functions(&mut self) -> anyhow::Result<()> {
        let funcs = powerbuilder_proto::Functions::decode(Bytes::from_static(include_bytes!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/system/system_functions.pb")
        )))?;

        for func in funcs.function {
            let name = func.name.clone();
            let iname = (&func.name).into();
            let (returns, arguments, has_vararg, help) = self.load_proto_function(func)?;

            let new_func = arc_mut(Function::new_declaration(
                parser::Function {
                    returns,
                    scope_modif: None,
                    access: None,
                    name: Token {
                        token_type: TokenType::ID,
                        content: name,
                        range: self.builtin_empty(),
                        error: None,
                    },
                    arguments,
                    vararg: has_vararg.then(|| Token {
                        token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                        content: "...".into(),
                        range: self.builtin_empty(),
                        error: None,
                    }),
                    range: self.builtin_empty(),
                },
                help,
            ));

            match self.builtin_functions.get_mut(&iname) {
                Some(funcs) => funcs.push(new_func),
                None => {
                    self.builtin_functions.insert(iname, vec![new_func]);
                }
            }
        }

        Ok(())
    }
}
