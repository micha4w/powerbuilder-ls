use std::{collections::HashMap, path::PathBuf, sync::Arc};

use anyhow::anyhow;
use prost::{bytes::Bytes, Message as _};
use tokio::sync::{Mutex, RwLock};

use super::{
    powerbuilder_proto::{self, variable},
    types::*,
    Linter,
};
use crate::{
    parser::{self, GroupedName, Parser},
    tokenizer::{self, Token, TokenType},
    types::*,
};

#[derive(Debug)]
pub struct File {
    pub classes: HashMap<IString, Arc<Mutex<Class>>>,
    // Shared and Global variables
    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,

    pub diagnostics: Vec<parser::Diagnostic>,

    pub top_levels: ProgressedTopLevels,
    pub path: PathBuf,
}

impl File {
    pub fn new(path: PathBuf) -> anyhow::Result<File> {
        let mut file = Parser::new_from_file(&path)?;

        Ok(File {
            classes: HashMap::new(),
            variables: HashMap::new(),

            top_levels: ProgressedTopLevels::None(file.parse_tokens()),
            diagnostics: file.get_syntax_errors(),

            path,
        })
    }
}

#[derive(Debug)]
pub struct Project {
    // Keep the writing of RwLock to the minimal
    pub files: HashMap<PathBuf, RwLock<File>>,

    pub builtin_enums: HashMap<IString, Arc<Mutex<Enum>>>,
    pub builtin_functions: HashMap<IString, Vec<Arc<Mutex<Function>>>>,
    pub builtin_classes: HashMap<IString, Arc<Mutex<Class>>>,
}

impl Project {
    pub async fn new(
        enums_file: PathBuf,
        classes_file: PathBuf,
        functions_file: PathBuf,
    ) -> anyhow::Result<Project> {
        let mut proj = Project {
            files: HashMap::new(),
            builtin_enums: HashMap::new(),
            builtin_functions: HashMap::new(),
            builtin_classes: HashMap::new(),
        };

        proj.load_enums(enums_file)?;
        proj.load_builtin_classes(classes_file).await?;
        proj.load_builtin_functions(functions_file)?;

        Ok(proj)
    }

    pub async fn lint_file_to(&self, file_lock: &RwLock<File>, lint_progress: LintProgress) {
        let mut current_progress = file_lock.read().await.top_levels.get_progress();
        if current_progress >= lint_progress {
            return;
        }

        let mut file = file_lock.write().await;

        while current_progress < lint_progress {
            match current_progress.next() {
                Some(progress) => {
                    Linter::new(self, MaybeMut::Mut(&mut file))
                        .lint_file()
                        .await;
                    current_progress = progress;
                }
                None => break,
            }
        }
    }

    pub async fn add_file(
        &mut self,
        path: &PathBuf,
        lint_progress: LintProgress,
    ) -> anyhow::Result<()> {
        if !self.files.contains_key(path) {
            self.files
                .insert(path.clone(), RwLock::new(File::new(path.clone())?));
        }

        let _file_lock = self.files.get(path).unwrap();

        self.lint_file_to(&_file_lock, lint_progress).await;

        // for diagnostic in &_file_lock.read().await.diagnostics {
        //     println!("{} - {}", diagnostic.range, diagnostic.message)
        // }

        Ok(())
    }

    fn parse_type(mut name: String) -> anyhow::Result<parser::DataType> {
        name += "\n";

        let mut parser = Parser::new_from_string(&(name + "\n"))?;
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

    fn load_proto_function(
        func: powerbuilder_proto::Function,
    ) -> anyhow::Result<(Option<parser::DataType>, Vec<parser::Argument>, bool)> {
        let mut has_vararg = false;
        let mut returns = None;
        let mut arguments = Vec::new();

        if let Some(ret) = func.ret {
            if ret != "\u{1}void" {
                returns = Some(Project::parse_type(ret)?);
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
                        data_type: Project::parse_type(arg.r#type.unwrap())?,
                        name: Token {
                            token_type: TokenType::ID,
                            content: arg.name.clone().unwrap(),
                            range: Range::default(),
                            error: None,
                        },
                        initial_value: None,
                        range: Default::default(),
                    },
                })
            }
        }

        Ok((returns, arguments, has_vararg))
    }

    pub fn load_enums(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let enums = powerbuilder_proto::Enums::decode(buf)?;

        self.builtin_enums
            .extend(enums.r#enum.into_iter().map(|en| {
                (
                    (&en.name).into(),
                    Mutex::new(Enum {
                        name: en.name,
                        help: en.help,
                        values: en.value,
                    })
                    .into(),
                )
            }));

        Ok(())
    }

    pub async fn load_builtin_classes(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let classes = powerbuilder_proto::Classes::decode(buf)?;

        // TODO make not stupid
        let mut skipped = std::collections::VecDeque::<powerbuilder_proto::Class>::new();
        skipped.extend(classes.class.iter().cloned());

        loop {
            let class = match skipped.pop_front() {
                Some(class) => class,
                None => break,
            };

            match self
                .builtin_classes
                .get(&(&class.base).into())
                .cloned()
                .map(Complex::Class)
                .or_else(|| {
                    self.builtin_enums
                        .get(&(&class.base).into())
                        .cloned()
                        .map(Complex::Enum)
                }) {
                Some(_) => {
                    let iname = (&class.name).into();
                    let new_class_mut = Arc::new(Mutex::new(Class::new(
                        class.name,
                        GroupedName::new(None, class.base),
                        None,
                        true,
                    )));
                    {
                        let mut new_class = new_class_mut.lock().await;
                        new_class.help = class.help;

                        for var in class.variable {
                            let iname = var.name.as_ref().unwrap().into();
                            let parsed = parser::Variable {
                                constant: var.flags.unwrap_or(0) & variable::Flag::NoWrite as u32
                                    > 0,
                                data_type: Project::parse_type(var.r#type.unwrap())?,
                                name: Token {
                                    token_type: TokenType::ID,
                                    content: var.name.unwrap(),
                                    range: Range::default(),
                                    error: None,
                                },
                                initial_value: None,
                                range: Default::default(),
                            };
                            new_class.instance_variables.insert(
                                iname,
                                Mutex::new(Variable {
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
                                })
                                .into(),
                            );
                        }

                        for func in class.function {
                            let help = func.help.clone();
                            let name = func.name.clone();
                            let iname = (&func.name).into();
                            let (returns, arguments, has_vararg) = Self::load_proto_function(func)?;

                            let new_func = Mutex::new(Function::new(
                                parser::Function {
                                    returns,
                                    scope_modif: None,
                                    access: None,
                                    name: Token {
                                        token_type: TokenType::ID,
                                        content: name,
                                        range: Range::default(),
                                        error: None,
                                    },
                                    arguments,
                                    vararg: has_vararg.then(|| Token {
                                        token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                                        content: "...".into(),
                                        range: Range::default(),
                                        error: None,
                                    }),
                                    range: Default::default(),
                                },
                                None,
                                None,
                                help,
                            ))
                            .into();

                            match new_class.functions.get_mut(&iname) {
                                Some(funcs) => funcs.push(new_func),
                                None => {
                                    new_class.functions.insert(iname, vec![new_func]);
                                }
                            };
                        }

                        for event in class.event {
                            let name = event.name.clone();
                            let (returns, arguments, has_vararg) =
                                Self::load_proto_function(event)?;
                            if has_vararg {
                                todo!();
                            }
                            new_class.events.insert(
                                (&name).into(),
                                Mutex::new(Event::new(
                                    parser::Event {
                                        name: Token {
                                            token_type: TokenType::ID,
                                            content: name,
                                            range: Range::default(),
                                            error: None,
                                        },
                                        range: Default::default(),
                                        event_type: parser::EventType::User(returns, arguments),
                                    },
                                    None,
                                    None,
                                ))
                                .into(),
                            );
                        }
                    }

                    self.builtin_classes.insert(iname, new_class_mut);
                }
                None => skipped.push_back(class),
            }
        }

        Ok(())
    }

    pub fn load_builtin_functions(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let buf = Bytes::from_iter(std::fs::read(path)?.iter().cloned());
        let funcs = powerbuilder_proto::Functions::decode(buf)?;

        for func in funcs.function {
            let help = func.help.clone();
            let name = func.name.clone();
            let iname = (&func.name).into();
            let (returns, arguments, has_vararg) = Self::load_proto_function(func)?;

            let new_func = Mutex::new(Function::new(
                parser::Function {
                    returns,
                    scope_modif: None,
                    access: None,
                    name: Token {
                        token_type: TokenType::ID,
                        content: name,
                        range: Range::default(),
                        error: None,
                    },
                    arguments,
                    vararg: has_vararg.then(|| Token {
                        token_type: TokenType::Symbol(tokenizer::Symbol::DOTDOTDOT),
                        content: "...".into(),
                        range: Range::default(),
                        error: None,
                    }),
                    range: Default::default(),
                },
                None,
                None,
                help,
            ))
            .into();

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
