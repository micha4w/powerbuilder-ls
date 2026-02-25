use std::{
    collections::{HashMap, HashSet},
    fs,
    sync::Arc,
    time::Instant,
};

use encoding_rs_io::DecodeReaderBytes;
use ropey::Rope;

use super::{
    node_searcher::{Node, NodeGetter},
    types::*,
};
use crate::{
    parser::{self, Parser},
    types::*,
};

#[derive(Debug)]
pub struct FileDiagnostic {
    pub messages: Vec<Diagnostic>,
    pub changed: bool,
}

#[derive(Debug)]
pub struct FileMeta {
    pub uri: Url,
    pub content: Rope,
    pub classes: HashSet<IString>,
    pub variables: HashSet<IString>,

    pub parse_diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub meta: FileMeta,
    pub top_levels: Vec<parser::TopLevel>,
}

impl ParsedFile {
    pub fn new(uri: Url, content: Rope) -> ParsedFile {
        let top_levels;
        let parse_diagnostics;
        {
            let start = Instant::now();
            let mut parser = Parser::new_file(content.chars(), Arc::new(uri.clone()));
            top_levels = parser.parse_tokens();
            parse_diagnostics = parser.get_syntax_errors();
            eprintln!(
                "Parsed File {:?} in {:?}",
                uri.path_segments().unwrap().last(),
                start.elapsed()
            );
        }

        let mut parsed = ParsedFile {
            meta: FileMeta {
                uri,
                content,
                classes: HashSet::new(),
                variables: HashSet::new(),
                parse_diagnostics,
            },
            top_levels,
        };
        parsed.load_globals();
        parsed
    }

    fn load_globals(&mut self) {
        self.meta.classes.clear();
        self.meta.variables.clear();

        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                parser::TopLevelType::ForwardDecl(decl) => {
                    for var in &decl.variables {
                        self.meta
                            .variables
                            .insert((&var.variable.access.name.content).into());
                    }

                    for dt in &decl.classes {
                        self.meta
                            .classes
                            .insert((&dt.class.name.name.content).into());
                    }
                }
                parser::TopLevelType::ScopedVariablesDecl(vars) => {
                    for var in vars {
                        self.meta
                            .variables
                            .insert((&var.variable.access.name.content).into());
                    }
                }
                parser::TopLevelType::ScopedVariableDecl(vars) => {
                    for var in vars {
                        self.meta
                            .variables
                            .insert((&var.variable.access.name.content).into());
                    }
                }
                parser::TopLevelType::DatatypeDecl(datatype) => {
                    self.meta
                        .classes
                        .insert((&datatype.class.name.name.content).into());
                }
                parser::TopLevelType::TypeVariablesDecl(_) => {}
                parser::TopLevelType::FunctionsForwardDecl(_) => {}
                parser::TopLevelType::ExternalFunctions(_) => {}
                parser::TopLevelType::FunctionBody(_) => {}
                parser::TopLevelType::EventBody(_) => {}
                parser::TopLevelType::OnBody(_) => {}
            }
        }
    }
}

#[derive(Debug)]
pub struct BuiltFile {
    pub meta: FileMeta,

    pub bodies_processed: bool,

    pub classes: HashMap<IString, Arc<Class>>,
    // Shared and Global variables
    pub variables: HashMap<IString, Arc<Variable>>,

    // pub sql_cursors: HashMap<IString, SQLCursor>,
    // pub sql_procedures: HashMap<IString, SQLProcedure>,
    pub top_levels: Vec<TopLevel>,
}

impl BuiltFile {
    pub(super) fn fill_caches(&mut self) {
        for top_level in &self.top_levels {
            match &top_level.top_level_type {
                TopLevelType::ForwardDecl(_, vars) // TODO(forward): classes
                | TopLevelType::ScopedVariableDecl(vars)
                | TopLevelType::ScopedVariablesDecl(vars) => {
                    self.variables
                        .extend(vars.iter().map(|var| (var.iname(), var.clone())));
                }
                TopLevelType::DatatypeDecl(decl) => {
                    self.classes.insert(decl.class.iname(), decl.class.clone());
                }
                TopLevelType::TypeVariablesDecl(..) => {}
                TopLevelType::FunctionsForwardDecl(..) => {}
                TopLevelType::ExternalFunctions(..) => {}
                TopLevelType::FunctionBody(..) => {}
                TopLevelType::EventBody(..) => {}
                TopLevelType::OnBody(_) => {}
            }
        }
    }

    pub fn get_nodes_at<'a>(&'a self, pos: &Position) -> Option<(&'a TopLevel, Vec<Node<'a>>)> {
        let top_level = &self.top_levels.iter().find(|tl| tl.range.contains(pos))?;
        let nodes = top_level.top_level_type.get_nodes_at(pos);

        Some((top_level, nodes))
    }
}

#[derive(Debug)]
pub enum File {
    Parsed(ParsedFile),
    Built(BuiltFile),
}

impl File {
    pub fn new(uri: Url) -> anyhow::Result<File> {
        let decoded_path = uri_to_path(&uri)?;
        eprintln!("Reading File {:?}", decoded_path);

        let content = Rope::from_reader(DecodeReaderBytes::new(fs::File::open(&decoded_path)?))?;

        Ok(File::Parsed(ParsedFile::new(uri, content)))
    }

    pub fn new_from_content(uri: Url, content: &str) -> File {
        File::Parsed(ParsedFile::new(uri, Rope::from_str(content)))
    }

    pub fn new_from_meta(meta: FileMeta) -> File {
        File::Parsed(ParsedFile::new(meta.uri, meta.content))
    }

    pub fn meta(&self) -> &FileMeta {
        match self {
            File::Parsed(parsed) => &parsed.meta,
            File::Built(built) => &built.meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut FileMeta {
        match self {
            File::Parsed(parsed) => &mut parsed.meta,
            File::Built(built) => &mut built.meta,
        }
    }

    pub fn into_meta(self) -> FileMeta {
        match self {
            File::Parsed(parsed) => parsed.meta,
            File::Built(built) => built.meta,
        }
    }

    pub fn reparse(&mut self) {
        replace_with(self, |file| File::new_from_meta(file.into_meta()))
    }

    pub fn rebuild(&mut self) {
        let File::Built(_) = self else {
            return;
        };

        // TODO(perf): keep parsed toplevels, can probably do this when and if we change the parser references instead of clones
        self.reparse();
    }
}
