use std::{
    collections::{HashMap, HashSet},
    fs::{self, File},
    sync::Arc,
    time::Instant,
};

use anyhow::Context;
use encoding_rs_io::DecodeReaderBytes;
use ropey::Rope;
use tracing::{debug, debug_span, error, info};

use super::{
    node_searcher::{Node, NodeGetter},
    types::*,
};
use crate::{
    parser::{self, Parser},
    types::*,
};
use self_cell::self_cell;

#[derive(Debug)]
pub struct FileDiagnostic {
    pub messages: Vec<Diagnostic>,
    pub changed: bool,
}

#[derive(Debug)]
pub struct FileMeta {
    pub uri: Arc<Url>,
    pub content: Rope,

    pub parse_diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub meta: FileMeta,
    pub top_levels: Vec<parser::TopLevel>,
}

impl ParsedFile {
    pub fn new(uri: Arc<Url>, content: Rope) -> ParsedFile {
        let (top_levels, parse_diagnostics);
        {
            let _e = debug_span!("parse.file", %uri).entered();

            let mut parser = Parser::new_file(content.chars(), uri.clone());
            top_levels = parser.parse_tokens();
            parse_diagnostics = parser.get_syntax_errors();
            if !parse_diagnostics.is_empty() {
                info!(%uri, diagnostics=parse_diagnostics.len(), "parsing resulted in errors");
            }
        }

        ParsedFile {
            meta: FileMeta {
                uri,
                content,
                parse_diagnostics,
            },
            top_levels,
        }
    }

    pub fn new_from_filesystem(uri: Arc<Url>) -> anyhow::Result<ParsedFile> {
        let decoded_path = uri_to_path(&uri)?;

        debug!(?decoded_path, "reading file from filesystem");
        let content = Rope::from_reader(DecodeReaderBytes::new(
            fs::File::open(&decoded_path).context("Failed to open file")?,
        ))
        .context("Failed to read file")?;

        Ok(ParsedFile::new(uri, content))
    }
}

#[derive(Debug)]
pub struct BuiltFileInner<'pars> {
    pub bodies_processed: bool,

    pub classes: HashMap<IString, Arc<Class<'pars>>>,
    // Shared and Global variables
    pub variables: HashMap<IString, Arc<Variable<'pars>>>,

    // pub sql_cursors: HashMap<IString, SQLCursor>,
    // pub sql_procedures: HashMap<IString, SQLProcedure>,
    pub top_levels: Vec<TopLevel<'pars>>,
}

impl BuiltFileInner<'_> {
    pub(super) fn fill_caches(&mut self) {
        assert!(self.classes.is_empty());
        assert!(self.variables.is_empty());

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

        debug!(
            classes = self.classes.len(),
            variables = self.variables.len(),
            "filled caches"
        );
    }

    pub fn get_nodes_at<'a>(&'a self, pos: &Position) -> Option<(&'a TopLevel<'a>, Vec<Node<'a>>)> {
        let top_level = &self.top_levels.iter().find(|tl| tl.range.contains(pos))?;
        let nodes = top_level.top_level_type.get_nodes_at(pos);

        Some((top_level, nodes))
    }
}

self_cell!(
    pub struct BuiltFile {
        owner: ParsedFile,

        #[covariant]
        dependent: BuiltFileInner,
    }
    impl {Debug}
);

impl BuiltFile {
    pub fn parsed(&self) -> &ParsedFile {
        self.borrow_owner()
    }

    pub fn inner(&self) -> &BuiltFileInner<'_> {
        self.borrow_dependent()
    }
}
