use std::{collections::HashMap, fs, sync::Arc};

use encoding_rs_io::DecodeReaderBytes;
use ropey::Rope;
use tokio::{
    sync::{Mutex, RwLock},
    time::Instant,
};

use super::{types::*, Linter};
use crate::{
    parser::{self, Parser},
    types::*,
};

#[derive(Debug)]
pub struct File {
    pub classes: HashMap<IString, Arc<Mutex<Class>>>,
    // Shared and Global variables
    pub variables: HashMap<IString, Arc<Mutex<Variable>>>,

    pub sql_cursors: HashMap<IString, Arc<Mutex<SQLCursor>>>,
    pub sql_procedures: HashMap<IString, Arc<Mutex<SQLProcedure>>>,

    pub diagnostics: Vec<parser::Diagnostic>,

    pub top_levels: ProgressedTopLevels,
    pub uri: Url,
    pub content: Rope,

    pub content_changed: bool,
    pub diagnostics_changed: bool,
}

impl File {
    pub fn new(uri: Url) -> anyhow::Result<File> {
        let path = uri.path();
        let decoded_path = urlencoding::decode(&path)?;
        eprintln!("Reading File {:?}", decoded_path);

        let content = Rope::from_reader(DecodeReaderBytes::new(fs::File::open(&*decoded_path)?))?;

        let mut file = File {
            classes: HashMap::new(),
            variables: HashMap::new(),

            sql_cursors: HashMap::new(),
            sql_procedures: HashMap::new(),

            top_levels: ProgressedTopLevels::None(Vec::new()),
            diagnostics: Vec::new(),

            uri,
            content,
            content_changed: true,
            diagnostics_changed: false,
        };

        file.reparse();
        Ok(file)
    }

    pub fn reparse(&mut self) {
        if !self.content_changed {
            return;
        }

        let start = Instant::now();
        let mut parser = Parser::new_file(self.content.chars(), self.uri.clone());
        let top_levels = parser.parse_tokens();

        eprintln!(
            "Parsed File {:?} in {:?}",
            self.uri.path_segments().unwrap().last(),
            start.elapsed()
        );

        self.classes.clear();
        self.variables.clear();
        self.sql_cursors.clear();
        self.sql_procedures.clear();

        self.top_levels = ProgressedTopLevels::None(top_levels);
        self.diagnostics = parser.get_syntax_errors();

        self.content_changed = false;
        self.diagnostics_changed = true;
    }
}

#[derive(Debug)]
pub struct Project {
    // Keep the writing of RwLock to the minimal
    pub files: HashMap<Url, RwLock<File>>,

    pub builtin_enums: HashMap<IString, Arc<Mutex<Enum>>>,
    pub builtin_enums_value_cache: HashMap<IString, Arc<Mutex<Enum>>>,
    pub builtin_functions: HashMap<IString, Vec<Arc<Mutex<Function>>>>,
    pub builtin_classes: HashMap<IString, Arc<Mutex<Class>>>,

    pub builtin_url: Url,
}

impl Project {
    pub async fn new() -> anyhow::Result<Project> {
        let mut proj = Project {
            files: HashMap::new(),

            builtin_enums: HashMap::new(),
            builtin_enums_value_cache: HashMap::new(),
            builtin_functions: HashMap::new(),
            builtin_classes: HashMap::new(),

            builtin_url: Url::from_file_path("/builtins.pb").unwrap()
        };

        proj.load_enums().await?;
        proj.load_builtin_classes().await?;
        proj.load_builtin_functions()?;

        Ok(proj)
    }

    pub async fn lint_file_to(&self, file_lock: &RwLock<File>, lint_progress: LintProgress) {
        // eprintln!("Linting {}", Backtrace::capture());

        file_lock.write().await.reparse();

        let mut current_progress = file_lock.read().await.top_levels.get_progress();
        if current_progress >= lint_progress {
            return;
        }

        let mut file = file_lock.write().await;

        while current_progress < lint_progress {
            let start = Instant::now();
            match current_progress.next() {
                Some(progress) => {
                    Linter::new(self, MaybeMut::Mut(&mut file))
                        .lint_file()
                        .await;
                    current_progress = progress;
                    eprintln!(
                        "Linted File {:?} to stage {:?} in {:?}",
                        file.uri.path_segments().unwrap().last(),
                        current_progress,
                        start.elapsed()
                    );
                }
                None => break,
            }
        }
    }

    pub async fn add_file(
        &mut self,
        uri: &Url,
        lint_progress: LintProgress,
    ) -> anyhow::Result<&RwLock<File>> {
        if !self.files.contains_key(uri) {
            self.files
                .insert(uri.clone(), RwLock::new(File::new(uri.clone())?));
        }

        let _file_lock = self.files.get(uri).unwrap();

        self.lint_file_to(&_file_lock, lint_progress).await;

        // for diagnostic in &_file_lock.read().await.diagnostics {
        //     println!("{} - {}", diagnostic.range, diagnostic.message)
        // }

        Ok(_file_lock)
    }

    pub fn get_node_at<'a>(
        &'a self,
        file: &'a File,
        pos: &Position,
    ) -> Option<(&'a TopLevelType<LintProgressComplete>, Vec<Node<'a>>)> {
        let top_levels = match &file.top_levels {
            ProgressedTopLevels::Complete(top_levels) => top_levels,
            _ => return None,
        };

        let Some((_, top_level_type)) = top_levels.iter().find(|(range, _)| range.contains(pos))
        else {
            return None;
        };

        let nodes = top_level_type.get_nodes_at(pos);
        Some((top_level_type, nodes))
    }
}
