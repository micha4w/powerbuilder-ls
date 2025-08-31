use core::str;
use std::{collections::HashMap, ffi::OsStr, fs, io::Read, path::PathBuf, str::FromStr, sync::Arc};

use anyhow::anyhow;

use encoding_rs_io::DecodeReaderBytes;
use ropey::Rope;
use tokio::time::Instant;

use super::{types::*, Linter};
use crate::{
    parser::{self, Parser},
    types::*,
};

fn uri_to_path(uri: &Url) -> anyhow::Result<PathBuf> {
    uri.to_file_path()
        .map_err(|_| anyhow!("Failed to convert Url to PathBuf {:?}", uri))
}

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
        let decoded_path = uri_to_path(&uri)?;
        eprintln!("Reading File {:?}", decoded_path);

        let content = Rope::from_reader(DecodeReaderBytes::new(fs::File::open(&decoded_path)?))?;

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

            builtin_url: Url::from_file_path("/builtins.pb").unwrap(),
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

        while current_progress < lint_progress {
            let start = Instant::now();
            match current_progress.next() {
                Some(progress) => {
                    Linter::new(self, file_lock, true).lint_file().await;
                    current_progress = progress;
                    eprintln!(
                        "Linted File {:?} to stage {:?} in {:?}",
                        file_lock.read().await.uri.path_segments().unwrap().last(),
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

    async fn add_directory(&mut self, folder: &Url) -> anyhow::Result<()> {
        for file in std::fs::read_dir(&uri_to_path(folder)?)? {
            let path = file?.path();

            let extensions =
                ["sra", "srf", "srm", "srs", "sru", "srw"].map(|f| Some(OsStr::new(f)));
            if extensions.contains(&path.extension()) {
                self.add_file(
                    &folder.join(&path.file_name().unwrap().to_string_lossy())?,
                    LintProgress::OnlyTypes,
                )
                .await?;
            }
        }
        Ok(())
    }

    async fn add_project(&mut self, folder: &Url, project: &str) -> anyhow::Result<()> {
        let mut content = String::new();

        eprintln!("Adding project at: {}", folder.join(project)?);
        DecodeReaderBytes::new(fs::File::open(&uri_to_path(&folder.join(project)?)?)?)
            .read_to_string(&mut content)?;

        let mut folder = folder.clone();
        if let Some(parent) = PathBuf::from_str(project)?.parent() {
            if let Some(dir) = parent.file_name() {
                folder = folder.join(
                    &(dir
                        .to_str()
                        .ok_or_else(|| {
                            anyhow!("Failed to turn path into str {}", parent.display())
                        })?
                        .to_owned()
                        + "/"),
                )?;
            }
        }

        let doc = roxmltree::Document::parse(content.as_str())?;
        if let Some(libs) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Libraries"))
        {
            for lib in libs.children() {
                if lib.has_tag_name("Library") {
                    if let Some(lib_path) = lib.attribute("Path") {
                        let lib_path = lib_path.replace('\\', std::path::MAIN_SEPARATOR_STR) + "/";
                        self.add_directory(&folder.join(&lib_path)?).await?;
                    }
                }
            }
        };

        Ok(())
    }

    pub async fn add_default_project(&mut self, folder: &Url) -> anyhow::Result<()> {
        let mut solution = None;
        for file in std::fs::read_dir(uri_to_path(&folder)?)? {
            let path = file?.path();
            if path.extension().is_some_and(|ext| ext == "pbsln") {
                solution = Some(path);
            }
        }

        let Some(solution) = solution else {
            return Err(anyhow!("No *.pbsln found in {}", folder));
        };

        eprintln!("Found Solution file: {}", solution.display());

        let mut folder = folder.clone();
        folder.set_path(&(folder.path().to_owned() + "/"));

        let mut content = String::new();
        DecodeReaderBytes::new(fs::File::open(&solution)?).read_to_string(&mut content)?;
        let doc = roxmltree::Document::parse(content.as_str())?;
        if let Some(projects) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Projects"))
        {
            if let Some(default) = projects.attribute("Default") {
                return self
                    .add_project(
                        &folder,
                        default
                            .replace('\\', std::path::MAIN_SEPARATOR_STR)
                            .as_str(),
                    )
                    .await;
            }
        };

        Err(anyhow!(
            "XML Format Invalid (Solution.Projects[Default] not found in {})",
            solution.display()
        ))
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
