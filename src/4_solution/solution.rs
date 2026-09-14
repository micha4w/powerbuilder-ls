use core::str;
use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fs,
    io::Read,
    iter,
    sync::Arc,
};

use anyhow::Context;

use encoding_rs_io::DecodeReaderBytes;
use tracing::{debug, error, info, info_span, warn};

use crate::{
    builder::{self, Builder, BuiltFile, FileMeta, ParsedFile},
    tokenizer,
    types::*,
};

use super::{
    builtins::{Builtins, BuiltinsInner, BuiltinsParsed},
    types::*,
};
fn range_err(message: String, range: Range) -> Vec<Diagnostic> {
    vec![Diagnostic {
        range,
        severity: Severity::Error,
        message,
    }]
}

fn pos_err(message: String, uri: Arc<Url>, pos: Position) -> Vec<Diagnostic> {
    range_err(message, Range::new_point(pos, uri))
}

fn xml_pos(pos: roxmltree::TextPos) -> Position {
    Position {
        line: pos.row - 1,
        column: pos.col - 1,
    }
}

fn xml_range(doc: &roxmltree::Document<'_>, uri: Arc<Url>, range: std::ops::Range<usize>) -> Range {
    let start = doc.text_pos_at(range.start);
    let end = doc.text_pos_at(range.end);
    Range::new(xml_pos(start), xml_pos(end), uri)
}

fn read_xml<'a>(
    content: &'a mut String,
    uri: &Arc<Url>,
) -> Result<roxmltree::Document<'a>, Vec<Diagnostic>> {
    let path = uri_to_path(&uri).map_err(|err| {
        pos_err(
            format!("Cannot convert URI to path: {}", err),
            uri.clone(),
            Position::default(),
        )
    })?;
    let file = fs::File::open(&path).map_err(|err| {
        pos_err(
            format!("Failed to open file: {}", err),
            uri.clone(),
            Position::default(),
        )
    })?;
    DecodeReaderBytes::new(file)
        .read_to_string(content)
        .map_err(|err| {
            pos_err(
                format!("Failed to read file: {}", err),
                uri.clone(),
                Position::default(),
            )
        })?;
    let doc = roxmltree::Document::parse(content.as_str()).map_err(|err| {
        pos_err(
            format!("Failed to parse file: {}", err),
            uri.clone(),
            xml_pos(err.pos()),
        )
    })?;

    Ok(doc)
}

#[derive(Debug)]
pub struct Project {
    pub application: Arc<Url>,
    pub libraries: Vec<(Arc<Url>, Vec<Arc<Url>>)>,
}

impl Project {
    fn new(folder: &Url, file: &str) -> Result<Project, Vec<Diagnostic>> {
        let path = url_join_path(folder.clone(), file);
        let _e = info_span!("project.load", %path).entered();
        let uri = Arc::new(path);

        let mut content = String::new();
        let doc = read_xml(&mut content, &uri).map_err(|errs| {
            error!(%file, "failed to read project");
            errs
        })?;

        let mut libraries = Vec::new();

        let Some(libs) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Libraries"))
        else {
            return Err(range_err(
                "XML Format Invalid (<Libraries> not found)".to_string(),
                xml_range(&doc, uri, doc.root_element().range()),
            ));
        };

        // TODO(diagnostic): ...
        let mut _errs = Vec::new();
        for lib in libs.children().filter(roxmltree::Node::is_element) {
            if !lib.has_tag_name("Library") {
                warn!(tag = &doc.input_text()[lib.range()], "unkwon tag");
                // errs.push(Diagnostic {
                //     severity: Severity::Warning,
                //     message: format!("Ignoring unknown tag {}", &doc.input_text()[lib.range()],),
                //     range: xml_range(&doc, uri.clone(), lib.range()),
                // });
                continue;
            }

            let Some(lib_path) = lib.attribute("Path") else {
                return Err(range_err(
                    "XML Format Invalid (Path attribute missing)".to_string(),
                    xml_range(&doc, uri, lib.range()),
                ));
            };

            let lib_path = lib_path.replace('\\', std::path::MAIN_SEPARATOR_STR) + "/";
            libraries.push((
                Arc::new(url_join_path(folder.clone(), &lib_path)),
                Vec::new(),
            ));
        }
        if !_errs.is_empty() {
            return Err(_errs);
        }

        let Some(entry_lib) = libs.attribute("AppEntry") else {
            return Err(range_err(
                "XML Format Invalid ('AppEntry' attribute missing)".to_string(),
                xml_range(&doc, uri, libs.range()),
            ));
        };

        let Some(application) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Application"))
        else {
            return Err(range_err(
                "XML Format Invalid (<Application> not found)".to_string(),
                xml_range(&doc, uri, doc.root_element().range()),
            ));
        };
        let Some(app_name) = application.attribute("Name") else {
            return Err(range_err(
                "XML Format Invalid ('Name' attribute missing)".to_string(),
                xml_range(&doc, uri, application.range()),
            ));
        };

        let app_uri = url_join_path(
            folder.clone(),
            &(entry_lib.to_string() + "/" + app_name + ".sra"),
        );

        info!(
            %app_uri,
            "application file",
        );

        Ok(Project {
            application: Arc::new(app_uri),
            libraries,
        })
    }
}

#[derive(Debug)]
pub enum SolutionType {
    PbSln {
        projects: BTreeMap<Arc<Url>, Project>,
        /// Always exists inside projects
        default: Arc<Url>,
    },
    FailedPbSln(Vec<Diagnostic>),
    OneOff(Vec<Arc<Url>>),
}

#[derive(Debug)]
pub struct Solution {
    pub solution_type: SolutionType,

    pub files: HashMap<Arc<Url>, BuiltFile>,
    // TODO(globals): ...
    // pub global_functions: HashMap<IString, Url>,

    // TODO(perf): cache classes and variables by name
    pub builtins: Builtins,
    pub root: Option<Url>,
}

impl<'sol> Solution {
    pub fn new(root: Option<Url>) -> Solution {
        let solution_type = Self::parse_solution(root.clone());

        let builtins = Builtins::new(BuiltinsParsed::new(), |parsed| {
            let mut inner = BuiltinsInner::new();
            inner.load_enums();
            inner.load_classes(parsed);
            inner.load_functions(parsed);
            inner
        });

        Solution {
            solution_type,
            files: HashMap::new(),
            builtins,
            root,
        }
    }

    pub fn parse_solution(root: Option<Url>) -> SolutionType {
        let _e = info_span!("solution.load", ?root).entered();

        let mut solution = None;
        let res = || -> anyhow::Result<_> {
            if let Some(folder) = &root {
                for file in std::fs::read_dir(uri_to_path(&folder)?)? {
                    let file = file?;
                    let path = file.path();
                    if path.extension().is_some_and(|ext| ext == "pbsln") {
                        if solution.is_some() {
                            warn!(
                                ignored = ?path,
                                "found another *.pbsln",
                            );
                        } else {
                            info!(
                                solution = ?path,
                                "found solution",
                            );
                            solution = Some(file.file_name());
                        }
                    }
                }
            }
            Ok(())
        }();
        if let Err(err) = res {
            error!(%err, "failed to look for pbsln");
        }

        if let (Some(solution), Some(folder)) = (solution, &root) {
            let mut content = String::new();
            let uri = Arc::new(url_join_path(folder.clone(), &solution.to_string_lossy()));

            let doc = match read_xml(&mut content, &uri).map_err(|errs| {
                error!(?solution, "failed to read solution");
                errs
            }) {
                Ok(d) => d,
                Err(err) => return SolutionType::FailedPbSln(err),
            };

            let Some(projs) = doc
                .root_element()
                .children()
                .find(|node| node.has_tag_name("Projects"))
            else {
                return SolutionType::FailedPbSln(range_err(
                    "XML Format Invalid (<Projects> not found)".to_string(),
                    xml_range(&doc, uri, doc.root_element().range()),
                ));
            };

            let Some(default_path) = projs.attribute("Default") else {
                return SolutionType::FailedPbSln(range_err(
                    "XML Format Invalid ('Default' attribute missing)".to_string(),
                    xml_range(&doc, uri, projs.range()),
                ));
            };

            let default_path = default_path.replace('\\', "/");
            let mut default_arc = None;

            info!(
                default = %default_path,
                "default project",
            );

            let mut errs = Vec::new();
            let mut projects = BTreeMap::new();
            for proj in projs.children().filter(roxmltree::Node::is_element) {
                if !proj.has_tag_name("Project") {
                    warn!(tag = &doc.input_text()[proj.range()], "unkwon tag");
                    // errs.push(Diagnostic {
                    //     severity: Severity::Warning,
                    //     message: format!(
                    //         "Ignoring unknown tag {}",
                    //         &doc.input_text()[proj.range()],
                    //     ),
                    //     range: xml_range(&doc, uri.clone(), proj.range()),
                    // });
                    continue;
                }

                let Some(proj_path) = proj.attribute("Path") else {
                    return SolutionType::FailedPbSln(range_err(
                        "XML Format Invalid ('Path' attribute missing)".to_string(),
                        xml_range(&doc, uri, proj.range()),
                    ));
                };

                let proj_path = proj_path.replace('\\', "/");

                match Project::new(&folder, &proj_path) {
                    Ok(project) => {
                        let uri = Arc::new(url_join_path(folder.clone(), &proj_path));

                        if proj_path.eq_ignore_ascii_case(&default_path) {
                            info!("default project found in list");
                            default_arc = Some(uri.clone());
                        }

                        projects.insert(uri, project);
                    }
                    Err(mut err) => {
                        errs.append(&mut err);
                        errs.append(&mut range_err(
                            "Project failed to load".to_string(),
                            xml_range(&doc, uri.clone(), proj.range()),
                        ));
                    }
                };
            }
            if !errs.is_empty() {
                return SolutionType::FailedPbSln(errs);
            }

            let Some(default) = default_arc else {
                return SolutionType::FailedPbSln(range_err(
                    format!(
                        "XML Format Invalid (No Project with Path = {})",
                        default_path
                    ),
                    xml_range(&doc, uri, projs.range()),
                ));
            };

            SolutionType::PbSln { projects, default }
        } else {
            SolutionType::OneOff(Vec::new())
        }
    }

    fn reload_file(
        uri: Arc<Url>,
        pending_changes: &mut HashMap<Arc<Url>, impl FnOnce(&mut FileMeta)>,
        old_files: &mut HashMap<Arc<Url>, BuiltFile>,
        new_files: &mut HashMap<Arc<Url>, BuiltFile>,
    ) -> anyhow::Result<()> {
        debug!(%uri, "found file with pb extension");

        let file = if let Some(mut old_file) = old_files.remove(&uri) {
            debug!("file already loaded, reusing");

            // TODO(proj): currently assuming no change happened except if it was opened in editor (so changes by scripts are ignored), do hashing or timesamping?
            if let Some(changes) = pending_changes.remove(&uri) {
                let mut meta = old_file.into_owner().meta;
                changes(&mut meta);

                old_file =
                    Builder::new().build_file_shallow(ParsedFile::new(meta.uri, meta.content));
            }

            old_file
        } else {
            let parse = ParsedFile::new_from_filesystem(uri.clone())
                .with_context(|| format!("Failed to parse file: {}", uri))?;
            Builder::new().build_file_shallow(parse)
        };

        new_files.insert(uri.clone(), file);
        Ok(())
    }

    pub fn rebuilt(
        mut old: Solution,
        mut pending_changes: HashMap<Arc<Url>, impl FnOnce(&mut FileMeta)>,
    ) -> (Solution, Vec<anyhow::Error>) {
        let _e = info_span!("solution.rebuild").entered();

        let mut new = Solution {
            files: HashMap::new(),
            ..old
        };

        let mut errs = Vec::new();

        // TODO(proj): also reread pbsln and pbproj files
        match &mut new.solution_type {
            SolutionType::PbSln { projects, default } => {
                let proj = projects.get_mut(default).expect("checked in new()");
                for (lib, lib_files) in &mut proj.libraries {
                    lib_files.clear();

                    let res = || -> anyhow::Result<()> {
                        let lib_path = uri_to_path(&lib)?;
                        for file in std::fs::read_dir(&lib_path)? {
                            let path = file?.path();
                            if path.extension().is_some_and(|ext| {
                                // TODO: add all extensions
                                vec![
                                    "sru", "srm", "srw", "srs", "srf", "sra", "srp", "srj", "srq",
                                ]
                                .contains(&&*ext.to_string_lossy())
                            }) {
                                let mut uri = (**lib).clone();
                                uri.set_path(&path.to_string_lossy());
                                let uri = Arc::new(uri);

                                Self::reload_file(
                                    uri.clone(),
                                    &mut pending_changes,
                                    &mut old.files,
                                    &mut new.files,
                                )?;
                                lib_files.push(uri);
                            }
                        }
                        Ok(())
                    }();

                    if let Err(err) = res {
                        errs.push(err);
                    };
                }
            }
            SolutionType::OneOff(uris) => {
                for uri in uris {
                    let res = Self::reload_file(
                        uri.clone(),
                        &mut pending_changes,
                        &mut old.files,
                        &mut new.files,
                    );
                    if let Err(err) = res {
                        errs.push(err);
                    }
                }
            }
            SolutionType::FailedPbSln(_) => {}
        };

        (new, errs)
    }

    pub fn open_file(&mut self, uri: &Arc<Url>) -> anyhow::Result<()> {
        let file = match self.files.entry(uri.clone()) {
            Entry::Occupied(file) => file.into_mut(),
            Entry::Vacant(entry) => {
                let SolutionType::OneOff(uris) = &mut self.solution_type else {
                    info!(%uri, "file not part of pbsln");
                    return Ok(());
                };

                uris.push(uri.clone());

                let parse = ParsedFile::new_from_filesystem(uri.clone())
                    .with_context(|| format!("Failed to parse file: {}", uri))?;
                let file = Builder::new().build_file_shallow(parse);
                entry.insert(file)
            }
        };

        Builder::new().build_file_bodies(file);
        Ok(())
    }

    pub fn builtins(&self) -> &BuiltinsInner<'_> {
        self.builtins.borrow_dependent()
    }

    pub fn application(&self) -> Option<&BuiltFile> {
        match &self.solution_type {
            SolutionType::PbSln { projects, default } => {
                let proj = projects.get(default)?;
                self.files.get(&proj.application)
            }
            SolutionType::OneOff(_) => None,
            SolutionType::FailedPbSln(_) => None,
        }
    }

    pub fn files_iter(&self) -> impl Iterator<Item = &BuiltFile> {
        let uris: Box<dyn Iterator<Item = _>> = match &self.solution_type {
            SolutionType::PbSln { projects, default } => Box::new(
                projects
                    .get(default)
                    .into_iter()
                    .flat_map(|proj| proj.libraries.iter())
                    .flat_map(|(_, urls)| urls.iter()),
            ),
            SolutionType::OneOff(urls) => Box::new(urls.iter()),
            SolutionType::FailedPbSln(_) => Box::new(iter::empty()),
        };

        uris.filter_map(|url| self.files.get(url))
    }

    pub fn get_file_for_class(
        &self,
        class: &IString,
    ) -> Option<(&BuiltFile, &Arc<builder::Class<'_>>)> {
        for file in self.files_iter() {
            if let Some(class) = file.inner().classes.get(class) {
                return Some((file, class));
            }
        }

        None
    }

    pub fn builtin_class(&self, name: &'static str) -> ClassRef<'_> {
        ClassRef::builtin(
            self.builtins
                .borrow_dependent()
                .classes
                .get(&(name.into()))
                .expect(&format!("No builtin class with name {}", name)),
        )
    }

    /// Returns Found::No if one of classes in the chain could not be found
    pub fn inherits_from(&self, child: &Complex<'_>, base: &Complex<'_>) -> Found<bool> {
        if child == base {
            return Found::Yes(true);
        }

        let Complex::Class(r#ref) = child else {
            return Found::Yes(false);
        };

        match self.find_class(r#ref.file, &(&r#ref.class.base()).into()) {
            Found::Yes(parent) => self.inherits_from(&parent, base),
            Found::No => Found::No,
        }
    }

    pub fn get_parent(&'sol self, class_ref: ClassRef<'sol>) -> Option<ClassRef<'sol>> {
        if let Some(within) = class_ref.class.within() {
            if let Found::Yes(Complex::Class(class)) =
                self.find_class(class_ref.file, &(&within).into())
            {
                return Some(class);
            }
        } else {
            let window = self.builtin_class("windowobject");
            if let Found::Yes(true) =
                self.inherits_from(&Complex::Class(class_ref), &Complex::Class(window))
            {
                return Some(window);
            }
        }

        None
    }

    pub fn get_access_for(
        &self,
        accessor: ClassRef<'_>,
        accessed: ClassRef<'_>,
    ) -> tokenizer::AccessType {
        if Arc::ptr_eq(accessor.class, accessed.class) {
            tokenizer::AccessType::PRIVATE
        } else if self
            .inherits_from(&Complex::Class(accessor), &Complex::Class(accessed))
            .unwrap_or(false)
        {
            tokenizer::AccessType::PROTECTED
        } else {
            tokenizer::AccessType::PUBLIC
        }
    }

    pub fn find_class(
        &'sol self,
        current_file: Option<&'sol BuiltFile>,
        id: &Grouped<IString>,
    ) -> Found<Complex<'sol>> {
        if let Some(file) = current_file {
            if let Some(class) = file.inner().classes.get(&id.name) {
                if class.within().map(|w| w.name.into()) == id.group {
                    return Found::Yes(Complex::Class(ClassRef::new(file, class)));
                }
            }
        }

        if let Some((file, class)) = self.get_file_for_class(id.group.as_ref().unwrap_or(&id.name))
        {
            return Found::Yes(Complex::Class(ClassRef::new(file, class)));
        }

        if let Some(en) = self.builtins.borrow_dependent().enums.get(&id.name) {
            return Found::Yes(Complex::Enum(en));
        }

        if let Some(class) = self.builtins.borrow_dependent().classes.get(&id.name) {
            return Found::Yes(Complex::Class(ClassRef::builtin(class)));
        }

        Found::No
    }
}
