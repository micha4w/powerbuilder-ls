use core::str;
use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    ffi::OsStr,
    fs,
    io::Read,
    iter,
    mem::take,
    path::PathBuf,
    str::FromStr,
    string,
    sync::Arc,
};

use anyhow::{anyhow, bail, Context};

use encoding_rs_io::DecodeReaderBytes;
use tracing::{debug, error, info, info_span, span, warn};

use crate::{
    builder::{self, Builder, BuiltFile, FileMeta, ParsedFile},
    tokenizer,
    types::*,
};

use super::{
    builtins::{Builtins, BuiltinsInner, BuiltinsParsed},
    types::*,
};

#[derive(Debug)]
pub struct Project {
    pub application: Arc<Url>,
    pub libraries: Vec<(Arc<Url>, Vec<Arc<Url>>)>,
}

impl Project {
    fn new(folder: &Url, file: &str) -> anyhow::Result<Project> {
        let path = url_join_path(folder.clone(), file);
        let _e = info_span!("project.load", %path).entered();

        let mut content = String::new();
        DecodeReaderBytes::new(fs::File::open(&uri_to_path(&path)?)?)
            .read_to_string(&mut content)?;

        let doc = roxmltree::Document::parse(content.as_str())?;

        let mut libraries = Vec::new();

        let Some(libs) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Libraries"))
        else {
            bail!("XML Format Invalid (<Libraries> not found)",);
        };

        for lib in libs.children().filter(roxmltree::Node::is_element) {
            if !lib.has_tag_name("Library") {
                warn!(tag = &content[lib.range()], "ignoring unknown tag");
                continue;
            }

            let Some(lib_path) = lib.attribute("Path") else {
                bail!(
                    "XML Format Invalid ({} missing Path attribute)",
                    &content[lib.range()],
                );
            };

            let lib_path = lib_path.replace('\\', std::path::MAIN_SEPARATOR_STR) + "/";
            libraries.push((
                Arc::new(url_join_path(folder.clone(), &lib_path)),
                Vec::new(),
            ));
        }

        let Some(entry_lib) = libs.attribute("AppEntry") else {
            bail!("XML Format Invalid (<Libraries> missing AppEntry attribute)",);
        };

        let Some(application) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Application"))
        else {
            bail!("XML Format Invalid (<Application> not found)",);
        };
        let Some(app_name) = application.attribute("Name") else {
            bail!(
                "XML Format Invalid ({} missing Name attribute)",
                &content[application.range()],
            );
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
}

impl<'sol> Solution {
    pub fn new(folder: Option<&Url>) -> anyhow::Result<Solution> {
        let _e = info_span!("solution.load", ?folder).entered();

        let mut solution = None;
        if let Some(folder) = folder {
            for file in std::fs::read_dir(uri_to_path(&folder)?)? {
                let path = file?.path();
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
                        solution = Some(path);
                    }
                }
            }
        }

        let solution_type;
        if let (Some(solution), Some(folder)) = (solution, folder) {
            let mut content = String::new();
            DecodeReaderBytes::new(fs::File::open(&solution)?).read_to_string(&mut content)?;

            let doc = roxmltree::Document::parse(content.as_str())?;
            let Some(projs) = doc
                .root_element()
                .children()
                .find(|node| node.has_tag_name("Projects"))
            else {
                bail!(
                    "XML Format Invalid (<Projects> not found in {})",
                    solution.display()
                );
            };

            let Some(default_path) = projs.attribute("Default") else {
                bail!(
                    "XML Format Invalid (<Projects> missing Default attribute in {})",
                    solution.display()
                );
            };

            let default_path = default_path.replace('\\', "/");
            let mut default_arc = None;

            info!(
                default = %default_path,
                "default project",
            );

            let mut projects = BTreeMap::new();
            for proj in projs.children().filter(roxmltree::Node::is_element) {
                if !proj.has_tag_name("Project") {
                    warn!(tag = &content[proj.range()], "unkwon tag");
                    continue;
                }

                let Some(proj_path) = proj.attribute("Path") else {
                    bail!(
                        "XML Format Invalid ({} missing Path attribute in {})",
                        &content[proj.range()],
                        solution.display()
                    );
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
                    Err(err) => {
                        error!(%err, ?proj_path, "failed to load project");
                    }
                };
            }

            let Some(default) = default_arc else {
                bail!(
                    "XML Format Invalid (Solution.Projects.Project with Path attribute = {} not found in {})",
                    default_path,
                    solution.display(),
                );
            };

            solution_type = SolutionType::PbSln { projects, default };
        } else {
            solution_type = SolutionType::OneOff(Vec::new());
        }

        let builtins = Builtins::new(BuiltinsParsed::new(), |parsed| {
            let mut inner = BuiltinsInner::new();
            inner.load_enums();
            inner.load_classes(parsed);
            inner.load_functions(parsed);
            inner
        });

        Ok(Solution {
            solution_type,
            files: HashMap::new(),
            builtins,
        })
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
                                    "sru", "srd", "srm", "srw", "srs", "srf", "sra", "srp", "srj",
                                    "srq",
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
