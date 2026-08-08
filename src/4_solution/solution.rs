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
        eprintln!("Adding project at: {}", path);

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

        for lib in libs.children() {
            if !lib.has_tag_name("Library") {
                eprintln!("Warning: Ignoring unknown tag {}", &content[lib.range()],);
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

        Ok(Project {
            application: Arc::new(app_uri),
            libraries,
        })
    }
}

#[derive(Debug)]
pub struct Solution {
    pub projects: BTreeMap<Arc<Url>, Project>,
    pub default: Arc<Url>,

    pub files: HashMap<Arc<Url>, BuiltFile>,
    // TODO(globals): ...
    // pub global_functions: HashMap<IString, Url>,

    // TODO(perf): cache classes and variables by name
    pub builtins: Builtins,
}

impl<'sol> Solution {
    pub fn new(folder: &Url) -> anyhow::Result<Solution> {
        let mut solution = None;
        for file in std::fs::read_dir(uri_to_path(&folder)?)? {
            let path = file?.path();
            if path.extension().is_some_and(|ext| ext == "pbsln") {
                if solution.is_some() {
                    eprintln!(
                        "Warning: Found multiple *.pbsln in {} (ignoring {})",
                        folder,
                        path.display()
                    );
                } else {
                    eprintln!("Found Solution file: {}", path.display());
                    solution = Some(path);
                }
            }
        }
        let Some(solution) = solution else {
            bail!("No *.pbsln found in {}", folder);
        };

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

        let mut projects = BTreeMap::new();
        let default_path = default_path.replace('\\', "/");
        let mut default_arc = None;

        eprintln!("Want default path {}", default_path);

        for proj in projs.children() {
            if !proj.has_tag_name("Project") {
                eprintln!(
                    "Warning: Ignoring unknown tag {} in {}",
                    &content[proj.range()],
                    solution.display()
                );
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
            eprintln!("Found project path {}", proj_path);

            match Project::new(&folder, &proj_path) {
                Ok(project) => {
                    let uri = Arc::new(url_join_path(folder.clone(), &proj_path));

                    if proj_path.eq_ignore_ascii_case(&default_path) {
                        eprintln!("Default project found");
                        default_arc = Some(uri.clone());
                    }

                    projects.insert(uri, project);
                }
                Err(e) => {
                    eprintln!("Warning: Could not load project at {}: {}", proj_path, e);
                }
            };
        }

        let Some(default) = default_arc else {
            bail!(
                "XML Format Invalid (Solution.Projects.Project with Path attribute = {} not found in {})",
                default_path,
                solution.display()
            );
        };

        let builtins = Builtins::new(BuiltinsParsed::new(), |parsed| {
            let mut inner = BuiltinsInner::new();
            inner.load_enums();
            inner.load_classes(parsed);
            inner.load_functions(parsed);
            inner
        });

        Ok(Solution {
            projects,
            default,
            files: HashMap::new(),
            builtins,
        })
    }

    pub fn rebuilt(
        mut old: Solution,
        mut pending_changes: HashMap<Arc<Url>, impl FnOnce(&mut FileMeta)>,
    ) -> (Solution, Vec<anyhow::Error>) {
        let mut new = Solution {
            files: HashMap::new(),
            ..old
        };

        // TODO(proj): also reread pbsln and pbproj files

        let mut errs = Vec::new();
        let proj = new.projects.get_mut(&new.default).unwrap();
        for (lib, loaded_files) in &mut proj.libraries {
            loaded_files.clear();
            let err: anyhow::Result<_> = (|| {
                let lib_path = uri_to_path(&lib)?;
                for file in std::fs::read_dir(&lib_path)? {
                    let path = file?.path();
                    if path.extension().is_some_and(|ext| {
                        // TODO: add all extensions
                        vec!["sru", "srw", "srs", "sra", "srf"].contains(&&*ext.to_string_lossy())
                    }) {
                        let mut uri = (**lib).clone();
                        uri.set_path(&path.to_string_lossy());
                        let uri = Arc::new(uri);

                        let file = if let Some(mut old_file) = old.files.remove(&uri) {
                            // TODO(proj): currently assuming no change happened except if it was opened in editor (so changes by scripts are ignored), do hashing or timesamping?
                            if let Some(changes) = pending_changes.remove(&uri) {
                                let mut meta = old_file.into_owner().meta;
                                changes(&mut meta);

                                old_file = Builder::new()
                                    .build_file_shallow(ParsedFile::new(meta.uri, meta.content));
                            }

                            old_file
                        } else {
                            let parse = ParsedFile::new_from_filesystem(uri.clone())
                                .with_context(|| format!("Failed to parse file: {}", uri))?;
                            Builder::new().build_file_shallow(parse)
                        };
                        loaded_files.push(uri.clone());
                        new.files.insert(uri, file);
                    }
                }
                Ok(())
            })();
            if let Err(err) = err {
                errs.push(err);
            }
        }

        (new, errs)
    }

    pub fn open_file(&mut self, url: &Arc<Url>) -> Option<&mut BuiltFile> {
        let file = self.files.get_mut(url)?;
        Builder::new().build_file_bodies(file);
        Some(file)
    }

    pub fn builtins(&self) -> &BuiltinsInner<'_> {
        self.builtins.borrow_dependent()
    }

    pub fn project(&self) -> &Project {
        self.projects.get(&self.default).unwrap()
    }

    pub fn application(&self) -> Option<&BuiltFile> {
        self.files.get(&self.project().application)
    }

    pub fn files_iter(&self) -> impl Iterator<Item = &BuiltFile> {
        self.project()
            .libraries
            .iter()
            .flat_map(|(_, urls)| urls.iter().filter_map(|url| self.files.get(url)))
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

        match self.find_class(r#ref.file, &r#ref.class.base().into()) {
            Found::Yes(parent) => self.inherits_from(&parent, base),
            Found::No => Found::No,
        }
    }

    pub fn get_parent(&'sol self, class_ref: ClassRef<'sol>) -> Option<ClassRef<'sol>> {
        if let Some(within) = class_ref.class.within() {
            if let Found::Yes(Complex::Class(class)) =
                self.find_class(class_ref.file, &within.into())
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
        name: &IString,
    ) -> Found<Complex<'sol>> {
        if let Some(file) = current_file {
            if let Some(class) = file.inner().classes.get(name) {
                return Found::Yes(Complex::Class(ClassRef::new(file, class)));
            }
        }

        if let Some((file, class)) = self.get_file_for_class(name) {
            return Found::Yes(Complex::Class(ClassRef::new(file, class)));
        }

        if let Some(en) = self.builtins.borrow_dependent().enums.get(name) {
            return Found::Yes(Complex::Enum(en));
        }

        if let Some(class) = self.builtins.borrow_dependent().classes.get(name) {
            return Found::Yes(Complex::Class(ClassRef::builtin(class)));
        }

        Found::No
    }
}
