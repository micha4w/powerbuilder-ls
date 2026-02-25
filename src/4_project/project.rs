use core::str;
use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::OsStr,
    fs,
    io::Read,
    mem::take,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use anyhow::anyhow;

use encoding_rs_io::DecodeReaderBytes;

use crate::{
    builder::{self, Builder, BuiltFile, File},
    tokenizer,
    types::*,
};

use super::{builtins::Builtins, types::*};

#[derive(Debug)]
pub struct Project {
    // Keep the writing of RwLock to the minimal
    // TODO(proj): make these only be writable in this module, so only readable by linter and ls
    pub application: Option<Url>,

    pub files: HashMap<Url, File>,
    // TODO(globals): ...
    // pub global_functions: HashMap<IString, Url>,

    // TODO(perf): cache classes and variables by name
    pub builtins: Builtins,
}

impl<'proj> Project {
    pub fn new() -> Project {
        let mut proj = Project {
            application: None,
            files: HashMap::new(),
            builtins: Builtins::new(),
        };

        proj.builtins.load_enums();
        proj.builtins.load_classes();
        proj.builtins.load_functions();

        proj
    }

    pub fn load_file(&mut self, uri: Url) -> anyhow::Result<&mut File> {
        let entry = self.files.entry(uri.clone());

        match entry {
            Entry::Occupied(occupied) => Ok(occupied.into_mut()),
            Entry::Vacant(vacant) => Ok(vacant.insert(File::new(uri)?)),
        }
    }

    pub fn load_file_for_depending(&mut self, uri: &Url) -> anyhow::Result<&mut BuiltFile> {
        let file = self.load_file(uri.clone())?;

        let mut load_classes = Vec::new();
        replace_with(file, |file| match file {
            File::Parsed(parsed) => {
                let mut builder = Builder::new();
                let file = builder.build_file_shallow(parsed);

                load_classes = take(&mut builder.load_requests)
                    .into_iter()
                    .filter(|class| !file.meta.classes.contains(class))
                    .collect::<Vec<_>>();

                File::Built(file)
            }
            file @ File::Built(_) => file,
        });

        for class in load_classes {
            if let Some(uri) = self.get_file_for_class(&class) {
                // TODO(errors): ...
                self.load_file_for_depending(&uri);
            };
        }

        let entry = self.files.get_mut(uri).unwrap();
        Ok(unwrap_enum!(entry, File::Built))
    }

    pub fn load_file_for_editing(&mut self, uri: &Url) -> anyhow::Result<&mut BuiltFile> {
        let mut file = self.load_file_for_depending(uri)?;

        if !file.bodies_processed {
            let mut builder = Builder::new();
            builder.build_file_bodies(&mut file);

            for class in builder
                .load_requests
                .into_iter()
                .filter(|class| !file.meta.classes.contains(class))
                .collect::<Vec<_>>()
            {
                if let Some(uri) = self.get_file_for_class(&class) {
                    // TODO(errors): ...
                    self.load_file_for_depending(&uri);
                };
            }
        }

        let entry = self.files.get_mut(uri).unwrap();
        Ok(unwrap_enum!(entry, File::Built))
    }

    fn add_directory(&mut self, folder: &Url) -> anyhow::Result<()> {
        for file in std::fs::read_dir(&uri_to_path(folder)?)? {
            let path = file?.path();

            let extensions =
                ["sra", "srf", "srm", "srs", "sru", "srw"].map(|f| Some(OsStr::new(f)));
            if extensions.contains(&path.extension()) {
                self.load_file(folder.join(&path.file_name().unwrap().to_string_lossy())?)?;
            }
        }
        Ok(())
    }

    fn add_project(&mut self, folder: &Url, project: &str) -> anyhow::Result<()> {
        let path = folder.join(project)?;
        let mut content = String::new();

        eprintln!("Adding project at: {}", path);
        DecodeReaderBytes::new(fs::File::open(&uri_to_path(&path)?)?)
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

        let Some(libs) = doc
            .root_element()
            .children()
            .find(|node| node.has_tag_name("Libraries"))
        else {
            return Err(anyhow!(
                "XML Format Invalid (<Libraries> not found in {})",
                path
            ));
        };

        for lib in libs.children() {
            if lib.has_tag_name("Library") {
                if let Some(lib_path) = lib.attribute("Path") {
                    let lib_path = lib_path.replace('\\', std::path::MAIN_SEPARATOR_STR) + "/";
                    self.add_directory(&folder.join(&lib_path)?)?;
                }
            } else {
                // TODO(errors): warn or something?
            }
        }

        if let Some(entry_lib) = libs.attribute("AppEntry") {
            if let Some(application) = doc
                .root_element()
                .children()
                .find(|node| node.has_tag_name("Application"))
            {
                if let Some(name) = application.attribute("Name") {
                    let app_uri = folder.join(entry_lib)?.join(&(name.to_owned() + ".sra"))?;

                    //TODO(errors): ...
                    self.load_file_for_depending(&app_uri);
                    self.application = Some(app_uri);
                }
            }
        }

        Ok(())
    }

    pub fn add_default_project(&mut self, folder: &Url) -> anyhow::Result<()> {
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
                return self.add_project(
                    &folder,
                    default
                        .replace('\\', std::path::MAIN_SEPARATOR_STR)
                        .as_str(),
                );
            }
        };

        Err(anyhow!(
            "XML Format Invalid (Solution.Projects[Default] not found in {})",
            solution.display()
        ))
    }

    pub fn get_file_for_class(&self, class: &IString) -> Option<Url> {
        for (uri, file) in &self.files {
            // TODO: add some order to this?
            // powerbuilder only uses the class that is lowest in the project list
            // cache? load additiona files (if we are not in a project)

            if file.meta().classes.contains(class) {
                return Some(uri.clone());
            }
        }
        None
    }

    pub fn builtin_class(&self, name: &'static str) -> ClassRef<'_> {
        ClassRef::builtin(
            self.builtins
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

    pub fn get_parent(&'proj self, class_ref: ClassRef<'proj>) -> Option<ClassRef<'proj>> {
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
        &'proj self,
        current_file: Option<&'proj BuiltFile>,
        name: &IString,
    ) -> Found<Complex<'proj>> {
        if let Some(file) = current_file {
            if let Some(class) = file.classes.get(name) {
                return Found::Yes(Complex::Class(ClassRef::new(file, class)));
            }
        }

        if let Some(uri) = self.get_file_for_class(name) {
            if let Some(File::Built(built)) = self.files.get(&uri) {
                if let Some(class) = built.classes.get(name) {
                    return Found::Yes(Complex::Class(ClassRef::new(built, class)));
                }
            };
        }

        if let Some(en) = self.builtins.enums.get(name) {
            return Found::Yes(Complex::Enum(en));
        }

        if let Some(class) = self.builtins.classes.get(name) {
            return Found::Yes(Complex::Class(ClassRef::builtin(class)));
        }

        Found::No
    }
}
