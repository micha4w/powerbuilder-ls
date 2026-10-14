use tracing::{error, warn};
// pub use parking_lot::{
//     MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
// };
use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::{FromResidual, Residual, Try},
    path::PathBuf,
    sync::Arc,
    time::Duration,
};
// pub use tokio::sync::RwLock;
// pub use async_lock::RwLock;
pub use tower_lsp::lsp_types::OneOf;

#[derive(Debug)]
pub struct Mutex<T: ?Sized> {
    m: tokio::sync::Mutex<T>,
}

unsafe impl<T: ?Sized + Send> Send for Mutex<T> {}
unsafe impl<T: ?Sized + Send> Sync for Mutex<T> {}

impl<T: Sized> Mutex<T> {
    pub fn new(t: T) -> Self {
        Self {
            m: tokio::sync::Mutex::new(t),
        }
    }

    pub async fn lock(&self) -> tokio::sync::MutexGuard<'_, T> {
        loop {
            match tokio::time::timeout(Duration::from_secs(5), self.m.lock()).await {
                Ok(lock) => return lock,
                Err(_) => {
                    warn!(
                        backtrace = Backtrace::capture().to_string(),
                        "possible deadlock"
                    );
                }
            }
        }
    }

    pub fn into_inner(self) -> T {
        self.m.into_inner()
    }
}

// #[derive(Debug)]
// pub struct RwLock<T: ?Sized> {
//     m: tokio::sync::RwLock<T>,
// }

// unsafe impl<T: Send> Send for RwLock<T> {}
// unsafe impl<T: Send> Sync for RwLock<T> {}

// impl<T: Sized> RwLock<T> {
//     pub fn new(t: T) -> Self {
//         Self {
//             m: tokio::sync::RwLock::new(t),
//         }
//     }

//     pub async fn read(&self) -> tokio::sync::RwLockReadGuard<'_, T> {
//         loop {
//             match tokio::time::timeout(Duration::from_secs(5), self.m.read()).await {
//                 Ok(lock) => return lock,
//                 Err(_) => {
//                     eprintln!("Possible deadlock: {}!!", Backtrace::capture().to_string());
//                 }
//             }
//         }
//     }

//     pub async fn write(&self) -> tokio::sync::RwLockWriteGuard<'_, T> {
//         loop {
//             match tokio::time::timeout(Duration::from_secs(5), self.m.write()).await {
//                 Ok(lock) => return lock,
//                 Err(_) => {
//                     eprintln!("Possible deadlock: {}!!", Backtrace::capture().to_string());
//                 }
//             }
//         }
//     }
// }

// #[derive(Debug)]
// pub enum EitherOr<Left, Right> {
//     Left(Left),
//     Right(Right),
// }

#[derive(Debug, Clone)]
pub struct FilePath {
    uri: Url,
    path: PathBuf,
}

impl FilePath {
    pub fn new(uri: Url) -> Option<FilePath> {
        let Ok(path) = uri.to_file_path() else {
            error!(%uri, "failed to convert Url to PathBuf");
            return None;
        };

        Some(FilePath { uri, path })
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn uri(&self) -> &Url {
        &self.uri
    }

    pub fn join(&self, add_path: &str) -> FilePath {
        let path = self.path.join(add_path);
        let mut uri = self.uri.clone();

        let mut base_path = uri.path().to_string();
        if !base_path.ends_with('/') {
            base_path.push('/');
        }
        base_path.push_str(add_path);
        uri.set_path(&base_path);
        FilePath { uri, path }
    }

    pub fn into_id(self) -> FileID {
        FileID { m: Arc::new(self) }
    }
}

#[derive(Debug, Clone)]
pub struct FileID {
    m: Arc<FilePath>,
}
impl FileID {
    pub fn new(uri: Url) -> Option<FileID> {
        Some(FileID {
            m: Arc::new(FilePath::new(uri)?),
        })
    }

    pub fn new_virtual(uri: Url) -> FileID {
        FileID {
            m: Arc::new(FilePath {
                path: uri.to_string().into(),
                uri,
            }),
        }
    }

    pub fn path(&self) -> &PathBuf {
        &self.m.path
    }

    pub fn uri(&self) -> &Url {
        &self.m.uri
    }

    pub fn join(&self, path: &str) -> FileID {
        self.m.join(path).into_id()
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        Arc::ptr_eq(&a.m, &b.m)
    }

    fn cmp_key(&self) -> &PathBuf {
        self.path()
    }
}

impl std::fmt::Display for FileID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.uri().fmt(f)
    }
}

impl PartialEq for FileID {
    fn eq(&self, other: &Self) -> bool {
        self.cmp_key().eq(other.cmp_key())
    }
}
impl Eq for FileID {}

impl PartialOrd for FileID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp_key().partial_cmp(other.cmp_key())
    }
}

impl Ord for FileID {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_key().cmp(other.cmp_key())
    }
}

impl Hash for FileID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cmp_key().hash(state);
    }
}

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn end() -> Position {
        Position {
            line: u32::MAX,
            column: u32::MAX,
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
impl Into<tower_lsp::lsp_types::Position> for Position {
    fn into(self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position {
            line: self.line,
            character: self.column,
        }
    }
}
impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(value: tower_lsp::lsp_types::Position) -> Self {
        Self {
            line: value.line,
            column: value.character,
        }
    }
}

pub type Url = tower_lsp::lsp_types::Url;

#[derive(Clone, PartialEq, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
    pub file_id: FileID,
}

impl Range {
    pub(crate) fn new(start: Position, end: Position, file_id: FileID) -> Self {
        Self {
            start,
            end,
            file_id,
        }
    }

    pub(crate) fn merged(mut self, b: &Range) -> Self {
        self.merge(b);
        self
    }

    pub(crate) fn merge(&mut self, b: &Range) -> &Self {
        assert!(
            self.file_id == b.file_id,
            "Merging Ranges from 2 different files"
        );

        if b.start < self.start {
            self.start = b.start;
        }
        if self.end < b.end {
            self.end = b.end;
        }

        self
    }

    pub(crate) fn expanded(mut self, pos: &Position) -> Self {
        self.expand(pos);
        self
    }

    pub(crate) fn expand(&mut self, pos: &Position) -> &Self {
        if pos < &self.start {
            self.start = pos.clone()
        }
        if &self.end < pos {
            self.end = pos.clone()
        }
        self
    }

    pub(crate) fn new_point(base_pos: Position, file_id: FileID) -> Self {
        Self {
            start: base_pos,
            end: base_pos,
            file_id,
        }
    }

    pub(crate) fn empty(file_id: FileID) -> Self {
        Self::new_point(Position::default(), file_id)
    }

    pub(crate) fn all(file_id: FileID) -> Self {
        Self {
            start: Position::default(),
            end: Position::end(),
            file_id,
        }
    }

    pub fn contains(&self, pos: &Position) -> bool {
        &self.start <= pos && pos <= &self.end
    }

    pub fn contains_range(&self, other: &Range) -> bool {
        self.contains(&other.start) && self.contains(&other.end)
    }
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} - {})", self.start, self.end)
    }
}

impl Into<tower_lsp::lsp_types::Range> for Range {
    fn into(self) -> tower_lsp::lsp_types::Range {
        tower_lsp::lsp_types::Range {
            start: self.start.into(),
            end: self.end.into(),
        }
    }
}

// pub enum MaybeRef<'a, T> {
//     Ref(&'a T),
//     No(T),
// }

// impl<T> AsRef<T> for MaybeRef<'_, T> {
//     fn as_ref(&self) -> &T {
//         match self {
//             MaybeRef::Ref(t) => t,
//             MaybeRef::No(t) => t,
//         }
//     }
// }

// pub enum MaybeMut<'a, T> {
//     Mut(&'a mut T),
//     No(&'a T),
// }

// impl<T> MaybeMut<'_, T> {
//     pub fn unwrap_mut(&mut self) -> &mut T {
//         match self {
//             MaybeMut::Mut(t) => t,
//             MaybeMut::No(_) => panic!("MaybeMut unwrapped when it wasn't Mutable"),
//         }
//     }
// }

// impl<T> AsRef<T> for MaybeMut<'_, T> {
//     fn as_ref(&self) -> &T {
//         match self {
//             MaybeMut::Mut(t) => t,
//             MaybeMut::No(t) => t,
//         }
//     }
// }

// TODO: replace left, right with def, decl
#[derive(Clone, Copy, Debug)]
pub struct DefinitionDeclaration<Def, Decl = Def> {
    pub definition: Option<Def>,
    pub declaration: Option<Decl>,
}

pub enum DefinitionDeclarationEnum<'a, Def, Decl> {
    Definition(&'a Def),
    Declaration(&'a Decl),
}

impl<Def, Decl> DefinitionDeclaration<Def, Decl> {
    pub fn definition(def: Def) -> Self {
        Self {
            definition: Some(def),
            declaration: None,
        }
    }

    pub fn declaration(decl: Decl) -> Self {
        Self {
            definition: None,
            declaration: Some(decl),
        }
    }

    pub fn both(def: Def, decl: Decl) -> Self {
        Self {
            definition: Some(def),
            declaration: Some(decl),
        }
    }

    pub fn get_any(&self) -> DefinitionDeclarationEnum<'_, Def, Decl> {
        match (self.definition.as_ref(), self.declaration.as_ref()) {
            (Some(l), _) => DefinitionDeclarationEnum::Definition(l),
            (_, Some(r)) => DefinitionDeclarationEnum::Declaration(r),
            _ => unreachable!(),
        }
    }

    pub fn map<'a, T>(
        &'a self,
        def_func: impl FnOnce(&'a Def) -> Option<T>,
        decl_func: impl FnOnce(&'a Decl) -> Option<T>,
    ) -> Option<T> {
        if let Some(def) = &self.definition {
            if let Some(t) = def_func(def) {
                return Some(t);
            }
        }
        if let Some(decl) = &self.declaration {
            if let Some(t) = decl_func(decl) {
                return Some(t);
            }
        }

        None
    }

    pub fn get_definition(&self) -> &Option<Def> {
        return &self.definition;
    }

    pub fn get_declaration(&self) -> &Option<Decl> {
        return &self.declaration;
    }

    pub fn get_definition_mut(&mut self) -> &mut Option<Def> {
        return &mut self.definition;
    }

    pub fn get_declaration_mut(&mut self) -> &mut Option<Decl> {
        return &mut self.declaration;
    }

    pub fn split(self) -> (Option<Def>, Option<Decl>) {
        (self.definition, self.declaration)
    }
}

impl<T> DefinitionDeclaration<T, T> {
    pub fn get_one(&self) -> &T {
        match (self.definition.as_ref(), self.declaration.as_ref()) {
            (Some(l), _) => l,
            (_, Some(r)) => r,
            _ => unreachable!(),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct IString {
    _str: String,
}

impl From<&String> for IString {
    fn from(value: &String) -> Self {
        Self {
            _str: value.to_lowercase(),
        }
    }
}

impl From<&str> for IString {
    fn from(value: &str) -> Self {
        Self {
            _str: value.to_lowercase(),
        }
    }
}

impl std::ops::Deref for IString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self._str
    }
}

impl std::fmt::Display for IString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self._str.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct Grouped<T> {
    pub group: Option<T>,
    pub name: T,
}

impl<T> Grouped<T> {
    pub fn new(group: Option<T>, name: T) -> Self {
        Self { group, name }
    }
    pub fn simple(name: T) -> Self {
        Self { group: None, name }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Grouped<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(group) = &self.group {
            write!(f, "{}`", group)?;
        }
        write!(f, "{}", self.name)
    }
}
impl<T> From<(Option<T>, T)> for Grouped<T> {
    fn from((group, name): (Option<T>, T)) -> Self {
        Self { group, name }
    }
}

impl<T, U> From<&Grouped<T>> for Grouped<U>
where
    T: Into<U> + Copy,
{
    fn from(value: &Grouped<T>) -> Self {
        Self {
            group: value.group.map(|g| g.into()),
            name: value.name.into(),
        }
    }
}

// pub type SP<T> = Arc<Mutex<T>>;
// pub fn sp<T>(t: T) -> SP<T> {
//     Arc::new(Mutex::new(t))
// }

// pub type WP<T> = Weak<Mutex<T>>;

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub range: Range,
}

impl From<Diagnostic> for tower_lsp::lsp_types::Diagnostic {
    fn from(d: Diagnostic) -> Self {
        tower_lsp::lsp_types::Diagnostic {
            range: d.range.into(),
            severity: Some(match d.severity {
                Severity::Error => tower_lsp::lsp_types::DiagnosticSeverity::ERROR,
                Severity::Warning => tower_lsp::lsp_types::DiagnosticSeverity::WARNING,
                Severity::Info => tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION,
                Severity::Hint => tower_lsp::lsp_types::DiagnosticSeverity::HINT,
            }),
            message: d.message.clone(),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Found<T> {
    Yes(T),
    No,
}

impl<T> From<Option<T>> for Found<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => Found::Yes(t),
            None => Found::No,
        }
    }
}

impl<T> From<Found<T>> for Option<T> {
    fn from(value: Found<T>) -> Self {
        value.as_option()
    }
}

impl<T> Found<T> {
    // pub fn yes(self) -> FoundYes<T> {
    //     FoundYes { f: self }
    // }

    pub fn found(&self) -> bool {
        match self {
            Found::Yes(_) => true,
            Found::No => false,
        }
    }

    pub fn as_option(self) -> Option<T> {
        match self {
            Found::Yes(t) => Some(t),
            Found::No => None,
        }
    }

    // pub fn no(self) -> FoundNo<T> {
    //     FoundNo { f: self }
    // }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Found<U> {
        match self {
            Found::Yes(t) => Found::Yes(f(t)),
            Found::No => Found::No,
        }
    }

    pub fn map_or<U>(self, default: U, f: impl FnOnce(T) -> U) -> U {
        match self {
            Found::Yes(t) => f(t),
            Found::No => default,
        }
    }

    pub fn and_then<U>(self, f: impl FnOnce(T) -> Found<U>) -> Found<U> {
        match self {
            Found::Yes(t) => f(t),
            Found::No => Found::No,
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Found::Yes(t) => t,
            Found::No => default,
        }
    }

    pub fn as_ref(&self) -> Found<&T> {
        match self {
            Found::Yes(t) => Found::Yes(t),
            Found::No => Found::No,
        }
    }
}

impl<T> Try for Found<T> {
    type Output = ();
    type Residual = FoundResidual<T>;

    fn from_output(_: Self::Output) -> Found<T> {
        Found::No
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Found::Yes(t) => std::ops::ControlFlow::Break(Self::Residual { t }),
            Found::No => std::ops::ControlFlow::Continue(()),
        }
    }
}

pub struct FoundResidual<T> {
    pub t: T,
}
impl<T> Residual<()> for FoundResidual<T> {
    type TryType = Found<T>;
}

impl<T> FromResidual<<Found<T> as Try>::Residual> for Found<T> {
    fn from_residual(residual: <Found<T> as Try>::Residual) -> Found<T> {
        Found::Yes(residual.t)
    }
}

// pub struct FoundYes<T> {
//     f: Found<T>,
// }

// impl<T> Try for FoundYes<T> {
//     type Output = T;
//     type Residual = ();

//     fn from_output(t: Self::Output) -> Found {
//         Found::Yes(t)
//     }

//     fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
//         match self.f {
//             Found::Yes(t) => std::ops::ControlFlow::Continue(t),
//             Found::No => std::ops::ControlFlow::Break(()),
//         }
//     }
// }

// impl<T> FromResidual<()> for Found<T> {
//     fn from_residual(_: ()) -> Found<T> {
//         Found::No
//     }
// }

// impl<T> FromResidual<T> for Found<T> {
//     fn from_residual(t: T) -> Found<T> {
//         Found::Yes(t)
//     }
// }

// pub struct FoundNo<T> {
//     f: Found<T>,
// }

// impl<T> Try for FoundNo<T> {
//     type Output = ();
//     type Residual = T;

//     fn from_output(t: Self::Output) -> Found {
//         Found::Yes(t)
//     }

//     fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
//         match self.f {
//             Found::Yes(t) => std::ops::ControlFlow::Break(t),
//             Found::No => std::ops::ControlFlow::Continue(()),
//         }
//     }
// }

// impl<T> FromResidual for FoundNo<T> {
//     fn from_residual(_: <Self as Try>::Residual) -> Found {
//         Found::No
//     }
// }

// pub(crate) fn uri_to_path(uri: &Url) -> Option<PathBuf> {
//     if let Ok(path) = uri.to_file_path() {
//         return Some(path);
//     };
//     error!(%uri, "failed to convert Url to PathBuf");
//     None
// }

// pub(crate) fn url_join_path(mut base: Url, path: &str) -> Url {
//     let mut base_path = base.path().to_string();
//     if !base_path.ends_with('/') {
//         base_path.push('/');
//     }
//     base_path.push_str(path);
//     base.set_path(&base_path);
//     base
// }

pub(crate) fn replace_with<T, F>(slot: &mut T, f: F)
where
    F: FnOnce(T) -> T,
{
    unsafe {
        // Panics always lead to exits, for now...
        let old = std::ptr::read(slot);
        let new = f(old);
        std::ptr::write(slot, new);
    }
}

#[macro_export]
macro_rules! check_enum {
    ($var:expr, $enum:path $(,)?) => {
        match $var {
            $enum(inner) => Some(inner),
            _ => None,
        }
    };
}

#[macro_export]
macro_rules! unwrap_enum {
    ($var:expr, $enum:path $(,)?) => {{
        let Some(inner) = check_enum!($var, $enum) else {
            unreachable!(
                "Expected enum variant to be {}, but was {:?}",
                stringify!($enum),
                $var
            );
        };
        inner
    }};
}

#[allow(dead_code)]
/// for debugging stuf...
pub fn assert_lifetime<'a, T>(x: &'a T) -> &'a T {
    x
}
