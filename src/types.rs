use anyhow::anyhow;
// pub use parking_lot::{
//     MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
// };
use std::{
    backtrace::Backtrace,
    marker::PhantomData,
    ops::{FromResidual, Residual, Try},
    path::PathBuf,
    sync::{Arc, Weak},
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
                    eprintln!("Possible deadlock: {}!!", Backtrace::capture().to_string());
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

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    fn end() -> Position {
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
    pub uri: Arc<Url>,
}

impl Range {
    pub(super) fn new(start: Position, end: Position, uri: Arc<Url>) -> Self {
        Self { start, end, uri }
    }

    pub(super) fn merged(mut self, b: &Range) -> Self {
        self.merge(b);
        self
    }

    pub(super) fn merge(&mut self, b: &Range) -> &Self {
        assert!(self.uri == b.uri, "Merging Ranges from 2 different files");

        if b.start < self.start {
            self.start = b.start;
        }
        if self.end < b.end {
            self.end = b.end;
        }

        self
    }

    pub(super) fn expanded(mut self, pos: &Position) -> Self {
        self.expand(pos);
        self
    }

    pub(super) fn expand(&mut self, pos: &Position) -> &Self {
        if pos < &self.start {
            self.start = pos.clone()
        }
        if &self.end < pos {
            self.end = pos.clone()
        }
        self
    }

    pub(super) fn new_point(base_pos: Position, uri: Arc<Url>) -> Self {
        Self {
            start: base_pos,
            end: base_pos,
            uri,
        }
    }

    pub(super) fn empty(uri: Arc<Url>) -> Self {
        Self::new_point(Position::default(), uri)
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

impl From<&'static str> for IString {
    fn from(value: &'static str) -> Self {
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

// pub type SP<T> = Arc<Mutex<T>>;
// pub fn sp<T>(t: T) -> SP<T> {
//     Arc::new(Mutex::new(t))
// }

// pub type WP<T> = Weak<Mutex<T>>;

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub range: Range,
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

pub(super) fn uri_to_path(uri: &Url) -> anyhow::Result<PathBuf> {
    uri.to_file_path()
        .map_err(|_| anyhow!("Failed to convert Url to PathBuf {:?}", uri))
}

pub(super) fn replace_with<T, F>(slot: &mut T, f: F)
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

macro_rules! unwrap_enum {
    ($var:expr, $enum:path $(,)?) => {
        match $var {
            $enum(inner) => inner,
            _ => unreachable!(
                "Expected enum variant to be {}, but was {:?}",
                stringify!($enum),
                $var
            ),
        }
    };
}
pub(super) use unwrap_enum;

/// for debugging stuf...
pub fn assert_lifetime<'a, T>(x: &'a T) -> &'a T {
    x
}
