pub enum EitherOr<Left, Right> {
    Left(Left),
    Right(Right),
}

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Debug)]
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

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub(crate) fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub(crate) fn new_point(base_pos: Position) -> Self {
        Self {
            start: base_pos,
            end: base_pos,
        }
    }

    pub fn contains(&self, pos: &Position) -> bool {
        &self.start <= pos && pos <= &self.end
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

pub enum MaybeRef<'a, T> {
    Ref(&'a T),
    No(T),
}

impl<T> AsRef<T> for MaybeRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            MaybeRef::Ref(t) => t,
            MaybeRef::No(t) => t,
        }
    }
}

pub enum MaybeMut<'a, T> {
    Mut(&'a mut T),
    No(&'a T),
}

impl<T> MaybeMut<'_, T> {
    pub fn unwrap_mut(&mut self) -> &mut T {
        match self {
            MaybeMut::Mut(t) => t,
            MaybeMut::No(_) => panic!("MaybeMut unwrapped when it wasn't Mutable"),
        }
    }
}

impl<T> AsRef<T> for MaybeMut<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            MaybeMut::Mut(t) => t,
            MaybeMut::No(t) => t,
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

impl std::ops::Deref for IString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self._str
    }
}
