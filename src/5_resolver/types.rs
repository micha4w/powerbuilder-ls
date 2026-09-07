use bitflags::bitflags;
use core::fmt;
use std::{collections::HashMap, fmt::write, hash::Hash, ops::Deref};

use crate::{builder, parser, solution, tokenizer, types::*};

pub type BoxIter<'a, T> = Box<dyn Iterator<Item = T> + 'a>;

#[derive(Debug, Clone)]
pub enum ResolvedType<'sol> {
    Base(builder::BaseType),

    Complex(solution::Complex<'sol>),
    Array(Box<Self>), // TODO(arrays): fixed size arrays
    Unknown,
    Void,
}

impl<'sol> ResolvedType<'sol> {
    // pub fn new(ctx: &Context<'sol>, data_type: &parser::DataType) -> ResolvedType<'sol> {
    //     let base = BaseType::new(data_type);

    //     let typ = match base {
    //         Some(base) => ResolvedType::Base(base),
    //         None => {
    //             // TODO(groups)
    //             match ctx.find_class(&(&data_type.name.content).into()) {
    //                 Found::Yes(class) => ResolvedType::Complex(class),
    //                 Found::No => ResolvedType::Unknown,
    //             }
    //         }
    //     };

    //     if data_type.array_bounds.is_some() {
    //         ResolvedType::Array(Box::new(typ))
    //     } else {
    //         typ
    //     }
    // }

    pub fn is_numeric(&self) -> bool {
        self.numeric_precedence().is_some()
    }

    pub fn numeric_precedence(&self) -> Option<u8> {
        match self {
            Self::Base(builder::BaseType::Int) => Some(0),
            Self::Base(builder::BaseType::Uint) => Some(1),
            Self::Base(builder::BaseType::Long) => Some(2),
            Self::Base(builder::BaseType::Ulong) => Some(3),
            Self::Base(builder::BaseType::Longlong) => Some(4),
            Self::Base(builder::BaseType::Longptr) => Some(5),
            Self::Base(builder::BaseType::Real) => Some(6),
            Self::Base(builder::BaseType::Double) => Some(7),
            Self::Base(builder::BaseType::Decimal(_)) => Some(8),
            Self::Base(builder::BaseType::Any) => Some(9),
            Self::Unknown => Some(10),

            Self::Base(builder::BaseType::Blob(_))
            | Self::Base(builder::BaseType::Boolean)
            | Self::Base(builder::BaseType::Byte) // TODO: is this correct, can you not add bytes?
            | Self::Base(builder::BaseType::Char)
            | Self::Base(builder::BaseType::Date)
            | Self::Base(builder::BaseType::Datetime)
            | Self::Base(builder::BaseType::String)
            | Self::Base(builder::BaseType::Time)
            | Self::Void
            | Self::Complex(_)
            | Self::Array(_) => None,
        }
    }

    pub fn unnested(&self) -> &ResolvedType<'sol> {
        match self {
            Self::Array(inner) => inner.unnested(),
            _ => self,
        }
    }
}

impl std::fmt::Display for ResolvedType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Base(base) => write!(f, "{}", base),
            Self::Complex(class) => write!(f, "{}", class.name()),
            Self::Array(inner) => write!(f, "{}[]", inner),
            Self::Unknown => write!(f, "<unknown>"),
            Self::Void => write!(f, "<void>"),
        }
    }
}

#[derive(Clone)]
pub enum ResolvedLValue<'sol> {
    This(solution::ClassRef<'sol>),
    Super(solution::Complex<'sol>),
    Parent(solution::Complex<'sol>),

    /// `ClassRef` is `None` for scoped / local variables, `Some` for instance variables
    Variable(
        Option<solution::ClassRef<'sol>>,
        &'sol builder::Variable<'sol>,
    ),
    /// `ClassRef` is `None` for builtin functions
    Function(
        Option<solution::ClassRef<'sol>>,
        &'sol builder::Function<'sol>,
    ),

    Member(solution::ClassRef<'sol>, &'sol builder::Variable<'sol>),
    Method(
        solution::ClassRef<'sol>,
        OneOf<&'sol builder::Function<'sol>, &'sol builder::Event<'sol>>,
    ),
    // Index,
    // SQLAccess,
}

#[derive(Clone)]
pub struct Annotation<'sol> {
    pub resolved_type: ResolvedType<'sol>,
    pub lvalue: Option<ResolvedLValue<'sol>>,
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct ListError: u32 {
        const None = 0x0;
        const WrongName = 0x1;
        const NotAccessible = 0x2;
        const WrongArguments = 0x4;
        const NotAFunctionObject = 0x8;
    }
}

pub type ListResult<T> = (T, ListError);

// pub fn map_list_result<T, U>(res: ListResult<T>, f: impl FnOnce(T) -> U) -> ListResult<U> {
//     match res {
//         Ok(val) => ListResult::Ok(f(val)),
//         Err((err, val)) => ListResult::Err((err, f(val))),
//     }
// }

impl fmt::Display for ListError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut errs = Vec::new();

        if self.contains(ListError::WrongName) {
            errs.push("wrong name");
        }
        if self.contains(ListError::NotAccessible) {
            errs.push("not accessible");
        }
        if self.contains(ListError::WrongArguments) {
            errs.push("wrong arguments");
        }
        if self.contains(ListError::NotAFunctionObject) {
            errs.push("not a function object");
        }

        write!(f, "{}", errs.join(" + "))
    }
}

impl TryInto<String> for ListError {
    type Error = ();

    fn try_into(self) -> Result<String, Self::Error> {
        if self.is_empty() {
            Err(())
        } else {
            Ok(self.to_string())
        }
    }
}

#[derive(Clone)]
pub enum SearchFilter<T> {
    Filter { want: T, do_error: bool },
    All,
}

impl<T> SearchFilter<T> {
    pub fn simple(t: T) -> Self {
        Self::Filter {
            want: t,
            do_error: false,
        }
    }

    pub fn error(t: T) -> Self {
        Self::Filter {
            want: t,
            do_error: true,
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> SearchFilter<U> {
        match self {
            SearchFilter::Filter { want, do_error } => SearchFilter::Filter {
                want: f(want),
                do_error,
            },
            SearchFilter::All => SearchFilter::All,
        }
    }

    // pub fn filter(
    //     &self,
    //     value: U,
    //     err: ListError,
    //     f: impl FnOnce(&U, &T) -> bool,
    // ) -> Option<ListResult<U>> {
    //     match self {
    //         SearchFilter::Filter { want, do_error } => {
    //             if f(&value, want) {
    //                 Some(ListResult::Ok(value))
    //             } else if *do_error {
    //                 Some(ListResult::Err((err, value)))
    //             } else {
    //                 None
    //             }
    //         }
    //         SearchFilter::All => Some(ListResult::Ok(value)),
    //     }
    // }

    pub fn filter_hash_map<'flt, 'a, U>(
        &'flt self,
        map: &'a HashMap<T::Target, U>,
        err: ListError,
    ) -> BoxIter<'_, ListResult<&'a U>>
    where
        'a: 'flt,
        T: Deref,
        T::Target: Sized + Eq + Hash,
    {
        match self {
            SearchFilter::Filter {
                want,
                do_error: false,
            } => Box::new(
                map.get(want)
                    .into_iter()
                    .map(|value| (value, ListError::None)),
            ),
            _ => Box::new(
                self.filter(map.iter(), |(t, _), t_want| **t == **t_want, err)
                    .map(|((t, u), err)| (u, err)),
            ),
        }
    }

    pub fn filter<'a, U>(
        &'a self,
        iter: impl Iterator<Item = U> + 'a,
        f: impl Fn(&U, &T) -> bool + 'a,
        err: ListError,
    ) -> impl Iterator<Item = ListResult<U>> + 'a {
        iter.filter_map(move |value| match self {
            SearchFilter::Filter { want, do_error } => {
                if f(&value, want) {
                    Some((value, ListError::None))
                } else if *do_error {
                    Some((value, err))
                } else {
                    None
                }
            }
            SearchFilter::All => Some((value, ListError::None)),
        })
    }

    pub fn filter_res<'a, U>(
        &'a self,
        iter: impl Iterator<Item = ListResult<U>> + 'a,
        f: impl Fn(&U, &T) -> bool + 'a,
        err: ListError,
    ) -> impl Iterator<Item = ListResult<U>> + 'a {
        iter.filter_map(move |(value, old_err)| match self {
            SearchFilter::Filter { want, do_error } => {
                if f(&value, want) {
                    Some((value, old_err))
                } else if *do_error {
                    Some((value, old_err | err))
                } else {
                    None
                }
            }
            SearchFilter::All => Some((value, ListError::None)),
        })
    }
}

impl<T> Default for SearchFilter<T> {
    fn default() -> Self {
        SearchFilter::All
    }
}

#[derive(Clone, Default)]
pub struct VariableFilter<'tmp> {
    pub name: SearchFilter<&'tmp IString>,
    /// bool: is_write
    pub access: SearchFilter<(tokenizer::AccessType, bool)>,
}

#[derive(Clone, Default)]
pub struct GlobalVariableFilter<'tmp> {
    pub name: SearchFilter<&'tmp IString>,
    pub accessible: SearchFilter<()>,
}

#[derive(Clone, Default)]
pub struct FunctionFilter<'sol, 'tmp> {
    pub name: SearchFilter<&'tmp IString>,
    pub access: SearchFilter<tokenizer::AccessType>,
    pub arguments: SearchFilter<&'tmp Vec<&'tmp ResolvedType<'sol>>>,
    // pub signature: SearchFilter<&'tmp IString>,
}

#[derive(Clone, Default)]
pub struct GlobalFunctionFilter<'sol, 'tmp> {
    pub name: SearchFilter<&'tmp IString>,
    pub accessible: SearchFilter<()>,
    pub arguments: SearchFilter<&'tmp Vec<&'tmp ResolvedType<'sol>>>,
    // pub signature: SearchFilter<&'tmp IString>,
}

// impl<'sol, 'tmp> FunctionFilter<'sol, 'tmp> {
//     pub fn with_access(self, level: tokenizer::AccessType) -> FunctionFilter<'sol, 'tmp> {
//         match self {
//             FunctionFilter::All => FunctionFilter::All,
//             FunctionFilter::ForCall(name, arg_types, old_level) => FunctionFilter::ForCall(
//                 name,
//                 arg_types,
//                 if old_level.strictness() < level.strictness() {
//                     level
//                 } else {
//                     old_level
//                 },
//             ),
//         }
//     }
// }

#[derive(Clone, Default)]
pub struct EventFilter<'tmp> {
    pub name: SearchFilter<&'tmp IString>,
}

#[derive(Clone, Default)]
pub struct ClassFilter<'tmp> {
    pub name: SearchFilter<&'tmp IString>,
}
