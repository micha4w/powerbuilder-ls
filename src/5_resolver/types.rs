use core::fmt;

use crate::{builder, parser, project, tokenizer, types::*};

#[derive(Debug, Clone)]
pub enum ResolvedType<'proj> {
    Base(builder::BaseType),

    Complex(project::Complex<'proj>),
    Array(Box<Self>), // TODO(arrays): fixed size arrays
    Unknown,
    Void,
}

impl<'proj> ResolvedType<'proj> {
    // pub fn new(ctx: &Context<'proj>, data_type: &parser::DataType) -> ResolvedType<'proj> {
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

    pub fn unnested(&self) -> &ResolvedType<'proj> {
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
pub enum ResolvedLValue<'proj> {
    This(project::ClassRef<'proj>),
    Super(project::Complex<'proj>),
    Parent(project::Complex<'proj>),

    /// `ClassRef` is `None` for scoped / local variables, `Some` for instance variables
    Variable(Option<project::ClassRef<'proj>>, &'proj builder::Variable),
    /// `ClassRef` is `None` for builtin functions
    Function(Option<project::ClassRef<'proj>>, &'proj builder::Function),

    Member(project::ClassRef<'proj>, &'proj builder::Variable),
    Method(
        project::ClassRef<'proj>,
        OneOf<&'proj builder::Function, &'proj builder::Event>,
    ),
    // Index,
    // SQLAccess,
}

#[derive(Clone)]
pub struct Annotation<'proj> {
    pub resolved_type: ResolvedType<'proj>,
    pub lvalue: Option<ResolvedLValue<'proj>>,
}

#[derive(Debug)]
pub enum ListError {
    NotAccessible,
    WrongArguments,
    NotAFunctionObject,
}

pub type ListResult<T> = Result<T, (ListError, T)>;

impl fmt::Display for ListError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ListError::NotAccessible => write!(f, "not accessible"),
            ListError::WrongArguments => write!(f, "wrong arguments"),
            ListError::NotAFunctionObject => write!(f, "not a function object"),
        }
    }
}

#[derive(Clone)]
pub enum VariableFilter<'proj> {
    All,
    ForAccess(&'proj parser::VariableAccess, tokenizer::AccessType),
}

impl<'proj> VariableFilter<'proj> {
    pub fn with_access(self, level: tokenizer::AccessType) -> VariableFilter<'proj> {
        match self {
            VariableFilter::All => VariableFilter::All,
            VariableFilter::ForAccess(name, _) => VariableFilter::ForAccess(name, level),
        }
    }
}

#[derive(Clone)]
// TODO: change to a struct of Options?
pub enum FunctionFilter<'proj, 'tmp> {
    All,
    ForCall(
        &'tmp IString,
        &'tmp Vec<&'tmp ResolvedType<'proj>>,
        tokenizer::AccessType,
    ),
}

impl<'proj, 'tmp> FunctionFilter<'proj, 'tmp> {
    pub fn with_access(self, level: tokenizer::AccessType) -> FunctionFilter<'proj, 'tmp> {
        match self {
            FunctionFilter::All => FunctionFilter::All,
            FunctionFilter::ForCall(name, arg_types, old_level) => FunctionFilter::ForCall(
                name,
                arg_types,
                if old_level.strictness() < level.strictness() {
                    level
                } else {
                    old_level
                },
            ),
        }
    }
}

#[derive(Clone)]
pub enum EventFilter<'a> {
    All,
    WithName(&'a IString),
}
