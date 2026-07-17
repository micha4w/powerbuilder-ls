use core::panic;
use std::sync::Arc;

use crate::builder::{self, BuiltFile};

#[derive(Debug, Clone, Copy)]
pub struct ClassRef<'proj> {
    /// `None` for builtins
    pub file: Option<&'proj BuiltFile>,
    pub class: &'proj Arc<builder::Class<'proj>>,
}
impl<'proj> ClassRef<'proj> {
    pub fn new(
        file: &'proj BuiltFile,
        class: &'proj Arc<builder::Class<'proj>>,
    ) -> ClassRef<'proj> {
        ClassRef {
            file: Some(file),
            class,
        }
    }

    pub fn builtin(class: &'proj Arc<builder::Class<'proj>>) -> ClassRef<'proj> {
        ClassRef { file: None, class }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Complex<'proj> {
    Class(ClassRef<'proj>),
    Enum(&'proj builder::Enum),
}

impl Complex<'_> {
    pub fn name(&self) -> &str {
        match self {
            Complex::Class(r#ref) => &r#ref.class.name(),
            Complex::Enum(r#enum) => &r#enum.name,
        }
    }

    pub fn help(&self) -> Option<&String> {
        match self {
            Complex::Class(r#ref) => r#ref.class.help,
            Complex::Enum(r#enum) => r#enum.help.as_ref(),
        }
    }

    pub fn unwrap_class(&self) -> &ClassRef<'_> {
        match self {
            Complex::Class(class) => class,
            _ => panic!("unwrap_class failed"),
        }
    }
}

impl PartialEq for Complex<'_> {
    fn eq(&self, other: &Complex<'_>) -> bool {
        match (self, other) {
            (Complex::Class(a), Complex::Class(b)) => Arc::ptr_eq(a.class, b.class),
            (Complex::Enum(a), Complex::Enum(b)) => a.name == b.name,
            _ => false,
        }
    }
}
