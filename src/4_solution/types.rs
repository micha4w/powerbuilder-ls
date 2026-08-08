use core::panic;
use std::sync::Arc;

use crate::builder::{self, BuiltFile};

#[derive(Debug, Clone, Copy)]
pub struct ClassRef<'sol> {
    /// `None` for builtins
    pub file: Option<&'sol BuiltFile>,
    pub class: &'sol Arc<builder::Class<'sol>>,
}
impl<'sol> ClassRef<'sol> {
    pub fn new(
        file: &'sol BuiltFile,
        class: &'sol Arc<builder::Class<'sol>>,
    ) -> ClassRef<'sol> {
        ClassRef {
            file: Some(file),
            class,
        }
    }

    pub fn builtin(class: &'sol Arc<builder::Class<'sol>>) -> ClassRef<'sol> {
        ClassRef { file: None, class }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Complex<'sol> {
    Class(ClassRef<'sol>),
    Enum(&'sol builder::Enum),
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
