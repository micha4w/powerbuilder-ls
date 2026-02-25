use std::{backtrace::Backtrace, cell::RefCell};

use crate::{
    builder::BuiltFile,
    project::{self, Project},
    resolver::{self, FileAnnotations, ResolvedType},
    tokenizer,
    types::*,
};

pub struct Scope<'a> {
    pub return_type: Option<&'a ResolvedType<'a>>,
    // TODO: stuff
    pub context: resolver::Context<'a>,
}

pub struct Linter<'a> {
    pub proj: &'a Project,
    pub file: &'a BuiltFile,
    pub annotations: &'a FileAnnotations<'a>,
    pub class: Option<project::ClassRef<'a>>,

    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl<'a> Linter<'a> {
    pub fn new(
        proj: &'a Project,
        file: &'a BuiltFile,
        annotations: &'a FileAnnotations<'a>,
    ) -> Self {
        Self {
            proj,
            file,
            annotations,
            class: None,
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    pub(super) fn get_access_for(&self, class: project::ClassRef<'a>) -> tokenizer::AccessType {
        self.class
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.proj.get_access_for(current_class, class)
            })
    }

    pub fn push_diagnostic(&self, mut diagnostic: Diagnostic) {
        if cfg!(debug_assertions) {
            diagnostic.message += "\n";
            diagnostic.message += Backtrace::capture().to_string().as_str();
        }

        self.diagnostics.borrow_mut().push(diagnostic);
    }

    pub fn diagnostic_error(&self, message: String, range: Range) {
        self.push_diagnostic(Diagnostic {
            severity: Severity::Error,
            message,
            range,
        })
    }
    pub fn diagnostic_warning(&self, message: String, range: Range) {
        self.push_diagnostic(Diagnostic {
            severity: Severity::Warning,
            message,
            range,
        })
    }
    pub fn diagnostic_info(&self, message: String, range: Range) {
        self.push_diagnostic(Diagnostic {
            severity: Severity::Info,
            message,
            range,
        })
    }
    pub fn diagnostic_hint(&self, message: String, range: Range) {
        self.push_diagnostic(Diagnostic {
            severity: Severity::Hint,
            message,
            range,
        })
    }
}
