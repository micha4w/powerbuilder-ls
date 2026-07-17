use std::{backtrace::Backtrace, cell::RefCell};

use crate::{
    builder::BuiltFile,
    project::{self, Project},
    resolver::{self, FileAnnotations, ResolvedType},
    tokenizer,
    types::*,
};

pub struct Scope<'proj> {
    pub return_type: Option<&'proj ResolvedType<'proj>>,
    // TODO: stuff
    pub context: resolver::Context<'proj>,
}

pub struct Linter<'proj> {
    pub proj: &'proj Project,
    pub file: &'proj BuiltFile,
    pub annotations: &'proj FileAnnotations<'proj>,
    pub class: Option<project::ClassRef<'proj>>,

    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl<'proj> Linter<'proj> {
    pub fn new(
        proj: &'proj Project,
        file: &'proj BuiltFile,
        annotations: &'proj FileAnnotations<'proj>,
    ) -> Self {
        Self {
            proj,
            file,
            annotations,
            class: None,
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    pub(super) fn get_access_for(&self, class: project::ClassRef<'proj>) -> tokenizer::AccessType {
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
