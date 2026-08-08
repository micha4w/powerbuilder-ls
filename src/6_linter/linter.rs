use std::{backtrace::Backtrace, cell::RefCell};

use crate::{
    builder::BuiltFile,
    solution::{self, Solution},
    resolver::{self, FileAnnotations, ResolvedType},
    tokenizer,
    types::*,
};

pub struct Scope<'sol> {
    pub return_type: Option<&'sol ResolvedType<'sol>>,
    // TODO: stuff
    pub context: resolver::Context<'sol>,
}

pub struct Linter<'sol> {
    pub sol: &'sol Solution,
    pub file: &'sol BuiltFile,
    pub annotations: &'sol FileAnnotations<'sol>,
    pub class: Option<solution::ClassRef<'sol>>,

    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl<'sol> Linter<'sol> {
    pub fn new(
        sol: &'sol Solution,
        file: &'sol BuiltFile,
        annotations: &'sol FileAnnotations<'sol>,
    ) -> Self {
        Self {
            sol,
            file,
            annotations,
            class: None,
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    pub(super) fn get_access_for(&self, class: solution::ClassRef<'sol>) -> tokenizer::AccessType {
        self.class
            .map_or(tokenizer::AccessType::PUBLIC, |current_class| {
                self.sol.get_access_for(current_class, class)
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
