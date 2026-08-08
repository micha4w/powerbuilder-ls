use std::sync::Arc;

use tower_lsp::lsp_types::OneOf;

use super::types::*;
use crate::{builder, parser, solution, types::*};

pub struct AnnotationTree<'sol> {
    pub range: &'sol Range,
    pub annotation: Option<Annotation<'sol>>,
    pub children: Vec<AnnotationTree<'sol>>,
}

impl<'a, 'sol> AnnotationTree<'sol> {
    pub fn add_empty_child(&'a mut self, range: &'sol Range) -> &'a mut AnnotationTree<'sol> {
        self.children.push(AnnotationTree {
            range,
            annotation: None,
            children: Vec::new(),
        });
        self.children.last_mut().unwrap()
    }

    pub fn add_child(
        &'a mut self,
        range: &'sol Range,
        annotation: Annotation<'sol>,
    ) -> &'a mut Annotation<'sol> {
        self.add_empty_child(range).annotation.insert(annotation)
    }

    fn find_exact(&'sol self, range: &Range) -> Option<&'sol Annotation<'sol>> {
        if self.range == range {
            return self.annotation.as_ref();
        }

        if self.range.contains_range(&range) {
            // TODO(perf): binary search (wont work yet because the items are not ordered)
            for child in &self.children {
                let anot = child.find_exact(range);
                if anot.is_some() {
                    return anot;
                }
            }
        }

        None
    }

    pub fn for_lvalue(
        &'sol self,
        lvalue: &parser::LValue,
    ) -> Option<&'sol ResolvedLValue<'sol>> {
        self.find_exact(&lvalue.range)
            .and_then(|anot| anot.lvalue.as_ref())
    }

    pub fn for_range(&'sol self, range: &Range) -> Option<&'sol ResolvedType<'sol>> {
        self.find_exact(range).map(|anot| &anot.resolved_type)
    }
}

pub struct FileAnnotations<'sol> {
    pub top_levels: Vec<AnnotationTree<'sol>>,
}

impl<'sol> FileAnnotations<'sol> {
    // Returns Found::No if the lvalue failed to resolve (missing class/function) or the range is wrong
    // TODO(annotations): panic when the range does not exist?
    pub fn lvalue(&'sol self, lvalue: &parser::LValue) -> Found<&'sol ResolvedLValue<'sol>> {
        for top_level in &self.top_levels {
            if let Some(anot) = top_level.for_lvalue(lvalue) {
                return Found::Yes(anot);
            }
        }

        Found::No
    }

    pub fn datatype(&'sol self, range: &Range) -> Option<&'sol ResolvedType<'sol>> {
        for top_level in &self.top_levels {
            if let Some(anot) = top_level.for_range(range) {
                return Some(anot);
            }
        }
        None
    }

    pub fn must_type(&'sol self, range: &Range) -> &'sol ResolvedType<'sol> {
        self.datatype(range)
            .expect(&format!("Expected annotation for range {:?}", range))
    }
}
