use std::sync::Arc;

use tower_lsp::lsp_types::OneOf;

use super::types::*;
use crate::{builder, parser, project, types::*};

pub struct AnnotationTree<'proj> {
    pub range: &'proj Range,
    pub annotation: Option<Annotation<'proj>>,
    pub children: Vec<AnnotationTree<'proj>>,
}

impl<'a, 'proj> AnnotationTree<'proj> {
    pub fn add_empty_child(&'a mut self, range: &'proj Range) -> &'a mut AnnotationTree<'proj> {
        self.children.push(AnnotationTree {
            range,
            annotation: None,
            children: Vec::new(),
        });
        self.children.last_mut().unwrap()
    }

    pub fn add_child(
        &'a mut self,
        range: &'proj Range,
        annotation: Annotation<'proj>,
    ) -> &'a mut Annotation<'proj> {
        self.add_empty_child(range).annotation.insert(annotation)
    }

    fn find_exact(&'proj self, range: &Range) -> Option<&'proj Annotation<'proj>> {
        if self.range == range {
            return self.annotation.as_ref();
        }

        if self.range.contains_range(&range) {
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
        &'proj self,
        lvalue: &parser::LValue,
    ) -> Option<&'proj ResolvedLValue<'proj>> {
        self.find_exact(&lvalue.range)
            .and_then(|anot| anot.lvalue.as_ref())
    }

    pub fn for_range(&'proj self, range: &Range) -> Option<&'proj ResolvedType<'proj>> {
        self.find_exact(range).map(|anot| &anot.resolved_type)
    }
}

pub struct FileAnnotations<'proj> {
    pub top_levels: Vec<AnnotationTree<'proj>>,
}

impl<'proj> FileAnnotations<'proj> {
    // Returns Found::No if the lvalue failed to resolve (missing class/function) or the range is wrong
    // TODO(annotations): panic when the range does not exist?
    pub fn lvalue(&'proj self, lvalue: &parser::LValue) -> Found<&'proj ResolvedLValue<'proj>> {
        for top_level in &self.top_levels {
            if let Some(anot) = top_level.for_lvalue(lvalue) {
                return Found::Yes(anot);
            }
        }

        Found::No
    }

    pub fn datatype(&'proj self, range: &Range) -> Option<&'proj ResolvedType<'proj>> {
        for top_level in &self.top_levels {
            if let Some(anot) = top_level.for_range(range) {
                return Some(anot);
            }
        }
        None
    }

    pub fn must_type(&'proj self, range: &Range) -> &'proj ResolvedType<'proj> {
        self.datatype(range)
            .expect(&format!("Expected annotation for range {:?}", range))
    }
}
