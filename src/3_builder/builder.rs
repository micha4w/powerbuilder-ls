use std::{
    collections::{hash_map, HashMap, HashSet},
    fs,
    sync::Arc,
    time::Instant,
};

use encoding_rs_io::DecodeReaderBytes;
use ropey::Rope;

use super::{types::*, File};
use crate::{parser::Parser, types::*};

#[derive(Debug)]
pub struct Builder {
    pub load_requests: HashSet<IString>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            load_requests: HashSet::new(),
        }
    }

    pub(super) fn request_class_load(&mut self, ps_type: &PowerScriptType) {
        match ps_type {
            PowerScriptType::Complex(name) => {
                self.load_requests.insert(name.clone());
            }
            PowerScriptType::Array(inner) => self.request_class_load(inner),
            _ => {}
        }
    }
}
