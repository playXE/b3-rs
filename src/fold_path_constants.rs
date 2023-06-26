#![allow(dead_code, unused_variables)]
use crate::{insertion_set::InsertionSet, BlockId, Procedure, ValueId};

/// Does very basic simplification of uses of values that were branched on by a dominating branch.
pub fn fold_path_constants(proc: &mut Procedure) {}

struct FoldPathConstants<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
}

impl<'a> FoldPathConstants<'a> {
    fn new(proc: &'a mut Procedure) -> Self {
        Self {
            proc,
            insertion_set: InsertionSet::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Override {
    block: Option<BlockId>,
    has_value: bool,
    is_non_zero: bool,
    value: i64,
    value_node: ValueId,
}
