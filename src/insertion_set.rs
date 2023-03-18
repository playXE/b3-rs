use std::collections::HashMap;

use crate::{value::ValueId, typ::Type, procedure::Procedure, block::BlockId};

pub struct Insertion {
    pub index: usize,
    pub value: ValueId,
}

impl PartialEq for Insertion {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for Insertion {}

impl PartialOrd for Insertion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Insertion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

pub fn execute_insertions(target: &mut Vec<ValueId>, insertions: &mut Vec<Insertion>) -> usize {
    let num_insertions = insertions.len();

    if num_insertions == 0 {
        return 0;
    }
    
    target.resize(target.len() + num_insertions, ValueId(usize::MAX));

    let mut last_index = target.len();

    for index_in_insertions in (0..num_insertions).rev() {
        let first_index = insertions[index_in_insertions].index + index_in_insertions;
        let index_offset = index_in_insertions + 1;

        let mut i = last_index;

        while {
            i -= 1;
            i > first_index
        } {
            target[i] = std::mem::replace(&mut target[i - index_offset], ValueId(usize::MAX));
        }

        target[first_index] = insertions[index_in_insertions].value;

        last_index = first_index;
    }

    insertions.truncate(0);

    num_insertions
}

pub struct InsertionSet {
    insertions: Vec<Insertion>,
    bottom_for_type: HashMap<Type, ValueId>
}

impl InsertionSet {
    pub fn new() -> Self {
        Self {
            insertions: Vec::new(),
            bottom_for_type: HashMap::new()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.insertions.is_empty()
    }

    pub fn append_insertion(&mut self, insertion: Insertion) {
        self.insertions.push(insertion);
    }

    pub fn insert_value(&mut self, index: usize, value: ValueId) -> ValueId {
        self.insertions.push(Insertion { index, value });
        value
    }

    pub fn insert_int_constant(&mut self, index: usize, typ: Type, value: i64, proc: &mut Procedure) -> ValueId {
        let x = proc.add_int_constant(typ, value);
        self.insert_value(index, x)
    }

    pub fn insert_int_constant_like(&mut self, index: usize, like: ValueId, value: i64, proc: &mut Procedure) -> ValueId {
        let like = proc.value(like).typ();
        let x = proc.add_int_constant(like, value);
        self.insert_value(index, x)
    }

    pub fn insert_bottom(&mut self, index: usize, typ: Type, proc: &mut Procedure) -> ValueId {
        let x = *self.bottom_for_type.entry(typ).or_insert_with(|| proc.add_bits_constant(typ, 0u64));
        self.insert_value(index, x)
    }

    pub fn insert_bottom_like(&mut self, index: usize, like: ValueId, proc: &mut Procedure) -> ValueId {
        let like = proc.value(like).typ();
        let x = *self.bottom_for_type.entry(like).or_insert_with(|| proc.add_bits_constant(like, 0u64));
        self.insert_value(index, x)
    }

    pub fn insert_clone(&mut self, index: usize, value: ValueId, proc: &mut Procedure) -> ValueId {
        let x = proc.clone(value);
        self.insert_value(index, x)
    }

    pub fn execute(&mut self, proc: &mut Procedure, block: BlockId)  {

        for insertion in self.insertions.iter() {
            proc.value_mut(insertion.value).owner = Some(block);
        }
        self.insertions.sort();
        execute_insertions(&mut proc.block_mut(block).values, &  mut self.insertions);

        self.bottom_for_type.clear();
    }
}