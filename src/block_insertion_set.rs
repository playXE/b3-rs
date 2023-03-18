use crate::{block::BlockId, procedure::Procedure};

#[derive(Debug, Clone, Copy)]
pub struct BlockInsertion<T: Copy> {
    pub index: usize,
    pub block: T,
}

impl<T: Copy> PartialEq for BlockInsertion<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T: Copy> Eq for BlockInsertion<T> {}

impl<T: Copy> PartialOrd for BlockInsertion<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Copy> Ord for BlockInsertion<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

pub fn execute_insertions<T: Copy>(
    target: &mut Vec<T>,
    insertions: &mut Vec<BlockInsertion<T>>,
) -> usize {
    let num_insertions = insertions.len();

    if num_insertions == 0 {
        return 0;
    }

    target.reserve(num_insertions);

    let mut last_index = target.len();

    for index_in_insertions in (0..num_insertions).rev() {
        let first_index = insertions[index_in_insertions].index + index_in_insertions;
        let index_offset = index_in_insertions + 1;

        let mut i = last_index;

        while {
            i -= 1;
            i > first_index
        } {
            target[i] = target[i - index_offset];
        }

        target[first_index] = insertions[index_in_insertions].block;

        last_index = first_index;
    }

    insertions.truncate(0);

    num_insertions
}

pub struct GenericBlockInsertionSet {
    blocks: Vec<BlockId>,
    insertions: Vec<BlockInsertion<BlockId>>,
}

impl GenericBlockInsertionSet {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            insertions: vec![],
        }
    }

    pub fn insert(&mut self, insertion: BlockInsertion<BlockId>) {
        self.insertions.push(insertion);
    }

    pub fn insert_new(&mut self, index: usize) -> BlockId {
        self.insert(BlockInsertion {
            index,
            block: BlockId(usize::MAX),
        });
        BlockId(usize::MAX)
    }

    pub fn insert_before(&mut self, before: BlockId) -> BlockId {
        self.insert_new(before.0)
    }

    pub fn insert_after(&mut self, after: BlockId) -> BlockId {
        self.insert_new(after.0 + 1)
    }

    pub fn execute(&mut self, proc: &mut Procedure) -> bool {
        if self.insertions.is_empty() {
            return false;
        }

        self.insertions.sort();

        execute_insertions(&mut self.blocks, &mut self.insertions);

        for i in 0..self.blocks.len() {
            let block = self.blocks[i];
            proc.block_mut(block).index = i;
        }

        true
    }
}
