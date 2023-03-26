use crate::{
    block::{BasicBlock, BlockId},
    procedure::Procedure,
    utils::insertion::{execute_insertions, Insertion},
};

pub struct GenericBlockInsertionSet {
    blocks: Vec<BlockId>,
    insertions: Vec<Insertion<BlockId>>,
}

impl GenericBlockInsertionSet {
    pub fn new(proc: &Procedure) -> Self {
        Self {
            blocks: proc.blocks.iter().map(|x| BlockId(x.index)).collect(),
            insertions: vec![],
        }
    }

    pub fn insert_insertion(&mut self, insertion: Insertion<BlockId>) {
        self.insertions.push(insertion);
    }

    pub fn insert(&mut self, proc: &mut Procedure, index: usize, frequency: f64) -> BlockId {
        let block = BasicBlock::new(usize::MAX, frequency);
        let id = BlockId(proc.blocks.len());
        proc.blocks.push(block);
        self.insert_insertion(Insertion::new(index, id));
        id
    }

    pub fn insert_before(
        &mut self,
        proc: &mut Procedure,
        before: BlockId,
        frequency: Option<f64>,
    ) -> BlockId {
        self.insert(
            proc,
            before.0,
            frequency.unwrap_or_else(|| proc.block(before).frequency()),
        )
    }

    pub fn insert_after(
        &mut self,
        proc: &mut Procedure,
        after: BlockId,
        frequency: Option<f64>,
    ) -> BlockId {
        self.insert(
            proc,
            after.0 + 1,
            frequency.unwrap_or_else(|| proc.block(after).frequency()),
        )
    }

    pub fn execute(&mut self, proc: &mut Procedure) -> bool {
        if self.insertions.is_empty() {
            return false;
        }
        // We allow insertions to be given to us in any order. So, we need to sort them before
        // running `execute_insertions`. We strongly prefer a stable sort and we want it to be
        // fast.
        self.insertions.sort();

        execute_insertions(&mut self.blocks, &mut self.insertions);

        self.blocks.retain(|block| block.0 != usize::MAX);

        for i in 0..self.blocks.len() {
            println!(
                "{} -> {:?} (from {})",
                i,
                self.blocks[i],
                proc.block(self.blocks[i]).index
            );
            proc.block_mut(self.blocks[i]).index = i;
        }

        true
    }
}
