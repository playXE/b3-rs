use std::ops::{Deref, DerefMut};

use tinyvec::TinyVec;

use crate::{block::Frequency, sparse_collection::SparseElement, dominators::{PostOrderGraphNodeWorklist, GraphVisitOrder}, utils::index_set::KeyIndex};

use super::{inst::Inst, code::Code};

pub struct BasicBlock {
    pub index: usize,
    pub insts: Vec<Inst>,
    pub successors: TinyVec<[(BasicBlockId, Frequency); 2]>,
    pub predecessors: TinyVec<[BasicBlockId; 2]>,
    pub frequency: f64,
}

impl Deref for BasicBlock {
    type Target = Vec<Inst>;

    fn deref(&self) -> &Self::Target {
        &self.insts
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.insts
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct BasicBlockId(pub usize);

impl KeyIndex for BasicBlockId {
    fn index(&self) -> usize {
        self.0
    }
}

impl Default for BasicBlockId {
    fn default() -> Self {
        Self(usize::MAX)
    }
}

impl From<usize> for BasicBlockId {
    fn from(index: usize) -> Self {
        BasicBlockId(index)
    }
}

impl From<BasicBlockId> for usize {
    fn from(id: BasicBlockId) -> Self {
        id.0
    }
}

impl SparseElement for BasicBlock {
    type Id = BasicBlockId;

    fn id(&self) -> Self::Id {
        BasicBlockId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}

pub fn update_predecessors_after(root: BasicBlockId, code: &mut Code<'_>) {
    let mut worklist = TinyVec::<[BasicBlockId; 16]>::new();

    worklist.push(root);

    while let Some(block) = worklist.pop() {
        for i in 0..code.block(block).successors.len() {
            let succ = code.block(block).successors[i].0;

            if !code.block(succ).predecessors.contains(&block) {
                code.block_mut(succ).predecessors.push(block);
                worklist.push(succ);
            }
        }
    }
}

pub fn blocks_in_post_order(block: BasicBlockId, code: &Code<'_>) -> Vec<BasicBlockId> {
    let mut result = vec![];

    let mut worklist = PostOrderGraphNodeWorklist::new();

    worklist.push(block, GraphVisitOrder::Pre);

    while let Some((block, order)) = worklist.pop() {
        match order {
            GraphVisitOrder::Pre => {
                worklist.push_post(block);
                for successor in code.block(block).successors.iter() {
                    worklist.push(successor.0, GraphVisitOrder::Pre);
                }
            }
            GraphVisitOrder::Post => {
                result.push(block);
            }
        }
    }

    result
}