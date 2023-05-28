use std::collections::HashSet;

use tinyvec::TinyVec;

use crate::{
    utils::{
        bitvector::BitVector, index_set::KeyIndex, likely_dense_integer_set::LikelyDenseIntegerSet,
    },
    BlockId, Procedure, ValueId,
};

pub struct LoopAnalysis {
    loops: Vec<Loop>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LoopId(pub usize);

impl KeyIndex for LoopId {
    fn index(&self) -> usize {
        self.0
    }
}

/// A loop in the control flow graph.
pub struct Loop {
    pub parent: Option<LoopId>,
    pub blocks: TinyVec<[BlockId; 4]>,
    pub subloops: Vec<LoopId>,
    pub block_set: HashSet<BlockId>,
}

impl LoopAnalysis {
    /// Return the nesting level of this loop.  An outer-most loop has depth 1,
    /// for consistency with loop depth values used for basic blocks, where depth
    /// 0 is used for blocks not inside any loops.
    pub fn get_loop_depth(&self, loop_: LoopId) -> usize {
        let mut d = 1;
        let mut cur_loop = self.loops[loop_.index()].parent;

        while let Some(loop_) = cur_loop {
            d += 1;
            cur_loop = self.loops[loop_.index()].parent;
        }

        d
    }

    pub fn get_header(&self, loop_: LoopId) -> BlockId {
        self.loops[loop_.index()].blocks[0]
    }

    /// Return the parent loop if it exists or None for top
    /// level loops.
    /// A loop is either top-level in a function (that is, it is not
    /// contained in any other loop) or it is entirely enclosed in
    /// some other loop.
    /// If a loop is top-level, it has no parent, otherwise its
    /// parent is the innermost loop in which it is enclosed.
    pub fn get_parent(&self, loop_: LoopId) -> Option<LoopId> {
        self.loops[loop_.index()].parent
    }

    /// Get the outermost loop in which this loop is contained.
    /// This may be the loop itself, if it already is the outermost loop.
    pub fn get_outermost_loop(&self, loop_: LoopId) -> LoopId {
        let mut outer_loop = loop_;
        while let Some(parent) = self.get_parent(outer_loop) {
            outer_loop = parent;
        }
        outer_loop
    }

    pub fn set_parent_loop(&mut self, loop_: LoopId, parent: Option<LoopId>) {
        self.loops[loop_.index()].parent = parent;
    }

    pub fn contains_loop(&self, loop_: LoopId, other: LoopId) -> bool {
        if loop_ == other {
            return true;
        }

        let parent = self.get_parent(other);
        if parent.is_none() {
            return false;
        }
        self.contains_loop(loop_, parent.unwrap())
    }

    /// Return true if the specified basic block is in this loop.
    pub fn contains_block(&self, loop_: LoopId, block: BlockId) -> bool {
        self.loops[loop_.index()].block_set.contains(&block)
    }

    pub fn contains_value(&mut self, loop_: LoopId, proc: &Procedure, val: ValueId) -> bool {
        let owner = proc.value(val).owner;

        if let Some(owner) = owner {
            return self.contains_block(loop_, owner);
        }

        false
    }

    pub fn get_sub_loops(&self, loop_: LoopId) -> &[LoopId] {
        &self.loops[loop_.index()].subloops
    }

    /// Return true if the loop does not contain any (natural) loops.
    pub fn is_innermost(&self, loop_: LoopId) -> bool {
        self.get_sub_loops(loop_).is_empty()
    }

    /// Return true if the loop does not have a parent (natural) loop
    // (i.e. it is outermost, which is the same as top-level).
    pub fn is_outermost(&self, loop_: LoopId) -> bool {
        self.get_parent(loop_).is_none()
    }

    pub fn get_blocks(&self, loop_: LoopId) -> &[BlockId] {
        &self.loops[loop_.index()].blocks
    }

    pub fn get_num_blocks(&self, loop_: LoopId) -> usize {
        self.loops[loop_.index()].blocks.len()
    }

    pub fn get_block_set(&self, loop_: LoopId) -> &HashSet<BlockId> {
        &self.loops[loop_.index()].block_set
    }

    pub fn get_block_set_mut(&mut self, loop_: LoopId) -> &mut HashSet<BlockId> {
        &mut self.loops[loop_.index()].block_set
    }

    pub fn get_blocks_mut(&mut self, loop_: LoopId) -> &mut TinyVec<[BlockId; 4]> {
        &mut self.loops[loop_.index()].blocks
    }
    /// True if terminator in the block can branch to another block that is
    /// outside of the current loop.
    /// - `block` must be inside the loop.
    pub fn is_loop_exiting(&self, proc: &Procedure, loop_: LoopId, block: BlockId) -> bool {
        assert!(
            self.contains_block(loop_, block),
            "Exiting block must be a part of the loop"
        );

        for succ in proc.block(block).successor_list().iter() {
            if !self.contains_block(loop_, succ.0) {
                return true;
            }
        }

        false
    }

    /// Returns true if `block` is a loop-latch.
    ///
    /// A latch is a block that contains a branch back to the header.
    /// This function is useful when there are multiple latches in a loop
    /// because [get_loop_latch](LoopAnalysis::get_loop_latch) returns None in that case.
    pub fn is_loop_latch(&self, proc: &Procedure, loop_: LoopId, block: BlockId) -> bool {
        assert!(
            self.contains_block(loop_, block),
            "block does not belong to the loop"
        );

        let header = self.get_header(loop_);

        proc.block(header).predecessor_list.contains(&block)
    }

    pub fn get_num_back_edges(&self, proc: &Procedure, loop_: LoopId) -> usize {
        let header = self.get_header(loop_);
        let mut num_back_edges = 0;

        for pred in proc.block(header).predecessor_list.iter() {
            if self.contains_block(loop_, *pred) {
                num_back_edges += 1;
            }
        }

        num_back_edges
    }

    pub fn get_loop_latches(&self, proc: &Procedure, loop_: LoopId) -> Vec<BlockId> {
        let header = self.get_header(loop_);
        let mut latches = Vec::new();

        for pred in proc.block(header).predecessor_list.iter() {
            if self.contains_block(loop_, *pred) {
                latches.push(*pred);
            }
        }

        latches
    }

    pub fn get_inner_loops_in_pre_order(&self, loop_: LoopId) -> Vec<LoopId> {
        let mut worklist = Vec::<LoopId>::new();

        worklist.extend(self.get_sub_loops(loop_).iter().rev());
        let mut pre_order_loops = Vec::new();

        while let Some(l) = worklist.pop() {
            // Sub-loops are stored in forward program order, but will process the
            // worklist backwards so append them in reverse order.
            worklist.extend(self.get_sub_loops(l).iter().rev());
            pre_order_loops.push(l);
        }

        pre_order_loops
    }

    pub fn get_loops_in_pre_order(&self, l: LoopId) -> Vec<LoopId> {
        let mut pre_order_loops = Vec::new();
        pre_order_loops.push(l);
        let v = self.get_inner_loops_in_pre_order(l);
        pre_order_loops.extend(v);
        pre_order_loops
    }

    pub fn add_child_loop(&mut self, l: LoopId, child: LoopId) {
        self.loops[l.index()].subloops.push(child);
        self.loops[child.index()].parent = Some(l);
    }

    pub fn remove_child_loop(&mut self, l: LoopId, child: LoopId) {
        let index = self.loops[l.index()]
            .subloops
            .iter()
            .position(|x| *x == child)
            .unwrap();
        self.loops[l.index()].subloops.remove(index);
        self.loops[child.index()].parent = None;
    }

    pub fn add_block_entry(&mut self, l: LoopId, block: BlockId) {
        self.loops[l.index()].blocks.push(block);
        self.loops[l.index()].block_set.insert(block);
    }

    /// interface to reverse `blocks[from, end of loop]` in this loop
    pub fn reverse_block(&mut self, l: LoopId, from: usize) {
        let blocks = self.get_blocks_mut(l);
        blocks[from..].reverse();
    }

    pub fn reserve_blocks(&mut self, l: LoopId, n: usize) {
        self.loops[l.index()].blocks.reserve(n);
    }

    /// This method is used to move `block` (which must be part of this loop) to be the
    /// loop header of the loop (the block that dominates all others).
    pub fn move_to_header(&mut self, l: LoopId, block: BlockId) {
        if self.at(l).blocks[0] == block {
            return;
        }

        for i in 0.. {
            if self.at(l).blocks[i] == block {
                self.at_mut(l).blocks[i] = self.at(l).blocks[0];
                self.at_mut(l).blocks[0] = block;
                return;
            }
        }
    }

    /// This removes the specified basic block from the current loop, updating the
    /// Blocks as appropriate. This does not update the mapping in the LoopAnalysis.
    pub fn remove_block_from_loop(&mut self, l: LoopId, block: BlockId) {
        let blocks = self.get_blocks_mut(l);
        let index = blocks.iter().position(|x| *x == block).unwrap();
        blocks.remove(index);
        self.get_block_set_mut(l).remove(&block);
    }

    pub fn at(&self, l: LoopId) -> &Loop {
        &self.loops[l.index()]
    }

    pub fn at_mut(&mut self, l: LoopId) -> &mut Loop {
        &mut self.loops[l.index()]
    }
}
