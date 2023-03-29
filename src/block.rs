use std::ops::{Deref, DerefMut};
use crate::{
    dominators::{GraphNodeWorklist, GraphVisitOrder, PostOrderGraphNodeWorklist},
    kind::Kind,
    procedure::Procedure,
    sparse_collection::SparseElement,
    typ::Type,
    value::ValueId,
    variable::VariableId, jit::reg::Reg, utils::index_set::KeyIndex, effects::Effects,
};

pub struct BasicBlock {
    pub(crate) index: usize,
    pub(crate) values: Vec<ValueId>,
    pub(crate) predecessor_list: Vec<BlockId>,
    pub(crate) successor_list: Vec<FrequentBlock>,
    pub(crate) frequency: f64,
}

impl BasicBlock {
    pub fn new(index: usize, frequency: f64) -> Self {
        Self {
            index,
            values: Vec::new(),
            predecessor_list: Vec::new(),
            successor_list: Vec::new(),
            frequency,
        }
    }

    pub fn taken(&self) -> FrequentBlock {
        self.successor_list[0]
    }

    pub fn taken_mut(&mut self) -> &mut FrequentBlock {
        &mut self.successor_list[0]
    }

    pub fn not_taken(&self) -> FrequentBlock {
        self.successor_list[1]
    }

    pub fn not_taken_mut(&mut self) -> &mut FrequentBlock {
        &mut self.successor_list[1]
    }

    pub fn fallthrough(&self) -> FrequentBlock {
        self.successor_list[self.successor_list.len() - 1]
    }

    pub fn fallthrough_mut(&mut self) -> &mut FrequentBlock {
        let last = self.successor_list.len() - 1;
        &mut self.successor_list[last]
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn frequency(&self) -> f64 {
        self.frequency
    }

    pub fn append(&mut self, value: ValueId) {
        self.values.push(value);
    }

    pub fn append_non_terminal(&mut self, value: ValueId) {
        let last = self.values.last().unwrap();
        self.values.push(*last);
        let ix = self.values.len() - 2;
        self.values[ix] = value;
    }

    pub fn append_successor(&mut self, block: FrequentBlock) {
        self.successor_list.push(block);
    }

    pub fn set_successors(&mut self, target: FrequentBlock) {
        self.successor_list.clear();
        self.successor_list.push(target);
    }

    pub fn set_successors2(&mut self, target1: FrequentBlock, target2: FrequentBlock) {
        self.successor_list.clear();
        self.successor_list.push(target1);
        self.successor_list.push(target2);
    }

    pub fn replace_successor(&mut self, from: BlockId, to: BlockId) -> bool {
        let mut result = false;

        for successor in &mut self.successor_list {
            if successor.0 == from {
                successor.0 = to;
                result = true;
            }
        }

        result
    }

    pub fn predecessor_list(&self) -> &Vec<BlockId> {
        &self.predecessor_list
    }

    pub fn predecessor_list_mut(&mut self) -> &mut Vec<BlockId> {
        &mut self.predecessor_list
    }

    pub fn successor_list(&self) -> &Vec<FrequentBlock> {
        &self.successor_list
    }

    pub fn successor_list_mut(&mut self) -> &mut Vec<FrequentBlock> {
        &mut self.successor_list
    }

    pub fn add_predecessor(&mut self, predecessor: BlockId) -> bool {
        if self.predecessor_list.contains(&predecessor) {
            false
        } else {
            self.predecessor_list.push(predecessor);
            true
        }
    }

    pub fn remove_predecessor(&mut self, predecessor: BlockId) -> bool {
        let ix = self.predecessor_list.iter().position(|x| *x == predecessor);
        if let Some(ix) = ix {
            self.predecessor_list.remove(ix);
            true
        } else {
            false
        }
    }

    pub fn remove_successor(&mut self, successor: BlockId) -> bool {
        let ix = self.successor_list.iter().position(|x| x.0 == successor);
        if let Some(ix) = ix {
            self.successor_list.remove(ix);
            true
        } else {
            false
        }
    }

    pub fn remove_successor2(&mut self, successor1: BlockId, successor2: BlockId) -> bool {
        let ix = self
            .successor_list
            .iter()
            .position(|x| x.0 == successor1 || x.0 == successor2);
        if let Some(ix) = ix {
            self.successor_list.remove(ix);
            true
        } else {
            false
        }
    }

    pub fn replace_predecessor(&mut self, from: BlockId, to: BlockId) -> bool {
        let mut changed = false;

        changed |= self.remove_predecessor(from);
        changed |= self.add_predecessor(to);

        changed
    }

    pub(crate) fn fmt<W: std::fmt::Write>(&self, f: &mut W, proc: &Procedure) -> std::fmt::Result {
        write!(f, "BB{}: ; frequency = {}:\n", self.index, self.frequency)?;
        if !self.predecessor_list.is_empty() {
            write!(f, "  Predecessors: ")?;
            for (i, pred) in self.predecessor_list.iter().enumerate() {
                write!(f, "BB{}", pred.0)?;

                if i < self.predecessor_list.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            writeln!(f)?;
        }

        for value in &self.values {
            write!(f, "    ")?;
            let val = proc.value(*value);
            val.fmt(f, proc)?;
            writeln!(f)?;
        }

        if !self.successor_list.is_empty() {
            write!(f, "  Successors: ")?;

            if self.len() != 0 {
                proc.value(self.last().copied().unwrap())
                    .fmt_successors(f, proc, self)?;
            } else {
                for (i, succ) in self.successor_list.iter().enumerate() {
                    write!(f, "BB{}", succ.0 .0)?;

                    if i < self.successor_list.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

pub fn blocks_in_pre_order(block: BlockId, proc: &Procedure) -> Vec<BlockId> {
    let mut result = vec![];

    let mut worklist = GraphNodeWorklist::new();

    worklist.push(block);

    while let Some(block) = worklist.pop() {
        result.push(block);

        for successor in proc.block(block).successor_list() {
            worklist.push(successor.0);
        }
    }

    result
}

pub fn blocks_in_post_order(block: BlockId, proc: &Procedure) -> Vec<BlockId> {
    let mut result = vec![];

    let mut worklist = PostOrderGraphNodeWorklist::new();

    worklist.push(block, GraphVisitOrder::Pre);

    while let Some((block, order)) = worklist.pop() {
        match order {
            GraphVisitOrder::Pre => {
                worklist.push_post(block);
                for successor in proc.block(block).successor_list() {
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

impl Deref for BasicBlock {
    type Target = Vec<ValueId>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub usize);

impl BlockId {
    pub fn value(self, proc: &Procedure, index: usize) -> ValueId {
        proc.block(self)[index]
    }

    pub fn value_mut(self, proc: &mut Procedure, index: usize) -> &mut ValueId {
        &mut proc.block_mut(self)[index]
    }

    pub fn size(self, proc: &Procedure) -> usize {
        proc.block(self).len()
    }

    pub fn is_empty(self, proc: &Procedure) -> bool {
        proc.block(self).is_empty()
    }

    pub fn first(self, proc: &Procedure) -> Option<ValueId> {
        proc.block(self).first().copied()
    }

    pub fn last(self, proc: &Procedure) -> Option<ValueId> {
        proc.block(self).last().copied()
    }

    pub fn predecessor_list(self, proc: &Procedure) -> &[BlockId] {
        proc.block(self).predecessor_list()
    }

    pub fn successor_list(self, proc: &Procedure) -> &Vec<(BlockId, Frequency)> {
        proc.block(self).successor_list()
    }

    pub fn add_predecessor(self, proc: &mut Procedure, predecessor: BlockId) {
        proc.block_mut(self).add_predecessor(predecessor);
    }
}

impl KeyIndex for BlockId {
    fn index(&self) -> usize {
        self.0
    }
}

impl Default for BlockId {
    fn default() -> Self {
        Self(usize::MAX)
    }
}

impl Into<usize> for BlockId {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for BlockId {
    fn from(x: usize) -> Self {
        BlockId(x)
    }
}

impl SparseElement for BasicBlock {
    type Id = BlockId;

    fn id(&self) -> Self::Id {
        BlockId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Frequency {
    /// We don't have any hypothesis about the frequency of this control flow construct. This is
    /// the common case. We can still use basic block frequency in this case.
    Normal,
    /// We expect that this control flow construct will be reached super rarely. It's valid to
    /// perform optimizations that punish Rare code. Note that there will be situations where you
    /// have to somehow construct a new frequency class from a merging of multiple classes. When
    /// this happens, never choose Rare; always go with Normal. This is necessary because we
    /// really do punish Rare code very badly.
    Rare,
}

impl Default for Frequency {
    fn default() -> Self {
        Self::Normal
    }
}

pub fn max_frequency(a: Frequency, b: Frequency) -> Frequency {
    if a == Frequency::Normal {
        a
    } else {
        b
    }
}

pub type FrequentBlock = (BlockId, Frequency);

pub struct BasicBlockBuilder<'a> {
    pub func: &'a mut Procedure,
    pub block: BlockId,
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn new(func: &'a mut Procedure, block: BlockId) -> Self {
        BasicBlockBuilder { func, block }
    }

    pub fn append(&mut self, value: ValueId) {
        self.func.add_to_block(self.block, value);
    }

    pub fn add_int_constant<T>(
        &mut self,
        typ: Type,
        value: impl Into<i64>,
        next: impl FnOnce(&mut Self, ValueId) -> T,
    ) -> T {
        let value = self.func.add_int_constant(typ, value);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }
    pub fn add_bits_constant<T>(
        &mut self,
        typ: Type,
        value: impl Into<u64>,
        next: impl FnOnce(&mut Self, ValueId) -> T,
    ) -> T {
        let value = self.func.add_bits_constant(typ, value);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }


    pub fn add_binary<T>(
        &mut self,
        kind: impl Into<Kind>,
        lhs: ValueId,
        rhs: ValueId,
        next: impl FnOnce(&mut Self, ValueId) -> T,
    ) -> T {
        let value = self.func.add_binary(kind.into(), lhs, rhs);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }

    pub fn add_argument<T>(
        &mut self,
        typ: Type,
        reg: Reg,
        next: impl FnOnce(&mut Self, ValueId) -> T,
    ) -> T {
        let value = self.func.add_argument(typ, reg);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }

    pub fn add_return(&mut self, value: ValueId) {
        let val = self.func.add_return(value);
        self.func.add_to_block(self.block, val);
    }

    pub fn add_variable_get(
        &mut self,
        variable: VariableId,
        next: impl FnOnce(&mut Self, ValueId),
    ) {
        let value = self.func.add_variable_get(variable);
        self.func.add_to_block(self.block, value);
        next(self, value);
    }

    pub fn add_ccall<T>(&mut self, ret: Type, callee: ValueId, args: &[ValueId], effeects: Effects, next: impl FnOnce(&mut Self, ValueId) -> T) -> T {
        let value = self.func.add_ccall(ret, callee, args, effeects);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }

    pub fn add_variable_set<T>(
        &mut self,
        variable: VariableId,
        value: ValueId,
        next: impl FnOnce(&mut Self, ValueId) -> T,
    ) -> T {
        let value = self.func.add_variable_set(variable, value);
        self.func.add_to_block(self.block, value);
        next(self, value)
    }

    pub fn add_jump(&mut self, to: BlockId) {
        self.func.block_mut(self.block).successor_list.clear();
        let val = self.func.add_jump();
        self.func.add_to_block(self.block, val);
        self.func
            .block_mut(self.block)
            .set_successors((to, Frequency::Normal));
        self.func.block_mut(to).add_predecessor(self.block);
    }

    pub fn add_branch(
        &mut self,
        condition: ValueId,
        taken: BlockId,
        not_taken: (BlockId, Frequency),
    ) {
        self.func.block_mut(self.block).successor_list.clear();
        let val = self.func.add_branch(condition);
        self.func.add_to_block(self.block, val);
        self.func
            .block_mut(self.block)
            .set_successors2((taken, Frequency::Normal), not_taken);
        self.func.block_mut(not_taken.0).add_predecessor(self.block);
        self.func.block_mut(taken).add_predecessor(self.block);
    }
}

pub fn clear_predecessors(blocks: &mut Vec<BasicBlock>) {
    for block in blocks {
        block.predecessor_list.clear();
    }
}

pub fn recompute_predecessors(blocks: &mut Vec<BasicBlock>) {
    clear_predecessors(blocks);
    update_predecessors_after(BlockId(0), blocks)
}

pub fn update_predecessors_after(root: BlockId, blocks: &mut Vec<BasicBlock>) {
    let mut worklist = Vec::with_capacity(16);

    worklist.push(root);

    while let Some(block) = worklist.pop() {
        for (succ, _freq) in blocks[block.0].successor_list.clone().iter().copied() {
            if blocks[succ.0].add_predecessor(block) {
                worklist.push(succ);
            }
        }
    }
}

pub fn is_block_dead(block: &BasicBlock) -> bool {
    if block.index == usize::MAX {
        return true;
    }

    if block.index == 0 {
        return false;
    }

    block.predecessor_list.is_empty()
}
