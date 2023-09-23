use tinyvec::tiny_vec;

use crate::{
    air::stack_slot::StackSlotId,
    analysis::dominators::{GraphNodeWorklist, GraphVisitOrder, PostOrderGraphNodeWorklist},
    effects::Effects,
    jit::reg::Reg,
    opcode::Opcode,
    patchpoint_value::PatchpointValue,
    procedure::Procedure,
    sparse_collection::SparseElement,
    stackmap_value::StackMapValue,
    typ::Type,
    utils::index_set::KeyIndex,
    value::{NumChildren, Value, ValueData, ValueId},
    variable::VariableId,
    ConstrainedValue, ValueRep, ValueRepKind,
};
use std::ops::{Deref, DerefMut, Range};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Frequency {
    /// We don't have any hypothesis about the frequency of this control flow construct. This is
    /// the common case. We can still use basic block frequency in this case.
    Normal = 1,
    /// We expect that this control flow construct will be reached super rarely. It's valid to
    /// perform optimizations that punish Rare code. Note that there will be situations where you
    /// have to somehow construct a new frequency class from a merging of multiple classes. When
    /// this happens, never choose Rare; always go with Normal. This is necessary because we
    /// really do punish Rare code very badly.
    Rare = 0,
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

/// A builder for basic blocks.
pub struct BasicBlockBuilder<'a> {
    pub block: BlockId,
    pub procedure: &'a mut Procedure,
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn new(procedure: &'a mut Procedure, block: BlockId) -> Self {
        Self { block, procedure }
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.block = block;
    }

    pub fn add_value(&mut self, value: ValueId) {
        self.procedure.add_to_block(self.block, value);
    }

    pub fn phi(&mut self, typ: Type) -> ValueId {
        let value = self.procedure.add(Value::new(
            Opcode::Phi,
            typ,
            NumChildren::Zero,
            &[],
            ValueData::None,
        ));
        self.add_value(value);
        value
    }

    pub fn upsilon(&mut self, input: ValueId, phi: Option<ValueId>) -> ValueId {
        let value = self.procedure.add(Value::new(
            Opcode::Upsilon,
            Type::Void,
            NumChildren::One,
            &[input],
            ValueData::Upsilon(phi),
        ));
        self.add_value(value);
        value
    }

    pub fn frame_pointer(&mut self) -> ValueId {
        let value = self.procedure.add(Value::new(
            Opcode::FramePointer,
            Type::Int64,
            NumChildren::Zero,
            &[],
            ValueData::None,
        ));

        self.add_value(value);
        value
    }

    pub fn entry_switch(&mut self, blocks: &[(BlockId, Frequency)]) {
        assert!(
            self.procedure.num_entrypoints <= 1,
            "EntrySwitch already added"
        );
        let value = self.procedure.add(Value::new(
            Opcode::EntrySwitch,
            Type::Void,
            NumChildren::Zero,
            &[],
            ValueData::None,
        ));

        self.add_value(value);
        self.procedure.num_entrypoints = blocks.len();
        self.procedure.block_mut(self.block).successor_list = blocks.to_vec();
        for (block, _) in blocks.iter() {
            self.procedure.block_mut(*block).add_predecessor(self.block);
        }
    }

    /// Get stack slot base pointer.
    pub fn slot_base(&mut self, slot: StackSlotId) -> ValueId {
        let x = self.procedure.add(Value::new(
            Opcode::SlotBase,
            Type::Int64,
            NumChildren::Zero,
            &[],
            ValueData::SlotBase(slot),
        ));
        self.add_value(x);
        x
    }

    /// Add a new 32-bit integer constant.
    pub fn const32(&mut self, val: i32) -> ValueId {
        let x = self.procedure.add_int_constant(Type::Int32, val);
        self.add_value(x);
        x
    }

    /// Add a new 64-bit integer constant.
    pub fn const64(&mut self, val: i64) -> ValueId {
        let x = self.procedure.add_int_constant(Type::Int64, val);
        self.add_value(x);
        x
    }

    /// Add a new f32 constant.
    pub fn const_float(&mut self, val: f32) -> ValueId {
        let x = self
            .procedure
            .add_bits_constant(Type::Float, val.to_bits() as u64);
        self.add_value(x);
        x
    }

    /// Add a new f64 constant.
    pub fn const_double(&mut self, val: f64) -> ValueId {
        let x = self
            .procedure
            .add_bits_constant(Type::Double, val.to_bits());
        self.add_value(x);
        x
    }

    /// Access to a variable. It emits `Get` opcode but later it is lowered
    /// to SSA form.
    pub fn var_get(&mut self, var: VariableId) -> ValueId {
        let x = self.procedure.add_variable_get(var);
        self.add_value(x);
        x
    }

    //// Set a variable. It emits `Set` opcode but later it is lowered
    /// to SSA form.
    pub fn var_set(&mut self, var: VariableId, value: ValueId) {
        let x = self.procedure.add_variable_set(var, value);
        self.add_value(x);
    }

    /// Same as [slot_base](BasicBlockBuilder::slot_base). It is here only for convenience.
    pub fn stack_addr(&mut self, stack_slot: StackSlotId) -> ValueId {
        let value = Value::new(
            Opcode::SlotBase,
            Type::Int64,
            NumChildren::Zero,
            &[],
            ValueData::SlotBase(stack_slot),
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Load 8-bit value from memory and zero-extend it to 32-bit.
    pub fn load8z(
        &mut self,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) -> ValueId {
        let value = Value::new(
            Opcode::Load8Z,
            Type::Int32,
            NumChildren::One,
            &[ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Load 8-bit value from memory and sign-extend it to 32-bit.
    pub fn load8s(
        &mut self,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) -> ValueId {
        let value = Value::new(
            Opcode::Load8S,
            Type::Int32,
            NumChildren::One,
            &[ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Load 16-bit value from memory and zero-extend it to 32-bit.
    pub fn load16z(
        &mut self,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) -> ValueId {
        let value = Value::new(
            Opcode::Load16Z,
            Type::Int32,
            NumChildren::One,
            &[ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }
    /// Load 16-bit value from memory and sign-extend it to 32-bit.
    pub fn load16s(
        &mut self,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) -> ValueId {
        let value = Value::new(
            Opcode::Load16S,
            Type::Int32,
            NumChildren::One,
            &[ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Load value from memory.
    ///
    /// # Parameters
    /// - `ty` - type of the value to load
    /// - `ptr` - pointer to the memory location
    /// - `offset` - offset from the pointer
    /// - `range` - range of the memory location that is accessed by this load
    /// - `fence_range` - range for atomic fence (TODO: document)
    pub fn load(
        &mut self,
        ty: Type,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) -> ValueId {
        let value = Value::new(
            Opcode::Load,
            ty,
            NumChildren::One,
            &[ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Store value to memory.
    ///
    /// # Parameters
    ///
    /// - `value` - value to store
    /// - `ptr` - pointer to the memory location
    /// - `offset` - offset from the pointer
    /// - `range` - range of the memory location that is accessed by this store
    /// - `fence_range` - range for atomic fence (TODO: document)
    pub fn store(
        &mut self,
        value: ValueId,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) {
        let value = Value::new(
            Opcode::Store,
            Type::Void,
            NumChildren::Two,
            &[value, ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
    }

    /// Store 8-bit value to memory.
    pub fn store8(
        &mut self,
        value: ValueId,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) {
        assert!(self.procedure.value(value).typ() == Type::Int32);
        let value = Value::new(
            Opcode::Store8,
            Type::Void,
            NumChildren::Two,
            &[value, ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
    }

    /// Store 16-bit value to memory.
    pub fn store16(
        &mut self,
        value: ValueId,
        ptr: ValueId,
        offset: i32,
        range: Option<Range<usize>>,
        fence_range: Option<Range<usize>>,
    ) {
        assert!(self.procedure.value(value).typ() == Type::Int32);
        let value = Value::new(
            Opcode::Store16,
            Type::Void,
            NumChildren::Two,
            &[value, ptr],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
    }

    /// Create binary operation. This asserts that the types of the operands are the same and
    /// `op` is a binary operation.
    pub fn binary(&mut self, op: Opcode, lhs: ValueId, rhs: ValueId) -> ValueId {
        assert!(op.is_binary());
        assert!(self.procedure.value(lhs).typ() == self.procedure.value(rhs).typ());
        let typ = self.procedure.value(lhs).typ();
        let value = Value::new(op, typ, NumChildren::Two, &[lhs, rhs], ValueData::None);

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Return absolute value of a floating point value. The type of `value`
    /// must be `Type::Float` or `Type::Double`.
    pub fn abs(&mut self, value: ValueId) -> ValueId {
        assert!(self.procedure.value(value).typ().is_float());
        let value = Value::new(
            Opcode::Abs,
            self.procedure.value(value).typ(),
            NumChildren::One,
            &[value],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Return ceiling of a floating point value. The type of `value`
    /// must be `Type::Float` or `Type::Double`.
    pub fn ceil(&mut self, value: ValueId) -> ValueId {
        assert!(self.procedure.value(value).typ().is_float());
        let value = Value::new(
            Opcode::Ceil,
            self.procedure.value(value).typ(),
            NumChildren::One,
            &[value],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Return floor of a floating point value. The type of `value`
    /// must be `Type::Float` or `Type::Double`.
    pub fn floor(&mut self, value: ValueId) -> ValueId {
        assert!(self.procedure.value(value).typ().is_float());
        let value = Value::new(
            Opcode::Floor,
            self.procedure.value(value).typ(),
            NumChildren::One,
            &[value],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Return square root of a floating point value. The type of `value`
    /// must be `Type::Float` or `Type::Double`.
    pub fn sqrt(&mut self, value: ValueId) -> ValueId {
        assert!(self.procedure.value(value).typ().is_float());
        let value = Value::new(
            Opcode::Sqrt,
            self.procedure.value(value).typ(),
            NumChildren::One,
            &[value],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Bitwise cast `src` to `typ`.
    pub fn bitwise_cast(&mut self, typ: Type, src: ValueId) -> ValueId {
        assert!(typ.is_int() || typ.is_float());
        assert!(
            self.procedure.value(src).typ().is_int() || self.procedure.value(src).typ().is_float()
        );
        let value = Value::new(
            Opcode::BitwiseCast,
            typ,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Sign extends 8-bit value to 32-bit value.
    ///
    /// Takes and returns Int32 value.
    pub fn sext8(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::SExt8,
            Type::Int32,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Sign extends 16-bit value to 32-bit value.
    ///
    /// Takes and returns Int32 value.
    pub fn sext16(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::SExt16,
            Type::Int32,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Sign extends 8-bit value to 64-bit value.
    ///
    /// Takes Int32 value and returns Int64 value.
    pub fn sext8to64(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::SExt8To64,
            Type::Int64,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Sign extends 16-bit value to 64-bit value.
    ///
    /// Takes Int32 value and returns Int64 value.
    pub fn sext16to64(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::SExt16To64,
            Type::Int64,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Sign extends 32-bit value to 64-bit value.
    ///
    /// Takes Int32 value and returns Int64 value.
    pub fn sext32(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::SExt32,
            Type::Int64,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Zero extends 32-bit value to 32-bit value.
    ///
    /// Takes Int32 value and returns Int64 value.
    pub fn zext32(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Int32);
        let value = Value::new(
            Opcode::ZExt32,
            Type::Int64,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Truncates floating point value to integer value.
    ///
    /// Takes Double or Float value and returns Int32 or Int64 value.
    pub fn trunc(&mut self, src: ValueId) -> ValueId {
        let dest_ty = match self.procedure.value(src).typ() {
            Type::Int64 => Type::Int32,
            Type::Double => Type::Float,
            _ => panic!("Invalid type for trunc"),
        };

        let value = Value::new(
            Opcode::Trunc,
            dest_ty,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts integer value to floating point value.
    pub fn i2d(&mut self, src: ValueId) -> ValueId {
        assert!(
            self.procedure.value(src).typ() == Type::Int32
                || self.procedure.value(src).typ() == Type::Int64
        );
        let value = Value::new(
            Opcode::IToD,
            Type::Double,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts integer value to floating point value.
    pub fn i2f(&mut self, src: ValueId) -> ValueId {
        assert!(
            self.procedure.value(src).typ() == Type::Int32
                || self.procedure.value(src).typ() == Type::Int64
        );
        let value = Value::new(
            Opcode::IToF,
            Type::Float,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts floating point value to integer value.
    pub fn f2i(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Float);
        let value = Value::new(
            Opcode::FToI,
            Type::Int32,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts double precision floating point value to integer value.
    pub fn d2i(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Double);
        let value = Value::new(
            Opcode::DToI,
            Type::Int64,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts float to double.
    pub fn float_to_double(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Float);
        let value = Value::new(
            Opcode::FloatToDouble,
            Type::Double,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Converts double to float.
    pub fn double_to_float(&mut self, src: ValueId) -> ValueId {
        assert!(self.procedure.value(src).typ() == Type::Double);
        let value = Value::new(
            Opcode::DoubleToFloat,
            Type::Float,
            NumChildren::One,
            &[src],
            ValueData::None,
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }
    /// This is a regular ordinary C function call, using the system C calling convention. Make sure
    /// that the arguments are passed using the right types. The first argument is the callee.
    pub fn ccall(
        &mut self,
        ret: Type,
        callee: ValueId,
        args: &[ValueId],
        effects: Effects,
    ) -> ValueId {
        let value = Value::new(
            Opcode::CCall,
            ret,
            NumChildren::VarArgs,
            std::iter::once(callee)
                .chain(args.iter().cloned())
                .collect::<Vec<_>>()
                .as_slice(),
            ValueData::CCallValue(effects),
        );

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

    /// Jump to a block.
    ///
    /// `target` is the block to jump to. It is allowed to be `None`,
    /// you can patch the jump later with invoking [`Procedure::add_successor`](super::Procedure::add_successor).
    pub fn jump(&mut self, target: Option<BlockId>) {
        let value = Value::new(
            Opcode::Jump,
            Type::Void,
            NumChildren::Zero,
            &[],
            ValueData::None,
        );

        if let Some(target) = target {
            self.procedure
                .block_mut(self.block)
                .successor_list
                .push((target, Frequency::Normal));

            self.procedure
                .block_mut(target)
                .predecessor_list
                .push(self.block);
        }

        let id = self.procedure.add(value);

        self.add_value(id);
    }

    pub fn switch(&mut self, on: ValueId) -> ValueId {
        assert!(
            self.procedure.value(on).typ().is_int(),
            "switch value must be an integer"
        );
        let value = Value::new(
            Opcode::Switch,
            Type::Void,
            NumChildren::One,
            &[on],
            ValueData::Switch(vec![]),
        );

        let id = self.procedure.add(value);

        self.add_value(id);
        id
    }

    /// Conditional branch on `on` value.
    ///
    /// If `on` is non-zero, jump to `taken` block, otherwise jump to `not_taken` block.
    ///
    /// `not_taken` is a tuple of block id and frequency. The frequency is used for branch prediction and block
    /// order optimization.
    pub fn branch(&mut self, on: ValueId, taken: BlockId, not_taken: (BlockId, Frequency)) {
        let value = Value::new(
            Opcode::Branch,
            Type::Void,
            NumChildren::One,
            &[on],
            ValueData::None,
        );

        self.procedure
            .block_mut(self.block)
            .successor_list
            .push((taken, Frequency::Normal));
        self.procedure
            .block_mut(self.block)
            .successor_list
            .push(not_taken);

        self.procedure
            .block_mut(taken)
            .predecessor_list
            .push(self.block);
        self.procedure
            .block_mut(not_taken.0)
            .predecessor_list
            .push(self.block);

        let value = self.procedure.add(value);

        self.add_value(value);
    }

    /// Return from the current function.
    ///
    /// If `None` is passed this is a void return, otherwise the value is returned.
    pub fn return_(&mut self, value: Option<ValueId>) {
        let args = if value.is_none() {
            vec![]
        } else {
            vec![value.unwrap()]
        };
        let value = Value::new(
            Opcode::Return,
            Type::Void,
            value.map(|_| NumChildren::One).unwrap_or(NumChildren::Zero),
            &args,
            ValueData::None,
        );

        let value = self.procedure.add(value);

        self.add_value(value);
    }

    /// Create patchpoint that returns `typ`.
    ///
    /// Fill in the patchpoint using `stackmap_*` and `patchpoint_*` methods on [`Procedure`](super::Procedure).
    pub fn patchpoint(&mut self, typ: Type) -> ValueId {
        let value = Value::new(
            Opcode::Patchpoint,
            typ,
            NumChildren::Zero,
            &[],
            ValueData::Patchpoint(PatchpointValue {
                base: StackMapValue {
                    reps: vec![],
                    generator: None,
                    early_clobbered: Default::default(),
                    late_clobbered: Default::default(),
                    used_registers: Default::default(),
                },
                effects: Effects::for_call(),
                result_constraints: tiny_vec!([ValueRep; 1] =>
                    if typ == Type::Void
                    {
                         ValueRep::new(ValueRepKind::WarmAny)
                    } else {
                        ValueRep::new(ValueRepKind::SomeRegister)
                }),
                num_fp_scratch_registers: 0,
                num_gp_scratch_registers: 0,
            }),
        );

        let value = self.procedure.add(value);
        self.add_value(value);
        value
    }

    /// Exit check. Works for T = Int32 and Int64.
    ///
    /// Fill in the stackmap using `stackmap_*` methods on [`Procedure`](super::Procedure).
    pub fn check(&mut self, predicate: ValueId) -> ValueId {
        let value = Value::new(
            Opcode::Check,
            Type::Void,
            NumChildren::One,
            &[predicate],
            ValueData::StackMap(StackMapValue {
                reps: vec![],
                generator: None,
                early_clobbered: Default::default(),
                late_clobbered: Default::default(),
                used_registers: Default::default(),
            }),
        );

        let value = self.procedure.add(value);
        self.procedure.stackmap_append_constrained(
            value,
            ConstrainedValue::new(predicate, ValueRep::new(ValueRepKind::WarmAny)),
        );
        self.add_value(value);
        value
    }

    /// Argument access. `Reg` is register where the argument is passed, `Type` is the type of the argument.
    pub fn argument(&mut self, reg: Reg, typ: Type) -> ValueId {
        if reg.is_gpr() {
            assert!(typ.is_int());
        } else {
            assert!(typ.is_float());
        }

        let value = Value::new(
            Opcode::ArgumentReg,
            typ,
            NumChildren::Zero,
            &[],
            ValueData::Argument(reg),
        );

        let value = self.procedure.add(value);

        self.add_value(value);

        value
    }

    /// Identity value.
    pub fn identity(&mut self, value: ValueId) -> ValueId {
        let value = Value::new(
            Opcode::Identity,
            self.procedure.value(value).typ(),
            NumChildren::One,
            &[value],
            ValueData::None,
        );

        let value = self.procedure.add(value);

        self.add_value(value);

        value
    }

    pub fn alloca(&mut self, typ: Type) -> ValueId {
        let value = Value::new(
            Opcode::Alloca,
            Type::Int64,
            NumChildren::Zero,
            &[],
            ValueData::Alloca(typ),
        );

        let value = self.procedure.add(value);

        self.add_value(value);

        value
    }
}
