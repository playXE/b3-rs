use crate::{
    air::stack_slot::StackSlotId,
    dominators::{GraphNodeWorklist, GraphVisitOrder, PostOrderGraphNodeWorklist},
    effects::Effects,
    jit::reg::Reg,
    opcode::Opcode,
    procedure::Procedure,
    sparse_collection::SparseElement,
    typ::Type,
    utils::index_set::KeyIndex,
    value::{NumChildren, Value, ValueData, ValueId},
    variable::VariableId,
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

pub struct BasicBlockBuilder<'a> {
    pub block: BlockId,
    pub procedure: &'a mut Procedure,
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn new(block: BlockId, procedure: &'a mut Procedure) -> Self {
        Self { block, procedure }
    }

    pub fn add_value(&mut self, value: ValueId) {
        self.procedure.add_to_block(self.block, value);
    }

    pub fn const32(&mut self, val: i32) -> ValueId {
        let x = self.procedure.add_int_constant(Type::Int32, val);
        self.add_value(x);
        x
    }

    pub fn const64(&mut self, val: i64) -> ValueId {
        let x = self.procedure.add_int_constant(Type::Int64, val);
        self.add_value(x);
        x
    }

    pub fn const_float(&mut self, val: f32) -> ValueId {
        let x = self
            .procedure
            .add_bits_constant(Type::Float, val.to_bits() as u64);
        self.add_value(x);
        x
    }

    pub fn const_double(&mut self, val: f64) -> ValueId {
        let x = self
            .procedure
            .add_bits_constant(Type::Double, val.to_bits());
        self.add_value(x);
        x
    }

    pub fn var_get(&mut self, var: VariableId) -> ValueId {
        let x = self.procedure.add_variable_get(var);
        self.add_value(x);
        x
    }

    pub fn var_set(&mut self, var: VariableId, value: ValueId) {
        let x = self.procedure.add_variable_set(var, value);
        self.add_value(x);
    }

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
            &[ptr, value],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
    }

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
            &[ptr, value],
            ValueData::MemoryValue {
                offset,
                range: range.unwrap_or(0..usize::MAX),
                fence_range: fence_range.unwrap_or(0..usize::MAX),
            },
        );

        let x = self.procedure.add(value);
        self.add_value(x);
    }

    pub fn binary(&mut self, op: Opcode, lhs: ValueId, rhs: ValueId) -> ValueId {
        assert!(op.is_binary());
        assert!(self.procedure.value(lhs).typ() == self.procedure.value(rhs).typ());
        let typ = self.procedure.value(lhs).typ();
        let value = Value::new(op, typ, NumChildren::Two, &[lhs, rhs], ValueData::None);

        let x = self.procedure.add(value);
        self.add_value(x);
        x
    }

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

    /// Takes and returns Int32
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

    /// Takes and returns Int32
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
            
            self.procedure.block_mut(target).predecessor_list.push(self.block);
        }

        let id = self.procedure.add(value);

        self.add_value(id);
    }

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

        self.procedure.block_mut(taken).predecessor_list.push(self.block);
        self.procedure
            .block_mut(not_taken.0)
            .predecessor_list
            .push(self.block);

        let value = self.procedure.add(value);

        self.add_value(value);
    }

    pub fn return_(mut self, value: Option<ValueId>) {
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
}
