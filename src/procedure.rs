use std::ops::Range;

use crate::{
    block::{is_block_dead, recompute_predecessors, BasicBlock, BlockId, FrequentBlock},
    dominators::{Dominators, Graph},
    jit::reg::Reg,
    kind::Kind,
    natural_loops::NaturalLoops,
    opcode::Opcode,
    sparse_collection::SparseCollection,
    typ::{Type, TypeKind},
    value::{NumChildren, Value, ValueData, ValueId},
    variable::{Variable, VariableId}, air::{stack_slot::{StackSlot, StackSlotKind, StackSlotId}, special::{Special, SpecialId}}, effects::Effects, data_section::DataSection, rpo::rpo_sort,
};
use crate::Options;
pub struct Procedure {
    pub(crate) options: Options,
    pub(crate) values: SparseCollection<Value>,
    pub(crate) blocks: Vec<BasicBlock>,
    pub(crate) variables: SparseCollection<Variable>,
    pub(crate) dominators: Option<Dominators<Self>>,
    pub(crate) natural_loops: Option<NaturalLoops<Self>>,
    pub(crate) stack_slots: Vec<StackSlot>,
    pub(crate) specials: SparseCollection<Special>,
    pub(crate) data_sections: Vec<DataSection>,
}

impl Graph for Procedure {
    type Node = BlockId;

    fn node_index(&self, node: Self::Node) -> usize {
        node.0
    }

    fn node(&self, index: usize) -> Option<Self::Node> {
        Some(BlockId(index))
    }

    fn num_nodes(&self) -> usize {
        self.blocks.len()
    }

    fn root(&self) -> Self::Node {
        BlockId(0)
    }

    fn predecessors(&self, block: Self::Node) -> std::borrow::Cow<[Self::Node]> {
        std::borrow::Cow::Borrowed(self.blocks[block.0].predecessor_list())
    }

    fn successors(&self, block: Self::Node) -> std::borrow::Cow<[Self::Node]> {
        std::borrow::Cow::Owned(
            self.blocks[block.0]
                .successor_list()
                .iter()
                .map(|x| x.0)
                .collect(),
        )
    }
}

impl Procedure {
    pub fn new(options: Options) -> Self {
        Self {
            values: SparseCollection::new(),
            blocks: Vec::new(),
            variables: SparseCollection::new(),
            dominators: None,
            natural_loops: None,
            stack_slots: vec![],
            specials: SparseCollection::new(),
            options,
            data_sections: vec![],
        }
    }

   

    pub fn options(&self) -> &Options {
        &self.options
    }

    pub fn special(&self, id: SpecialId) -> &Special {
        self.specials.at(id).unwrap()
    }

    pub fn special_mut(&mut self, id: SpecialId) -> &mut Special {
        self.specials.at_mut(id).unwrap()
    }

    pub fn add_stack_slot(&mut self, size: usize, stack_slot_kind: StackSlotKind) -> StackSlotId {
        let slot = StackSlot {
            byte_size: size as _,
            kind: stack_slot_kind,
            index: self.stack_slots.len(),
            offset_from_fp: 0
        };

        self.stack_slots.push(slot);

        StackSlotId(self.stack_slots.len() - 1)
    }

    pub fn clone(&mut self, id: ValueId) -> ValueId {
        let val = self.values.at(id).unwrap().clone();

        self.values.add(val)
    }

    pub fn block(&self, id: BlockId) -> &BasicBlock {
        &self.blocks[id.0]
    }

    pub fn block_mut(&mut self, id: BlockId) -> &mut BasicBlock {
        &mut self.blocks[id.0]
    }

    pub fn add(&mut self, val: Value) -> ValueId {
        self.values.add(val)
    }

    pub fn value(&self, id: ValueId) -> &Value {
        self.values.at(id).unwrap()
    }

    pub fn value_mut(&mut self, id: ValueId) -> &mut Value {
        self.values.at_mut(id).unwrap()
    }

    pub fn cfg_root(&self) -> BlockId {
        BlockId(0)
    }

    pub fn cfg_num_nodes(&self) -> usize {
        self.blocks.len()
    }

    pub fn successors(&self, id: BlockId) -> &Vec<FrequentBlock> {
        self.blocks[id.0].successor_list()
    }

    pub fn successors_mut(&mut self, id: BlockId) -> &mut Vec<FrequentBlock> {
        self.blocks[id.0].successor_list_mut()
    }

    pub fn predecessors(&self, id: BlockId) -> &Vec<BlockId> {
        self.blocks[id.0].predecessor_list()
    }

    pub fn predecessors_mut(&mut self, id: BlockId) -> &mut Vec<BlockId> {
        self.blocks[id.0].predecessor_list_mut()
    }

    pub fn dominators(&self) -> &Dominators<Self> {
        self.dominators.as_ref().expect("Dominators not computed")
    }

    pub fn dominators_or_compute(&mut self) -> &Dominators<Self> {
        if self.dominators.is_none() {
            self.dominators = Some(Dominators::new(self));
        }

        self.dominators()
    }

    pub fn compute_dominators(&mut self) {
        self.dominators = Some(Dominators::new(self));
    }

    pub fn natural_loops_or_compute(&mut self) -> &NaturalLoops<Self> {
        if self.natural_loops.is_none() {
            self.dominators_or_compute();
            let doms = self.dominators();
            self.natural_loops = Some(NaturalLoops::new(self, doms));
        }

        self.natural_loops()
    }

    pub fn natural_loops(&self) -> &NaturalLoops<Self> {
        self.natural_loops
            .as_ref()
            .expect("Natural loops not computed")
    }

    pub fn invalidate_cfg(&mut self) {
        self.dominators = None;
        self.natural_loops = None;
    }

    pub fn add_block(&mut self, frequency: f64) -> BlockId {
        let block = BasicBlock::new(self.blocks.len(), frequency);

        self.blocks.push(block);

        BlockId(self.blocks.len() - 1)
    }

    pub fn add_nop(&mut self) -> ValueId {
        self.add(Value::new(Opcode::Nop, Type::Void, NumChildren::Zero, &[], ValueData::None))
    }

    pub fn add_int_constant(&mut self, typ: Type, value: impl Into<i64>) -> ValueId {
        let val = value.into();
        match typ.kind() {
            TypeKind::Int32 => self.add(Value::make_const32(val as _)),
            TypeKind::Int64 => self.add(Value::make_const64(val)),
            TypeKind::Float => self.add(Value::make_const_float(val as _)),
            TypeKind::Double => self.add(Value::make_const_double(val as _)),

            _ => panic!("Invalid type for constant"),
        }
    }

    pub fn add_bits_constant(&mut self, typ: Type, value: impl Into<u64>) -> ValueId {
        let val = value.into();
        match typ.kind() {
            TypeKind::Int32 => self.add(Value::make_const32(val as _)),
            TypeKind::Int64 => self.add(Value::make_const64(val as _)),
            TypeKind::Float => self.add(Value::make_const_float(f32::from_bits(val as _))),
            TypeKind::Double => self.add(Value::make_const_double(f64::from_bits(val as _))),

            _ => panic!("Invalid type for constant"),
        }
    }

    pub fn variable(&self, id: VariableId) -> &Variable {
        self.variables.at(id).unwrap()
    }

    pub fn add_variable(&mut self, typ: Type) -> VariableId {
        self.variables.add(Variable::new(0, typ))
    }

    pub fn add_variable_get(&mut self, var: VariableId) -> ValueId {
        let typ = self.variable(var).typ();
        self.add(Value::new(
            Opcode::Get,
            typ,
            NumChildren::Zero,
            &[],
            ValueData::Variable(var),
        ))
    }

    pub fn add_variable_set(&mut self, var: VariableId, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::Set,
            TypeKind::Void.into(),
            NumChildren::One,
            &[value],
            ValueData::Variable(var),
        ))
    }

    pub fn add_binary(&mut self, kind: Kind, lhs: ValueId, rhs: ValueId) -> ValueId {
        let typ = self.value(lhs).typ();

        assert_eq!(
            typ,
            self.value(rhs).typ(),
            "Binary operation with different types: {} and {}",
            typ,
            self.value(rhs).typ()
        );
        assert!(
            kind.opcode().is_binary(),
            "Opcode is not a binary operation: {:?}",
            kind.opcode()
        );
        self.add(Value::new(
            kind,
            typ,
            NumChildren::Two,
            &[lhs, rhs],
            ValueData::None,
        ))
    }

    pub fn add_bitcast(&mut self, value: ValueId, typ: Type) -> ValueId {
        self.add(Value::new(
            Opcode::BitwiseCast,
            typ,
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn add_load(
        &mut self,
        kind: Kind,
        typ: Type,
        pointer: ValueId,
        offset: i32,
        range: Range<usize>,
        fence_range: Range<usize>,
    ) -> ValueId {
        match kind.opcode() {
            Opcode::Load8Z | Opcode::Load8S | Opcode::Load16Z | Opcode::Load16S => {
                assert_eq!(
                    typ.kind(),
                    TypeKind::Int32,
                    "Can load only as 32-bit integer: {:?}",
                    kind.opcode()
                );
            }

            Opcode::Load => {}

            _ => panic!("Invalid opcode for load: {:?}", kind.opcode()),
        }

        self.add(Value::new(
            kind,
            typ,
            NumChildren::One,
            &[pointer],
            ValueData::MemoryValue {
                offset,
                range,
                fence_range,
            },
        ))
    }

    pub fn add_load32(
        &mut self,
        kind: Kind,
        pointer: ValueId,
        offset: i32,
        range: Range<usize>,
        fence_range: Range<usize>,
    ) -> ValueId {
        self.add(Value::new(
            kind,
            TypeKind::Int32.into(),
            NumChildren::One,
            &[pointer],
            ValueData::MemoryValue {
                offset,
                range,
                fence_range,
            },
        ))
    }

    pub fn add_store(
        &mut self,
        kind: Kind,
        value: ValueId,
        pointer: ValueId,
        offset: i32,
        range: Range<usize>,
        fence_range: Range<usize>,
    ) -> ValueId {
        assert!(
            kind.opcode().is_store(),
            "Opcode is not a store: {:?}",
            kind.opcode()
        );
        self.add(Value::new(
            kind,
            TypeKind::Void.into(),
            NumChildren::Two,
            &[value, pointer],
            ValueData::MemoryValue {
                offset,
                range,
                fence_range,
            },
        ))
    }

    pub fn add_argument(&mut self, typ: Type, reg: Reg) -> ValueId {
        self.add(Value::new(
            Opcode::ArgumentReg,
            typ,
            NumChildren::Zero,
            &[],
            ValueData::Argument(reg),
        ))
    }

    pub fn add_i2d(&mut self, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::IToD,
            TypeKind::Double.into(),
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn add_d2i(&mut self, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::DToI,
            TypeKind::Int32.into(),
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn add_i2f(&mut self, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::IToF,
            TypeKind::Float.into(),
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn add_f2i(&mut self, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::FToI,
            TypeKind::Int32.into(),
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn add_to_block(&mut self, block: BlockId, value: ValueId) {
        self.blocks[block.0].push(value);
    }

    pub fn reset_value_owners(&mut self) {
        for block in self.blocks.iter() {
            for value in block.iter() {
                self.values.at_mut(*value).unwrap().owner = Some(BlockId(block.index()));
            }
        }
    }

    pub fn reset_reachability(&mut self) {
        recompute_predecessors(&mut self.blocks);

        let mut found_dead = false;

        for block in self.blocks.iter() {
            if is_block_dead(block) {
                found_dead = true;
                break;
            }
        }

        if !found_dead {
            return;
        }
        
        self.reset_value_owners();


        for value_id in (0..self.values.size()).map(ValueId) {
            if let ValueData::Upsilon(ref phi) = self.value(value_id).data {
                let phi_id = phi.unwrap();
                let phi = self.value(phi_id);

                if is_block_dead(self.block(phi.owner.expect("Owners should be computed"))) {
                    self.values.at_mut(value_id).unwrap().replace_with_nop();
                }
            }
        }

        for block_id in (0..self.blocks.len()).map(BlockId) {
            if is_block_dead(self.block(block_id)) {
                for value_id in std::mem::take(&mut self.block_mut(block_id).values) {
                    self.delete_value(value_id);
                }
                self.blocks[block_id.0].index = usize::MAX;
                self.blocks[block_id.0].clear();
            }
        }

        rpo_sort(self);
    }

    pub fn delete_value(&mut self, value_id: ValueId) {
        self.values.remove(value_id);
    }

    pub fn add_return(&mut self, value: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::Return,
            TypeKind::Void.into(),
            NumChildren::One,
            &[value],
            ValueData::None,
        ))
    }

    pub fn display_(&self) -> ProcedureDisplay<'_> {
        ProcedureDisplay { procedure: &self }
    }

    pub fn add_jump(&mut self) -> ValueId {
        self.add(Value::new(
            Opcode::Jump,
            TypeKind::Void.into(),
            NumChildren::Zero,
            &[],
            ValueData::None,
        ))
    }

    pub fn add_branch(&mut self, val: ValueId) -> ValueId {
        self.add(Value::new(
            Opcode::Branch,
            TypeKind::Void.into(),
            NumChildren::One,
            &[val],
            ValueData::None,
        ))
    }

    pub fn add_ccall(&mut self, ret: Type, callee: ValueId, args: &[ValueId], effects: Effects) -> ValueId {
        self.add(Value::new(
            Opcode::CCall,
            ret,
            NumChildren::VarArgs,
            std::iter::once(callee).chain(args.iter().copied()).collect::<Vec<_>>().as_slice(),
            ValueData::CCallValue(effects),
        ))
    }

    pub fn add_data_section(&mut self, size: usize) -> (usize, *mut u8) {
        self.data_sections.push(DataSection::new(size));
        let index = self.data_sections.len() - 1;
        let ptr = self.data_sections[index].data.as_mut_ptr();

        (index, ptr)
    }
}

pub struct ProcedureDisplay<'a> {
    procedure: &'a Procedure,
}

impl std::fmt::Display for ProcedureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Procedure {{")?;

        for block in self.procedure.blocks.iter() {
            block.fmt(f, self.procedure)?;
        }

        if !self.procedure.variables.is_empty() {
            writeln!(f, "Variables:")?;
            for var in self.procedure.variables.iter() {
                writeln!(f, "  var@{}: {}", var.index(), var.typ())?;
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}
