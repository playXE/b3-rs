use tinyvec::TinyVec;

use crate::{
    bank::{for_each_bank, Bank},
    block::Frequency,
    dominators::Graph,
    jit::{
        reg::Reg,
        register_at_offset::{round_up_to_multiple_of, RegisterAtOffsetList},
        register_set::{RegisterSet, RegisterSetBuilder, ScalarRegisterSet},
    },
    procedure::Procedure,
    sparse_collection::SparseCollection,
};

use super::{
    basic_block::{update_predecessors_after, BasicBlock, BasicBlockId},
    special::Special,
    stack_slot::{StackSlot, StackSlotId, StackSlotKind},
    tmp::Tmp,
};

/// This is an IR that is very close to the bare metal. It requires about 40x more bytes than the
/// generated machine code - for example if you're generating 1MB of machine code, you need about
/// 40MB of Air.
pub struct Code<'a> {
    /// Reference to B3 IR. Used to access origins of Air values.
    pub proc: &'a mut Procedure,
    pub gp_regs_in_priority_order: Vec<Reg>,
    pub fp_regs_in_priority_order: Vec<Reg>,
    pub mutable_regs: ScalarRegisterSet,
    pub pinned_regs: ScalarRegisterSet,
    pub num_gp_tmps: usize,
    pub num_fp_tmps: usize,
    pub frame_size: usize,
    pub call_arg_area_size: usize,
    pub stack_is_allocated: bool,
    pub specials: SparseCollection<Special>,
    pub blocks: Vec<BasicBlock>,
    pub entrypoints: Vec<(BasicBlockId, Frequency)>,
    pub callee_save_stack_slot: Option<StackSlotId>,
    pub uncorrected_callee_save_registers_at_offset_list: RegisterAtOffsetList,
    pub callee_save_registers: RegisterSetBuilder,
}

impl<'a> Code<'a> {
    pub fn new(proc: &'a mut Procedure) -> Self {
        let mut this = Self {
            proc,
            gp_regs_in_priority_order: vec![],
            fp_regs_in_priority_order: vec![],
            mutable_regs: ScalarRegisterSet::new(&RegisterSet::new(&RegisterSetBuilder::new())),
            pinned_regs: ScalarRegisterSet::new(&RegisterSet::new(&RegisterSetBuilder::new())),
            num_gp_tmps: 0,
            num_fp_tmps: 0,
            specials: SparseCollection::new(),
            blocks: Vec::new(),
            entrypoints: vec![],
            frame_size: 0,
            call_arg_area_size: 0,
            stack_is_allocated: false,
            callee_save_stack_slot: None,
            uncorrected_callee_save_registers_at_offset_list: Default::default(),
            callee_save_registers: RegisterSetBuilder::new(),
        };

        for_each_bank(|bank| {
            let mut volatile_regs = vec![];
            let mut full_callee_save_regs = vec![];
            let mut callee_save_regs = vec![];

            let mut all = if bank == Bank::GP {
                RegisterSetBuilder::from_regs(&RegisterSetBuilder::all_gprs())
            } else {
                RegisterSetBuilder::from_regs(&RegisterSetBuilder::all_fprs())
            };
            all.exclude_regs(&RegisterSetBuilder::stack_registers());
            let callee_save = RegisterSetBuilder::callee_saved_registers();

            all.build_and_validate().for_each(|reg| {
                if !callee_save.contains(reg, reg.conservative_width_without_vectors()) {
                    volatile_regs.push(reg);
                }

                if callee_save.contains(reg, reg.conservative_width_without_vectors()) {
                    full_callee_save_regs.push(reg);
                    callee_save_regs.push(reg);
                }
            });

            let mut result = Vec::new();
            //result.append(&mut full_callee_save_regs);
            result.append(&mut callee_save_regs);
            result.append(&mut volatile_regs);

            this.set_regs_in_priority_order(bank, &result);
        });

        this
    }

    fn set_regs_in_priority_order(&mut self, bank: Bank, regs: &[Reg]) {
        let regs_in_priority_impl = match bank {
            Bank::GP => &mut self.gp_regs_in_priority_order,
            Bank::FP => &mut self.fp_regs_in_priority_order,
        };
        *regs_in_priority_impl = regs.to_vec();
        self.mutable_regs = Default::default();

        for_each_bank(|bank| {
            for i in 0..self.regs_in_priority_order(bank).len() {
                let reg = self.regs_in_priority_order(bank)[i];
                self.mutable_regs.add(reg);
            }
        });
    }

    pub fn regs_in_priority_order(&self, bank: Bank) -> &[Reg] {
        match bank {
            Bank::GP => &self.gp_regs_in_priority_order,
            Bank::FP => &self.fp_regs_in_priority_order,
        }
    }

    pub fn stack_slot(&self, id: StackSlotId) -> &StackSlot {
        &self.proc.stack_slots[id.0]
    }

    pub fn stack_slot_mut(&mut self, id: StackSlotId) -> &mut StackSlot {
        &mut self.proc.stack_slots[id.0]
    }

    pub fn num_tmps(&self, bank: Bank) -> usize {
        match bank {
            Bank::GP => self.num_gp_tmps,
            Bank::FP => self.num_fp_tmps,
        }
    }

    pub fn new_tmp(&mut self, bank: Bank) -> Tmp {
        match bank {
            Bank::GP => {
                self.num_gp_tmps += 1;
                Tmp::gp_tmp_for_index(self.num_gp_tmps - 1)
            }
            Bank::FP => {
                self.num_fp_tmps += 1;
                Tmp::fp_tmp_for_index(self.num_fp_tmps - 1)
            }
        }
    }

    pub fn block(&self, id: BasicBlockId) -> &BasicBlock {
        self.blocks.get(id.0).unwrap()
    }

    pub fn block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        self.blocks.get_mut(id.0).unwrap()
    }

    pub fn add_block(&mut self, frequency: f64) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len());
        self.blocks.push(BasicBlock {
            index: 0,
            insts: Vec::new(),
            successors: TinyVec::new(),
            predecessors: TinyVec::new(),
            frequency,
        });

        self.block_mut(id).index = id.0;
        id
    }

    pub fn for_each_tmp(&self, mut f: impl FnMut(Tmp)) {
        for i in 0..self.num_gp_tmps {
            f(Tmp::gp_tmp_for_index(i));
        }

        for i in 0..self.num_fp_tmps {
            f(Tmp::fp_tmp_for_index(i));
        }
    }

    pub fn for_each_tmp_this_mut(&mut self, mut f: impl FnMut(&mut Self, Tmp)) {
        for i in 0..self.num_gp_tmps {
            f(self, Tmp::gp_tmp_for_index(i));
        }

        for i in 0..self.num_fp_tmps {
            f(self, Tmp::fp_tmp_for_index(i));
        }
    }

    pub fn clear_predecessors(&mut self) {
        for block in self.blocks.iter_mut() {
            block.predecessors.clear();
        }
    }

    pub fn reset_reachability(&mut self) {
        self.clear_predecessors();

        if self.entrypoints.is_empty() {
            update_predecessors_after(BasicBlockId(0), self);
        } else {
            for i in 0..self.entrypoints.len() {
                let id = self.entrypoints[i].0;
                update_predecessors_after(id, self);
            }
        }
    }

    pub fn add_stack_slot(&mut self, byte_size: usize, kind: StackSlotKind) -> StackSlotId {
        let result = self.proc.add_stack_slot(byte_size, kind);

        if self.stack_is_allocated {
            let extent = round_up_to_multiple_of(
                self.proc.stack_slots[result.0].alignment() as _,
                self.frame_size as isize + byte_size as isize,
            );
            self.proc.stack_slots[result.0].offset_from_fp = -(extent as isize);
            self.frame_size = round_up_to_multiple_of(16, extent) as _;
        }

        result
    }

    pub fn set_callee_save_registers_at_offset_list(
        &mut self,
        register_at_offset_list: RegisterAtOffsetList,
        slot: StackSlotId,
    ) {
        self.uncorrected_callee_save_registers_at_offset_list = register_at_offset_list;

        for i in 0..self.uncorrected_callee_save_registers_at_offset_list.len() {
            let register_at_offset = self.uncorrected_callee_save_registers_at_offset_list
               [i];

            let reg = register_at_offset.reg();
            let width = register_at_offset.width();
            self.callee_save_registers.add(reg, width);
        }

        self.callee_save_stack_slot = Some(slot);
    }

    pub fn callee_save_registers_at_offset_list(&self) -> RegisterAtOffsetList {
        let mut result = self.uncorrected_callee_save_registers_at_offset_list.clone();

        if let Some(slot) = self.callee_save_stack_slot {
            let offset = self.stack_slot(slot).offset_from_fp;
            let byte_size = self.stack_slot(slot).byte_size();
            result.adjust_offsets(byte_size as isize + offset);
        }

        result
    }
}

impl<'a> std::fmt::Display for Code<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block in self.blocks.iter() {
            writeln!(f, "BB{}: ; frequency = {}", block.index, block.frequency)?;
            if block.predecessors.len() != 0 {
                write!(f, "  Predecessors: ")?;

                for (i, pred) in block.predecessors.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "BB{}", pred.0)?;
                }

                writeln!(f)?;
            }

            for inst in block.insts.iter() {
                writeln!(f, "     {}", inst)?;
            }

            if block.successors.len() != 0 {
                write!(f, "  Successors: ")?;

                for (i, succ) in block.successors.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "BB{}", succ.0 .0)?;
                }

                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl<'a> Graph for Code<'a> {
    type Node = BasicBlockId;

    fn display(&self, block: Option<Self::Node>) -> String {
        match block {
            Some(block) => format!("BB{}", block.0),
            None => "<none>".to_string(),
        }
    }

    fn node(&self, index: usize) -> Option<Self::Node> {
        Some(BasicBlockId(index))
    }

    fn node_index(&self, node: Self::Node) -> usize {
        node.0
    }

    fn num_nodes(&self) -> usize {
        self.blocks.len()
    }

    fn predecessors(&self, block: Self::Node) -> std::borrow::Cow<[Self::Node]> {
        std::borrow::Cow::Borrowed(&self.block(block).predecessors)
    }

    fn successors(&self, block: Self::Node) -> std::borrow::Cow<[Self::Node]> {
        std::borrow::Cow::Owned(
            self.block(block)
                .successors
                .iter()
                .map(|(id, _)| *id)
                .collect(),
        )
    }

    fn root(&self) -> Self::Node {
        BasicBlockId(0)
    }
}
