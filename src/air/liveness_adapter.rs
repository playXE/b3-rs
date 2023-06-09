use tinyvec::TinyVec;

use crate::{
    analysis::liveness::{Liveness, LivenessAdapter as Adapter},
    bank::Bank,
};

use super::{
    arg::{ArgRole, ArgTemperature},
    basic_block::BasicBlockId,
    code::Code,
    stack_slot::StackSlotId,
    tmp::{AbsoluteIndexed, Tmp},
};

type ActionsList = TinyVec<[usize; 4]>;

#[derive(Clone, Debug)]
pub struct Actions {
    pub def: ActionsList,
    pub use_: ActionsList,
}

pub type ActionsForBoundary = Vec<Actions>;

pub struct TmpLivenessAdapter<
    'a,
    const ADAPTER_BANK: i8,
    const MINIMUM_TEMPERATURE: u8 = { ArgTemperature::Cold as u8 },
> {
    code: &'a Code<'a>,
    actions: Vec<ActionsForBoundary>,
    verbose: bool,
}

impl<'a, const ADAPTER_BANK: i8, const MINIMUM_TEMPERATURE: u8>
    TmpLivenessAdapter<'a, ADAPTER_BANK, MINIMUM_TEMPERATURE>
{
    pub fn new(code: &'a Code<'a>) -> Self {
        let actions = vec![ActionsForBoundary::new(); code.blocks.len()];
        Self {
            code,
            actions,
            verbose: false,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn accepts_bank(bank: Bank) -> bool {
        bank == Bank::from(ADAPTER_BANK)
    }

    pub fn accepts_role(role: ArgRole) -> bool {
        role.temperature() >= MINIMUM_TEMPERATURE.into()
    }

    pub fn actions_at(&self, block: BasicBlockId, boundary: usize) -> &Actions {
        &self.actions[block.0][boundary]
    }

    pub fn actions_at_mut(&mut self, block: BasicBlockId, boundary: usize) -> &mut Actions {
        &mut self.actions[block.0][boundary]
    }
}

impl<'a, const ADAPTER_BANK: i8, const MINIMUM_TEMPERATURE: u8> Adapter
    for TmpLivenessAdapter<'a, ADAPTER_BANK, MINIMUM_TEMPERATURE>
{
    type CFG = Code<'a>;
    type Thing = Tmp;

    fn cfg(&self) -> &Self::CFG {
        &self.code
    }

    fn num_indices(&self) -> usize {
        if ADAPTER_BANK == Bank::GP as i8 {
            AbsoluteIndexed::<{ Bank::GP as i8 }>::absolute_index(&Tmp::gp_tmp_for_index(
                self.code.num_gp_tmps,
            ))
        } else {
            AbsoluteIndexed::<{ Bank::FP as i8 }>::absolute_index(&Tmp::fp_tmp_for_index(
                self.code.num_fp_tmps,
            ))
        }
    }

    fn value_to_index(_: &Self::CFG, thing: Self::Thing) -> usize {
        if ADAPTER_BANK == Bank::GP as i8 {
            AbsoluteIndexed::<{ Bank::GP as i8 }>::absolute_index(&thing)
        } else {
            AbsoluteIndexed::<{ Bank::FP as i8 }>::absolute_index(&thing)
        }
    }

    fn index_to_value(_: &Self::CFG, index: usize) -> Self::Thing {
        if ADAPTER_BANK == Bank::GP as i8 {
            AbsoluteIndexed::<{ Bank::GP as i8 }>::tmp_for_absolute_index(index)
        } else {
            AbsoluteIndexed::<{ Bank::FP as i8 }>::tmp_for_absolute_index(index)
        }
    }

    fn block_size(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
    ) -> usize {
        self.code.block(block).insts.len()
    }

    fn for_each_def<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).def.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn for_each_use<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).use_.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn prepare_to_compute(&mut self) {
        for block in (0..self.code.blocks.len()).map(BasicBlockId) {
            let actions_for_boundary = &mut self.actions[block.0];

            actions_for_boundary.resize(
                self.code.block(block).insts.len() + 1,
                Actions {
                    def: ActionsList::new(),
                    use_: ActionsList::new(),
                },
            );

            for inst_index in (0..self.code.block(block).len()).rev() {
                let inst = &self.code.block(block).insts[inst_index];
                let code = &self.code;
                inst.for_each_tmp(self.code, |thing, role, bank, _width| {
                    if !Self::accepts_bank(bank) || !Self::accepts_role(role) {
                        return;
                    }

                    let index = Self::value_to_index(code, thing);

                    if role.is_early_use() {
                        if !actions_for_boundary[inst_index].use_.contains(&index) {
                            actions_for_boundary[inst_index].use_.push(index);
                        }
                    }

                    if role.is_early_def() {
                        if !actions_for_boundary[inst_index].def.contains(&index) {
                            actions_for_boundary[inst_index].def.push(index);
                        }
                    }

                    if role.is_late_use() {
                        if !actions_for_boundary[inst_index + 1].use_.contains(&index) {
                            actions_for_boundary[inst_index + 1].use_.push(index);
                        }
                    }

                    if role.is_late_def() {
                        if !actions_for_boundary[inst_index + 1].def.contains(&index) {
                            actions_for_boundary[inst_index + 1].def.push(index);
                        }
                    }
                });
            }
        }

        if self.verbose {
            for block_index in (0..self.code.blocks.len()).rev() {
                let block = &self.code.blocks[block_index];
                let actions_for_boundary = &self.actions[block_index];
                println!("Block {}", block_index);

                println!(
                    "(null) | use: {:?} def: {:?}",
                    actions_for_boundary[block.len()].use_,
                    actions_for_boundary[block.len()].def
                );

                for inst_index in (0..block.len()).rev() {
                    let inst = &block.insts[inst_index];
                    println!(
                        "{} | use: {:?} def: {:?}",
                        inst,
                        actions_for_boundary[inst_index].use_,
                        actions_for_boundary[inst_index].def
                    );
                }

                println!(
                    "{} | use: {:?} def: {:?}",
                    block.insts[0], actions_for_boundary[0].use_, actions_for_boundary[0].def
                );
            }
        }
    }
}

pub type TmpLiveness<'a, const ADAPTER_BANK: i8, const MINIMUM_TEMPERATURE: u8> =
    Liveness<'a, TmpLivenessAdapter<'a, ADAPTER_BANK, MINIMUM_TEMPERATURE>>;

pub struct UnifiedTmpLivenessAdapter<'a> {
    pub code: &'a Code<'a>,
    pub actions: Vec<Vec<Actions>>,
    pub verbose: bool,
}

impl<'a> UnifiedTmpLivenessAdapter<'a> {
    pub fn new(code: &'a Code<'a>, verbose: bool) -> Self {
        Self {
            code,
            actions: vec![Vec::new(); code.blocks.len()],
            verbose,
        }
    }

    pub fn actions_at(&self, block: BasicBlockId, boundary: usize) -> &Actions {
        &self.actions[block.0][boundary]
    }

    pub fn actions_at_mut(&mut self, block: BasicBlockId, boundary: usize) -> &mut Actions {
        &mut self.actions[block.0][boundary]
    }
}

impl<'a> Adapter for UnifiedTmpLivenessAdapter<'a> {
    type CFG = Code<'a>;
    type Thing = Tmp;

    fn cfg(&self) -> &Self::CFG {
        self.code
    }

    fn num_indices(&self) -> usize {
        AbsoluteIndexed::<{ Bank::GP as i8 }>::absolute_index(&Tmp::gp_tmp_for_index(
            self.code.num_gp_tmps,
        )) + AbsoluteIndexed::<{ Bank::FP as i8 }>::absolute_index(&Tmp::fp_tmp_for_index(
            self.code.num_fp_tmps,
        ))
    }

    fn block_size(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
    ) -> usize {
        self.code.block(block).insts.len()
    }

    fn value_to_index(cfg: &Self::CFG, thing: Self::Thing) -> usize {
        thing.linearly_indexed(cfg).index()
    }

    fn index_to_value(cfg: &Self::CFG, index: usize) -> Self::Thing {
        Tmp::tmp_for_linear_index(cfg, index)
    }

    fn for_each_def<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).def.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn for_each_use<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).use_.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn prepare_to_compute(&mut self) {
        for block in (0..self.code.blocks.len()).map(BasicBlockId) {
            let actions_for_boundary = &mut self.actions[block.0];

            actions_for_boundary.resize(
                self.code.block(block).insts.len() + 1,
                Actions {
                    def: ActionsList::new(),
                    use_: ActionsList::new(),
                },
            );

            for inst_index in (0..self.code.block(block).len()).rev() {
                let inst = &self.code.block(block).insts[inst_index];
                let code = &self.code;
                inst.for_each_tmp(self.code, |thing, role, _bank, _width| {
                    let index = Self::value_to_index(code, thing);

                    if role.is_early_use() {
                        if !actions_for_boundary[inst_index].use_.contains(&index) {
                            actions_for_boundary[inst_index].use_.push(index);
                        }
                    }

                    if role.is_early_def() {
                        if !actions_for_boundary[inst_index].def.contains(&index) {
                            actions_for_boundary[inst_index].def.push(index);
                        }
                    }

                    if role.is_late_use() {
                        if !actions_for_boundary[inst_index + 1].use_.contains(&index) {
                            actions_for_boundary[inst_index + 1].use_.push(index);
                        }
                    }

                    if role.is_late_def() {
                        if !actions_for_boundary[inst_index + 1].def.contains(&index) {
                            actions_for_boundary[inst_index + 1].def.push(index);
                        }
                    }
                });
            }
        }

        if self.verbose {
            for block_index in (0..self.code.blocks.len()).rev() {
                let block = &self.code.blocks[block_index];
                let actions_for_boundary = &self.actions[block_index];
                println!("Block {}", block_index);

                println!(
                    "(null) | use: {:?} def: {:?}",
                    actions_for_boundary[block.len()].use_,
                    actions_for_boundary[block.len()].def
                );

                for inst_index in (0..block.len()).rev() {
                    let inst = &block.insts[inst_index];
                    println!(
                        "{} | use: {:?} def: {:?}",
                        inst,
                        actions_for_boundary[inst_index].use_,
                        actions_for_boundary[inst_index].def
                    );
                }

                println!(
                    "{} | use: {:?} def: {:?}",
                    block.insts[0], actions_for_boundary[0].use_, actions_for_boundary[0].def
                );
            }
        }
    }
}

pub struct StackSlotLivenessAdapter<'a, 'b> {
    code: &'a mut Code<'b>,
    actions: Vec<ActionsForBoundary>,
    verbose: bool,
}

impl<'a, 'b> StackSlotLivenessAdapter<'a, 'b> {
    pub fn new(code: &'a mut Code<'b>) -> Self {
        let actions = vec![ActionsForBoundary::new(); code.blocks.len()];
        Self {
            code,
            actions,
            verbose: false,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn accepts_bank(_bank: Bank) -> bool {
        true
    }

    pub fn accepts_role(_role: ArgRole) -> bool {
        true
    }

    pub fn actions_at(&self, block: BasicBlockId, boundary: usize) -> &Actions {
        &self.actions[block.0][boundary]
    }

    pub fn actions_at_mut(&mut self, block: BasicBlockId, boundary: usize) -> &mut Actions {
        &mut self.actions[block.0][boundary]
    }
}

impl<'a, 'b> Adapter for StackSlotLivenessAdapter<'a, 'b> {
    type CFG = Code<'b>;
    type Thing = StackSlotId;

    fn cfg(&self) -> &Self::CFG {
        &self.code
    }

    fn num_indices(&self) -> usize {
        self.code.proc.stack_slots.len()
    }

    fn value_to_index(_: &Self::CFG, thing: Self::Thing) -> usize {
        thing.0
    }

    fn index_to_value(_: &Self::CFG, index: usize) -> Self::Thing {
        StackSlotId(index)
    }

    fn block_size(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
    ) -> usize {
        self.code.block(block).insts.len()
    }

    fn for_each_def<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).def.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn for_each_use<F>(
        &self,
        block: <<Self as Adapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        for index in self.actions_at(block, value_boundary_index).use_.iter() {
            func(Self::index_to_value(self.cfg(), *index));
        }
    }

    fn prepare_to_compute(&mut self) {
        for block in (0..self.code.blocks.len()).map(BasicBlockId) {
            let actions_for_boundary = &mut self.actions[block.0];

            actions_for_boundary.resize(
                self.code.block(block).insts.len() + 1,
                Actions {
                    def: ActionsList::new(),
                    use_: ActionsList::new(),
                },
            );

            for inst_index in (0..self.code.block(block).len()).rev() {
                let inst = &self.code.block(block).insts[inst_index];
                let code = &self.code;
                inst.for_each_stack_slot(self.code, |thing, role, bank, _width| {
                    if !Self::accepts_bank(bank) || !Self::accepts_role(role) {
                        return;
                    }

                    let index = Self::value_to_index(code, thing);

                    if role.is_early_use() {
                        if !actions_for_boundary[inst_index].use_.contains(&index) {
                            actions_for_boundary[inst_index].use_.push(index);
                        }
                    }

                    if role.is_early_def() {
                        if !actions_for_boundary[inst_index].def.contains(&index) {
                            actions_for_boundary[inst_index].def.push(index);
                        }
                    }

                    if role.is_late_use() {
                        if !actions_for_boundary[inst_index + 1].use_.contains(&index) {
                            actions_for_boundary[inst_index + 1].use_.push(index);
                        }
                    }

                    if role.is_late_def() {
                        if !actions_for_boundary[inst_index + 1].def.contains(&index) {
                            actions_for_boundary[inst_index + 1].def.push(index);
                        }
                    }
                });
            }
        }

        if self.verbose {
            for block_index in (0..self.code.blocks.len()).rev() {
                let block = &self.code.blocks[block_index];
                let actions_for_boundary = &self.actions[block_index];
                println!("Block {}", block_index);

                println!(
                    "(null) | use: {:?} def: {:?}",
                    actions_for_boundary[block.len()].use_,
                    actions_for_boundary[block.len()].def
                );

                for inst_index in (0..block.len()).rev() {
                    let inst = &block.insts[inst_index];
                    println!(
                        "{} | use: {:?} def: {:?}",
                        inst,
                        actions_for_boundary[inst_index].use_,
                        actions_for_boundary[inst_index].def
                    );
                }

                println!(
                    "{} | use: {:?} def: {:?}",
                    block.insts[0], actions_for_boundary[0].use_, actions_for_boundary[0].def
                );
            }
        }
    }
}

pub type StackSlotLiveness<'a, 'b> = Liveness<'a, StackSlotLivenessAdapter<'a, 'b>>;
