use bitvec::vec::BitVec;

use crate::{
    jit::{
        reg::Reg,
        register_set::{RegisterSet, },
    },
    liveness::Liveness,
    utils::index_set::IndexMap,
};

use super::{
    basic_block::BasicBlockId, code::Code, liveness_adapter::UnifiedTmpLivenessAdapter, tmp::Tmp, 
};

/// Although we could trivially adapt [Liveness<>](crate::liveness::Liveness) to work with Reg, this would not be so
/// efficient. There is a small number of registers, so it's much better to use bitvectors for
/// register liveness. This is a specialization of [Liveness<>](crate::liveness::Liveness) that uses bitvectors directly.
/// This makes the code sufficiently different that it didn't make sense to try to share code.
pub struct RegLiveness {
    live_at_head: IndexMap<RegisterSet, BasicBlockId>,
    live_at_tail: IndexMap<RegisterSet, BasicBlockId>,
    actions: IndexMap<ActionsForBoundary, BasicBlockId>,
}

impl RegLiveness {
    pub fn new(code: &mut Code) -> Self {
        let mut live_at_head = IndexMap::with_capacity(code.blocks.len());
        let mut live_at_tail = IndexMap::with_capacity(code.blocks.len());
        let mut actions = IndexMap::with_capacity(code.blocks.len());

        for (id, block) in code.blocks.iter().enumerate() {
            live_at_head.insert(BasicBlockId(id), RegisterSet::default());
            live_at_tail.insert(BasicBlockId(id), RegisterSet::default());
            actions.insert(
                BasicBlockId(id),
                vec![
                    Actions {
                        use_: RegisterSet::default(),
                        def: RegisterSet::default(),
                    };
                    block.insts.len() + 1
                ],
            );
        }

        for i in 0..code.blocks.len() {
            let actions_for_boundary = &mut actions[BasicBlockId(i)];

            for (inst_index, inst) in code.blocks[i].insts.iter().rev().enumerate() {
                inst.for_each_reg(code, |reg, role, _bank, width| {
                    if role.is_early_use() {
                        actions_for_boundary[inst_index].use_.add(reg, width);
                        assert!(actions_for_boundary[inst_index].use_.contains(reg, width));
                    }

                    if role.is_early_def() {
                        actions_for_boundary[inst_index].def.add(reg, width);
                        assert!(actions_for_boundary[inst_index].def.contains(reg, width));
                    }

                    if role.is_late_use() {
                        actions_for_boundary[inst_index + 1].use_.add(reg, width);
                        assert!(actions_for_boundary[inst_index + 1].use_.contains(reg, width));
                    }

                    if role.is_late_def() {
                        actions_for_boundary[inst_index + 1].def.add(reg, width);
                        assert!(actions_for_boundary[inst_index + 1].def.contains(reg, width));
                    }
                });
               
            }
        }

        for i in 0..code.blocks.len() {
            let live_at_tail = &mut live_at_tail[BasicBlockId(i)];

            code.block(BasicBlockId(i)).last().unwrap().for_each_reg(
                code,
                |reg, role, _bank, width| {

                    if role.is_late_use() {
                        live_at_tail.add(reg, width);
                    }
                },
            );
        }

        let mut dirty_blocks: BitVec<usize> = bitvec::vec::BitVec::new();

        dirty_blocks.resize(code.blocks.len(), true);

        let mut changed;

        let mut this = Self {
            live_at_head,
            live_at_tail,
            actions,
        };

        loop {
            changed = false;

            for block_index in (0..code.blocks.len()).rev() {
                if !dirty_blocks[block_index] {
                    continue;
                }

                dirty_blocks.set(block_index, false);

                let mut local_calc = LocalCalc::new(&this, BasicBlockId(block_index));

                for inst_index in (0..code.blocks[block_index].insts.len()).rev() {
                    local_calc.execute(inst_index);
                }

                code.block(BasicBlockId(block_index))[0].for_each_reg(
                    code,
                    |reg, role, _bank, _width| {
                        if role.is_early_def() {
                            local_calc.workset.remove(reg);
                        }
                    },
                );

                let workset = local_calc.workset;

                let live_at_head = &this.live_at_head[BasicBlockId(block_index)];
                if live_at_head.subsumes(&workset) {
                    continue;
                }

                this.live_at_head[BasicBlockId(block_index)].merge(&workset);

                for pred in code.blocks[block_index].predecessors.iter() {
                    let live_at_tail = &this.live_at_tail[*pred];

                    if live_at_tail.subsumes(&workset) {
                        continue;
                    }
                    
                    this.live_at_tail[*pred].merge(&workset);
                    dirty_blocks.set(pred.0, true);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        if true {
            println!("Reg Liveness result:");

            for block_index in (0..code.blocks.len()).rev() {
                let actions_for_boundary = &this.actions[BasicBlockId(block_index)];
                println!("Block {}", block_index);
                println!("Live at head: {}", this.live_at_head[BasicBlockId(block_index)]);
                println!("Live at tail: {}", this.live_at_tail[BasicBlockId(block_index)]);

                for inst_index in (0..code.blocks[block_index].insts.len()).rev() {
                    println!(
                        "{} | use: {} | def: {}",
                        code.blocks[block_index][inst_index],
                        actions_for_boundary[inst_index].use_,
                        actions_for_boundary[inst_index].def
                    );
                }
            }
        }

        this
    }

    pub fn live_at_tail(&self, block: BasicBlockId) -> &RegisterSet {
        &self.live_at_tail[block]
    }

    pub fn live_at_head(&self, block: BasicBlockId) -> &RegisterSet {
        &self.live_at_head[block]
    }
}

#[derive(Clone, Copy)]
pub struct Actions {
    use_: RegisterSet,
    def: RegisterSet,
}

pub type ActionsForBoundary = Vec<Actions>;

pub struct LocalCalcBase {
    pub block: BasicBlockId,
    pub workset: RegisterSet,
}

impl LocalCalcBase {
    pub fn new(block: BasicBlockId) -> Self {
        Self {
            block,
            workset: RegisterSet::default(),
        }
    }

    pub fn live(&self) -> &RegisterSet {
        &self.workset
    }

    pub fn is_live(&self, reg: Reg) -> bool {
        self.workset
            .contains(reg, reg.conservative_width_without_vectors())
    }
}

pub struct LocalCalc<'a> {
    base: LocalCalcBase,
    actions: &'a ActionsForBoundary,
}

impl<'a> LocalCalc<'a> {
    pub fn new(liveness: &'a RegLiveness, block: BasicBlockId) -> Self {
        let mut this = Self {
            base: LocalCalcBase::new(block),
            actions: &liveness.actions[block],
        };

        this.base.workset = liveness.live_at_tail[block];

        this
    }

    pub fn execute(&mut self, inst_index: usize) {
        self.actions[inst_index + 1].def.for_each(|reg| {
            println!("remove {} at {} from {}", reg, inst_index, self.workset);
            self.workset.remove(reg);
        });

        let used = self.actions[inst_index].use_;

        self.workset.merge(&used);
    }
}

impl std::ops::Deref for LocalCalc<'_> {
    type Target = LocalCalcBase;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl std::ops::DerefMut for LocalCalc<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

pub struct LocalCalcForUnifiedTmpLiveness<'a> {
    pub base: LocalCalcBase,
    pub code: &'a Code<'a>,
    pub actions: &'a super::liveness_adapter::ActionsForBoundary,
}

impl<'a> LocalCalcForUnifiedTmpLiveness<'a> {
    pub fn new(liveness: &'a Liveness<UnifiedTmpLivenessAdapter<'a>>, block: BasicBlockId) -> Self {
        let mut this = Self {
            base: LocalCalcBase::new(block),
            code: liveness.adapter.code,
            actions: &liveness.adapter.actions[block.0],
        };

        for tmp in liveness.live_at_tail(block) {
            if tmp.is_reg() {
                this.base
                    .workset
                    .add(tmp.reg(), tmp.reg().conservative_width_without_vectors());
            }
        }

        this
    }

    pub fn execute(&mut self, inst_index: usize) {
        for index in self.actions[inst_index + 1].def.iter().copied() {
            let tmp = Tmp::tmp_for_linear_index(self.code, index);
            if tmp.is_reg() {
                self.workset.remove(tmp.reg());
            }
        }

        for index in self.actions[inst_index].use_.iter().copied() {
            let tmp = Tmp::tmp_for_linear_index(self.code, index);
            if tmp.is_reg() {
                self.workset
                    .add(tmp.reg(), tmp.reg().conservative_width_without_vectors());
            }
        }
    }
}

impl<'a> std::ops::Deref for LocalCalcForUnifiedTmpLiveness<'a> {
    type Target = LocalCalcBase;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a> std::ops::DerefMut for LocalCalcForUnifiedTmpLiveness<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}
