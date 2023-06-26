use crate::{
    analysis::dominators::GraphNodeWorklist, bank::Bank, block::Frequency,
    utils::bitvector::BitVector,
};

use super::{
    basic_block::BasicBlockId,
    code::Code,
    opcode::Opcode,
    tmp::{AbsoluteIndexed, Tmp},
};

/// Computes the number of uses of a tmp based on frequency of execution. The frequency of blocks
/// that are only reachable by rare edges is scaled by Options::rareBlockPenalty().
pub struct UseCounts {
    gp_num_warm_uses_and_defs: Vec<f32>,
    fp_num_warm_uses_and_defs: Vec<f32>,
    gp_const_defs: BitVector,
    fp_const_defs: BitVector,
}

impl UseCounts {
    pub fn new(code: &Code) -> Self {
        let mut fast_worklist = GraphNodeWorklist::new();

        fast_worklist.push(BasicBlockId(0));

        while let Some(block) = fast_worklist.pop() {
            for succ in code.block(block).successors.iter() {
                if succ.1 != Frequency::Rare {
                    fast_worklist.push(succ.0);
                }
            }
        }

        let gp_array_size = AbsoluteIndexed::<{ Bank::GP }>::absolute_index(
            &Tmp::gp_tmp_for_index(code.num_tmps(Bank::GP)),
        );
        let mut gp_num_warm_uses_and_defs = vec![0.0; gp_array_size];
        let mut gp_const_defs = BitVector::with_capacity(gp_array_size);

        let fp_array_size = AbsoluteIndexed::<{ Bank::FP }>::absolute_index(
            &Tmp::fp_tmp_for_index(code.num_tmps(Bank::FP)),
        );
        let mut fp_num_warm_uses_and_defs = vec![0.0f32; fp_array_size];
        let mut fp_const_defs = BitVector::with_capacity(fp_array_size);

        for block in (0..code.blocks.len()).map(BasicBlockId) {
            let mut frequency = code.block(block).frequency;
            if !fast_worklist.saw(block) {
                frequency *= 0.001;
            }

            for inst in code.block(block).insts.iter() {
                inst.for_each_tmp(code, |tmp, role, bank, _| {
                    if role.is_warm_use() || role.is_any_def() {
                        if bank == Bank::GP {
                            let absolute_index =
                                AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp);
                            gp_num_warm_uses_and_defs[absolute_index] += frequency as f32;
                        } else {
                            let absolute_index =
                                AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp);
                            fp_num_warm_uses_and_defs[absolute_index] += frequency as f32;
                        }
                    }
                });

                if (inst.kind.opcode == Opcode::Move || inst.kind.opcode == Opcode::Move32)
                    && inst.args[0].is_some_imm()
                    && inst.args[1].is_tmp()
                {
                    let tmp = inst.args[1].tmp();

                    if tmp.bank() == Bank::GP {
                        gp_const_defs
                            .set(AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp), true);
                    } else {
                        fp_const_defs
                            .set(AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp), true);
                    }
                }
            }
        }

        Self {
            gp_num_warm_uses_and_defs,
            fp_num_warm_uses_and_defs,
            gp_const_defs,
            fp_const_defs,
        }
    }

    pub fn is_const_def<const BANK: Bank>(&self, absolute_index: usize) -> bool {
        if BANK == Bank::GP {
            self.gp_const_defs.get(absolute_index)
        } else {
            self.fp_const_defs.get(absolute_index)
        }
    }

    pub fn num_warm_uses_and_defs<const BANK: Bank>(&self, absolute_index: usize) -> f32 {
        if BANK == Bank::GP {
            self.gp_num_warm_uses_and_defs[absolute_index]
        } else {
            self.fp_num_warm_uses_and_defs[absolute_index]
        }
    }
}
