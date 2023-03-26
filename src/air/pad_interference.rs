use crate::value::ValueId;

use super::{code::Code, insertion_set::InsertionSet, inst::Inst, opcode::Opcode, basic_block::BasicBlockId};

/// This isn't a phase - it's meant to be a utility that other phases use. Air reasons about liveness by
/// reasoning about interference at boundaries between instructions. This is convenient because it works
/// great in the most common case: early uses and late defs. However, this can go wrong - for example, a
/// late use in one instruction doesn't actually interfere with an early def of the next instruction, but
/// Air thinks that it does. It can also go wrong by having liveness incorrectly report that something is
/// dead when it isn't.
///
/// See https://bugs.webkit.org/show_bug.cgi?id=163548#c2 for more info.
pub fn pad_interference(code: &mut Code<'_>) {
    let mut insertion_set = InsertionSet::new();

    for block_id in 0..code.blocks.len() {
        let block = &code.blocks[block_id];
        for inst_index in 1..block.insts.len() {
            if Inst::needs_padding(&block[inst_index - 1], &block[inst_index]) {
                insertion_set.insert_inst(inst_index, Inst::new(Opcode::Nop.into(), ValueId(0), &[]));
            }
        }

        insertion_set.execute(code, BasicBlockId(block_id));
    }
}