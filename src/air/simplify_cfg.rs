use crate::block::Frequency;

use super::{basic_block::BasicBlockId, code::Code, inst::Inst, opcode::Opcode};

/// Simplifies the control flow graph by removing jump-only blocks and merging jumps.
pub fn simplify_cfg(code: &mut Code<'_>) -> bool {
    // We have three easy simplification rules:
    //
    // 1) If a successor is a block that just jumps to another block, then jump directly to
    //    that block.
    //
    // 2) If all successors are the same and the operation has no effects, then use a jump
    //    instead.
    //
    // 3) If you jump to a block that is not you and has one predecessor, then merge.
    //
    // Note that because of the first rule, this phase may introduce critical edges. That's fine.
    // If you need broken critical edges, then you have to break them yourself.

    let mut result = false;

    loop {
        let mut changed = false;

        for block_id in (0..code.blocks.len()).map(BasicBlockId) {
            if code.block(block_id).successors.len() == 0 {
                continue; // we don't care about blocks that don't have successors.
            }

            for i in 0..code.block(block_id).successors.len() {
                if code.block(block_id).successors[i].0 != block_id
                    && code.block(code.block(block_id).successors[i].0).len() == 1
                    && code
                        .block(code.block(block_id).successors[i].0)
                        .last()
                        .unwrap()
                        .kind
                        .opcode
                        == Opcode::Jump
                {
                    let new_successor =
                        code.block(code.block(block_id).successors[i].0).successors[0].0;

                    if new_successor != code.block(block_id).successors[i].0 {
                        code.block_mut(block_id).successors[i].0 = new_successor;

                        code.block_mut(new_successor).predecessors.push(block_id);

                        changed = true;
                    }
                }
            }

            // Now check if the block's terminal can be replaced with a jump. The terminal must not
            // have weird effects.
            if code.block(block_id).successors.len() > 1
                && !code
                    .block(block_id)
                    .last()
                    .unwrap()
                    .has_non_arg_control_effects(code)
            {
                let mut all_same = true;

                let first_successor = code.block(block_id).successors[0].0;

                for i in 1..code.block(block_id).successors.len() {
                    if code.block(block_id).successors[i].0 != first_successor {
                        all_same = false;
                        break;
                    }
                }

                if all_same {
                    code.block_mut(block_id).successors.truncate(1);
                    code.block_mut(block_id).successors[0].1 = Frequency::Normal;
                    let origin = code.block(block_id).last().unwrap().origin;
                    *code.block_mut(block_id).insts.last_mut().unwrap() =
                        Inst::new(Opcode::Jump.into(), origin, &[]);
                    changed = true;
                }
            }
        }

        if changed {
            code.reset_reachability();
            result = true;
        } else {
            break;
        }
    }

    result
}
