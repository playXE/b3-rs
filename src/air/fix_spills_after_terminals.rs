use crate::air::{inst::Inst, opcode::Opcode};

use super::{code::Code, insertion_set::InsertionSet, basic_block::BasicBlockId, block_order::rpo_sort};

/// Because there may be terminals that produce values, the register allocator may
/// want to spill those terminals. It'll happen to spill it after
/// the terminal. If we left the graph in this state, it'd be invalid
/// because a terminal must be the last instruction in a block.
/// We fix that here.
pub fn fix_spills_after_terminals(code: &mut Code<'_>) {
    // Because there may be terminals that produce values, IRC and LSRA may
    // want to spill those terminals. It'll happen to spill it after
    // the terminal. If we left the graph in this state, it'd be invalid
    // because a terminal must be the last instruction in a block.
    // We fix that here.

    let mut insertion_set = InsertionSet::new();

    let mut changed = false;

    for block_id in (0..code.blocks.len()).map(BasicBlockId) {
        let mut terminal_index = code.block(block_id).len();
        let mut found_terminal = false;

        while terminal_index > 0 {
            terminal_index -= 1;
            if code.block(block_id)[terminal_index].is_terminal(code) {
                found_terminal = true;
                break;
            }
        }

        assert!(found_terminal);

        if terminal_index == code.block(block_id).len() - 1 {
            continue;
        }

        // There must be instructions after the terminal because it's not the last instruction.

        let mut insts_to_move = Vec::with_capacity(1);

        for i in terminal_index + 1..code.block(block_id).len() {
            insts_to_move.push(code.block_mut(block_id)[i].clone());
        }

        for i in 0..code.block(block_id).successors.len() {
            let successor = code.block(block_id).successors[i];
            // If successor's only predecessor is block, we can plant the spill inside
            // the successor. Otherwise, we must split the critical edge and create
            // a new block for the spill.
            if code.block(successor.0).predecessors.len() == 1 {
                insertion_set.insert_insts(0, insts_to_move.clone());
                insertion_set.execute(code, successor.0);
            } else {
                let frequency = code.block(successor.0).frequency;
                let new_block = code.add_block(frequency);

                let origin = insts_to_move.last().unwrap().origin;
                for inst in insts_to_move.iter().cloned() {
                    code.block_mut(new_block).insts.push(inst);
                }

                code.block_mut(new_block).insts.push(Inst::new(Opcode::Jump.into(), origin, &[]));
                code.block_mut(new_block).successors.push(successor);
                code.block_mut(block_id).successors[i].0 = new_block;

                changed = true;
            }
        }

        code.block_mut(block_id).insts.truncate(terminal_index + 1);
    }

    if changed {
        code.reset_reachability();
        rpo_sort(code);
    }
}
