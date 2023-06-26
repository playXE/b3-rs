use tinyvec::TinyVec;

use crate::{rpo::rpo_sort, BlockId, Frequency, Procedure};

/// Creates a pre-header for any loop that don't already have one. A loop has a pre-header if its header has
/// exactly one predecessor that isn't in the loop body. If a loop header has more than one out-of-body
/// predecessor, then this creates a new block and rewires those predecessors to it.
pub fn ensure_loop_pre_headers(proc: &mut Procedure) -> bool {
    let natural_loops = proc.natural_loops_or_compute();
    let mut changed = false;
    for loop_index in (0..natural_loops.num_loops()).rev() {
        let loop_ = natural_loops.loop_(loop_index);

        let mut out_of_body_predecessors = TinyVec::<[BlockId; 4]>::new();

        let mut total_frequency = 0.0;

        for &predecessor in proc.block(loop_.header()).predecessor_list().iter() {
            if !natural_loops.belongs_to(predecessor, loop_) {
                out_of_body_predecessors.push(predecessor);
                total_frequency += proc.block(predecessor).frequency();
            }
        }

        if out_of_body_predecessors.len() <= 1 {
            continue;
        }
        changed = true;
        let pre_header = proc.add_block(total_frequency);

        let jmp = proc.add_jump();
        proc.add_to_block(pre_header, jmp);
        proc.successors_mut(pre_header)
            .push((loop_.header(), Frequency::Normal));

        for predecessor in out_of_body_predecessors {
            proc.block_mut(predecessor)
                .replace_successor(loop_.header(), pre_header);
            proc.block_mut(pre_header).add_predecessor(predecessor);
            proc.block_mut(loop_.header())
                .remove_predecessor(predecessor);
        }

        proc.block_mut(loop_.header()).add_predecessor(pre_header);
    }

    if changed {
        proc.invalidate_cfg();
        rpo_sort(proc);
    }
    changed
}
