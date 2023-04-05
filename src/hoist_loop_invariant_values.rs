use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use crate::{
    blocks_in_pre_order,
    dominators::{BackwardsGraph, Dominators, SingleGraphNode},
    ensure_loop_pre_headers::ensure_loop_pre_headers,
    utils::RangeExt,
    BlockId, Procedure,
};

pub fn hoist_loop_invariant_values(proc: &mut Procedure) -> bool {
    ensure_loop_pre_headers(proc);

    let natural_loops = proc.natural_loops_or_compute();
    if natural_loops.num_loops() == 0 {
        return false;
    }

    proc.reset_value_owners();
    proc.dominators_or_compute();
    let dominators = proc.dominators().clone();
    let proc2 = unsafe {
        &mut *(proc as *mut Procedure as *mut Procedure)
    };
    
    let backwards_dominators = {
        let backwards = BackwardsGraph::new(proc2);
        Dominators::new(&backwards)
    };

    #[derive(Default)]
    struct LoopData {
        writes: HashSet<Range<usize>>,
        writes_local_state: bool,
        writes_pinned: bool,
        side_exits: bool,
        pre_header: Option<BlockId>,
    }

    let mut data = HashMap::<usize, LoopData>::new();

    for loop_index in (0..natural_loops.num_loops()).rev() {
        let loop_ = natural_loops.loop_(loop_index);

        for &predecessor in proc.block(loop_.header()).predecessor_list().iter() {
            if !natural_loops.belongs_to(predecessor, loop_) {
                let entry = data.entry(loop_index).or_default();
                assert!(entry.pre_header.is_none());
                entry.pre_header = Some(predecessor);
            }
        }
    }

    for (id, block) in proc.blocks.iter().enumerate() {
        let loop_ = natural_loops.inner_most_loop_of(BlockId(id));

        if let Some(loop_) = loop_ {
            for &value in block.iter() {
                let effects = proc.value(value).effects();

                let entry = data.get_mut(&loop_.index()).unwrap();

                entry.writes.insert(effects.writes);
                entry.writes_local_state |= effects.writes_local_state;
                //entry.writes_pinned |= effects.writes_pinned;
                entry.side_exits |= effects.exit_sideways;
            }
        }
    }

    for loop_index in (0..natural_loops.num_loops()).rev() {
        let loop_ = natural_loops.loop_(loop_index);

        let mut current = natural_loops.inner_most_outer_loop_of(&loop_);

        let writes = data.get(&loop_.index()).unwrap().writes.clone();
        let writes_local_state = data.get(&loop_.index()).unwrap().writes_local_state;
        let writes_pinned = data.get(&loop_.index()).unwrap().writes_pinned;
        let side_exits = data.get(&loop_.index()).unwrap().side_exits;

        while let Some(cur) = current {
            let entry = data.get_mut(&cur.index()).unwrap();

            for write in writes.iter() {
                entry.writes.insert(write.clone());
            }

            entry.writes_local_state |= writes_local_state;
            entry.writes_pinned |= writes_pinned;
            entry.side_exits |= side_exits;

            current = natural_loops.inner_most_outer_loop_of(&cur);
        }
    }

    let mut changed = false;

    for block in blocks_in_pre_order(BlockId(0), proc) {
        let block_loops = natural_loops.loops_of(block);

        if block_loops.is_empty() {
            continue;
        }

        for value_index in 0..proc.block(block).len() {
            let value = proc.block(block)[value_index];

            let effects = proc.value(value).effects();

            // We never hoist write effects or control constructs.
            if effects.must_execute() {
                continue;
            }

            // Try outermost loop first.
            for i in (0..block_loops.len()).rev() {
                let loop_ = block_loops[i];
                let mut ok = true;
                let loop_data = data.get(&loop_.index()).unwrap();
                for &child in proc.value(value).children.iter() {
                    if !dominators.dominates(
                        proc.value(child).owner.unwrap(),
                        loop_data.pre_header.unwrap(),
                    ) {
                        ok = false;
                        break;
                    }
                }

                if !ok {
                    continue;
                }

                if effects.control_dependent {
                    if !backwards_dominators.dominates(
                        SingleGraphNode::new(block),
                        SingleGraphNode::new(loop_data.pre_header.unwrap()),
                    ) {
                        continue;
                    }

                    if loop_data.side_exits {
                        continue;
                    }
                }

                if effects.reads_local_state && loop_data.writes_local_state {
                    continue;
                }

                if loop_data
                    .writes
                    .iter()
                    .any(|write| write.overlaps(&effects.writes))
                {
                    continue;
                }
               
                proc.block_mut(loop_data.pre_header.unwrap())
                    .append_non_terminal(value);
                let nop = proc.add_nop();

                proc.block_mut(block)[value_index] = nop;
                proc.value_mut(nop).owner = Some(block);
                changed = true;
            }
        }
    }

    changed
}
