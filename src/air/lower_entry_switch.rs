use crate::{dominators::GraphNodeWorklist, Frequency, utils::index_set::IndexMap};
use super::{basic_block::BasicBlockId, code::Code, opcode::Opcode};

/// Converts code that seems to have one entrypoint and emulates multiple entrypoints with
/// EntrySwitch into code that really has multiple entrypoints. This is accomplished by duplicating
/// the backwards transitive closure from all EntrySwitches.
pub fn lower_entry_switch(code: &mut Code) {
    let mut worklist = GraphNodeWorklist::new();

    for block in code.indices() {
        if code.block(block).last().unwrap().kind.opcode == Opcode::EntrySwitch {
            worklist.push(block);
        }
    }

    // It's possible that we don't have any EntrySwitches. That's fine.
    if worklist.seen().is_empty() {
        let entrypoints = vec![(BasicBlockId(0), Frequency::Normal)];
        code.entrypoints = entrypoints;
        return;
    }

    while let Some(block) = worklist.pop() {
        worklist.push_all(code.block(block).predecessors.iter().copied());
    }

    debug_assert!(worklist.saw(BasicBlockId(0)), "EntrySwitch must be reachable from the entry block");

    let mut entrypoint_frequencies = vec![Frequency::Rare; code.proc.num_entrypoints()];

    for block in code.indices() {
        if code.block(block).last().unwrap().kind.opcode != Opcode::EntrySwitch {
            continue;
        }

        for entrypoint_index in (0.. code.proc.num_entrypoints()).rev() {
            let entrypoint_frequency = std::cmp::max(
                entrypoint_frequencies[entrypoint_index],
                code.block(block).successors[entrypoint_index].1,
            );

            entrypoint_frequencies[entrypoint_index] = entrypoint_frequency;
        }
    }

    let fix_entry_switch = |code: &mut Code, block: BasicBlockId, entrypoint_index: usize| {
        if code.block(block).last().unwrap().kind.opcode != Opcode::EntrySwitch {
            return;
        }

        let target = code.block(block).successors[entrypoint_index];

        code.block_mut(block).last_mut().unwrap().kind.opcode = Opcode::Jump;
        code.block_mut(block).successors.resize(1, target);
    };

    let mut entrypoints = Vec::new();

    entrypoints.push((BasicBlockId(0), entrypoint_frequencies[0]));

    let mut map = IndexMap::with_capacity(code.blocks.len());


    for entrypoint_index in 1..code.proc.num_entrypoints() {
        map.clear();
        for block in worklist.seen().indices() {
            let frequency = code.block(BasicBlockId(block)).frequency;
            map.insert(BasicBlockId(block),code.add_block(frequency));
        }

        entrypoints.push((map[BasicBlockId(0)], entrypoint_frequencies[entrypoint_index]));

        for block in worklist.seen().indices().map(BasicBlockId) {
            let new_block = map[block];

            for inst_index in 0.. code.block(block).len() {
                let inst = code.block(block)[inst_index].clone();
                code.block_mut(new_block).push(inst.clone());
            }

            code.block_mut(new_block).successors = code.block(block).successors.clone();
            for successor in code.block_mut(block).successors.iter_mut().map(|(block, _)| block) {
                if let Some(replacement) = map.get(&successor) {
                    *successor = *replacement;
                }
            }

            fix_entry_switch(code, new_block, entrypoint_index);
        }
    }

    for block in worklist.seen().indices().map(BasicBlockId) {
        fix_entry_switch(code, block, 0);
    }
    code.entrypoints = entrypoints;
    code.reset_reachability();
}
