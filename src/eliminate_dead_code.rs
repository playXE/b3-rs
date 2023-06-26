use tinyvec::TinyVec;

use crate::{
    analysis::dominators::GraphNodeWorklist, effects::Effects, utils::index_set::IndexSet,
    variable::VariableId, BlockId, Opcode, Procedure, ValueId,
};

#[allow(dead_code)]
pub fn eliminate_dead_code(proc: &mut Procedure) -> bool {
    let mut changed = false;
    let mut worklist = GraphNodeWorklist::new();
    let mut upsilons = TinyVec::<[ValueId; 64]>::new();

    for block in (0..proc.blocks.len()).map(BlockId) {
        for i in 0..block.size(proc) {
            let value = block.value(proc, i);

            let mut effects = Effects::default();

            if proc.value(value).kind.opcode() != Opcode::Phi
                && proc.value(value).kind.opcode() != Opcode::Upsilon
            {
                effects = proc.value(value).effects();
            }

            if effects.must_execute() {
                worklist.push(value);
            }

            if let Some(_) = proc.value(value).phi() {
                upsilons.push(value);
            }
        }
    }

    loop {
        while let Some(value) = worklist.pop() {
            for child in proc.value(value).children.iter().copied() {
                worklist.push(child);
            }
        }

        let mut did_push = false;

        let mut upsilon_ix = 0;
        while upsilon_ix < upsilons.len() {
            let upsilon = upsilons[upsilon_ix];

            if worklist.saw(proc.value(upsilon).phi().unwrap()) {
                worklist.push(upsilon);
                upsilons[upsilon_ix] = upsilons.last().unwrap().clone();
                upsilons.pop();
                upsilon_ix = upsilon_ix.wrapping_sub(1);
                did_push = true;
            }
            upsilon_ix = upsilon_ix.wrapping_add(1);
        }

        if !did_push {
            break;
        }
    }

    let mut live_variables = IndexSet::new();

    for block in (0..proc.blocks.len()).map(BlockId) {
        let mut source_index = 0;
        let mut target_index = 0;

        while source_index < block.size(proc) {
            let value = block.value(proc, source_index);
            source_index += 1;

            if worklist.saw(value) {
                if let Some(var) = proc.value(value).as_variable() {
                    live_variables.insert(var);
                }

                proc.block_mut(block).values[target_index] = value;
                target_index += 1;
            } else {
                {
                    let mut out = String::new();
                    proc.value(value).fmt(&mut out, proc).unwrap();
                }
                proc.values.remove(value);
                changed = true;
            }
        }

        proc.block_mut(block).values.truncate(target_index);
    }

    for var in (0..proc.variables.size()).map(VariableId) {
        if !live_variables.contains(&var) {
            proc.delete_variable(var);
        }
    }

    changed
}
