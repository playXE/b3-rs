use std::collections::HashMap;

use tinyvec::TinyVec;

use crate::{
    block::{blocks_in_pre_order, BlockId},
    dominators::Dominators,
    opcode::Opcode,
    procedure::Procedure,
    value::{Value, ValueId},
};

//' This is a reusable utility for doing pure CSE. You can use it to do pure CSE on a program by just
/// proceeding in order and calling process().
pub struct PureCSE {
    map: HashMap<ValueId, TinyVec<[ValueId; 1]>>,
}

impl PureCSE {
    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn find_match(
        &self,
        key: Option<ValueId>,
        proc: &Procedure,
        block: BlockId,
        dominators: &Dominators<Procedure>,
    ) -> Option<ValueId> {
        if let Some(key) = key {
            let result = self.map.get(&key)?;

            for match_ in result.iter() {
                if let Some(owner) = proc.value(*match_).owner {
                    // Value is moved to a new BasicBlock which is not inserted yet.
                    // In that case, we should just ignore it. PureCSE will be recomputed after new BasicBlocks are actually inserted
                    if proc.block(owner).index == usize::MAX {
                        continue;
                    }

                    if dominators.dominates(owner, block) {
                        return Some(*match_);
                    }
                } else {
                    continue;
                }
            }

            None
        } else {
            None
        }
    }

    pub fn process(
        &mut self,
        proc: &mut Procedure,
        value: ValueId,
        dominators: &Dominators<Procedure>,
    ) -> bool {
        if proc.value(value).kind.opcode() == Opcode::Identity || proc.value(value).is_constant() {
            return false;
        }

        self.map.insert(value, TinyVec::new());

        let matches = match self.map.entry(value) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => entry.insert(TinyVec::new()),
        };
        for match_ in matches.iter().copied() {
            if let Some(owner) = proc.value(match_).owner {
                if proc.block(owner).index == usize::MAX {
                    continue;
                }

                if dominators.dominates(owner, proc.value(value).owner.unwrap()) {
                    proc.value_mut(value).replace_with_identity(match_);
                    return true;
                }
            }
        }

        matches.push(value);
        false
    }
}

pub fn pure_cse(proc: &mut Procedure) -> bool {
    proc.dominators_or_compute();
    let dominators = proc.dominators().clone();

    let mut pure_cse = PureCSE::new();

    let mut result = false;

    for block_id in blocks_in_pre_order(BlockId(0), proc) {
        for value in proc.block(block_id).values.clone() {
            result |= Value::perform_substitution(value, proc);
            result |= pure_cse.process(proc, value, &dominators);
        }
    }

    result
}
