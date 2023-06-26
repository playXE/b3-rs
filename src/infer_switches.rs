use crate::{
    analysis::use_counts::UseCounts, insertion_set::InsertionSet, rpo::rpo_sort, BlockId,
    FrequentBlock, Opcode, Procedure, Type, Value, ValueData, ValueId,
};

/// Fixpoints to convert chains of branches into switches.
pub fn infer_switches(proc: &mut Procedure) -> bool {
    let mut infer = InferSwitches::new(proc);
    infer.run()
}

struct InferSwitches<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
    use_counts: UseCounts,
}

impl<'a> InferSwitches<'a> {
    fn new(proc: &'a mut Procedure) -> Self {
        let use_counts = UseCounts::new(proc);

        Self {
            insertion_set: InsertionSet::new(),
            proc,
            use_counts,
        }
    }

    fn run(&mut self) -> bool {
        let mut changed = true;
        let mut ever_changed = false;

        while changed {
            changed = false;

            for block in (0..self.proc.blocks.len()).map(BlockId) {
                changed |= self.attempt_to_merge_with_predecessor(block);
            }

            ever_changed |= changed;
        }

        if ever_changed {
            self.proc.reset_reachability();
            self.proc.invalidate_cfg();

            rpo_sort(self.proc);
            return true;
        }

        false
    }

    fn attempt_to_merge_with_predecessor(&mut self, block: BlockId) -> bool {
        // No point in considering the root block. We also don't consider blocks with multiple
        // predecessors, but we could handle this if we made this code a bit more general and we were
        // not afraid of code bloat.
        if self.proc.block(block).predecessor_list().len() != 1 {
            return false;
        }

        let description = self.describe(block);

        if description.block.is_none() {
            return false;
        }

        // We know that this block behaves like a switch. But we need to verify that it doesn't also
        // perform any effects or do expensive things. We don't want to create a switch if that will
        // make expensive things execute unconditionally. We're very conservative about how we define
        // "expensive".
        for &value in self.proc.block(block).iter() {
            if self.proc.value(value).is_free() {
                continue;
            }

            if description.extra == Some(value) {
                continue;
            }

            if description.branch == Some(value) {
                continue;
            }

            return false;
        }

        let predecessor = self.proc.block(block).predecessor_list()[0];
        let predecessor_description = self.describe(predecessor);

        if predecessor_description.block.is_none() {
            return false;
        }

        // Both us and the predecessor are switch-like, but that doesn't mean that we're compatible.
        // We may be switching on different values!
        if description.source != predecessor_description.source {
            return false;
        }

        // We expect that we are the fall-through destination of the predecessor. This is a bit of a
        // goofy condition. If we were not the fall-through destination then our switch is probably
        // just totally redundant and we should be getting rid of it. But we don't handle that here,
        // yet.
        if predecessor_description.fallthrough.0 != block {
            return false;
        }

        // Make sure that there ain't no loops.
        if description.fallthrough.0 == block || description.fallthrough.0 == predecessor {
            return false;
        }

        for &(_, case) in description.cases.iter() {
            if case.0 == block || case.0 == predecessor {
                return false;
            }
        }

        for &(_, case) in predecessor_description.cases.iter() {
            if case.0 == block || case.0 == predecessor {
                return false;
            }
        }

        // We're committed to doing the thing.

        // Delete the extra value from the predecessor, since that would break downstream inference
        // on the next fixpoint iteration. We would think that this block is too expensive to merge
        // because of the Equal or NotEqual value even though that value is dead! We know it's dead
        // so we kill it ourselves.

        for value_index in 0..self.proc.block(predecessor).len() {
            let value = self.proc.block(predecessor)[value_index];

            if predecessor_description.extra == Some(value) {
                self.proc.value_mut(value).replace_with_nop_ignoring_type();
            }
        }

        // Insert all non-terminal values from our block into our predecessor. We definitely need to
        // do this for constants. We must not do it for the extra value, since that would break
        // downstream inference on the next fixpoint iteration. As a bonus, we don't do it for nops,
        // so that we limit how big blocks get in this phase.

        for i in 0..self.proc.block(block).len() - 1 {
            let value = self.proc.block(block)[i];
            if description.extra != Some(value) && value.opcode(self.proc) != Opcode::Nop {
                let at = self.proc.block(predecessor).len() - 1;
                self.insertion_set.insert_value(at, value);
            }
        }

        self.insertion_set.execute(self.proc, predecessor);

        self.proc.block_mut(block).truncate(0);
        let oops = Value::new(
            Opcode::Oops,
            Type::Void,
            crate::NumChildren::Zero,
            &[],
            ValueData::None,
        );
        let oops = self.proc.add(oops);
        self.proc.block_mut(block).push(oops);
        self.proc.block_mut(block).remove_predecessor(predecessor);

        for successor_index in 0..self
            .proc
            .block(description.block.unwrap())
            .successor_list()
            .len()
        {
            let successor =
                self.proc.block(description.block.unwrap()).successor_list()[successor_index];

            self.proc
                .block_mut(successor.0)
                .replace_predecessor(block, predecessor);
        }

        self.proc.block_mut(block).successor_list_mut().clear();

        let switch_value = Value::new(
            Opcode::Switch,
            Type::Void,
            crate::NumChildren::One,
            &[description.source.unwrap()],
            ValueData::Switch(vec![]),
        );
        let switch_value = self.proc.add(switch_value);

        *self.proc.block_mut(predecessor).last_mut().unwrap() = switch_value;
        self.proc.value_mut(switch_value).owner = Some(predecessor);

        self.proc
            .block_mut(predecessor)
            .successor_list_mut()
            .clear();
        self.proc
            .switch_fallthrough(switch_value, predecessor, description.fallthrough);

        let mut predecessor_cases = vec![];

        for &switch_case in predecessor_description.cases.iter() {
            self.proc.switch_append_case(switch_value, switch_case);
            predecessor_cases.push(switch_case.0);
        }

        predecessor_cases.sort();

        let is_predecessor_case = |value: i64| predecessor_cases.binary_search(&value).is_ok();

        for switch_case in description.cases.iter() {
            if !is_predecessor_case(switch_case.0) {
                self.proc.switch_append_case(switch_value, *switch_case);
            }
        }

        true
    }

    fn describe(&mut self, block: BlockId) -> SwitchDescription {
        let mut result = SwitchDescription::default();
        result.block = Some(block);
        result.branch = Some(self.proc.block(block).last().copied().unwrap());

        match result.branch.unwrap().opcode(self.proc) {
            Opcode::Branch => {
                let predicate = result.branch.unwrap().child(self.proc, 0);
                let taken = self.proc.block(block).taken();
                let not_taken = self.proc.block(block).not_taken();

                let mut handled = false;
                // NOTE: This uses UseCounts that we computed before any transformation. This is fine
                // because although we may have mutated the IR, we would not have added any new
                // predicates.
                if self.proc.value(predicate).children.len() == 2
                    && self.proc.value(predicate.child(self.proc, 1)).has_int()
                    && self.use_counts.num_uses(predicate) == 1
                {
                    match predicate.opcode(self.proc) {
                        Opcode::Equal => {
                            result.source = Some(predicate.child(self.proc, 0));
                            result.extra = Some(predicate);
                            result.cases.push((
                                self.proc
                                    .value(predicate.child(self.proc, 1))
                                    .as_int()
                                    .unwrap(),
                                taken,
                            ));
                            result.fallthrough = not_taken;
                            handled = true;
                        }

                        Opcode::NotEqual => {
                            result.source = Some(predicate.child(self.proc, 0));
                            result.extra = Some(predicate);
                            result.cases.push((
                                self.proc
                                    .value(predicate.child(self.proc, 1))
                                    .as_int()
                                    .unwrap(),
                                not_taken,
                            ));
                            result.fallthrough = taken;
                            handled = true;
                        }

                        _ => (),
                    }
                }

                if !handled {
                    result.source = Some(predicate);
                    result.cases.push((0, not_taken));
                    result.fallthrough = taken;
                }
            }

            Opcode::Switch => {
                let source = result.branch.unwrap().child(self.proc, 0);
                result.source = Some(source);

                let cases =
                    CaseCollection::new(block, &self.proc.value(source).switch_cases().unwrap());

                for i in 0..cases.len(self.proc) {
                    let (value, block) = cases.at(self.proc, i);
                    result.cases.push((value, block));
                }

                result.fallthrough = cases.fallthrough(self.proc);
            }

            _ => {
                result.block = None;
                result.branch = None;
            }
        }
        return result;
    }
}

#[derive(Default, PartialEq, Eq, Debug)]
struct SwitchDescription {
    block: Option<BlockId>,
    branch: Option<ValueId>,
    extra: Option<ValueId>,
    source: Option<ValueId>,
    cases: Vec<(i64, FrequentBlock)>,
    fallthrough: FrequentBlock,
}

pub struct CaseCollection<'a> {
    pub owner: BlockId,
    pub cases: &'a [i64],
}

impl<'a> CaseCollection<'a> {
    pub fn new(owner: BlockId, cases: &'a [i64]) -> Self {
        Self { owner, cases }
    }

    pub fn fallthrough(&self, proc: &Procedure) -> FrequentBlock {
        proc.block(self.owner).fallthrough()
    }

    pub fn len(&self, _proc: &Procedure) -> usize {
        self.cases.len()
    }

    pub fn at(&self, proc: &Procedure, ix: usize) -> (i64, FrequentBlock) {
        let value = self.cases[ix];
        (value, proc.block(self.owner).successor_list()[ix])
    }
}
