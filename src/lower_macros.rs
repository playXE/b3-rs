#![allow(dead_code)]
use std::{mem::size_of, rc::Rc};

use macroassembler::assembler::{
    abstract_macro_assembler::{BaseIndex, Extend, Scale},
    TargetMacroAssembler,
};

use crate::{
    effects::Effects,
    infer_switches::CaseCollection,
    insertion_set::InsertionSet,
    jit::{reg::Reg, register_set::RegisterSetBuilder},
    update_predecessors_after,
    analysis::use_counts::UseCounts,
    utils::bitvector::BitVector,
    BlockId, Frequency, FrequentBlock, NumChildren, Opcode, Procedure, Type, Value, ValueData,
    ValueId, Width,
};

/// Lowers high-level operations that it's easier to deal with once they are broken up. Currently
/// this includes Switch and ChillDiv.
pub fn lower_macros(proc: &mut Procedure) -> bool {
    let mut lowerer = LowerMacros::new(proc);
    lowerer.run()
}

struct LowerMacros<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
    use_counts: UseCounts,
    value: ValueId,
    block: BlockId,
    changed: bool,
}

impl<'a> LowerMacros<'a> {
    fn new(proc: &'a mut Procedure) -> Self {
        Self {
            use_counts: UseCounts::new(proc),
            insertion_set: InsertionSet::new(),
            proc,
            value: Default::default(),
            block: Default::default(),
            changed: false,
        }
    }

    fn run(&mut self) -> bool {
        for block in (0..self.proc.blocks.len()).map(BlockId) {
            self.block = block;
            self.process_current_block();
        }

        if self.changed {
            self.proc.reset_reachability();
            self.proc.invalidate_cfg();
        }

        self.changed
    }

    fn process_current_block(&mut self) {
        for index in 0..self.proc.block(self.block).len() {
            let value = self.proc.block(self.block)[index];
            self.value = value;

            match self.value.opcode(self.proc) {
                Opcode::Switch => {
                    let mut cases = vec![];
                    let case_collection = CaseCollection::new(
                        self.block,
                        self.proc.value(self.value).switch_cases().unwrap(),
                    );

                    for i in 0..case_collection.len(self.proc) {
                        let case = case_collection.at(self.proc, i);
                        cases.push(case);
                    }

                    cases.sort_by(|a, b| a.0.cmp(&b.0));

                    let fallthrough = case_collection.fallthrough(self.proc);
                    self.proc.block_mut(self.block).pop().unwrap();
                    let end = cases.len();
                    self.recursively_build_switch(cases, fallthrough, 0, false, end, self.block);
                    update_predecessors_after(self.block, &mut self.proc.blocks);
                    self.changed = true;
                }
                _ => (),
            }
        }
    }

    fn recursively_build_switch(
        &mut self,
        cases: Vec<(i64, FrequentBlock)>,
        fallthrough: FrequentBlock,
        start: usize,
        hard_start: bool,
        end: usize,
        mut before: BlockId,
    ) {
        let child = self.value.child(self.proc, 0);
        let typ = self.proc.value(child).typ();

        // It's a good idea to use a table-based switch in some cases: the number of cases has to be
        // large enough and they have to be dense enough. This could probably be improved a lot. For
        // example, we could still use a jump table in cases where the inputs are sparse so long as we
        // shift off the uninteresting bits. On the other hand, it's not clear that this would
        // actually be any better than what we have done here and it's not clear that it would be
        // better than a binary switch.
        const MIN_CASES_FOR_TABLE: usize = 7;
        const DENSITY_LIMIT: usize = 4;

        if end - start >= MIN_CASES_FOR_TABLE {
            let first_value = cases[start].0;
            let last_value = cases[end - 1].0;

            if (last_value - first_value + 1) / (end as i64 - start as i64) < DENSITY_LIMIT as i64 {
                let switch_block = self.proc.add_block(1.0);

                let constant = self.proc.add_int_constant(typ, first_value);
                self.proc.add_to_block(before, constant);
                let index = self.proc.add_binary(Opcode::Sub.into(), child, constant);
                self.proc.add_to_block(before, index);
                let constant2 = self
                    .proc
                    .add_int_constant(typ, (last_value - first_value) as i64);
                self.proc.add_to_block(before, constant2);
                let cmp = self.proc.add_binary(Opcode::Above.into(), index, constant2);
                self.proc.add_to_block(self.block, cmp);
                let br = self.proc.add_branch(cmp);
                self.proc.add_to_block(before, br);

                self.proc
                    .block_mut(before)
                    .set_successors2(fallthrough, (switch_block, Frequency::Normal));

                let table_size = (last_value - first_value + 1) as usize;

                let index = if self.proc.value(index).typ() == Type::Int32 {
                    let zext = self.proc.add(Value::new(
                        Opcode::ZExt32,
                        Type::Int64,
                        NumChildren::One,
                        &[index],
                        ValueData::None,
                    ));
                    self.proc.add_to_block(switch_block, zext);
                    zext
                } else {
                    index
                };

                let patchpoint = self.proc.add_patchpoint(Type::Void);
                self.proc.add_to_block(switch_block, patchpoint);

                // Even though this loads from the jump table, the jump table is immutable. For the
                // purpose of alias analysis, reading something immutable is like reading nothing.
                let mut effects = Effects::default();
                effects.terminal = true;

                *self.proc.patchpoint_effects_mut(patchpoint) = effects;

                self.proc.stackmap_append_some_register(patchpoint, index);
                self.proc
                    .patchpoint_mut(patchpoint)
                    .num_gp_scratch_registers = 2;
                let mut clobbered = RegisterSetBuilder::new();
                clobbered.add(
                    Reg::new_gpr(TargetMacroAssembler::SCRATCH_REGISTER),
                    Width::W64,
                );

                self.proc.stackmap_clobber(patchpoint, &clobbered);

                let mut handled_indices = BitVector::new();

                for i in start..end {
                    let block = cases[i].1;
                    let value = cases[i].0;

                    self.proc
                        .block_mut(switch_block)
                        .successor_list_mut()
                        .push(block);
                    let index = value - first_value;
                    handled_indices.set(index as usize, true);
                }

                let mut has_unhandled_indices = false;
            
                for i in 0..table_size {
                    if !handled_indices.get(i) {
                        has_unhandled_indices = true;
                        break;
                    }
                }

                if has_unhandled_indices {
                    self.proc
                        .block_mut(switch_block)
                        .successor_list_mut()
                        .push(fallthrough);
                }

                self.proc.stackmap_set_generator(
                    patchpoint,
                    Rc::new(move |jit, params| {
                        let (_, jump_table) = params
                            .proc_mut()
                            .add_data_section(size_of::<usize>() * table_size);

                        let index = params[0].gpr();
                        let scratch = params.gp_scratch(0);
                        jit.mov(jump_table as i64, scratch);
                        jit.load64(
                            BaseIndex::new(scratch, index, Scale::TimesEight, 0, Extend::None),
                            scratch,
                        );
                        jit.far_jump(scratch);

                        // These labels are guaranteed to be populated before either late paths or
                        // link tasks run.
                        let successors = params.successor_labels();
                        let bvec = handled_indices.clone();

                        jit.add_link_task(Box::new(move |link_buffer| unsafe {
                            if has_unhandled_indices {
                                let fallthrough =
                                    link_buffer.rx_location_of(*successors.last().unwrap().borrow());

                                for i in (0..table_size).rev() {
                                    jump_table.cast::<*const u8>().add(i).write(fallthrough);
                                }
                            }

                            let mut label_index = 0;
                            for table_index in bvec.iter() {
                                let loc = link_buffer.rx_location_of(*successors[label_index].borrow());
                               
                                jump_table.cast::<*const u8>().add(table_index).write(loc);
                                label_index += 1;
                            }
                        }));
                    }),
                );

                return;
            }
        }

        const LEAF_THRESHOLD: usize = 3;

        let size = end - start;

        if size <= LEAF_THRESHOLD {
            let mut all_consecutive = false;

            if (hard_start || (start != 0 && cases[start - 1].0 == cases[start].0 - 1))
                && end < cases.len()
                && cases[end - 1].0 == cases[end].0 - 1
            {
                all_consecutive = true;

                for i in 0..size {
                    if cases[start + i].0 + 1 != cases[start + i + 1].0 {
                        all_consecutive = false;
                        break;
                    }
                }
            }

            let limit = if all_consecutive { size - 1 } else { size };

            for i in 0..limit {
                let next_check = self.proc.add_block(1.0);

                let constant = self.proc.add_int_constant(typ, cases[start + i].0);
                self.proc.add_to_block(before, constant);
                let cmp = self.proc.add_binary(Opcode::Equal.into(), child, constant);
                self.proc.add_to_block(before, cmp);
                let br = self.proc.add_branch(cmp);
                self.proc.add_to_block(before, br);
                self.proc
                    .block_mut(before)
                    .set_successors2(cases[start + i].1, (next_check, Frequency::Normal));

                before = next_check;
            }

            let jmp = self.proc.add_jump();
            self.proc.add_to_block(before, jmp);
            if all_consecutive {
                self.proc.block_mut(before).set_successors(cases[end - 1].1);
            } else {
                self.proc.block_mut(before).set_successors(fallthrough);
            }
            return;
        }

        let median_index = (start + end) / 2;

        let left = self.proc.add_block(1.0);
        let right = self.proc.add_block(1.0);

        let constant = self.proc.add_int_constant(typ, cases[median_index].0);
        self.proc.add_to_block(before, constant);
        let cmp = self
            .proc
            .add_binary(Opcode::LessThan.into(), child, constant);
        self.proc.add_to_block(before, cmp);
        let br = self.proc.add_branch(cmp);
        self.proc.add_to_block(before, br);
        self.proc
            .block_mut(before)
            .set_successors2((left, Frequency::Normal), (right, Frequency::Normal));

        self.recursively_build_switch(
            cases.clone(),
            fallthrough,
            start,
            hard_start,
            median_index,
            left,
        );
        self.recursively_build_switch(cases, fallthrough, median_index, true, end, right);
    }
}
