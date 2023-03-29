use std::{collections::HashMap, mem::size_of};

use crate::{
    air::arg::Arg,
    block::BlockId,
    insertion_set::InsertionSet,
    opcode::Opcode,
    procedure::Procedure,
    utils::{index_set::{IndexMap, IndexSet}, phase_scope::phase_scope},
    value::{Value, ValueId}, typ::Type,
};

/// Moves large constants around, with the goal of placing them in the optimal points in the program.
pub fn move_constants(proc: &mut Procedure) {
    phase_scope("b3::move_constants", || {
        
        let mut move_constants = MoveConstants {
            proc,
            insertion_set: InsertionSet::new(),
        };

        move_constants.hoist_constants(|value| {
            matches!(
                value.kind.opcode(),
                Opcode::ConstFloat | Opcode::ConstDouble 
            )
        });
        
        move_constants.lower_materialization_cost_heavy_constants();

        move_constants.hoist_constants(|value| {
            matches!(
                value.kind.opcode(),
                Opcode::Const32 | Opcode::Const64 | Opcode::ArgumentReg
            )
        });
        
    });
}

struct MoveConstants<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
}

impl<'a> MoveConstants<'a> {
    #[allow(dead_code)]
    fn goes_in_table(val: ValueId, proc: &Procedure) -> bool {
        match proc.value(val).kind.opcode() {
            Opcode::ConstDouble => {
                let double_zero = 0.0;
                proc.value(val).as_double().unwrap() != double_zero
            }

            Opcode::ConstFloat => {
                let float_zero = 0.0;
                proc.value(val).as_float().unwrap() != float_zero
            }

            _ => false,
        }
    }

    fn hoist_constants(&mut self, filter: impl Fn(&Value) -> bool) {
        let dominators = self.proc.dominators().clone();
        let mut value_for_constant = HashMap::new();
        let mut materializations =
            IndexMap::<Vec<ValueId>, BlockId>::with_capacity(self.proc.blocks.len());

        for block in (0..self.proc.blocks.len()).map(BlockId) {
            materializations.insert(block, Vec::new());
        }

        for block in (0..self.proc.blocks.len()).map(BlockId) {
            for i in 0..self.proc.block(block).len() {
                let value = self.proc.block(block)[i];

                for i in 0..self.proc.value(value).children.len() {
                    let mut child = self.proc.value(value).children[i];

                    if filter(self.proc.value(child)) {
                        let result = value_for_constant.insert(child, child);

                        if let None = result {
                            // Assume that this block is where we want to materialize the value.
                            self.proc.value_mut(child).owner = Some(block);
                            continue;
                        }
                        // Make 'value' use the canonical constant rather than the one it was using.
                        child = result.unwrap();
                        self.proc.value_mut(value).children[i] = child;

                        // Determine the least common dominator. That's the lowest place in the CFG where
                        // we could materialize the constant while still having only one materialization
                        // in the resulting code.
                        while !dominators.dominates(self.proc.value(child).owner.unwrap(), block) {
                            self.proc.value_mut(child).owner =
                                dominators.idom(self.proc.value(child).owner.unwrap());
                        }
                    }
                }
            }
        }

        // Make sure that each basic block knows what to materialize. This also refines the
        // materialization block based on execution frequency. It finds the minimum block frequency
        // of all of its dominators, and selects the closest block amongst those that are tied for
        // lowest frequency.

        for (_, &entry) in value_for_constant.iter() {
            let mut block = self.proc.value(entry).owner;

            while let Some(bb) = block {
                if self.proc.block(bb).frequency() < self.proc.block(block.unwrap()).frequency() {
                    self.proc.value_mut(entry).owner = Some(bb);
                }

                block = dominators.idom(bb);
            }

            materializations
                .get_mut(&self.proc.value(entry).owner.unwrap())
                .unwrap()
                .push(entry);
        }

        // Get rid of Value's that are fast constants but aren't canonical. Also remove the canonical
        // ones from the CFG, since we're going to reinsert them elsewhere.

        for block in (0..self.proc.blocks.len()).map(BlockId) {
            for i in 0..self.proc.block(block).len() {
                let value = self.proc.block(block)[i];

                if filter(self.proc.value(value)) {
                    if value_for_constant.get(&value) == Some(&value) {
                        let nop = self.proc.add_nop();

                        self.proc.block_mut(block)[i] = nop;
                    } else {
                        Value::replace_with_nop_ignoring_type(&mut self.proc.value_mut(value));
                    }
                }
            }
        }

        // Now make sure that we move constants to where they are supposed to go. Again, we do this
        // based on uses.
        for block in (0..self.proc.blocks.len()).map(BlockId) {
            for value_index in 0..self.proc.block(block).len() {
                let find_best_constant = |this: &mut Self,
                                          predicate: &dyn Fn(&mut Self, ValueId) -> bool|
                 -> Option<ValueId> {
                    let mut result = None;

                    dominators.for_all_dominators_of(block, |dominator| {
                        for &value in &materializations[dominator] {
                            if predicate(this, value) {
                                result = Some(value);
                                return;
                            }
                        }
                    });

                    result
                };

                // We call this when we have found a constant that we'd like to use. It's possible that
                // we have computed that the constant should be materialized in this block, but we
                // haven't inserted it yet. This inserts the constant if necessary.
                let materialize = |this: &mut Self, child: ValueId| {
                    if !filter(this.proc.value(child)) {
                        return;
                    }

                    assert!(value_for_constant.get(&child) == Some(&child));

                    if this.proc.value(child).owner != Some(block) {
                        // This constant isn't our problem. It's going to be materialized in another
                        // block.
                        return;
                    }

                    // We're supposed to materialize this constant in this block, and we haven't
                    // done it yet.
                  
                    this.insertion_set.insert_value(value_index, child);
                    this.proc.value_mut(child).owner = None;
                };

                let value = self.proc.block(block)[value_index];

                if self.proc.value_mut(value).memory_value().is_some() {
                    let pointer = self.proc.value(value).children.last().copied().unwrap();

                    if self.proc.value(pointer).has_int64() && filter(&self.proc.value(pointer)) {
                        let desired_offset = |this: &Self, other_pointer: &Value| -> usize {
                            // We would turn this:
                            //
                            //     Load(@p, offset = c)
                            //
                            // into this:
                            //
                            //     Load(@q, offset = ?)
                            //
                            // The offset should be c + @p - @q, because then we're loading from:
                            //
                            //     @q + c + @p - @q
                            let c = this.proc.value(value).memory_value().unwrap().0 as usize;
                            let p = this.proc.value(pointer).as_int64().unwrap() as usize;
                            let q = other_pointer.as_int64().unwrap() as usize;

                            c + p - q
                        };

                        let best_pointer = find_best_constant(self, &|this, candidate_pointer| {
                            if !this.proc.value(candidate_pointer).has_int64() {
                                return false;
                            }

                            let candidate_offset =
                                desired_offset(this, this.proc.value(candidate_pointer));

                            let is_legal = candidate_offset as i32 as usize == candidate_offset
                                && is_legal_offset(candidate_offset as _);
                            is_legal
                        });

                        if let Some(best_pointer) = best_pointer {
                            let offset = desired_offset(self, self.proc.value(best_pointer));
                            *self.proc.value_mut(value).children.last_mut().unwrap() = best_pointer;
                            *self.proc.value_mut(value).memory_value_mut().unwrap().0 =
                                offset as i32;
                        }
                    }
                } else {
                    match self.proc.value(value).kind.opcode() {
                        Opcode::Add | Opcode::Sub => {
                            let is_add = self.proc.value(value).kind.opcode() == Opcode::Add;
                            let typ = self.proc.value(value).typ();
                            let addend = self.proc.value(value).children[1];

                            if let Some(addend_const) = self.proc.value(addend).as_int() {
                                let best_addend =
                                    find_best_constant(self, &|this, candidate_addend| {
                                        if this.proc.value(candidate_addend).typ()
                                            != this.proc.value(addend).typ()
                                        {
                                            return false;
                                        }

                                        if let Some(c) = this.proc.value(candidate_addend).as_int()
                                        {
                                            candidate_addend == addend || c == -addend_const
                                        } else {
                                            false
                                        }
                                    });

                                if best_addend.is_none() || best_addend == Some(addend) {
                                } else {
                                    let child = self.proc.value(value).children[0];
                                    materialize(self, child);
                                    materialize(self, best_addend.unwrap());

                                    let op = Value::new(
                                        if is_add { Opcode::Add } else { Opcode::Sub },
                                        typ,
                                        crate::value::NumChildren::Two,
                                        &[child, best_addend.unwrap()],
                                        crate::value::ValueData::None,
                                    );

                                    let new_value = self.proc.add(op);

                                    self.proc.value_mut(value).replace_with_identity(new_value);
                                }
                            }
                        }

                        _ => (),
                    }
                }

                for i in 0..self.proc.value(value).children.len() {
                    let child = self.proc.value(value).children[i];
                    materialize(self, child);
                }
            }

            // We may have some constants that need to be materialized right at the end of this
            // block.
            for value in materializations[block].iter().copied() {
                if self.proc.value(value).owner == None {
                    continue;
                }

                self.insertion_set
                    .insert_value(self.proc.block(block).len() - 1, value);
            }

            self.insertion_set.execute(&mut self.proc, block);
        }
    }

    fn lower_materialization_cost_heavy_constants(&mut self) {
        let mut float_size = 0usize;
        let mut double_size = 0usize;

        let mut const_table = HashMap::new();

        for value in (0..self.proc.values.size()).map(ValueId) {
            if !Self::goes_in_table(value, self.proc) {
                continue;
            }

            let val = self.proc.value(value);

            match val.kind.opcode() {
                Opcode::ConstDouble => {
                    const_table.insert(value, double_size);
                    double_size += 1;
                }

                Opcode::ConstFloat => {
                    const_table.insert(value, float_size);
                    float_size += 1;
                }

                _ => (),
            }
        }

        let get_offset = |opcode, index_in_kind| -> usize {
            match opcode {
                Opcode::ConstDouble => index_in_kind * size_of::<f64>(),
                Opcode::ConstFloat => size_of::<f64>() * double_size + index_in_kind * size_of::<f32>(),

                _ => unreachable!()
            }
        };

        let (_, data_section) = self.proc.add_data_section(double_size * size_of::<f64>() + float_size * size_of::<f32>());

        for (key, value) in const_table.iter() {
            let pointer = data_section as usize + get_offset(self.proc.value(*key).kind.opcode(), *value);

            match self.proc.value(*key).kind.opcode() {
                Opcode::ConstDouble => {
                    let val = self.proc.value(*key).as_double().unwrap();
                    unsafe {
                        *(pointer as *mut f64) = val;
                    }
                }

                Opcode::ConstFloat => {
                    let val = self.proc.value(*key).as_float().unwrap();
                    unsafe {
                        *(pointer as *mut f32) = val;
                    }
                }

                _ => unreachable!()
            }
        }

        let mut off_limits = IndexSet::new();

        for block in (0..self.proc.blocks.len()).map(BlockId) {
            for value_index in 0..self.proc.block(block).len() {
                let value = self.proc.block(block)[value_index];

                if self.proc.value(value).stackmap().is_some() {
                    for child_index in 0..self.proc.value(value).children.len() {
                        let child = self.proc.value(value).constrained_child(child_index);
                        if !child.rep.is_any() {
                            continue;
                        }

                        let child = value.child(self.proc, child_index);

                        if !Self::goes_in_table(child, self.proc) {
                            continue;
                        }

                        let new_child = child.materialize(self.proc);
                        let val = self.insertion_set.insert_value(value_index, new_child);
                        self.proc.value_mut(value).children[child_index] = val;
                        off_limits.insert(val);
                    }
                }
            }

            self.insertion_set.execute(&mut self.proc, block);
        }

        for block in (0..self.proc.blocks.len()).map(BlockId) {
            for value_index in 0..block.size(self.proc) {
                let value = block.value(self.proc, value_index);

                if !Self::goes_in_table(value, self.proc) {
                    continue;
                }

                if off_limits.contains(&value) {
                    continue;
                }

                let offset = get_offset(self.proc.value(value).kind.opcode(), *const_table.get(&value).unwrap());

                if offset as i32 as usize != offset {
                    continue;
                }

                let table_base = self.proc.add_int_constant(Type::Int64, data_section as i64);
                let typ = self.proc.value(value).typ();
                let result = self.proc.add_load(Opcode::Load.into(), typ, table_base, offset as i32, 0..0, 0..0);
                self.insertion_set.insert_value(value_index, result);
                self.proc.value_mut(value).replace_with_identity(result);
            }

            self.insertion_set.execute(&mut self.proc, block);
        }
    }
}

pub fn is_legal_offset(offset: i32) -> bool {
    Arg::is_valid_addr_form(crate::air::opcode::Opcode::Move, offset as _)
}
