use std::collections::{HashMap, HashSet};

use crate::{BlockId, NumChildren, Opcode, Procedure, Type, Value, ValueId};

/// Finds all alloca instructions that do not have the identity
/// property, that is, all alloca instructions that are used
/// at least once by something that is neither a load or a store.
///
/// # Return
///
/// A set of all alloca instructions without the identity property.
pub fn get_allocas_without_identity(proc: &mut Procedure) -> (HashSet<ValueId>, HashSet<ValueId>) {
    let mut allocas = HashSet::new();
    let mut blacklisted_allocas = HashSet::new();

    for block in (0..proc.blocks.len()).map(BlockId) {
        for value_index in 0..proc.block(block).len() {
            let value = proc.block(block)[value_index];

            visit_for_identity_property(proc, value, &mut allocas, &mut blacklisted_allocas);
        }
    }

    for &blacklisted_alloca in &blacklisted_allocas {
        allocas.remove(&blacklisted_alloca);
    }

    (allocas, blacklisted_allocas)
}

fn visit_for_identity_property(
    proc: &mut Procedure,
    value: ValueId,
    allocas: &mut HashSet<ValueId>,
    blacklisted_allocas: &mut HashSet<ValueId>,
) {
    Value::perform_substitution(value, proc);
    match value.opcode(proc) {
        Opcode::Load => {
            let memory_value = proc.value(value).memory_value().unwrap();

            if memory_value.0 != 0 && value.child(proc, 0).opcode(proc) == Opcode::Alloca {
                println!("ayo");
                blacklisted_allocas.insert(value.child(proc, 0));
            }
        }
        Opcode::Store => {
            if value.child(proc, 0).opcode(proc) == Opcode::Alloca {
                blacklisted_allocas.insert(value.child(proc, 0));
            }
        }

        op => {
            if op == Opcode::Alloca {
                allocas.insert(value);
            }

            let args = &proc.value(value).children;

            // union-with the blacklisted allocas
            for arg in args {
                if arg.opcode(proc) == Opcode::Alloca {
                    blacklisted_allocas.insert(*arg);
                }
            }
        }
    }
}

pub fn alloca_to_reg(proc: &mut Procedure) {
    let (allocas, blacklisted) = get_allocas_without_identity(proc);

    let mut alloca_to_var = HashMap::new();

    for &alloca in allocas.iter() {
        let typ = proc.value(alloca).alloca().expect("alloca");

        let var = proc.add_variable(typ);

        alloca_to_var.insert(alloca, var);
    }

    let mut alloca_to_slot = HashMap::new();
    for &blacklisted in blacklisted.iter() {
        println!("{:?}", blacklisted);
        let size = proc.value(blacklisted).alloca().expect("alloca").size() as usize;
        let slot = proc.add_stack_slot(size, crate::air::stack_slot::StackSlotKind::Locked);

        alloca_to_slot.insert(blacklisted, slot);
    }

    for block in (0..proc.blocks.len()).map(BlockId) {
        for value_index in 0..proc.block(block).len() {
            let value = proc.block(block)[value_index];

            if value.opcode(proc) == Opcode::Alloca {
                if allocas.contains(&value) {
                    proc.value_mut(value).replace_with_nop_ignoring_type();
                } else {
                    assert!(blacklisted.contains(&value));

                    let slot = *alloca_to_slot.get(&value).unwrap();
                    let index = proc.value(value).index;
                    *proc.value_mut(value) = Value::new(
                        Opcode::SlotBase,
                        Type::Int64,
                        NumChildren::Zero,
                        &[],
                        crate::ValueData::SlotBase(slot),
                    );
                    proc.value_mut(value).index = index;
                }
            } else if value.opcode(proc) == Opcode::Load {
                let ptr = value.child(proc, 0);

                if let Some(var) = alloca_to_var.get(&ptr) {
                    let var_get = proc.add_variable_get(*var);

                    proc.block_mut(block)[value_index] = var_get;
                    proc.value_mut(var_get).owner = Some(block);
                }
            } else if value.opcode(proc) == Opcode::Store {
                let ptr = value.child(proc, 1);
                let value = value.child(proc, 0);

                if let Some(var) = alloca_to_var.get(&ptr) {
                    let var_set = proc.add_variable_set(*var, value);

                    proc.block_mut(block)[value_index] = var_set;
                    proc.value_mut(var_set).owner = Some(block);
                }
            }
        }
    }

    if allocas.len() != 0 || blacklisted.len() != 0 {
        proc.reset_value_owners();
    }

    println!("{}", proc.display());
}
