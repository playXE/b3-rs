use std::collections::HashMap;


use crate::{
    block::{blocks_in_pre_order, BlockId},
    break_critical_edges::break_critical_edges,
    insertion_set::InsertionSet,
    liveness::{IndexSparseSet, LiveAtHeadCloned, LocalCalc},
    opcode::Opcode,
    procedure::Procedure,
    ssa_calculator::{SSACalculator},
    typ::Type,
    utils::index_set::{IndexMap, IndexSet},
    value::{NumChildren, Value, ValueData, ValueId},
    variable::VariableId,
    variable_liveness::{VariableLiveness, VariableLivenessAdapter},
};

/// Turns all mentions of the given values into accesses to variables. This is meant to be used
/// from phases that don't like SSA for whatever reason.
pub fn demote_values(proc: &mut Procedure, values: &IndexSet<ValueId>) {
    let mut map = HashMap::new();
    let mut phi_map = HashMap::new();

    for value in values.indices().map(ValueId) {
        let typ = proc.value(value).typ();

        let variable = proc.add_variable(typ);

        map.insert(value, variable);

        if proc.value(value).kind.opcode() == Opcode::Phi {
            phi_map.insert(value, variable);
        }
    }

    let mut insertion_set = InsertionSet::new();

    for block in (0..proc.blocks.len()).map(BlockId) {
        if proc.block(block).predecessor_list.len() != 0 {
            let value = proc
                .block(proc.block(block).predecessor_list[0])
                .last()
                .unwrap();

            if let Some(variable) = phi_map.get(&value) {
                let val = Value::new(
                    Opcode::Set,
                    Type::Void,
                    NumChildren::One,
                    &[*value],
                    ValueData::Variable(*variable),
                );
                let vid = proc.add(val);
                insertion_set.insert_value(0, vid);
            }
        }

        for value_index in 0..proc.block(block).len() {
            let value = proc.block(block).values[value_index];

            if proc.value(value).kind.opcode() == Opcode::Phi {
                if let Some(variable) = phi_map.get(&value) {
                    let var = Value::new(
                        Opcode::Get,
                        proc.value(value).typ(),
                        NumChildren::Zero,
                        &[],
                        ValueData::Variable(*variable),
                    );

                    let vid = proc.add(var);
                    let child = insertion_set.insert_value(value_index, vid);

                    proc.value_mut(value).replace_with_identity(child);
                }
            } else {
                for child in 0..proc.value(value).children.len() {
                    let ix = child;
                    if let Some(variable) = map.get(&proc.value(value).children[child]) {
                        let var = Value::new(
                            Opcode::Get,
                            proc.value(value).typ(),
                            NumChildren::Zero,
                            &[],
                            ValueData::Variable(*variable),
                        );

                        let vid = proc.add(var);
                        let child = insertion_set.insert_value(value_index, vid);

                        proc.value_mut(value).children[ix] = child;
                    }
                }

                if proc.value(value).kind.opcode() == Opcode::Upsilon {
                    if let Some(variable) = phi_map.get(&proc.value(value).phi().unwrap()) {
                        let var = Value::new(
                            Opcode::Set,
                            Type::Void,
                            NumChildren::One,
                            &[proc.value(value).children[0]],
                            ValueData::Variable(*variable),
                        );

                        let vid = proc.add(var);
                        insertion_set.insert_value(value_index + 1, vid);

                        proc.value_mut(value).replace_with_nop();
                    }
                }
            }

            if let Some(variable) = map.get(&value) {
                if value_index + 1 < proc.block(block).len() {
                    let val = Value::new(
                        Opcode::Set,
                        Type::Void,
                        NumChildren::One,
                        &[value],
                        ValueData::Variable(*variable),
                    );
                    let vid = proc.add(val);
                    insertion_set.insert_value(value_index + 1, vid);
                }
            }
        }

        insertion_set.execute(proc, block);
    }
}

pub fn demoted_values(proc: &Procedure) -> IndexSet<ValueId> {
    let mut values = IndexSet::new();

    for block in (0..proc.blocks.len()).map(BlockId) {
        for value in proc.block(block).values.iter() {
            if proc.value(*value).kind.opcode() == Opcode::Phi {
                values.insert(*value);
            }

            for child in proc.value(*value).children.iter() {
                if proc.value(*child).owner != Some(block) {
                    values.insert(*child);
                }
            }
        }
    }
    values
}

/// This fixes SSA for you. Use this after you have done [demote_values()](demote_values) and you have performed
/// whatever evil transformation you needed.
pub fn fix_ssa(proc: &mut Procedure) -> bool {
    if proc.variables.is_empty() {
        return false;
    }
    fix_ssa_locally(proc);
    kill_dead_variables(proc);

    if proc.variables.is_empty() {
        return false;
    }

    break_critical_edges(proc);
    fix_ssa_globally(proc);

    true
}

/// Simple pass that removes all dead variables.
fn kill_dead_variables(proc: &mut Procedure) {
    let mut live_variables = IndexSet::new();

    for value in proc.values.iter() {
        if value.kind.opcode() == Opcode::Get {
            live_variables.insert(value.as_variable().unwrap());
        }
    }

    for value in proc.values.iter_mut() {
        if value.kind.opcode() == Opcode::Set
            && !live_variables.contains(&value.as_variable().unwrap())
        {
            value.replace_with_nop();
        }
    }

    for variable in (0..proc.variables.size()).map(VariableId) {
        if !live_variables.contains(&variable) {
            proc.variables.remove(variable);
        }
    }
}

fn fix_ssa_locally(proc: &mut Procedure) {
    let mut mapping = IndexSparseSet::<(usize, ValueId)>::new(proc.variables.size());

    let blocks = blocks_in_pre_order(BlockId(0), proc);

    for block in blocks {
        mapping.clear();

        for value_index in 0..proc.block(block).values.len() {
            let value = proc.block(block).values[value_index];

            Value::perform_substitution(value, proc);

            match proc.value(value).kind.opcode() {
                Opcode::Get => {
                    let variable = proc.value(value).as_variable().unwrap();

                    if let Some(replacement) = mapping.get(variable.0) {
                        proc.value_mut(value).replace_with_identity(replacement.1);
                    }
                }

                Opcode::Set => {
                    let variable = proc.value(value).as_variable().unwrap();

                    mapping.set(variable.0, proc.value(value).children[0]);
                }

                _ => (),
            }
        }
    }
}

fn fix_ssa_globally(proc: &mut Procedure) {
    let mut adapter = VariableLivenessAdapter { cfg: proc };
    let mut liveness = VariableLiveness::new(&mut adapter);
    liveness.compute();

    for block in 0..liveness.adapter.cfg.blocks.len() {
        let block = BlockId(block);
        let mut local_calc = LocalCalc::new(&mut liveness, block);

        for value_index in (0..local_calc.liveness.adapter.cfg.block(block).len()).rev() {
            let value_id = local_calc.liveness.adapter.cfg.block(block).values[value_index];

            let value = local_calc.liveness.adapter.cfg.value(value_id);

            if value.kind.opcode() == Opcode::Set
                && !local_calc.is_live(value.as_variable().unwrap())
            {
                let value = local_calc.liveness.adapter.cfg.value_mut(value_id);
                value.replace_with_nop();
            }

            local_calc.execute(value_index);
        }
    }

    let mut ssa = SSACalculator::new(liveness.adapter.cfg);

    let mut calc_var_to_variable = vec![];
    let mut variable_to_calc_var = IndexMap::with_capacity(liveness.adapter.cfg.variables.size());
    //vec![SSAVariableId(usize::MAX); liveness.adapter.cfg.variables.size()];

    for var in (0..liveness.adapter.cfg.variables.size()).map(VariableId) {
        let calc_var = ssa.new_variable();
        calc_var_to_variable.push(var);
        variable_to_calc_var.insert(var, calc_var);
    }

    for block in (0..liveness.adapter.cfg.blocks.len()).map(BlockId) {
        for val in liveness.adapter.cfg.block(block).values.iter() {
            let value = liveness.adapter.cfg.value(*val);

            if value.kind.opcode() != Opcode::Set {
                continue;
            }

            let variable = value.as_variable().unwrap();
            if let Some(calc_var) = variable_to_calc_var.get(&variable) {
                ssa.new_def(*calc_var, block, value.children[0]);
            }
        }
    }

    // Decide where Phis are to be inserted. This creates them but does not insert them.
    {
        let live_at_head = LiveAtHeadCloned::new(&mut liveness);
        ssa.compute_phis(liveness.adapter.cfg, |calc_var, block, proc| {
            let variable = calc_var_to_variable[calc_var.0];

            if !live_at_head.is_live_at_head(block, proc, variable) {
                return None;
            }
            let typ = proc.variable(variable).typ();
            let phi = proc.add(Value::new(
                Opcode::Phi,
                typ,
                crate::value::NumChildren::Zero,
                &[],
                crate::value::ValueData::None,
            ));

            Some(phi)
        });
    }

    ssa.dominators = Some(liveness.adapter.cfg.dominators().clone());
    let mut insertion_set = InsertionSet::new();

    let mut mapping = IndexSparseSet::new(proc.variables.size());
    let mut values_to_delete = IndexSet::new();

    for block in blocks_in_pre_order(BlockId(0), proc) {
        mapping.clear();

        let ensure_mapping = |insertion_set: &mut InsertionSet,
                              variable: VariableId,
                              value_index: usize,
                              mapping: &mut IndexSparseSet<(usize, ValueId)>,
                              proc: &mut Procedure,
                              ssa: &mut SSACalculator|
         -> ValueId {
            if let Some(replacement) = mapping.get(variable.0) {
                replacement.1
            } else {
                let calc_var = variable_to_calc_var[variable];
                let def = ssa.reaching_def_at_head(block, calc_var, proc);
                if let Some(def) = def {
                    mapping.set(variable.0, def.value.get());
                    return def.value.get();
                }
                let typ = proc.variable(variable).typ();
                insertion_set.insert_bottom(value_index, typ, proc)
            }
        };

        for phi_def in ssa.phis_for_block(block) {
            let variable = calc_var_to_variable[phi_def.variable.0];
            insertion_set.insert_value(0, phi_def.value.get());
            mapping.set(variable.0, phi_def.value.get());
        }

        for value_index in 0..proc.block(block).len() {
            let val = proc.block(block).values[value_index];
            Value::perform_substitution(val, proc);

            match proc.value(val).kind.opcode() {
                Opcode::Get => {
                    let variable = proc.value(val).as_variable().unwrap();

                    let replacement = ensure_mapping(
                        &mut insertion_set,
                        variable,
                        value_index,
                        &mut mapping,
                        proc,
                        &mut ssa,
                    );

                    proc.value_mut(val).replace_with_identity(replacement);
                    values_to_delete.insert(val);
                }

                Opcode::Set => {
                    let variable = proc.value(val).as_variable().unwrap();
                    mapping.set(variable.0, proc.value(val).children[0]);

                    proc.value_mut(val).replace_with_nop();
                }

                _ => (),
            }
        }

        let upsilon_insertion_point = proc.block(block).len() - 1;

        for successor_block in proc
            .block(block)
            .successor_list()
            .iter()
            .map(|x| x.0)
            .collect::<Vec<_>>()
        {
            let phi_def = ssa.phis_for_block(successor_block).to_vec();

            for phi_def in phi_def {
                let phi = phi_def.value.get();

                let calc_var = phi_def.variable;
                let variable = calc_var_to_variable[calc_var.0];

                let mapped_value = ensure_mapping(
                    &mut insertion_set,
                    variable,
                    upsilon_insertion_point,
                    &mut mapping,
                    proc,
                    &mut ssa,
                );

                let mapped_value = Value::fold_identity(mapped_value, proc);

                let upsilon = proc.add(Value::new(
                    Opcode::Upsilon,
                    Type::Void,
                    crate::value::NumChildren::One,
                    &[mapped_value],
                    crate::value::ValueData::Upsilon(Some(phi)),
                ));

                insertion_set.insert_value(upsilon_insertion_point, upsilon);
            }
        }

        insertion_set.execute(proc, block);
    }

    for block in (0..proc.blocks.len()).map(BlockId) {
        proc.block_mut(block)
            .values
            .retain(|x| !values_to_delete.contains(x))
    }
}
