use crate::utils::{index_set::IndexSet, phase_scope::phase_scope};

use super::{
    arg::{Arg, ArgKind, ArgRole},
    code::Code,
    inst::Inst,
    stack_slot::StackSlotId,
    tmp_set::TmpSet,
};

/// This eliminates instructions that have no observable effect. These are instructions whose only
/// effect would be storing to some Arg, except that we proved that the location specified by the Arg
/// is never loaded from. The only Args for which we can do such analysis are non-Reg Tmps and
/// anonymous StackSlots.
pub fn eliminate_dead_code(code: &mut Code<'_>) -> bool {
    phase_scope("air::eliminate_dead_code", || {
        let mut live_tmps = TmpSet::new();
        let mut live_stack_slots = IndexSet::<StackSlotId>::new();

        let mut changed = false;

        let is_arg_live =
            |arg: &Arg, live_tmps: &TmpSet, live_stack_slots: &IndexSet<StackSlotId>| -> bool {
                match arg.kind() {
                    ArgKind::Tmp => {
                        if arg.is_reg() {
                            true
                        } else {
                            live_tmps.contains(arg.tmp())
                        }
                    }

                    ArgKind::Stack => {
                        if code.stack_slot(arg.stack_slot()).is_locked() {
                            true
                        } else {
                            live_stack_slots.contains(&arg.stack_slot())
                        }
                    }

                    _ => true,
                }
            };

        let is_inst_live =
            |inst: &Inst, live_tmps: &TmpSet, live_stack_slots: &IndexSet<StackSlotId>| -> bool {
                if inst.has_non_arg_effects(code) {
                    return true;
                }

                let mut stores_to_live = false;

                inst.for_each_arg(code, |arg, role, _, _width| {
                    if !role.is_any_def() {
                        return;
                    }

                    if role == ArgRole::Scratch {
                        return;
                    }

                    stores_to_live |= is_arg_live(&arg, live_tmps, live_stack_slots);
                });

                stores_to_live
            };

        let handle_inst = |code: &Code,
                           live_tmps: &mut TmpSet,
                           live_stack_slots: &mut IndexSet<StackSlotId>,
                           inst: &Inst,
                           changed: &mut bool|
         -> bool {
            if !is_inst_live(inst, &live_tmps, &live_stack_slots) {
                return false;
            }

            for arg in inst.args.iter() {
                if arg.is_stack() && !code.stack_slot(arg.stack_slot()).is_locked() {
                    *changed |= live_stack_slots.insert(arg.stack_slot());
                }

                arg.for_each_tmp_fast(|tmp| {
                    if !tmp.is_reg() {
                        *changed |= live_tmps.add(tmp);
                    }
                });
            }

            true
        };

        let mut possibly_dead = Vec::new();

        for block in code.blocks.iter() {
            for inst in block.insts.iter() {
                if !handle_inst(
                    code,
                    &mut live_tmps,
                    &mut live_stack_slots,
                    inst,
                    &mut changed,
                ) {
                    possibly_dead.push(inst);
                }
            }
        }

        let run_forward = |code: &Code,
                           live_tmps: &mut TmpSet,
                           live_stack_slots: &mut IndexSet<StackSlotId>,
                           changed: &mut bool,
                           possibly_dead: &mut Vec<&Inst>|
         -> bool {
            *changed = false;
            possibly_dead.retain(|inst| {
                let result = handle_inst(code, live_tmps, live_stack_slots, inst, changed);
                *changed |= result;
                !result
            });
            *changed
        };

        let run_backward = |code: &Code,
                            live_tmps: &mut TmpSet,
                            live_stack_slots: &mut IndexSet<StackSlotId>,
                            changed: &mut bool,
                            possibly_dead: &mut Vec<&Inst>|
         -> bool {
            *changed = false;
            for i in (0..possibly_dead.len()).rev() {
                let result = handle_inst(
                    code,
                    live_tmps,
                    live_stack_slots,
                    &possibly_dead[i],
                    changed,
                );

                if result {
                    possibly_dead[i] = possibly_dead[possibly_dead.len() - 1];
                    possibly_dead.pop();
                    *changed = true;
                }
            }
            *changed
        };

        loop {
            // Propagating backward is most likely to be profitable.
            if !run_backward(
                code,
                &mut live_tmps,
                &mut live_stack_slots,
                &mut changed,
                &mut possibly_dead,
            ) {
                break;
            }

            if !run_forward(
                code,
                &mut live_tmps,
                &mut live_stack_slots,
                &mut changed,
                &mut possibly_dead,
            ) {
                break;
            }

            // Occasionally propagating forward greatly reduces the likelihood of pathologies.
            if !run_forward(
                code,
                &mut live_tmps,
                &mut live_stack_slots,
                &mut changed,
                &mut possibly_dead,
            ) {
                break;
            }
        }

        let mut removed_insts = 0;

        // FIXME: Make this safe. To do so we need to assign index to each instruction, create worklist of dead instructions and remove them in the end.
        let code2 = unsafe { &mut *(code as *const Code as *mut Code) };

        for block in code2.blocks.iter_mut() {
            block.insts.retain(|inst| {
                if is_inst_live(inst, &live_tmps, &live_stack_slots) {
                    true
                } else {
                    removed_insts += 1;
                    false
                }
            });
        }

        removed_insts != 0
    })
}
