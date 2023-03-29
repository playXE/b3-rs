use std::collections::HashMap;

use crate::{
    bank::Bank,
    jit::register_set::{RegisterSetBuilder, ScalarRegisterSet},
    utils::phase_scope::phase_scope,
    width::{bytes_for_width, width_for_type},
};

use super::{
    arg::Arg,
    basic_block::BasicBlockId,
    ccalling_convention::{
        build_ccall, ccall_result, ccall_result_count, compute_ccalling_convention,
    },
    code::Code,
    emit_shuffle::{emit_shuffle, ShufflePair},
    insertion_set::InsertionSet,
    inst::Inst,
    opcode::Opcode,
    pad_interference::pad_interference,
    reg_liveness::{LocalCalc, RegLiveness},
    stack_slot::{StackSlotId, StackSlotKind},
    tmp::Tmp,
};

pub fn lower_after_regalloc(code: &mut Code) {
    phase_scope("air::lower_after_regalloc", || {
        let is_relevant = |inst: &Inst| -> bool {
            inst.kind.opcode == Opcode::Shuffle || inst.kind.opcode == Opcode::ColdCCall
        };

        let mut have_any_relevant = false;

        for block in code.blocks.iter() {
            for inst in block.insts.iter() {
                if is_relevant(inst) {
                    have_any_relevant = true;
                    break;
                }
            }

            if have_any_relevant {
                break;
            }
        }

        if !have_any_relevant {
            return;
        }

        pad_interference(code);

        let mut used_registers = HashMap::<*const Inst, RegisterSetBuilder>::new();

        let liveness = RegLiveness::new(code);

        for (id, block) in code.blocks.iter().enumerate() {
            let mut local_calc = LocalCalc::new(&liveness, BasicBlockId(id));

            for inst_index in (0..block.insts.len()).rev() {
                let inst = &block.insts[inst_index];

                let mut set = RegisterSetBuilder::new();

                if is_relevant(inst) {
                    set = RegisterSetBuilder::from_regs(local_calc.live());
                }

                local_calc.execute(inst_index);

                if is_relevant(inst) {
                    used_registers.insert(inst, set);
                }
            }
        }

        let mut slots = [[StackSlotId(usize::MAX); 2]; 2];

        // If we run after stack allocation then we cannot use those callee saves that aren't in
        // the callee save list. Note that we are only run after stack allocation in -O1, so this
        // kind of slop is OK.
        let mut disallowed_callee_saves = ScalarRegisterSet::default();
        if code.stack_is_allocated {
            let mut disallowed =
                RegisterSetBuilder::from_regs(&RegisterSetBuilder::callee_saved_registers());
            let used_callee_saves = code.callee_save_registers;

            disallowed.exclude(&used_callee_saves);

            disallowed_callee_saves = disallowed.build_and_validate().build_scalar_register_set();
        }

        let mut get_scratches =
            |code: &mut Code, mut set: ScalarRegisterSet, bank: Bank| -> [Arg; 2] {
                let mut result = [Arg::default(); 2];

                for i in 0..2 {
                    let mut found = false;

                    for &reg in code.regs_in_priority_order(bank) {
                        if !set.contains(reg) && !disallowed_callee_saves.contains(reg) {
                            result[i] = Arg::new_tmp(Tmp::from_reg(reg));
                            set.add(reg);
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        let slot = &mut slots[i][bank as usize];

                        if *slot == StackSlotId(usize::MAX) {
                            *slot = code.add_stack_slot(8, StackSlotKind::Spill);
                        }

                        result[i] = Arg::new_stack(*slot, 0);
                    }
                }

                result
            };

        let mut insertion_set = InsertionSet::new();

        for i in 0..code.blocks.len() {
            for inst_index in 0..code.block(BasicBlockId(i)).insts.len() {
                let inst = &code.block(BasicBlockId(i)).insts[inst_index];

                match inst.kind.opcode {
                    Opcode::Shuffle => {
                        let mut set = used_registers
                            .get(&(inst as *const Inst))
                            .unwrap()
                            .build_and_validate()
                            .build_scalar_register_set();

                        let mut pairs = vec![];

                        let mut i = 0;

                        while i < inst.args.len() {
                            let src = inst.args[i];
                            let dst = inst.args[i + 1];
                            let width = inst.args[i + 2].width();

                            src.for_each_tmp_fast(|tmp| {
                                if tmp.is_reg() {
                                    set.add(tmp.reg());
                                }
                            });
                            dst.for_each_tmp_fast(|tmp| {
                                if tmp.is_reg() {
                                    set.add(tmp.reg());
                                }
                            });

                            pairs.push(ShufflePair::new(src, dst, width));

                            i += 3;
                        }
                        let origin = inst.origin;
                        let gp_scratch = get_scratches(code, set, Bank::GP);
                        let fp_scratch = get_scratches(code, set, Bank::FP);

                        insertion_set.insert_insts(
                            inst_index,
                            emit_shuffle(code, &pairs, gp_scratch, fp_scratch, origin),
                        );

                        code.block_mut(BasicBlockId(i)).insts[inst_index] = Inst::default();
                    }

                    Opcode::ColdCCall => {
                        let old_kind = inst.kind;
                        let value = inst.origin;
                        let live_regs = *used_registers.get(&(inst as *const Inst)).unwrap();
                        let inst = inst.clone();
                        let mut unsaved_regs = live_regs;

                        unsaved_regs.exclude_regs(&RegisterSetBuilder::callee_saved_registers());
                        unsaved_regs.exclude_regs(&RegisterSetBuilder::stack_registers());

                        let mut regs_to_save = unsaved_regs.build_and_validate();

                        let mut pre_used =
                            live_regs.build_and_validate().build_scalar_register_set();
                        let mut post_used = pre_used;
                        let destinations = compute_ccalling_convention(code, value);

                        let mut results = Vec::new();
                        let mut original_results = Vec::new();

                        for i in 0..ccall_result_count(code, value) {
                            results.push(ccall_result(code, value, i));
                            original_results.push(inst.args[i + 2]);
                        }

                        let mut pairs = Vec::new();

                        for i in 0..destinations.len() {
                            let child = code.proc.value(value).children[i];
                            let src = inst.args[if i >= 1 { i + results.len() + 1 } else { i + 1 }];

                            let dst = destinations[i];

                            let width = width_for_type(code.proc.value(child).typ());

                            pairs.push(ShufflePair::new(src, dst, width));

                            src.for_each_tmp_fast(|tmp| {
                                if tmp.is_reg() {
                                    pre_used.add(tmp.reg());
                                }
                            });
                            dst.for_each_tmp_fast(|tmp| {
                                if tmp.is_reg() {
                                    post_used.add(tmp.reg());
                                }
                            });
                        }

                        let mut gp_scratch = get_scratches(code, pre_used, Bank::GP);
                        let mut fp_scratch = get_scratches(code, pre_used, Bank::FP);

                        for original_result in original_results.iter().copied() {
                            if original_result.is_reg() {
                                regs_to_save.remove(original_result.reg());
                            }
                        }

                        let mut stack_slots = vec![];

                        regs_to_save.for_each_with_width(|reg, width| {
                            let tmp = Tmp::from_reg(reg);
                            let arg = Arg::new_tmp(tmp);

                            let stack_slot =
                                code.add_stack_slot(bytes_for_width(width), StackSlotKind::Spill);

                            pairs.push(ShufflePair::new(arg, Arg::new_stack(stack_slot, 0), width));

                            stack_slots.push(stack_slot);
                        });

                        let insts = emit_shuffle(code, &pairs, gp_scratch, fp_scratch, inst.origin);

                        insertion_set.insert_insts(inst_index, insts);

                        let mut new_inst = build_ccall(code, inst.origin, &destinations);
                        if old_kind.effects {
                            new_inst.kind.effects = true;
                        }

                        code.block_mut(BasicBlockId(i)).insts[inst_index] = new_inst;

                        pairs.truncate(0);

                        let mut stack_slot_index = 0;

                        regs_to_save.for_each_with_width(|reg, width| {
                            let tmp = Tmp::from_reg(reg);
                            let arg = Arg::new_tmp(tmp);
                            let stack_slot = stack_slots[stack_slot_index];
                            stack_slot_index += 1;
                            pairs.push(ShufflePair::new(Arg::new_stack(stack_slot, 0), arg, width));
                        });

                        for i in 0..results.len() {
                            let typ = code.proc.value(value).typ();
                            let pair = ShufflePair::new(
                                Arg::new_tmp(results[i]),
                                original_results[i],
                                width_for_type(typ),
                            );
                            pairs.push(pair);

                            if original_results[i].is_reg() {
                                post_used.add(original_results[i].reg());
                            }
                        }

                        gp_scratch = get_scratches(code, post_used, Bank::GP);
                        fp_scratch = get_scratches(code, post_used, Bank::FP);

                        let insts = emit_shuffle(code, &pairs, gp_scratch, fp_scratch, inst.origin);

                        insertion_set.insert_insts(inst_index + 1, insts);
                    }

                    _ => (),
                }
            }

            insertion_set.execute(code, BasicBlockId(i));

            code.block_mut(BasicBlockId(i))
                .insts
                .retain(|inst| inst != &Inst::default());
        }
    });
}
