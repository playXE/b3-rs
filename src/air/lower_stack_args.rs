use macroassembler::{assembler::TargetMacroAssembler, jit::gpr_info::CALL_FRAME_REGISTER};

use crate::{
    air::insertion_set::InsertionSet,
    air::{form_table::is_valid_form, inst::Inst},
    bank::conservative_width_without_vectors,
    jit::reg::Reg,
    utils::phase_scope::phase_scope,
    width::{bytes_for_width, Width},
};

use super::{
    arg::{Arg, ArgKind},
    basic_block::BasicBlockId,
    code::Code,
    opcode::Opcode,
    stack_slot::StackSlotKind,
    tmp::Tmp,
};

/// This turns stack and callArg references into actual FP-relative or SP-relative addresses.
/// Also fixes ZDefs to anonymous stack slots.
pub fn lower_stack_args(code: &mut Code) {
    phase_scope("air::lower_stack_args", || {
        // Now we need to deduce how much argument area we need.
        let mut call_arg_area_size = code.call_arg_area_size;
        for block_id in (0..code.blocks.len()).map(BasicBlockId) {
            for inst_index in 0..code.block(block_id).insts.len() {
                let inst = &code.block(block_id).insts[inst_index];

                for arg in inst.args.iter() {
                    if arg.is_call_arg() {
                        // For now, we assume that we use 8 bytes of the call arg. But that's not
                        // such an awesome assumption.
                        // FIXME: https://bugs.webkit.org/show_bug.cgi?id=150454
                        call_arg_area_size = (arg.offset() as usize
                            + bytes_for_width(conservative_width_without_vectors(arg.bank())))
                        .max(call_arg_area_size);
                    }
                }
            }
        }

        code.call_arg_area_size = call_arg_area_size;
        code.frame_size += call_arg_area_size;

        // Finally, transform the code to use Addr's instead of StackSlot's. This is a lossless
        // transformation since we can search the StackSlots array to figure out which StackSlot any
        // offset-from-FP refers to.

        let mut insertion_set = InsertionSet::new();

        for block_id in (0..code.blocks.len()).map(BasicBlockId) {
            // FIXME We can keep track of the last large offset which was materialized in this block, and reuse the register
            // if it hasn't been clobbered instead of renetating imm+add+addr every time. https://bugs.webkit.org/show_bug.cgi?id=171387
            for inst_index in 0..code.block(block_id).insts.len() {
                // we will walk arguments, and then replace arguments if needed. Then put instruction back.
                let mut inst = std::mem::take(&mut code.block_mut(block_id).insts[inst_index]);
                let code2 = unsafe { &*(code as *const Code) };
                let origin = inst.origin;

                let inst_clone = inst.clone();
                inst.for_each_arg_mut(code, |index, arg, role, _, width| {
                    let stack_addr = |_inst_index, offset_from_fp: isize| -> Arg {
                        let offset_from_sp = offset_from_fp + code2.frame_size as isize;

                        if inst_clone.admits_extended_offset_addr(index, code2) {
                            // Stackmaps and patchpoints expect addr inputs relative to SP or FP only. We might as well
                            // not even bother generating an addr with valid form for these opcodes since extended offset
                            // addr is always valid.
                            return Arg::extended_offset_addr(offset_from_fp);
                        }

                        let result = Arg::new_addr(
                            Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER)),
                            offset_from_fp as _,
                        );

                        if result.is_valid_form(Opcode::Move, Some(width)) {
                            return result;
                        }

                        let result = Arg::new_addr(
                            Tmp::from_reg(Reg::new_gpr(
                                TargetMacroAssembler::STACK_POINTER_REGISTER,
                            )),
                            offset_from_sp as _,
                        );

                        if result.is_valid_form(Opcode::Move, Some(width)) {
                            return result;
                        }

                        unreachable!("immediates are too large for frame size");
                    };

                    match arg.kind() {
                        ArgKind::Stack => {
                            let slot = code2.stack_slot(arg.stack_slot());

                            if role.is_zdef()
                                && slot.kind() == StackSlotKind::Spill
                                && slot.byte_size() > bytes_for_width(width) as u32
                            {
                                // Currently we only handle this simple case because it's the only one
                                // that arises: ZDef's are only 32-bit right now. So, when we hit these
                                // assertions it means that we need to implement those other kinds of
                                // zero fills.
                                debug_assert!(slot.byte_size() == 8);
                                debug_assert!(width == Width::W32);

                                let store_opcode = Opcode::Move32;
                                let kind = ArgKind::Imm;
                                let operand = Arg::new_imm(0);

                                debug_assert!(is_valid_form(store_opcode, &[kind, ArgKind::Stack]));
                                let new_inst = Inst::new(
                                    store_opcode.into(),
                                    origin,
                                    &[
                                        operand,
                                        stack_addr(
                                            inst_index + 1,
                                            arg.offset() as isize + 4 + slot.offset_from_fp,
                                        ),
                                    ],
                                );

                                insertion_set.insert_inst(inst_index + 1, new_inst);
                            }

                            *arg = stack_addr(
                                inst_index,
                                arg.offset() as isize + slot.offset_from_fp as isize,
                            );
                        }
                        ArgKind::CallArg => {
                            *arg = stack_addr(
                                inst_index,
                                arg.offset() as isize - code2.frame_size as isize,
                            );
                        }

                        _ => (),
                    }
                });

                code.block_mut(block_id).insts[inst_index] = inst;
            }

            insertion_set.execute(code, block_id);
        }
    });
}
