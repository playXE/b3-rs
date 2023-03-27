use super::{code::Code, opcode::Opcode, stack_slot::StackSlotKind};
use crate::jit::{
    register_at_offset::{OffsetBaseType, RegisterAtOffsetList},
    register_set::RegisterSetBuilder,
};

pub fn handle_callee_saves_with_used(
    code: &mut Code<'_>,
    mut used_callee_saves: RegisterSetBuilder,
) {
    used_callee_saves.filter_regs(&RegisterSetBuilder::callee_saved_registers());
    used_callee_saves.filter_regs(&code.mutable_regs.to_register_set());

    let callee_saves_to_save = used_callee_saves.to_register_set();

    if callee_saves_to_save.number_of_set_registers() == 0 {
        return;
    }

    let callee_save_registers =
        RegisterAtOffsetList::new(callee_saves_to_save, OffsetBaseType::FramePointerBased);

    let mut byte_size = 0;

    for entry in callee_save_registers.iter() {
        byte_size = ((-entry.offset()) as usize).max(byte_size);
    }

    let slot = code.add_stack_slot(byte_size, StackSlotKind::Locked);

    code.set_callee_save_registers_at_offset_list(callee_save_registers, slot)
}

pub fn handle_callee_saves(code: &mut Code<'_>) {
    let mut used_callee_saves = RegisterSetBuilder::new();

    for block in code.blocks.iter() {
        for inst in block.insts.iter() {
            inst.for_each_tmp_fast(|tmp| {
                used_callee_saves.add(tmp.reg(), tmp.reg().conservative_width_without_vectors());
            });

            if inst.kind.opcode == Opcode::Patch {
                todo!()
            }
        }
    }

    handle_callee_saves_with_used(code, used_callee_saves);
}
