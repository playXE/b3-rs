use crate::jit::register_set::RegisterSetBuilder;

use super::{
    basic_block::BasicBlockId,
    code::Code,
    inst::Inst,
    opcode::Opcode,
    reg_liveness::{LocalCalc, RegLiveness},
};

/// Performs a liveness analysis over registers and reports the live registers to every Special. Takes
/// the opportunity to kill dead assignments to registers, since it has access to register liveness.
pub fn report_used_registers(code: &mut Code) {
    let liveness = RegLiveness::new(code);

    for block in (0..code.blocks.len()).map(BasicBlockId) {
        let mut local_calc = LocalCalc::new(&liveness, block);

        for inst_index in (0..code.block(block).insts.len()).rev() {
            let inst = &code.block(block).insts[inst_index];
            println!("{:?}: {} live {}", block, inst, local_calc.live());
            // Kill dead assignments to registers. For simplicity we say that a store is killable if
            // it has only late defs and those late defs are to registers that are dead right now.
            if !inst.has_non_arg_effects(code) {
                let mut can_delete = true;

                inst.for_each_arg(code, |_, arg, role, _, _| {
                    if role.is_early_def() {
                        can_delete = false;
                        return;
                    }

                    if !role.is_late_def() {
                        return;
                    }

                    if !arg.is_reg() {
                        can_delete = false;
                        return;
                    }

                    if local_calc.is_live(arg.reg()) {
                        can_delete = false;
                        return;
                    }
                });

                if can_delete {
                    code.block_mut(block).insts[inst_index] = Inst::default();
                }
            }

            let inst = &code.block(block).insts[inst_index];

            if inst.kind.opcode == Opcode::Patch {
                let special = inst.args[0].special();
                let code2 = unsafe { &mut *(code as *mut Code) };
                let code3 = unsafe { &mut *(code2 as *mut Code) };
                code.special_mut(special).report_used_registers(
                    code2,
                    &code3.block(block)[inst_index],
                    &RegisterSetBuilder::from_regs(local_calc.live()),
                );
            }

            local_calc.execute(inst_index);
        }

        code.block_mut(block)
            .insts
            .retain(|inst| inst != &Inst::default());
    }
}
