use std::rc::Rc;

use b3::{ValueRep, Reg, BasicBlockBuilder};
use macroassembler::{jit::{gpr_info::*, helpers::AssemblyHelpers}, assembler::{abstract_macro_assembler::Address, TargetMacroAssembler}};

pub fn tail_call(bb: &mut b3::BasicBlockBuilder, callee: b3::ValueId, args: &[b3::ValueId]) {
    let patchpoint = bb.patchpoint(b3::Type::Void);

    bb.procedure.patchpoint_effects_mut(patchpoint).terminal = true;
    bb.procedure.patchpoint_effects_mut(patchpoint).exit_sideways = true;

    for (i, &arg) in args.iter().enumerate() {
        
        let rep = match i {
            0 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0)),
            1 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR1)),
            2 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR2)),
            3 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR3)),
            #[cfg(not(windows))]
            4 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR4)),
            #[cfg(not(windows))]
            5 => ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR5)),

            _ => todo!()
        };

        bb.procedure.stackmap_append(patchpoint, arg, rep);
    }

    bb.procedure.stackmap_append(patchpoint, callee, ValueRep::new(b3::ValueRepKind::WarmAny));

    bb.procedure.stackmap_set_generator(patchpoint, Rc::new(|jit, params| {
        let callee = params.last().unwrap();

     
        jit.emit_function_epilogue();
        if callee.is_reg() {
            jit.far_jump(callee.get_reg().gpr());
        } else if callee.is_stack() {
            jit.far_jump(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, callee.offset_from_fp() as _));
        } else if callee.is_stack_argument() {
            jit.far_jump(Address::new(TargetMacroAssembler::STACK_POINTER_REGISTER, callee.offset_from_sp() as _));
        } else if callee.is_constant() {
            jit.mov(callee.value(), TargetMacroAssembler::SCRATCH_REGISTER);
            jit.far_jump(TargetMacroAssembler::SCRATCH_REGISTER);
        } else {
            unreachable!();
        }

    }));
}


fn main() {
    let mut proc = b3::Procedure::new(b3::Options::default());

    let entry = proc.add_block(1.0);
    let mut bb = BasicBlockBuilder::new(&mut proc, entry);

    let callee = bb.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
    let x1 = bb.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int32);
    let y2 = bb.const32(42);

    tail_call(&mut bb, callee, &[x1, y2]);

    let code = b3::compile(proc);

    println!("{}", code.disassembly());
}