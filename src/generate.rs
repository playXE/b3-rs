use macroassembler::assembler::TargetMacroAssembler;

use crate::{procedure::Procedure, air::{code::Code, self}, lower_to_air::lower_to_air, OptLevel, reduce_strength::reduce_strength, eliminate_dead_code::eliminate_dead_code, fix_ssa::fix_ssa, legalize_memory_offsets::legalize_memory_offsets, move_constants::move_constants, estimate_static_exec_counts::estimate_static_execution_counts};


pub fn prepare_for_generation<'a>(proc: &'a mut Procedure) -> Code<'a> {
    let mut air = generate_to_air(proc);
    air::generate::prepare_for_generation(&mut air);
    air
}  

pub fn generate_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    proc.reset_reachability();
    proc.dominators_or_compute();
    
    if proc.options.opt_level >= OptLevel::O2 {
        fix_ssa(proc);
        reduce_strength(proc);
   //     eliminate_dead_code(proc);
    } else if proc.options.opt_level >= OptLevel::O1 {
        //reduce_strength(proc);
    }

    legalize_memory_offsets(proc);
    move_constants(proc);
    legalize_memory_offsets(proc);
    eliminate_dead_code(proc);
    estimate_static_execution_counts(proc);
    
    let code = lower_to_air(proc);
    code
}

pub fn generate<'a>(air: &'a mut Code<'a>, jit: &mut TargetMacroAssembler) {
    air::generate::generate(air, jit);
}