use macroassembler::assembler::TargetMacroAssembler;

use crate::{procedure::Procedure, air::{code::Code, self}, lower_to_air::lower_to_air};


pub fn prepare_for_generation<'a>(proc: &'a mut Procedure) -> Code<'a> {
    let mut air = generate_to_air(proc);
    air::generate::prepare_for_generation(&mut air);
    air
}  

pub fn generate_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    proc.reset_reachability();
    proc.dominators_or_compute();
    super::fix_ssa::fix_ssa(proc);
    super::legalize_memory_offsets::legalize_memory_offsets(proc);

    super::move_constants::move_constants(proc);
    super::estimate_static_exec_counts::estimate_static_execution_counts(proc);

    let code = lower_to_air(proc);

    code
}

pub fn generate<'a>(air: &'a mut Code<'a>, jit: &mut TargetMacroAssembler) {
    air::generate::generate(air, jit);
}