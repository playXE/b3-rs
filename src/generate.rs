use macroassembler::assembler::TargetMacroAssembler;

use crate::{procedure::Procedure, air::{code::Code, self}, lower_to_air::lower_to_air, OptLevel, reduce_strength::reduce_strength, fix_ssa::fix_ssa, legalize_memory_offsets::legalize_memory_offsets, move_constants::move_constants, estimate_static_exec_counts::estimate_static_execution_counts, hoist_loop_invariant_values::hoist_loop_invariant_values};


pub fn prepare_for_generation<'a>(proc: &'a mut Procedure) -> Code<'a> {
    let mut air = generate_to_air(proc);
    air::generate::prepare_for_generation(&mut air);
    air
}  

pub fn generate_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    proc.reset_reachability();
    proc.dominators_or_compute();
    
    if proc.options.opt_level >= OptLevel::O2 {
        // TODO: Should we run `fix_ssa` after or before `reduce_strength`? 
        // Seems like running it before is better since `reduce_strength` can 
        // work out better because it knows how to work with Phi's

        // Converts code to SSA form.
        fix_ssa(proc);
        // Reduces strength until fixpoint. 
        reduce_strength(proc);
        
    } else if proc.options.opt_level >= OptLevel::O1 {
        // Reduces strength in one pass. 
        reduce_strength(proc);
    }

    legalize_memory_offsets(proc);
    // Move constants to places where program might benefit from them
    // Plus eliminates `ConstFloat` and `ConstDouble` opcodes
    // replacing them with loads from data section.
    move_constants(proc);
    legalize_memory_offsets(proc);
    //eliminate_dead_code(proc);

    if proc.options.estimate_static_execution_counts {
        // Estimate frequency of each basic block based on loop analysis.
        // 
        // Without this pass the code generator won't generate optimal block ordering.
        // But sometimes user provides their own frequency estimates, so we don't want to
        // overwrite them.
        estimate_static_execution_counts(proc);
    }
    
    let code = lower_to_air(proc);
    code
}

pub fn generate<'a>(air: &'a mut Code<'a>, jit: &mut TargetMacroAssembler) {
    air::generate::generate(air, jit);
}