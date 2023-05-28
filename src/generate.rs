use macroassembler::assembler::TargetMacroAssembler;

use crate::{
    air::{self, code::Code},
    estimate_static_exec_counts::estimate_static_execution_counts,
    fix_ssa::fix_ssa,
    infer_switches::infer_switches,
    legalize_memory_offsets::legalize_memory_offsets,
    lower_macros::lower_macros,
    lower_to_air::lower_to_air,
    move_constants::move_constants,
    procedure::Procedure,
    reduce_strength::reduce_strength,
    OptLevel, 
};

pub fn prepare_for_generation<'a>(proc: &'a mut Procedure) -> Code<'a> {
    let mut air = generate_to_air(proc);
    air::generate::prepare_for_generation(&mut air);
    air
}

pub fn generate_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    proc.reset_reachability();
    proc.dominators_or_compute();
    if proc.options.opt_level >= OptLevel::O2 {
        // Convert to SSA form.
        fix_ssa(proc);
        // SCCP is quite expensive and untested pass. We do not run it by default.
        if proc.options.enable_sccp {
            
            crate::sccp::sccp(proc);
        }

        // TODO: Should we run `fix_ssa` after or before `reduce_strength`?
        // 
        // Running it before seems more beneficial because `reduce_strength`
        // can simplify SSA form and entirely remove some phi nodes.

        // Reduces strength until fixpoint.
        reduce_strength(proc);

        // convet sequence of branches to switches when possible
        infer_switches(proc);

        
    } else if proc.options.opt_level >= OptLevel::O1 {
        // Reduces strength in one pass.
        reduce_strength(proc);
    }

    
    lower_macros(proc);

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
    if code.proc.options.dump_air_at_each_phase {
        println!("AIR after lowering to AIR:");
        println!("{}", code);
    }
    code
}

pub fn generate<'a, 'b>(air: &'a mut Code<'b>, jit: &mut TargetMacroAssembler) {
    air::generate::generate(air, jit);
}
