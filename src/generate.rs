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
    //alloca_to_reg(proc);

    if proc.options.opt_level >= OptLevel::O2 {
        // TODO: Should we run `fix_ssa` after or before `reduce_strength`?
        // Seems like running it before is better since `reduce_strength` can
        // work out better because it knows how to work with Phi's

        // Reduces strength until fixpoint.
        reduce_strength(proc);

        // convet sequence of branches to switches when possible
        infer_switches(proc);
    } else if proc.options.opt_level >= OptLevel::O1 {
        // Reduces strength in one pass.
        reduce_strength(proc);
    }

    // Convert to SSA form.
    fix_ssa(proc);

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
    println!("{}", proc.display_());
    let code = lower_to_air(proc);
    println!("{}", code);
    code
}

pub fn generate<'a>(air: &'a mut Code<'a>, jit: &mut TargetMacroAssembler) {
    air::generate::generate(air, jit);
}
