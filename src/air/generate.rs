use super::{
    code::Code, eliminate_dead_code::eliminate_dead_code,
    lsra::allocate_registers_and_stack_by_linear_scan, simplify_cfg::simplify_cfg,
};

pub fn prepare_for_generation(code: &mut Code<'_>) {
    code.reset_reachability();

    simplify_cfg(code);
    eliminate_dead_code(code);

    // TODO: Port IRC and "fast" -O0 allocator from original source code.

    // When we're compiling quickly, we do register and stack allocation in one linear scan
    // phase. It's fast because it computes liveness only once.
    allocate_registers_and_stack_by_linear_scan(code);

    simplify_cfg(code);
}
