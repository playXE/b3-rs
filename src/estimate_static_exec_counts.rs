use crate::{block::BlockId, procedure::Procedure};

/// Simple pass that estimates basic block frequencies based on loop analysis.
pub fn estimate_static_execution_counts(proc: &mut Procedure) {
    proc.natural_loops_or_compute();
    let natural_loops = proc.natural_loops();

    for block in (0..proc.blocks.len()).map(BlockId) {
        const BASE: f64 = 10.0;

        let freq = BASE.powi(natural_loops.loop_depth(block) as _);

        proc.block_mut(block).frequency = freq;
    }
}
