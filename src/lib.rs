#![allow(incomplete_features)]
#![feature(adt_const_params, fmt_internals)]

pub mod air;
pub mod bank;
pub mod block;
pub mod block_insertion_set;
pub mod break_critical_edges;
pub mod check_special;
pub mod compile;
pub mod data_section;
pub mod dominators;
pub mod effects;
pub mod estimate_static_exec_counts;
pub mod fix_ssa;
pub mod eliminate_dead_code;
pub mod generate;
pub mod insertion_set;
pub mod jit;
pub mod kind;
pub mod legalize_memory_offsets;
pub mod liveness;
pub mod compute_division_magic;
pub mod lower_to_air;
pub mod move_constants;
pub mod natural_loops;
pub mod opcode;
pub mod patchpoint_special;
pub mod patchpoint_value;
pub mod phi_children;
pub mod procedure;
pub mod pure_cse;
pub mod reduce_strength;
pub mod rpo;
pub mod sparse_collection;
pub mod ssa_calculator;
pub mod stackmap_generation_params;
pub mod stackmap_special;
pub mod stackmap_value;
pub mod typ;
pub mod use_counts;
pub mod utils;
pub mod value;
pub mod variable;
pub mod variable_liveness;
pub mod width;

#[cfg(test)]
pub mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TriState {
    False,
    True,
    Undeterminate,
}

impl TriState {
    pub fn from_bool(x: bool) -> Self {
        tri_state(x)
    }
}

pub fn tri_state(x: bool) -> TriState {
    if x {
        TriState::True
    } else {
        TriState::False
    }
}
pub fn invert(tri: TriState) -> TriState {
    match tri {
        TriState::False => TriState::True,
        TriState::True => TriState::False,
        TriState::Undeterminate => TriState::Undeterminate,
    }
}

use num::traits::{Float, One, PrimInt, Unsigned};
pub fn chill_div<T: PrimInt + One>(numerator: T, denominator: T) -> T {
    if denominator == T::zero() {
        T::zero()
    } else if denominator == T::one().sub(T::one()).sub(T::one()) && numerator == T::min_value() {
        T::min_value()
    } else {
        numerator / denominator
    }
}

pub fn chill_mod<T: PrimInt + One>(numerator: T, denominator: T) -> T {
    if denominator == T::zero() {
        T::zero()
    } else if denominator == T::one().sub(T::one()).sub(T::one()) && numerator == T::min_value() {
        T::zero()
    } else {
        numerator % denominator
    }
}

pub fn chill_udiv<T: PrimInt + One + Unsigned>(numerator: T, denominator: T) -> T {
    if denominator == T::zero() {
        T::zero()
    } else {
        numerator / denominator
    }
}

pub fn chill_umod<T: PrimInt + One + Unsigned>(numerator: T, denominator: T) -> T {
    if denominator == T::zero() {
        T::zero()
    } else {
        numerator % denominator
    }
}

pub fn fmax<T: Float>(a: T, b: T) -> T {
    if a.is_nan() {
        b
    } else if b.is_nan() {
        a
    } else {
        a.max(b)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub enum OptLevel {
    /// No optimizations at all.
    ///
    /// The compilation process should be as fast as possible.
    None = 0,
    /// Perform basic optimizations.
    O1,
    /// Perform more complex optimizations.
    ///
    /// Might use graph coloring to reduce register pressure.
    O2,
    /// Perform all optimizations.
    ///
    /// Uses graph coloring to reduce register pressure.
    O3,
}

impl PartialEq<u8> for OptLevel {
    fn eq(&self, other: &u8) -> bool {
        *self as u8 == *other
    }
}

impl PartialEq<OptLevel> for u8 {
    fn eq(&self, other: &OptLevel) -> bool {
        *self == *other as u8
    }
}

impl From<u8> for OptLevel {
    fn from(x: u8) -> Self {
        match x {
            0 => OptLevel::None,
            1 => OptLevel::O1,
            2 => OptLevel::O2,
            3 => OptLevel::O3,
            _ => panic!("Invalid opt level"),
        }
    }
}

impl From<OptLevel> for u8 {
    fn from(x: OptLevel) -> Self {
        x as u8
    }
}

impl PartialOrd<u8> for OptLevel {
    fn partial_cmp(&self, other: &u8) -> Option<std::cmp::Ordering> {
        Some((*self as u8).cmp(other))
    }
}

impl PartialOrd<OptLevel> for u8 {
    fn partial_cmp(&self, other: &OptLevel) -> Option<std::cmp::Ordering> {
        Some((*self as u8).cmp(&(*other as u8)))
    }
}

impl Ord for OptLevel {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Options {
    pub opt_level: OptLevel,
    /// The maximum number of tmps an Air program can have before always register allocating with Linear Scan
    pub maximum_tmps_for_graph_coloring: usize,
    pub linear_scan_spill_everything: bool,
    pub air_force_briggs_allocator: bool,
    pub air_force_irc_allocator: bool,
    pub air_force_linear_scan_allocator: bool,
    pub coalesce_spill_costs: bool,
    pub use_b3_tail_dup: bool,
    pub max_b3_tail_dup_block_size: usize,
    pub max_b3_tail_dup_block_successors: usize,
    pub use_b3_hoist_loop_invariant_values: bool,
    pub dump_b3_at_each_phase: bool,
    pub dump_air_at_each_phase: bool,
    pub dump_b3_reduce_strength: bool,
    /// Should we estimate basic block frequency based on how deep it is inside a loop?
    /// 
    /// Turn this option off if you provide your own frequency estimates. By default 
    /// it is set to true.
    pub estimate_static_execution_counts: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            estimate_static_execution_counts: true,
            opt_level: OptLevel::O2,
            maximum_tmps_for_graph_coloring: 60000,
            linear_scan_spill_everything: false,
            air_force_briggs_allocator: false,
            air_force_irc_allocator: false,
            air_force_linear_scan_allocator: false,
            coalesce_spill_costs: true,
            use_b3_tail_dup: true,
            max_b3_tail_dup_block_size: 3,
            max_b3_tail_dup_block_successors: 3,
            use_b3_hoist_loop_invariant_values: false,
            dump_b3_at_each_phase: false,
            dump_air_at_each_phase: false,
            dump_b3_reduce_strength: false,
        }
    }
}

pub use block::*;
pub use compile::*;
pub use generate::*;
pub use opcode::*;
pub use procedure::*;
pub use value::*;
pub use typ::*;
pub use width::*;
pub use bank::*;