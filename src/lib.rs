#![allow(incomplete_features)]
#![feature(adt_const_params)]

pub mod utils;
pub mod air;
pub mod bank;
pub mod block;
pub mod block_insertion_set;
pub mod break_critical_edges;
pub mod cfg;
pub mod dominators;
pub mod effects;
pub mod fix_ssa;
pub mod insertion_set;
pub mod jit;
pub mod kind;
pub mod legalize_memory_offsets;
pub mod liveness;
pub mod lower_to_air;
pub mod natural_loops;
pub mod opcode;
pub mod phi_children;
pub mod procedure;
pub mod pure_cse;
pub mod reduce_strength;
pub mod rpo;
pub mod sparse_collection;
pub mod ssa_calculator;
pub mod stackmap_special;
pub mod stackmap_value;
pub mod typ;
pub mod use_counts;
pub mod value;
pub mod variable;
pub mod variable_liveness;
pub mod width;

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
