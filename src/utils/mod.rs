use std::ops::Range;

pub mod bitmap;
pub mod bitvector;
pub mod deque;
pub mod index_set;
pub mod insertion;
pub mod interference_graph;
pub mod likely_dense_integer_set;
pub mod phase_scope;

pub trait RangeExt {
    fn overlaps(&self, other: &Self) -> bool;
}

impl<T: PartialOrd> RangeExt for Range<T> {
    fn overlaps(&self, other: &Self) -> bool {
        let left_max = &self.end;
        let right_min = &other.start;
        let right_max = &other.end;
        let left_min = &self.start;

        left_max > right_min && right_max > left_min
    }
}
