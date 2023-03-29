
//! This file offers multiple implementations of a simple interference graph, with varying performance characteristics.
//! Conceptually they are just sets of pairs of integers
//! All of them have the same interface:
//!   bool contains(IndexType u, IndexType v)
//!   bool addAndReturnIsNewEntry(IndexType u, IndexType v)
//!   void add(IndexType u, IndexType v)
//!   void clear()
//!   void mayClear(IndexType u)
//!   void setMaxIndex(unsigned n)
//!   void forEach(const Functor& functor)
//!   unsigned size()
//!   unsigned memoryUse()
//!   void dumpMemoryUseInKB()
//! Three useful type aliases are defined:
//!   SmallInterferenceGraph, which is the fastest but has quadratic memory use in the maximum vertex index (best for n < 400 as it is already 20kB by that point)
//!   LargeInterferenceGraph, as long as the indices fit in a uint16_t
//!   HugeInterferenceGraph otherwise
//! If you need to iterate over the indices that interfere with a given index, you must use the following:
//!   SmallIterableInterferenceGraph
//!   LargeIterableInterferenceGraph
//!   HugeIterableInterferenceGraph
//! The large and huge versions are 2x the memory of their non-iterable versions, but offer an additional operator[] method, which returns an iterable object


pub trait InterferenceGraph {
    type IndexType: Copy + Eq + Ord + std::fmt::Debug + num::PrimInt + TryInto<usize> + TryFrom<usize>;
}