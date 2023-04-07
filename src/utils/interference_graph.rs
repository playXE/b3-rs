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

use super::{bitvector::BitVector, likely_dense_integer_set::LikelyDenseIntegerSet};

pub trait InterferenceGraph {
    type Iter<'a>: Iterable
    where
        Self: 'a;

    fn contains(&self, u: u32, v: u32) -> bool;
    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool;
    fn add(&mut self, u: u32, v: u32);
    fn clear(&mut self);
    fn may_clear(&mut self, u: u32);
    fn set_max_index(&mut self, n: u32);
    fn for_each(&self, functor: impl FnMut(u32, u32));
    fn size(&self) -> usize;
    fn memory_use(&self) -> usize;
    fn dump_memory_use_in_kb(&self);
    fn iter<'a>(&'a self, u: u32) -> Self::Iter<'a>;
}

pub struct InterferenceBitVector {
    bitvector: BitVector,
    size: usize,
    num_elements: u32,
}

pub trait Iterable {
    type Iter<'a>: Iterator<Item = u32>
    where
        Self: 'a;
    fn iter<'a>(&'a self) -> Self::Iter<'a>;
}

#[allow(dead_code)]
pub struct InterferenceBitVectorIter<'a> {
    bitvector: &'a BitVector,
    starting_index: u32,
    begin: u32,
    end: u32,
}

impl<'a> Iterable for InterferenceBitVectorIter<'a> {
    type Iter<'b>
    = InterfaceBitVectorItrerable<'a, 'b>  where Self: 'b;

    fn iter<'b>(&'b self) -> InterfaceBitVectorItrerable<'a, 'b> {
        InterfaceBitVectorItrerable {
            iterator: self,
            index: self.begin,
        }
    }
}

impl<'a> InterferenceBitVectorIter<'a> {
    pub fn iter<'b>(&'b self) -> InterfaceBitVectorItrerable<'a, 'b> {
        InterfaceBitVectorItrerable {
            iterator: self,
            index: self.begin,
        }
    }
}

pub struct InterfaceBitVectorItrerable<'a, 'b> {
    iterator: &'b InterferenceBitVectorIter<'a>,
    index: u32,
}

impl<'a, 'b> Iterator for InterfaceBitVectorItrerable<'a, 'b> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let old = self.index;

        if old >= self.iterator.end {
            return None;
        }
        self.index = self
            .iterator
            .bitvector
            .find_bit(self.index as usize + 1, true) as u32;
        Some(old - self.iterator.starting_index)
    }
}

impl InterferenceBitVector {
    pub fn new() -> Self {
        Self {
            bitvector: BitVector::new(),
            size: 0,
            num_elements: 0,
        }
    }

    pub fn index(&self, i: u32, j: u32) -> usize {
        (i * self.num_elements + j) as _
    }
}

impl InterferenceGraph for InterferenceBitVector {
    type Iter<'a> = InterferenceBitVectorIter<'a>;

    fn contains(&self, u: u32, v: u32) -> bool {
        self.bitvector.quick_get(self.index(u, v))
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let already_in = self.bitvector.quick_set(self.index(u, v), true);
        self.size += !already_in as usize;
        !already_in
    }

    fn add(&mut self, u: u32, v: u32) {
        self.add_and_return_is_new_entry(u, v);
    }

    fn clear(&mut self) {
        self.bitvector.clear_all();
        self.size = 0;
    }

    fn may_clear(&mut self, _u: u32) {}

    fn set_max_index(&mut self, n: u32) {
        self.num_elements = n;
        self.bitvector.ensure_size(n as usize * n as usize);
    }

    fn for_each(&self, mut functor: impl FnMut(u32, u32)) {
        for i in 0..self.num_elements {
            for j in i + 1..self.num_elements {
                if self.contains(i, j) {
                    functor(i, j);
                }
            }
        }
    }

    fn size(&self) -> usize {
        self.size as _
    }

    fn memory_use(&self) -> usize {
        (self.bitvector.len() + 7) >> 3
    }

    fn dump_memory_use_in_kb(&self) {
        eprint!("InterferenceGraph uses {} kB", self.memory_use() / 1024);
    }

    fn iter<'a>(&'a self, u: u32) -> Self::Iter<'a> {
        let index = self.index(u, 0);
        let begin = self.bitvector.find_bit(self.index(u, 0), true);
        let end = self.index(u + 1, 0);

        InterferenceBitVectorIter {
            bitvector: &self.bitvector,
            starting_index: index as _,
            begin: begin as _,
            end: end as _,
        }
    }
}

pub struct InterferenceVector {
    vector: Vec<LikelyDenseIntegerSet>,
    size: usize,
}

impl InterferenceVector {
    pub fn new() -> Self {
        Self {
            vector: Vec::new(),
            size: 0,
        }
    }
}

pub struct InterferenceVectorIter<'a> {
    set: &'a LikelyDenseIntegerSet,
}

impl<'a> InterferenceVectorIter<'a> {
    pub fn iter(&self) -> super::likely_dense_integer_set::Iter<'a> {
        self.set.iter()
    }
}

impl<'a> Iterable for InterferenceVectorIter<'a> {
    type Iter<'b>
    = super::likely_dense_integer_set::Iter<'a>  where Self: 'b;

    fn iter<'b>(&'b self) -> super::likely_dense_integer_set::Iter<'a> {
        self.set.iter()
    }
}

impl InterferenceGraph for InterferenceVector {
    type Iter<'a> = InterferenceVectorIter<'a>;

    fn iter<'a>(&'a self, u: u32) -> Self::Iter<'a> {
        InterferenceVectorIter {
            set: &self.vector[u as usize],
        }
    }

    fn add(&mut self, u: u32, v: u32) {
        self.add_and_return_is_new_entry(u, v);
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let is_new_entry = self.vector[u as usize].add(v);
        self.size += is_new_entry as usize;
        is_new_entry
    }

    fn may_clear(&mut self, u: u32) {
        self.vector[u as usize].clear();
    }

    fn clear(&mut self) {
        for set in self.vector.iter_mut() {
            set.clear();
        }
        self.size = 0;
    }

    fn for_each(&self, mut functor: impl FnMut(u32, u32)) {
        for (i, set) in self.vector.iter().enumerate() {
            for j in set.iter() {
                functor(i as _, j);
            }
        }
    }

    fn size(&self) -> usize {
        self.size
    }

    fn contains(&self, u: u32, v: u32) -> bool {
        self.vector[u as usize].contains(v)
    }

    fn memory_use(&self) -> usize {
        self.vector.iter().map(|set| set.memory_use()).sum()
    }

    fn dump_memory_use_in_kb(&self) {
        eprint!("InterferenceGraph uses {} kB", self.memory_use() / 1024);
    }

    fn set_max_index(&mut self, n: u32) {
        self.vector.resize(n as _, LikelyDenseIntegerSet::new());
    }
}

pub struct UndirectedEdgesDuplicatingAdapter<G: InterferenceGraph> {
    pub graph: G,
}

impl<G: InterferenceGraph> UndirectedEdgesDuplicatingAdapter<G> {
    pub fn new(graph: G) -> Self {
        Self { graph }
    }
}

impl<G: InterferenceGraph> InterferenceGraph for UndirectedEdgesDuplicatingAdapter<G> {
    type Iter<'a> = G::Iter<'a>
    where G: 'a;

    fn iter<'a>(&'a self, u: u32) -> Self::Iter<'a> {
        self.graph.iter(u)
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let is_new_entry1 = self.graph.add_and_return_is_new_entry(u, v);
        let is_new_entry2 = self.graph.add_and_return_is_new_entry(v, u);
        debug_assert!(is_new_entry1 == is_new_entry2);
        is_new_entry1
    }

    fn add(&mut self, u: u32, v: u32) {
        self.graph.add(u, v);
        self.graph.add(v, u);
    }

    fn clear(&mut self) {
        self.graph.clear();
    }

    fn may_clear(&mut self, u: u32) {
        self.graph.may_clear(u);
    }

    fn set_max_index(&mut self, n: u32) {
        self.graph.set_max_index(n);
    }

    fn for_each(&self, functor: impl FnMut(u32, u32)) {
        self.graph.for_each(functor);
    }

    fn size(&self) -> usize {
        self.graph.size()
    }

    fn memory_use(&self) -> usize {
        self.graph.memory_use()
    }

    fn contains(&self, u: u32, v: u32) -> bool {
        self.graph.contains(u, v)
    }

    fn dump_memory_use_in_kb(&self) {
        self.graph.dump_memory_use_in_kb();
    }
}

pub struct UndirectedEdgesDedupAdapter<G: InterferenceGraph> {
    pub graph: G,
}

impl<G: InterferenceGraph> UndirectedEdgesDedupAdapter<G> {
    pub fn new(graph: G) -> Self {
        Self { graph }
    }
}

impl<G: InterferenceGraph> InterferenceGraph for UndirectedEdgesDedupAdapter<G> {
    type Iter<'a> = G::Iter<'a>
        where G: 'a
    ;

    fn iter<'a>(&'a self, u: u32) -> Self::Iter<'a> {
        self.graph.iter(u)
    }

    fn add_and_return_is_new_entry(&mut self, mut u: u32, mut v: u32) -> bool {
        if v < u {
            std::mem::swap(&mut u, &mut v);
        }

        self.graph.add_and_return_is_new_entry(u, v)
    }

    fn add(&mut self, mut u: u32, mut v: u32) {
        if v < u {
            std::mem::swap(&mut u, &mut v);
        }

        self.graph.add(u, v);
    }

    fn clear(&mut self) {
        self.graph.clear();
    }

    fn may_clear(&mut self, u: u32) {
        self.graph.may_clear(u);
    }

    fn set_max_index(&mut self, n: u32) {
        self.graph.set_max_index(n);
    }

    fn for_each(&self, functor: impl FnMut(u32, u32)) {
        self.graph.for_each(functor);
    }

    fn size(&self) -> usize {
        self.graph.size()
    }

    fn memory_use(&self) -> usize {
        self.graph.memory_use()
    }

    fn contains(&self, mut u: u32, mut v: u32) -> bool {
        if v < u {
            std::mem::swap(&mut u, &mut v);
        }
        self.graph.contains(u, v)
    }

    fn dump_memory_use_in_kb(&self) {
        self.graph.dump_memory_use_in_kb();
    }
}

pub const MAX_SIZE_FOR_SMALL_INTERFERENCE_GRAPH: usize = 400;

pub type DefaultSmallInterferenceGraph = UndirectedEdgesDuplicatingAdapter<InterferenceBitVector>;
pub type DefaultLargeInterferenceGraph = UndirectedEdgesDedupAdapter<InterferenceVector>;

pub type SmallInterferenceGraph = DefaultSmallInterferenceGraph;
pub type LargeInterferenceGraph = DefaultLargeInterferenceGraph;
