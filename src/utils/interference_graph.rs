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

use bitvec::vec::BitVec;

use super::likely_dense_integer_set::LikelyDenseIntegerSet;

pub trait InterferenceGraph {
    type Iter<'a>: Iterator<Item = u32>
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
    fn iter<'a>(&'a self) -> Self::Iter<'a>;
}

pub struct InterferenceBitVector {
    bitvector: BitVec<usize, bitvec::order::Lsb0>,
    size: usize,
    num_elements: u32,
}

#[allow(dead_code)]
pub struct InterferenceBitVectorIter<'a> {
    bitvector: &'a BitVec<usize, bitvec::order::Lsb0>,
    size: usize,
    starting_index: usize,
    num_elements: u32,
    index: u32,
}

impl<'a> Iterator for InterferenceBitVectorIter<'a> {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        self.bitvector
            .iter_ones()
            .take(self.index as usize + 1)
            .next()
            .map(|x| x as u32)
            .and_then(|i| {
                self.index = i as _;
                Some(i - self.starting_index as u32)
            })
    }
}

impl InterferenceBitVector {
    pub fn new() -> Self {
        Self {
            bitvector: BitVec::new(),
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
        self.bitvector
            .get(self.index(u, v))
            .map(|x| *x)
            .unwrap_or(false)
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let alrady_in = self.contains(u, v);
        let index = self.index(u, v);
        if index >= self.bitvector.len() {
            self.bitvector.resize(index + 1, false);
        }
        self.bitvector.set(index, true);
        self.size += !alrady_in as usize;

        !alrady_in
    }

    fn add(&mut self, u: u32, v: u32) {
        self.add_and_return_is_new_entry(u, v);
    }

    fn clear(&mut self) {
        self.bitvector.clear();
        self.size = 0;
    }

    fn may_clear(&mut self, _u: u32) {}

    fn set_max_index(&mut self, n: u32) {
        self.num_elements = n;
        self.bitvector.resize((n * n) as _, false);
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
        println!("InterferenceGraph uses {} kB", self.memory_use() / 1024);
    }

    fn iter<'a>(&'a self) -> Self::Iter<'a> {
        InterferenceBitVectorIter {
            bitvector: &self.bitvector,
            size: self.size,
            starting_index: 0,
            num_elements: self.num_elements,
            index: 0,
        }
    }
}

pub struct InterferenceVector {
    vector: Vec<LikelyDenseIntegerSet>,
    size: usize,
}

#[allow(dead_code)]
pub struct InterferenceVectorIter<'a> {
    vector: &'a Vec<LikelyDenseIntegerSet>,
    size: usize,
    starting_index: usize,
    index: usize,
}

impl<'a> Iterator for InterferenceVectorIter<'a> {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        self.vector.iter().skip(self.index).next().and_then(|x| {
            self.index += 1;
            x.iter().next().map(|x| x - self.starting_index as u32)
        })
    }
}

impl InterferenceVector {
    pub fn new() -> Self {
        Self {
            vector: Vec::new(),
            size: 0,
        }
    }
}

impl InterferenceGraph for InterferenceVector {
    type Iter<'a> = InterferenceVectorIter<'a>;
    fn iter<'a>(&'a self) -> Self::Iter<'a> {
        InterferenceVectorIter {
            vector: &self.vector,
            size: self.size,
            starting_index: 0,
            index: 0,
        }
    }

    fn contains(&self, u: u32, v: u32) -> bool {
        if u as usize >= self.vector.len() {
            false
        } else {
            self.vector[u as usize].contains(v as _)
        }
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let alrady_in = self.contains(u, v);
        if u as usize >= self.vector.len() {
            self.vector
                .resize(u as usize + 1, LikelyDenseIntegerSet::new());
        }
        let is_new_entry = self.vector[u as usize].add(v as _);
        self.size += !alrady_in as usize;

        !alrady_in && is_new_entry
    }

    fn add(&mut self, u: u32, v: u32) {
        self.add_and_return_is_new_entry(u, v);
    }

    fn clear(&mut self) {
        self.vector.clear();
        self.size = 0;
    }

    fn may_clear(&mut self, u: u32) {
        if (u as usize) < self.vector.len() {
            self.vector[u as usize].clear();
        }
    }

    fn set_max_index(&mut self, n: u32) {
        self.vector.resize(n as _, LikelyDenseIntegerSet::new());
    }

    fn for_each(&self, mut functor: impl FnMut(u32, u32)) {
        for (i, set) in self.vector.iter().enumerate() {
            for j in set.iter() {
                functor(i as _, j as _);
            }
        }
    }

    fn size(&self) -> usize {
        self.size as _
    }

    fn memory_use(&self) -> usize {
        self.vector.iter().map(|x| x.memory_use()).sum()
    }

    fn dump_memory_use_in_kb(&self) {
        let mut memory_use = 0;
        for set in self.vector.iter() {
            memory_use += set.memory_use();
        }
        println!("InterferenceVector uses {} kB", memory_use / 1024);
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

    fn iter<'a>(&'a self) -> Self::Iter<'a> {
        self.graph.iter()
    }

    fn add_and_return_is_new_entry(&mut self, u: u32, v: u32) -> bool {
        let is_new_entry1 = self.graph.add_and_return_is_new_entry(u, v);
        let is_new_entry2 = self.graph.add_and_return_is_new_entry(v, u);
        assert!(is_new_entry1 == is_new_entry2);
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

    fn iter<'a>(&'a self) -> Self::Iter<'a> {
        self.graph.iter()
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