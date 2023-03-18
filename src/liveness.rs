use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

use indexmap::{IndexMap, IndexSet};

use crate::dominators::Graph;

pub trait LivenessAdapter {
    type Thing: Copy + Clone + PartialEq + Eq + PartialOrd + Hash + Debug + Ord + PartialOrd;
    type CFG: Graph;
    fn cfg(&self) -> &Self::CFG;
    fn prepare_to_compute(&mut self);
    fn num_indices(&self) -> usize;
    fn value_to_index(thing: Self::Thing) -> usize;
    fn index_to_value(index: usize) -> Self::Thing;

    fn for_each_use<F>(
        &self,
        block: <<Self as LivenessAdapter>::CFG as Graph>::Node,
        value_boundary_index: usize,
        func: F,
    ) where
        F: FnMut(Self::Thing);

    fn for_each_def<F>(
        &self,
        block: <<Self as LivenessAdapter>::CFG as Graph>::Node,
        value_boundary_index: usize,
        func: F,
    ) where
        F: FnMut(Self::Thing);

    fn block_size(&self, block: <<Self as LivenessAdapter>::CFG as Graph>::Node) -> usize;
}

pub struct Liveness<'a, A: LivenessAdapter> {
    pub adapter: &'a mut A,
    pub workset: IndexSparseSet<DefaultIndexSparseSetEntry>,
    pub live_at_head: IndexMap<<A::CFG as Graph>::Node, Vec<usize>>,
    pub live_at_tail: IndexMap<<A::CFG as Graph>::Node, Vec<usize>>,
}

impl<'a, A: LivenessAdapter> Liveness<'a, A> {
    pub fn new(adapter: &'a mut A) -> Self {
        let this = Liveness {
            workset: IndexSparseSet::new(adapter.num_indices()),
            adapter,
            live_at_head: IndexMap::new(),
            live_at_tail: IndexMap::new(),
        };

        this
    }

    pub fn raw_live_at_head(
        &self,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
    ) -> &[usize] {
        self.live_at_head.get(&block).unwrap()
    }

    pub fn live_at_head(
        &'a self,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
    ) -> LiveIterable<'a, A> {
        LiveIterable {
            iter: self.raw_live_at_head(block).iter(),
            liveness: self,
        }
    }

    pub fn live_at_tail(
        &'a self,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
    ) -> LiveIterable<'a, A> {
        LiveIterable {
            iter: self.live_at_tail.get(&block).unwrap().iter(),
            liveness: self,
        }
    }

    pub fn workset_mut(&mut self) -> &mut IndexSparseSet<DefaultIndexSparseSetEntry> {
        &mut self.workset
    }

    pub fn compute(&mut self) {
        self.adapter.prepare_to_compute();

        for block_index in (0..self.adapter.cfg().num_nodes()).rev() {
            let block = self.adapter.cfg().node(block_index);

            if let Some(block) = block {
                let live_at_tail = self.live_at_tail.entry(block).or_insert_with(Vec::new);

                self.adapter
                    .for_each_use(block, self.adapter.block_size(block), |index| {
                        live_at_tail.push(A::value_to_index(index));
                    });

                live_at_tail.sort();
                live_at_tail.dedup();
            }
        }

        let mut dirty_blocks = IndexSet::new();

        for block_index in (0..self.adapter.cfg().num_nodes()).rev() {
            dirty_blocks.insert(block_index);
        }

        let mut merge_buffer = Vec::new();
        let mut changed;
        loop {
            changed = false;

            for block_index in (0..self.adapter.cfg().num_nodes()).rev() {
                let block = self.adapter.cfg().node(block_index);

                if let Some(block) = block {
                    if !dirty_blocks.remove(&block_index) {
                        continue;
                    }

                    {
                        let mut local_calc = LocalCalc::new(self, block);

                        for inst_index in (0..local_calc.liveness.adapter.block_size(block)).rev() {
                            local_calc.execute(inst_index);
                        }
                    }

                    self.adapter.for_each_def(block, 0, |index| {
                        self.workset.remove(A::value_to_index(index));
                    });

                    let live_at_head = self.live_at_head.entry(block).or_insert_with(Vec::new);

                    if self.workset.len() == live_at_head.len() {
                        self.workset.clear();
                    } else {
                        for index in live_at_head.iter() {
                            self.workset.remove(*index);
                        }
                    }

                    if self.workset.is_empty() {
                        continue;
                    }

                    live_at_head.reserve(self.workset.len() + live_at_head.len());

                    for new_value in self.workset.iter() {
                        live_at_head.push(new_value.key);
                    }

                    self.workset.sort();

                    for predecessor in self.adapter.cfg().predecessors(block).iter() {
                        let live_at_tail = self.live_at_tail.get_mut(predecessor).unwrap();

                        if live_at_tail.is_empty() {
                            *live_at_tail = self.workset.values.iter().map(|x| x.key()).collect();
                        } else {
                            merge_buffer.resize(live_at_tail.len() + self.workset.len(), 0);

                            let k = merge_deduplicated_sorted(
                                live_at_tail.iter().copied(),
                                self.workset.values.iter().map(|x| x.key),
                                &mut merge_buffer,
                            );

                            merge_buffer.truncate(k);

                            if merge_buffer.len() == live_at_tail.len() {
                                continue;
                            }

                            *live_at_tail = merge_buffer.clone();
                        }

                        dirty_blocks.insert(self.adapter.cfg().node_index(*predecessor));
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
    }
}

pub struct LiveAtHead<'a, A: LivenessAdapter> {
    liveness: &'a Liveness<'a, A>,
}

impl<'a, A: LivenessAdapter> LiveAtHead<'a, A> {
    pub fn new(liveness: &'a mut Liveness<'a, A>) -> Self {
        for block_index in (0..liveness.adapter.cfg().num_nodes()).rev() {
            let block = liveness.adapter.cfg().node(block_index);

            if let Some(block) = block {
                liveness.live_at_head.get_mut(&block).unwrap().sort();
            }
        }
        let this = LiveAtHead { liveness };

        this
    }

    pub fn is_live_at_head(
        &self,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
        value: A::Thing,
    ) -> bool {
        let index = A::value_to_index(value);
        self.liveness
            .live_at_head
            .get(&block)
            .unwrap()
            .binary_search(&index)
            .is_ok()
    }
}

pub struct LiveAtHeadCloned<A: LivenessAdapter> {
    live_at_head: IndexMap<<<A as LivenessAdapter>::CFG as Graph>::Node, Vec<usize>>,
}

impl<A: LivenessAdapter> LiveAtHeadCloned<A> {
    pub fn new(liveness: &mut Liveness<A>) -> Self {
        for block_index in (0..liveness.adapter.cfg().num_nodes()).rev() {
            let block = liveness.adapter.cfg().node(block_index);

            if let Some(block) = block {
                liveness.live_at_head.get_mut(&block).unwrap().sort();
            }
        }
        let this = LiveAtHeadCloned {
            live_at_head: liveness.live_at_head.clone(),
        };

        this
    }

    pub fn is_live_at_head(
        &self,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
        value: A::Thing,
    ) -> bool {
        let index = A::value_to_index(value);
        self.live_at_head
            .get(&block)
            .unwrap()
            .binary_search(&index)
            .is_ok()
    }
}

pub struct LiveIterable<'a, A: LivenessAdapter> {
    iter: std::slice::Iter<'a, usize>,
    liveness: &'a Liveness<'a, A>,
}

impl<'a, A: LivenessAdapter> Iterator for LiveIterable<'a, A> {
    type Item = A::Thing;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&index| A::index_to_value(index))
    }
}

impl<'a, A: LivenessAdapter> LiveIterable<'a, A> {
    pub fn contains(&self, value: A::Thing) -> bool {
        let index = A::value_to_index(value);
        self.liveness.workset.contains(index)
    }
}

pub struct LocalCalc<'a, 'b, A: LivenessAdapter> {
    pub liveness: &'a mut Liveness<'b, A>,
    pub block: <<A as LivenessAdapter>::CFG as Graph>::Node,
}

impl<'a, 'b, A: LivenessAdapter> LocalCalc<'a, 'b, A> {
    pub fn new(
        liveness: &'a mut Liveness<'b, A>,
        block: <<A as LivenessAdapter>::CFG as Graph>::Node,
    ) -> Self {
        let this = LocalCalc { liveness, block };
        let live_at_tail = this.liveness.live_at_tail.get(&block).unwrap().clone();
        let workset = &mut this.liveness.workset;

        workset.clear();

        for index in live_at_tail {
            workset.add(index, ());
        }

        this
    }

    pub fn live(&'a self) -> Iterable<'a, A> {
        Iterable::new(self.liveness)
    }

    pub fn is_live(&'a self, value: A::Thing) -> bool {
        self.live().contains(value)
    }

    pub fn execute(&mut self, inst_index: usize) {
        let workset = &mut self.liveness.workset;

        self.liveness
            .adapter
            .for_each_def(self.block, inst_index + 1, |index| {
                workset.remove(A::value_to_index(index));
            });

        self.liveness
            .adapter
            .for_each_use(self.block, inst_index, |index| {
                workset.add(A::value_to_index(index), ());
            });
    }
}

pub struct Iterable<'a, A: LivenessAdapter> {
    liveness: &'a Liveness<'a, A>,
}

impl<'a, A: LivenessAdapter> Iterable<'a, A> {
    pub fn new(liveness: &'a Liveness<'a, A>) -> Self {
        Iterable { liveness }
    }

    pub fn contains(&self, value: A::Thing) -> bool {
        let index = A::value_to_index(value);
        self.liveness.workset.contains(index)
    }

    pub fn iter(&self) -> std::slice::Iter<DefaultIndexSparseSetEntry> {
        self.liveness.workset.iter()
    }
}

pub trait IndexSparseSetEntry: Copy + Clone {
    type Init;
    fn create(entry: usize, init: Self::Init) -> Self;
    fn key(&self) -> usize;
}

#[derive(Debug, Copy, Clone)]
pub struct DefaultIndexSparseSetEntry {
    key: usize,
}

impl IndexSparseSetEntry for DefaultIndexSparseSetEntry {
    type Init = ();
    fn create(key: usize, _init: ()) -> Self {
        DefaultIndexSparseSetEntry { key }
    }

    fn key(&self) -> usize {
        self.key
    }
}

impl<T: Copy + Clone> IndexSparseSetEntry for (usize, T) {
    type Init = T;
    fn create(key: usize, init: T) -> Self {
        (key, init)
    }

    fn key(&self) -> usize {
        self.0
    }
}


/// IndexSparseSet is an efficient set of integers that can only be valued
/// between zero and size() - 1.
///
/// The implementation is using Briggs Sparse Set representation. We allocate
/// memory from 0 to size() - 1 to do mapping in O(1), but we never initialize
/// that memory. When adding/removing values to the set, they are added in a list
/// and the corresponding bucket is initialized to the position in the list.
///
/// The assumption here is that we only need a sparse subset of number live at any
/// time.
#[derive(Debug)]
pub struct IndexSparseSet<T: IndexSparseSetEntry> {
    map: Vec<usize>,
    values: Vec<T>,
}

impl<T: IndexSparseSetEntry> IndexSparseSet<T> {
    pub fn new(size: usize) -> Self {
        IndexSparseSet {
            map: vec![0; size],
            values: Vec::new(),
        }
    }

    pub fn get(&self, value: usize) -> Option<T> {
        let position = self.map[value];

        if position >= self.values.len() {
            return None;
        }

        if self.values[position].key() != value {
            return None;
        }

        Some(self.values[position])
    }

    pub fn get_mut(&mut self, value: usize) -> Option<&mut T> {
        let position = self.map[value];

        if position >= self.values.len() {
            return None;
        }

        if self.values[position].key() != value {
            return None;
        }

        Some(&mut self.values[position])
    }

    pub fn contains(&self, value: usize) -> bool {
        let position = self.map.get(value).copied();

        if let Some(position) = position {
            if position >= self.values.len() {
                return false;
            }

            return self.values[position].key() == value;
        } else {
            false
        }
    }

    pub fn sort(&mut self) {
        self.values.sort_by(|a, b| a.key().cmp(&b.key()));

        for index in 0..self.values.len() {
            let key = self.values[index].key();

            self.map[key] = index;
        }
    }

    pub fn add(&mut self, value: usize, init: T::Init) -> bool {
        if !self.contains(value) {
            self.values.push(T::create(value, init));
            self.map[value] = self.values.len() - 1;

            true
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn set(&mut self, value: usize, init: T::Init) -> bool {
        if let Some(entry) = self.get_mut(value) {
            *entry = T::create(value, init);

            false
        } else {
            self.values.push(T::create(value, init));
            self.map[value] = self.values.len() - 1;
            false
        }
    }

    pub fn remove(&mut self, value: usize) -> bool {
        let position = self.map[value];

        if position >= self.values.len() {
            return false;
        }

        if self.values[position].key() == value {
            let last_value = self.values.last().copied().unwrap();

            self.values[position] = last_value;
            self.map[last_value.key()] = position;
            self.values.pop().unwrap();

            return true;
        }

        false
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl<T: IndexSparseSetEntry> Deref for IndexSparseSet<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<T: IndexSparseSetEntry> DerefMut for IndexSparseSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

fn merge_deduplicated_sorted<T: Copy + PartialOrd + Ord>(
    left: impl Iterator<Item = T>,
    right: impl Iterator<Item = T>,
    dst: &mut [T],
) -> usize {
    /*let mut i = 0;
    let mut j = 0;
    let mut k = 0;

    while i < left.len() && j < right.len() {
        if left[i] < right[j] {
            dst[k] = left[i];
            i += 1;
        } else if left[i] > right[j] {
            dst[k] = right[j];
            j += 1;
        } else {
            dst[k] = left[i];
            i += 1;
            j += 1;
        }

        k += 1;
    }

    while i < left.len() {
        dst[k] = left[i];
        i += 1;
        k += 1;
    }

    while j < right.len() {
        dst[k] = right[j];
        j += 1;
        k += 1;
    }

    k*/

    let mut left = left.peekable();
    let mut right = right.peekable();

    let mut i = 0;

    while let (Some(l), Some(r)) = (left.peek(), right.peek()) {
        if l < r {
            dst[i] = left.next().unwrap();
        } else if l > r {
            dst[i] = right.next().unwrap();
        } else {
            dst[i] = left.next().unwrap();
            right.next();
        }

        i += 1;
    }

    while let Some(l) = left.next() {
        dst[i] = l;
        i += 1;
    }

    while let Some(r) = right.next() {
        dst[i] = r;
        i += 1;
    }

    i
}
