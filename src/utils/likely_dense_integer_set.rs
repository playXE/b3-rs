use std::{
    mem::{size_of, ManuallyDrop},
};
use indexmap::IndexSet;
use bitvec::vec::BitVec;

pub struct LikelyDenseIntegerSet {
    u: LikelyDenseIntegerSetU,
    size: usize,
    max: u32,
    min: u32,
}

impl Drop for LikelyDenseIntegerSet {
    fn drop(&mut self) {
        if self.is_bitvector() {
            unsafe {
                ManuallyDrop::drop(&mut self.u.bitvector);
            }
        } else {
            unsafe {
                ManuallyDrop::drop(&mut self.u.hashset);
            }
        }
    }
}

union LikelyDenseIntegerSetU {
    hashset: ManuallyDrop<IndexSet<u32>>,
    bitvector: ManuallyDrop<BitVec<usize>>,
}


enum IterVariant<'a> {
    BitVector(bitvec::slice::Iter<'a, usize, bitvec::order::Lsb0>),
    HashSet(indexmap::set::Iter<'a, u32>),
}

pub struct Iter<'a> {
    iter: IterVariant<'a>,
    shift: u32, 
}

impl<'a> Iterator for Iter<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.iter {
            IterVariant::BitVector(iter) => iter.next().map(|x| *x as u32 + self.shift),
            IterVariant::HashSet(iter) => iter.next().map(|x| *x),
        }
    }
}


impl LikelyDenseIntegerSet {
    pub fn new() -> Self {
        Self {
            u: LikelyDenseIntegerSetU {
                hashset: ManuallyDrop::new(IndexSet::new()),
            },
            size: 0,
            max: 0,
            min: u32::MAX,
        }
    }

    pub fn contains(&self, value: u32) -> bool {
        if self.is_bitvector() {
            let ix = value as usize - self.min as usize;
            if ix >= self.bitvector().len() {
                return false;
            }
            *self.bitvector().get(ix).unwrap()
        } else {
            self.hashset().contains(&value)
        }
    }

    pub fn add(&mut self, value: u32) -> bool {
        let compute_new_min = |this: &Self| -> u32 {
            let rounded_down_value = value & !63;
            this.min.min(rounded_down_value)
        };

        if !self.is_bitvector() {
            let is_new_entry = self.hashset_mut().insert(value);

            if !is_new_entry {
                return false;
            }

            self.min = compute_new_min(self) as u32;
            self.max = self.max.max(value);

            let hash_set_size = self.hashset().capacity() * size_of::<u32>();
            let would_be_bitvector_size = (self.max as usize - self.min as usize + 1) / size_of::<usize>();

            if would_be_bitvector_size * 2 < hash_set_size {
                self.transition_to_bitvector();
            }

            true 
        } else {
            let ix = value as usize - self.min as usize;
            if self.bitvector_mut().len() < ix {
                self.bitvector_mut().resize(ix + 1, false);
            }
            let is_new_entry = !self.bitvector_mut().get(ix).unwrap();
            self.bitvector_mut().set(ix, true);
            if is_new_entry {
                self.size += 1;
                self.min = compute_new_min(self) as u32;
                self.max = self.max.max(value);
            }
            is_new_entry
        }
    }

    pub fn len(&self) -> usize {
        self.is_bitvector().then(|| self.size)
            .unwrap_or_else(|| self.hashset().len())
    }

    pub fn iter(&self) -> Iter {
        if self.is_bitvector() {
            Iter {
                iter: IterVariant::BitVector(self.bitvector().iter()),
                shift: self.min,
            }
        } else {
            Iter {
                iter: IterVariant::HashSet(self.hashset().iter()),
                shift: 0,
            }
        }
    }

    pub fn memory_use(&self) -> usize {
        let result = size_of::<Self>();
        if self.is_bitvector() {
            return result + self.bitvector().len() / size_of::<usize>()
        } else {
            return result + self.hashset().capacity() * size_of::<u32>()
        }
    }

    fn is_bitvector(&self) -> bool {
        self.size != u32::MAX as usize
    }

    fn estimated_hashset_size(n: usize) -> usize {
        let hash_set_estimated_occupancy_overhead: usize = 3;
        let would_be_hash_set_capacity = 8.max(n) * hash_set_estimated_occupancy_overhead;
        would_be_hash_set_capacity * size_of::<u32>()
    }

    fn bitvector(&self) -> &BitVec<usize> {
        unsafe { &*self.u.bitvector }
    }

    fn bitvector_mut(&mut self) -> &mut BitVec<usize> {
        unsafe { &mut *self.u.bitvector }
    }

    fn hashset(&self) -> &IndexSet<u32> {
        unsafe { &*self.u.hashset }
    }

    fn hashset_mut(&mut self) -> &mut IndexSet<u32> {
        unsafe { &mut *self.u.hashset }
    }

    fn transition_to_hash_set(&mut self) {
        assert!(self.is_bitvector());
        let mut new_set = IndexSet::with_capacity(self.size + 1);

        for old_index in self.bitvector().iter_ones() {
            new_set.insert(old_index as u32 + self.min);
        }

        unsafe {
            ManuallyDrop::drop(&mut self.u.bitvector);

            self.u.hashset = ManuallyDrop::new(new_set);
        }

        self.size = u32::MAX as _;
        assert!(!self.is_bitvector());
    }

    fn transition_to_bitvector(&mut self) {
        assert!(!self.is_bitvector());

        let mut new_bitvector = bitvec::bitvec![0; self.max as usize - self.min as usize + 1];
        self.size = 0;
        let hashset = unsafe {
            ManuallyDrop::take(&mut self.u.hashset)
        };
        for old_index in hashset.iter() {
            new_bitvector.set(*old_index as usize - self.min as usize, true);
            self.size += 1;
        }

        unsafe {
            self.u.bitvector = ManuallyDrop::new(new_bitvector);
        }
    }
}
