use indexmap::IndexSet;
use std::mem::{size_of, ManuallyDrop};

use super::bitvector::{BitVector, BitVectorIter};

/// This is effectively a std::variant<HashSet, Pair<BitVector, IndexType>>
/// If it is in BitVector mode, it keeps track of the minimum value in the set, and has the bitVector shifted by the same amount.
/// So for example {64000, 64002, 64003} would be represented as the bitVector 1101 with a m_min of 64000.
/// It shifts between the two modes whenever that would at least halve its memory usage. So it will never use more than twice the optimal amount of memory, and yet should not ping-pong between the two modes too often.
/// As an optimization, instead of keeping track of the minimum value, it keeps track of the minimum value rounded down to the next multiple of 64.
/// This reduces repeated re-indexings of the bitvector when repeatedly adding a value just below the current minimum.
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
    bitvector: ManuallyDrop<BitVector>,
}

enum IterVariant<'a> {
    BitVector(BitVectorIter<'a>),
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
            IterVariant::BitVector(iter) => iter.next().map(|x| x as u32 + self.shift),
            IterVariant::HashSet(iter) => iter.next().map(|x| *x),
        }
    }
}

impl LikelyDenseIntegerSet {
    pub fn new() -> Self {
        Self {
            u: LikelyDenseIntegerSetU {
                bitvector: ManuallyDrop::new(BitVector::new()),
            },
            size: 0,
            max: 0,
            min: 0,
        }
    }

    pub fn contains(&self, value: u32) -> bool {
        if self.is_bitvector() {
            if self.min > value || self.max < value {
                return false;
            }
            let ix = value as usize - self.min as usize;
            if ix >= self.bitvector().len() {
                return false;
            }
            self.bitvector().get(ix)
        } else {
            self.hashset().contains(&value)
        }
    }

    pub fn clear(&mut self) {
        if self.is_bitvector() {
            self.bitvector_mut().clear_all();
        } else {
            self.hashset_mut().clear();
        }
        self.size = 0;
        self.max = 0;
        self.min = 0;
    }

    pub fn add(&mut self, value: u32) -> bool {
        if self.size == 0 {
            assert!(self.is_bitvector());
            self.min = value & !63;
            self.max = value;
            self.size = 1;
            // not quickSet, as value - m_min might be 63, and the inline bit vector cannot store that value.
            // So there might be some overflow here, forcing an allocation of an outline bit vector.
            let ix = value as usize - self.min as usize;
            self.bitvector_mut().set(ix, true);
            return true;
        }

        let compute_new_min = |this: &Self| {
            let rounded_down_value = value & !63;
            this.min.min(rounded_down_value)
        };

        if !self.is_bitvector() {
            if self.hashset().contains(&value) {
                return false;
            }
            self.hashset_mut().insert(value);
            self.min = compute_new_min(self);
            self.max = self.max.max(value);
            let hash_set_size = self.hashset().capacity() * size_of::<u32>();
            let would_be_bitvector_size = (self.max - self.min) / 8;

            if would_be_bitvector_size * 2 < hash_set_size as u32 {
                self.transition_to_bitvector();
            }

            return true;
        }

        if value >= self.min && value <= self.max {
            let ix = value as usize - self.min as usize;
            let is_new_entry = !self.bitvector_mut().set(ix, true);
            self.size += is_new_entry as usize;
            return is_new_entry;
        }

        // We are in BitVector mode, and value is not in the bounds: we will definitely insert it as a new entry.
        self.size += 1;

        let new_min = compute_new_min(self);
        let new_max = self.max.max(value);
        let bitvector_size = (self.max - self.min) / 8;
        let would_be_hashset_size = Self::estimated_hashset_size(self.size as _);

        if would_be_hashset_size * 2 < bitvector_size as usize {
            self.transition_to_hash_set();
            let result = self.hashset_mut().insert(value);
            assert!(result);
            self.min = new_min;
            self.max = new_max;
            return true;
        }

        if value < self.min {
            assert!(new_min < self.min);
            let shift = self.min - new_min;
            self.bitvector_mut()
                .shift_right_by_multiple_of_64(shift as usize);
            self.min = new_min;
        }

        let ix = value as usize - self.min as usize;

        let is_new_entry = !self.bitvector_mut().set(ix, true);
        assert!(is_new_entry);
        self.max = new_max;
        return true;
    }

    pub fn len(&self) -> usize {
        self.is_bitvector()
            .then(|| self.size)
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
            return result + self.bitvector().len() / size_of::<usize>();
        } else {
            return result + self.hashset().capacity() * size_of::<u32>();
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

    fn bitvector(&self) -> &BitVector {
        unsafe { &*self.u.bitvector }
    }

    fn bitvector_mut(&mut self) -> &mut BitVector {
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

        for old_index in self.bitvector().iter() {
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

        let mut new_bitvector = BitVector::new();
        new_bitvector.ensure_size(self.max as usize - self.min as usize + 1);
        self.size = 0;
        let hashset = unsafe { ManuallyDrop::take(&mut self.u.hashset) };
        for old_index in hashset.iter() {
            new_bitvector.set(*old_index as usize - self.min as usize, true);
            self.size += 1;
        }

        self.u.bitvector = ManuallyDrop::new(new_bitvector);
    }
}

impl Clone for LikelyDenseIntegerSet {
    fn clone(&self) -> Self {
        if self.is_bitvector() {
            Self {
                u: LikelyDenseIntegerSetU {
                    bitvector: ManuallyDrop::new(self.bitvector().clone()),
                },
                size: self.size,
                max: self.max,
                min: self.min,
            }
        } else {
            Self {
                u: LikelyDenseIntegerSetU {
                    hashset: ManuallyDrop::new(self.hashset().clone()),
                },
                size: u32::MAX as _,
                max: 0,
                min: u32::MAX,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_dense_set() {
        let mut set = LikelyDenseIntegerSet::new();
        for i in 0..1000 {
            assert!(set.add(i));
        }
        for i in 0..1000 {
            assert!(!set.add(i));
        }
        for i in 0..1000 {
            assert!(set.contains(i));
        }
        for i in 1000..2000 {
            assert!(!set.contains(i));
        }
        assert_eq!(set.len(), 1000);
        assert_eq!(set.iter().count(), 1000);
        assert_eq!(set.iter().sum::<u32>(), 499500);
        assert_eq!(set.iter().max(), Some(999));
        assert_eq!(set.iter().min(), Some(0));
        assert_eq!(set.iter().nth(500), Some(500));
        assert_eq!(set.iter().nth(1000), None);
        assert_eq!(set.iter().nth(1001), None);
        assert_eq!(set.iter().nth(1002), None);
        assert_eq!(set.iter().nth(1003), None);
        assert_eq!(set.iter().nth(1004), None);
        assert_eq!(set.iter().nth(1005), None);
        assert_eq!(set.iter().nth(1006), None);
        assert_eq!(set.iter().nth(1007), None);
        assert_eq!(set.iter().nth(1008), None);
        assert_eq!(set.iter().nth(1009), None);
        assert_eq!(set.iter().nth(1010), None);
        assert_eq!(set.iter().nth(1011), None);
        assert_eq!(set.iter().nth(1012), None);
        assert_eq!(set.iter().nth(1013), None);
        assert_eq!(set.iter().nth(1014), None);
        assert_eq!(set.iter().nth(1015), None);
        assert_eq!(set.iter().nth(1016), None);
        assert_eq!(set.iter().nth(1017), None);
        assert_eq!(set.iter().nth(1018), None);
        assert_eq!(set.iter().nth(1019), None);
        assert_eq!(set.iter().nth(1020), None);
        assert_eq!(set.iter().nth(1021), None);
    }

    #[test]
    fn test_diff_set() {
        let mut set = LikelyDenseIntegerSet::new();

        set.add(64000);
        set.add(64002);
        set.add(64004);

        assert!(set.contains(64000));
        assert!(set.contains(64002));
        assert!(set.contains(64004));

        set.add(13);

        assert!(set.contains(64000));
        assert!(set.contains(64002));
        assert!(set.contains(64004));
        assert!(set.contains(13));
    }
}
