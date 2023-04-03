use std::{marker::PhantomData, fmt::Debug};

use super::bitvector::BitVector;

pub trait KeyIndex: Copy {
    fn index(&self) -> usize;
}

pub struct IndexSet<T: KeyIndex> {
    set: BitVector,
    marker: PhantomData<T>
}

impl<T: KeyIndex> Default for IndexSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: KeyIndex> IndexSet<T> {
    pub fn new() -> Self {
        Self {
            set: BitVector::new(),
            marker: PhantomData,
        }
    }

    pub fn insert(&mut self, value: T) -> bool {
        let index = value.index();
        
        
        if self.set.get(index) {
            return false;
        }

        self.set.set(index, true);
        true
    }

    pub fn remove(&mut self, value: &T) -> bool {
        let index = value.index();
    
        if index < self.set.len() {
            if !self.set.get(index) {
                return false;
            }
            self.set.set(index, false);
            true
        } else {
            false
        }
    }

    pub fn contains(&self, value: &T) -> bool {
        let index = value.index();
        index < self.set.len() && self.set.get(index)
    }

    pub fn iter<'a>(&'a self, collection: &'a [T]) -> impl Iterator<Item = &T> + 'a {
        self.set.iter().map(move |value| {
            &collection[value]
        })
    }


    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.set.iter()
    }

    pub fn values<'a>(&'a self, collection: &'a [T]) -> impl Iterator<Item = &T> + 'a {
        self.indices().map(move |index| &collection[index])
    }
}

pub struct IndexMap<V, T: KeyIndex> {
    map: Vec<Option<V>>,
    marker: PhantomData<T>
}

impl<V, T: KeyIndex> IndexMap<V, T> {
    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn from_iter(iter: impl IntoIterator<Item = (T, V)>) -> Self {
        let mut map = Self::new();
        for (key, value) in iter {
            map.insert(key, value);
        }
        map
    }
    pub fn new() -> Self {
        Self {
            map: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: {
                let mut v = Vec::with_capacity(capacity);
                v.resize_with(capacity, || None);
                v
            },
            marker: PhantomData,
        }
    }

    pub fn insert(&mut self, value: T, item: V) -> Option<V> {
        let index = value.index();
        if index >= self.map.len() {
            self.map.resize_with(index + 1, || None);
        }
        
        std::mem::replace(&mut self.map[index], Some(item))
    }

    pub fn remove(&mut self, value: &T) -> Option<V> {
        let index = value.index();
    
        if index < self.map.len() {
            self.map[index].take()
        } else {
            None
        }
    }

    pub fn get(&self, value: &T) -> Option<&V> {
        let index = value.index();
        if index < self.map.len() {
            self.map[index].as_ref()
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, value: &T) -> Option<&mut V> {
        let index = value.index();
        if index < self.map.len() {
            self.map[index].as_mut()
        } else {
            None
        }
    }

    pub fn contains(&self, value: &T) -> bool {
        let index = value.index();
        index < self.map.len() && self.map[index].is_some()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &V)> + 'a {
        self.map.iter().enumerate().filter_map(move |(index, value)| {
            if let Some(value) = value {
                Some((index, value))
            } else {
                None
            }
        })
    }

    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = (usize, &'a mut V)> + 'a {
        self.map.iter_mut().enumerate().filter_map(|(item, value)| {
            if let Some(value) = value {
                Some((item, value))
            } else {
                None
            }
        })
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty() || self.map.iter().all(|x| x.is_none())
    }

    pub fn len(&self) -> usize {
        self.map.iter().filter(|x| x.is_some()).count()
    }

    pub fn at(&self, index: usize) -> Option<&V> {
        self.map.get(index).and_then(|x| x.as_ref())
    }

    pub fn at_mut(&mut self, index: usize) -> Option<&mut V> {
        self.map.get_mut(index).and_then(|x| x.as_mut())
    }

    pub fn entry(&mut self, key: T) -> Entry<'_, V, T> {
        let index = key.index();
        if index >= self.map.len() {
            self.map.resize_with(index + 1, || None);
        }

        if self.map[index].is_some() {
            Entry::Occupied(OccupiedEntry {
                key,
                value: &mut self.map[index],
            })
        } else {
            Entry::Vacant(VacantEntry {
                key,
                map: self,
            })
        }
    }
}

impl<V, T: KeyIndex> std::ops::Index<T> for IndexMap<V, T> {
    type Output = V;

    fn index(&self, index: T) -> &Self::Output {
        self.get(&index).unwrap()
    }
}

impl<V, T: KeyIndex> std::ops::IndexMut<T> for IndexMap<V, T> {
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        self.get_mut(&index).unwrap()
    }
}

impl KeyIndex for usize {
    fn index(&self) -> usize {
        *self
    }
}

pub enum Entry<'a, V, T: KeyIndex> {
    Occupied(OccupiedEntry<'a, V, T>),
    Vacant(VacantEntry<'a, V, T>),
}

impl<'a, V, T: KeyIndex> Entry<'a, V, T> {
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default),
        }
    }

    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default()),
        }
    }
}

pub struct OccupiedEntry<'a, V, T: KeyIndex> {
    key: T,
    value: &'a mut Option<V>,
}

impl<'a, V, T: KeyIndex> OccupiedEntry<'a, V, T> {
    pub fn into_mut(self) -> &'a mut V {
        self.value.as_mut().unwrap()
    }

    pub fn remove(self) -> V {
        self.value.take().unwrap()
    }

    pub fn insert(&mut self, value: V) -> V {
        self.value.replace(value).unwrap()
    }

    pub fn key(&self) -> &T {
        &self.key
    }

    pub fn get(&self) -> &V {
        self.value.as_ref().unwrap()
    }

    pub fn get_mut(&mut self) -> &mut V {
        self.value.as_mut().unwrap()
    }
}

pub struct VacantEntry<'a, V, T: KeyIndex> {
    map: &'a mut IndexMap<V, T>,
    key: T,
}

impl<'a, V, T: KeyIndex + Copy> VacantEntry<'a, V, T> {
    pub fn insert(self, value: V) -> &'a mut V {
        self.map.insert(self.key, value);
        self.map.get_mut(&self.key).unwrap()
    }
}


impl<V, T: KeyIndex> Default for IndexMap<V, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V: Clone, T: KeyIndex> Clone for IndexMap<V, T> {
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
            marker: self.marker
        }
    }
}

impl<V: Debug, T: KeyIndex> Debug for IndexMap<V, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}