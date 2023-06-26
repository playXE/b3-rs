use crate::{
    bank::Bank,
    utils::index_set::{Entry, IndexMap, IndexSet},
};

use super::tmp::{AbsoluteIndexed, Tmp};

pub struct TmpSet {
    gp: IndexSet<AbsoluteIndexed<{ Bank::GP as i8 }>>,
    fp: IndexSet<AbsoluteIndexed<{ Bank::FP as i8 }>>,
}

impl TmpSet {
    pub fn new() -> Self {
        Self {
            gp: IndexSet::new(),
            fp: IndexSet::new(),
        }
    }

    pub fn add(&mut self, tmp: Tmp) -> bool {
        if tmp.is_gp() {
            self.gp.insert(AbsoluteIndexed(tmp))
        } else {
            self.fp.insert(AbsoluteIndexed(tmp))
        }
    }

    pub fn remove(&mut self, tmp: Tmp) -> bool {
        if tmp.is_gp() {
            self.gp.remove(&AbsoluteIndexed(tmp))
        } else {
            self.fp.remove(&AbsoluteIndexed(tmp))
        }
    }

    pub fn contains(&self, tmp: Tmp) -> bool {
        if tmp.is_gp() {
            self.gp.contains(&AbsoluteIndexed(tmp))
        } else {
            self.fp.contains(&AbsoluteIndexed(tmp))
        }
    }

    pub fn is_empty(&self) -> bool {
        self.gp.is_empty() && self.fp.is_empty()
    }

    pub fn len(&self) -> usize {
        self.gp.len() + self.fp.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = Tmp> + '_ {
        self.gp
            .indices()
            .map(|tmp| AbsoluteIndexed::<{ Bank::GP as i8 }>::tmp_for_absolute_index(tmp))
            .chain(
                self.fp
                    .indices()
                    .map(|tmp| AbsoluteIndexed::<{ Bank::FP as i8 }>::tmp_for_absolute_index(tmp)),
            )
    }
}

pub struct TmpMap<V> {
    gp: IndexMap<V, AbsoluteIndexed<{ Bank::GP as i8 }>>,
    fp: IndexMap<V, AbsoluteIndexed<{ Bank::FP as i8 }>>,
}

impl<V> TmpMap<V> {
    pub fn new() -> Self {
        Self {
            gp: IndexMap::new(),
            fp: IndexMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            gp: IndexMap::with_capacity(capacity),
            fp: IndexMap::with_capacity(capacity),
        }
    }

    pub fn insert(&mut self, tmp: Tmp, value: V) -> Option<V> {
        if tmp.is_gp() {
            self.gp.insert(AbsoluteIndexed(tmp), value)
        } else {
            self.fp.insert(AbsoluteIndexed(tmp), value)
        }
    }

    pub fn remove(&mut self, tmp: Tmp) -> Option<V> {
        if tmp.is_gp() {
            self.gp.remove(&AbsoluteIndexed(tmp))
        } else {
            self.fp.remove(&AbsoluteIndexed(tmp))
        }
    }

    pub fn get(&self, tmp: Tmp) -> Option<&V> {
        if tmp.is_gp() {
            self.gp.get(&AbsoluteIndexed(tmp))
        } else {
            self.fp.get(&AbsoluteIndexed(tmp))
        }
    }

    pub fn get_mut(&mut self, tmp: Tmp) -> Option<&mut V> {
        if tmp.is_gp() {
            self.gp.get_mut(&AbsoluteIndexed(tmp))
        } else {
            self.fp.get_mut(&AbsoluteIndexed(tmp))
        }
    }

    pub fn contains(&self, tmp: Tmp) -> bool {
        if tmp.is_gp() {
            self.gp.contains(&AbsoluteIndexed(tmp))
        } else {
            self.fp.contains(&AbsoluteIndexed(tmp))
        }
    }

    pub fn clear(&mut self) {
        self.gp.clear();
        self.fp.clear();
    }

    pub fn entry(&mut self, tmp: Tmp) -> TmpEntry<'_, V> {
        if tmp.is_gp() {
            TmpEntry::GP(self.gp.entry(AbsoluteIndexed(tmp)))
        } else {
            TmpEntry::FP(self.fp.entry(AbsoluteIndexed(tmp)))
        }
    }
}

pub enum TmpEntry<'a, V> {
    GP(Entry<'a, V, AbsoluteIndexed<{ Bank::GP as i8 }>>),
    FP(Entry<'a, V, AbsoluteIndexed<{ Bank::FP as i8 }>>),
}

impl<'a, V> TmpEntry<'a, V> {
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            TmpEntry::GP(entry) => entry.or_insert(default),
            TmpEntry::FP(entry) => entry.or_insert(default),
        }
    }
}

impl<V> std::ops::Index<Tmp> for TmpMap<V> {
    type Output = V;

    fn index(&self, tmp: Tmp) -> &Self::Output {
        self.get(tmp).unwrap()
    }
}

impl<V> std::ops::IndexMut<Tmp> for TmpMap<V> {
    fn index_mut(&mut self, tmp: Tmp) -> &mut Self::Output {
        self.get_mut(tmp).unwrap()
    }
}
