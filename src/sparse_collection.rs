use std::{marker::PhantomData, ptr::NonNull};

pub struct SparseCollection<T: SparseElement> {
    vector: Vec<Option<T>>,
    index_free_list: Vec<usize>,
}

impl<T: SparseElement> SparseCollection<T> {
    pub fn new() -> Self {
        Self {
            vector: Vec::new(),
            index_free_list: Vec::new(),
        }
    }

    pub fn add(&mut self, mut element: T) -> T::Id {
        let id = if let Some(id) = self.index_free_list.pop() {
            id
        } else {
            self.vector.push(None);
            self.vector.len() - 1
        };
        element.set_id(id.into());
        self.vector[id] = Some(element);

        id.into()
    }

    pub fn pack_indices(&mut self) {
        if self.index_free_list.is_empty() {
            return;
        }

        let mut hole_index = 0;
        let mut end_index = self.vector.len();

        loop {
            while hole_index < end_index && self.vector[hole_index].is_some() {
                hole_index += 1;
            }

            if hole_index == end_index {
                break;
            }

            end_index -= 1;

            while end_index > hole_index && self.vector[end_index].is_none() {
                end_index -= 1;
            }

            if end_index == hole_index {
                break;
            }

            self.vector.swap(hole_index, end_index);
            self.vector[hole_index]
                .as_mut()
                .unwrap()
                .set_id(hole_index.into());
            hole_index += 1;
        }

        self.vector.truncate(end_index);
        self.index_free_list.clear();
    }

    pub fn clear_all(&mut self) {
        self.vector.clear();
        self.index_free_list.clear();
    }

    pub fn size(&self) -> usize {
        self.vector.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vector.iter().all(|x| x.is_none())
    }

    pub fn at(&self, id: T::Id) -> Option<&T> {
        let id = id.into();
        if id < self.vector.len() {
            self.vector[id].as_ref()
        } else {
            None
        }
    }

    pub fn at_mut(&mut self, id: T::Id) -> Option<&mut T> {
        let id = id.into();
        if id < self.vector.len() {
            self.vector[id].as_mut()
        } else {
            None
        }
    }

    pub fn iter(&self) -> SparseCollectionIter<T> {
        SparseCollectionIter {
            collection: self,
            index: 0,
        }
    }

    pub fn iter_mut(&mut self) -> SparseCollectionIterMut<T> {
        SparseCollectionIterMut {
            ptr: NonNull::new(&mut self.vector[0]).unwrap(),
            end: unsafe { self.vector.as_mut_ptr().add(self.vector.len()) },
            _marker: PhantomData,
        }
    }

    pub fn remove(&mut self, id: T::Id) {
        self.index_free_list.push(id.into());
        self.vector[id.into()] = None;
    }
}
pub struct SparseCollectionIter<'a, T: SparseElement> {
    collection: &'a SparseCollection<T>,
    index: usize,
}

impl<'a, T: SparseElement> Iterator for SparseCollectionIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.collection.vector.len() {
            let index = self.index;
            self.index += 1;
            if let Some(element) = &self.collection.vector[index] {
                return Some(element);
            }
        }

        None
    }
}

pub struct SparseCollectionIterMut<'a, T: SparseElement> {
    ptr: NonNull<Option<T>>,
    end: *mut Option<T>,
    _marker: PhantomData<&'a mut T>,
}

impl<'a, T: SparseElement> Iterator for SparseCollectionIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.ptr.as_ptr() < self.end {
            let ptr = self.ptr.as_ptr();
            self.ptr = unsafe { NonNull::new_unchecked(ptr.add(1)) };

            if let Some(element) = unsafe { &mut *ptr } {
                return Some(element);
            }
        }

        None
    }
}

pub trait SparseElement {
    type Id: Copy + Eq + std::hash::Hash + Into<usize> + From<usize>;

    fn id(&self) -> Self::Id;
    fn set_id(&mut self, id: Self::Id);
}
