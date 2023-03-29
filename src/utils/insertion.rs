pub struct Insertion<T> {
    pub index: usize,
    pub element: T,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Insertion<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Insertion")
            .field("index", &self.index)
            .field("element", &self.element)
            .finish()
    }
}

impl<T: Default> Default for Insertion<T> {
    fn default() -> Self {
        Self {
            index: usize::MAX,
            element: T::default(),
        }
    }
}

impl<T> Insertion<T> {
    pub fn new(index: usize, value: T) -> Self {
        Self {
            index,
            element: value,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn element(&self) -> &T {
        &self.element
    }

    pub fn element_mut(&mut self) -> &mut T {
        &mut self.element
    }
}

impl<T: PartialEq> PartialEq for Insertion<T> {
    fn eq(&self, other: &Self) -> bool {
        self.element == other.element && self.index == other.index
    }
}

impl<T: Eq> Eq for Insertion<T> {}

impl<T: PartialEq> PartialOrd for Insertion<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<T: PartialEq + Eq> Ord for Insertion<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

pub fn execute_insertions<TargetType: Default>(
    target: &mut Vec<TargetType>,
    insertions: &mut [Insertion<TargetType>],
) -> usize {
    let num_insertions = insertions.len();

    if num_insertions == 0 {
        return 0;
    }

    let original_target_size = target.len();
    target.resize_with(original_target_size + num_insertions, Default::default);

    let mut last_index = target.len();

    for index_in_insertion in (0..num_insertions).rev() {
        let first_index = insertions[index_in_insertion].index() + index_in_insertion;
        let index_offset = index_in_insertion + 1;

        let mut i = last_index;

        while {
            i -= 1;
            i > first_index
        } {
            target[i] = std::mem::take(&mut target[i - index_offset]);
        }
        
        target[first_index] = std::mem::take(insertions[index_in_insertion].element_mut());
        last_index = first_index;
    }

    num_insertions
}
