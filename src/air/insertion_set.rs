use tinyvec::TinyVec;

use crate::utils::insertion::execute_insertions;

use super::{basic_block::BasicBlockId, code::Code, inst::Inst};

pub type Insertion = crate::utils::insertion::Insertion<Inst>;

pub struct InsertionSet {
    insertions: TinyVec<[Insertion; 8]>,
}

impl InsertionSet {
    pub fn new() -> Self {
        Self {
            insertions: TinyVec::new(),
        }
    }

    pub fn append_insertion(&mut self, insertion: Insertion) {
        self.insertions.push(insertion);
    }

    pub fn insert_inst(&mut self, index: usize, inst: Inst) {
        self.append_insertion(Insertion::new(index, inst))
    }

    pub fn insert_insts(&mut self, index: usize, insts: impl IntoIterator<Item = Inst>) {
        for inst in insts.into_iter() {
            self.insert_inst(index, inst)
        }
    }

    pub fn execute(&mut self, code: &mut Code<'_>, block: BasicBlockId) {
        self.insertions.sort();
        
        execute_insertions(&mut code.block_mut(block).insts, &mut self.insertions);
        code.block_mut(block).retain(|x| x.index != usize::MAX);
        self.insertions.clear();
    }
}

pub mod phased {

    use crate::utils::insertion::Insertion;

    pub struct PhasedInsertion<T> {
        pub phase: usize,
        pub insertion: Insertion<T>,
    }

    impl<T> PhasedInsertion<T> {
        pub fn new(phase: usize, insertion: Insertion<T>) -> Self {
            Self { phase, insertion }
        }

        pub fn phase(&self) -> usize {
            self.phase
        }
    }

    impl<T> std::ops::Deref for PhasedInsertion<T> {
        type Target = Insertion<T>;

        fn deref(&self) -> &Self::Target {
            &self.insertion
        }
    }

    impl<T> std::ops::DerefMut for PhasedInsertion<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.insertion
        }
    }

    impl<T> PartialOrd for PhasedInsertion<T> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            if self.index() != other.index() {
                return self.index().partial_cmp(&other.index());
            }
            self.phase.partial_cmp(&other.phase)
        }
    }

    impl<T> PartialEq for PhasedInsertion<T> {
        fn eq(&self, other: &Self) -> bool {
            self.index() == other.index() && self.phase == other.phase
        }
    }

    impl<T> Eq for PhasedInsertion<T> {}

    impl<T> Ord for PhasedInsertion<T> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            if self.index() != other.index() {
                return self.index().cmp(&other.index());
            }
            self.phase.cmp(&other.phase)
        }
    }

    pub struct PhasedInsertionSet<T> {
        insertions: Vec<PhasedInsertion<T>>,
    }

    impl<T: Default> PhasedInsertionSet<T> {
        pub fn new() -> Self {
            Self {
                insertions: Vec::with_capacity(8),
            }
        }

        pub fn append_insertion(&mut self, insertion: PhasedInsertion<T>) {
            self.insertions.push(insertion);
        }

        pub fn insert_inst(&mut self, index: usize, phase: usize, inst: T) {
            self.append_insertion(PhasedInsertion::new(phase, Insertion::new(index, inst)))
        }

        pub fn execute(&mut self, block: &mut Vec<T>) {
            self.insertions.sort();
            execute_insertions(block, &mut self.insertions);
            self.insertions.clear();
        }
    }

    fn execute_insertions<TargetType: Default>(
        target: &mut Vec<TargetType>,
        insertions: &mut [PhasedInsertion<TargetType>],
    ) -> usize {
        let num_insertions = insertions.len();

        if num_insertions == 0 {
            return 0;
        }

        let original_target_size = target.len();

        target.resize_with(original_target_size + num_insertions, Default::default);

        let mut last_index = target.len();

        for index_in_insertion in (0..num_insertions).rev() {
            assert!(
                index_in_insertion == 0
                    || insertions[index_in_insertion].index()
                        >= insertions[index_in_insertion - 1].index()
            );

            assert!(insertions[index_in_insertion].index() <= original_target_size);
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
}
