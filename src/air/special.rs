use crate::{sparse_collection::SparseElement, stackmap_special::StackMapSpecial};

pub struct Special {
    pub index: usize,
    pub kind: SpecialKind,
}

pub enum SpecialKind {
    StackMap(StackMapSpecial),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct SpecialId(pub usize);

impl From<usize> for SpecialId {
    fn from(index: usize) -> Self {
        SpecialId(index)
    }
}

impl From<SpecialId> for usize {
    fn from(id: SpecialId) -> Self {
        id.0
    }
}

impl SparseElement for Special {
    type Id = SpecialId;

    fn id(&self) -> Self::Id {
        SpecialId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}
