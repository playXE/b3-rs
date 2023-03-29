use crate::{sparse_collection::SparseElement, utils::index_set::KeyIndex};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum StackSlotKind {
    /// A locked stack slot is an area of stack requested by the client. It cannot be killed. The
    /// client can get its FP offset and write to it from stack walking code, so we must assume
    /// that reads and writes to a locked stack slot can be clobbered the same way as reads and
    /// writes to any memory location.
    Locked,

    /// A spill slot. These have fundamentally different behavior than a typical memory location.
    /// They are lowered to from temporaries. This means for example that a 32-bit ZDef store to a
    /// 8 byte stack slot will zero the top 4 bytes, even though a 32-bit ZDef store to any other
    /// kind of memory location would do no such thing. UseAddr on a spill slot is not allowed, so
    /// they never escape.
    Spill, // FIXME: We should add a third mode, which means that the stack slot will be read asynchronously
           // as with Locked, but never written to asynchronously. Then, Air could optimize spilling and
           // filling by tracking whether the value had been stored to a read-only locked slot. If it had,
           // then we can refill from that slot.
           // https://bugs.webkit.org/show_bug.cgi?id=150587
}

impl Default for StackSlotKind {
    fn default() -> Self {
        StackSlotKind::Locked
    }
}


#[derive(Debug)]
pub struct StackSlot {
    pub byte_size: u32,
    pub kind: StackSlotKind,
    pub index: usize,
    pub offset_from_fp: isize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StackSlotId(pub usize);

impl KeyIndex for StackSlotId {
    fn index(&self) -> usize {
        self.0
    }
}

impl From<usize> for StackSlotId {
    fn from(index: usize) -> Self {
        StackSlotId(index)
    }
}

impl From<StackSlotId> for usize {
    fn from(id: StackSlotId) -> Self {
        id.0
    }
}

impl SparseElement for StackSlot {
    type Id = StackSlotId;

    fn id(&self) -> Self::Id {
        StackSlotId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}

impl StackSlot {
    pub fn is_spill(&self) -> bool {
        self.kind == StackSlotKind::Spill
    }

    pub fn is_locked(&self) -> bool {
        self.kind == StackSlotKind::Locked
    }

    pub fn kind(&self) -> StackSlotKind {
        self.kind
    }

    pub fn byte_size(&self) -> u32 {
        self.byte_size
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn alignment(&self) -> usize {
        if self.byte_size() <= 1 {
            1
        } else if self.byte_size() <= 2 {
            2
        } else if self.byte_size() <= 4 {
            4
        } else {
            8
        }
    }
}
