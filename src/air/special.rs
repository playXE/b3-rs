use crate::{sparse_collection::SparseElement, stackmap_special::StackMapSpecial, bank::Bank, width::Width, patchpoint_special::PatchpointSpecial, check_special::CheckSpecial};

use super::{arg::{Arg, ArgRole}, inst::Inst, code::Code, ccall_special::CCallSpecial};

pub struct Special {
    pub index: usize,
    pub kind: SpecialKind,
}

pub enum SpecialKind {
    CCall(CCallSpecial),
    Patchpoint(PatchpointSpecial),
    Check(CheckSpecial),
}


impl Special {
    pub fn for_each_arg(&self, code: &Code<'_>, inst: &Inst, mut lambda: impl FnMut(&Arg, ArgRole, Bank, Width)) {
        match self {
            Special { kind: SpecialKind::CCall(special), .. } => special.for_each_arg(code, inst, |arg, role, bank, width| lambda(&arg, role, bank, width)),
            Special { kind: SpecialKind::Patchpoint(special), .. } => special.for_each_arg(code, inst, lambda),
            Special { kind: SpecialKind::Check(special), .. } => special.for_each_arg(code, inst, |arg, role, bank, width| lambda(&arg, role, bank, width)),
        }
    }

    pub fn for_each_arg_mut(&mut self, code: &mut Code<'_>, inst: &mut Inst, lambda: impl FnMut(&mut Arg, ArgRole, Bank, Width)) {
        match self {
            Special { kind: SpecialKind::CCall(special), .. } => special.for_each_arg_mut(code, inst, lambda),
            Special { kind: SpecialKind::Patchpoint(special), .. } => special.for_each_arg_mut(code, inst, lambda),
            Special { kind: SpecialKind::Check(special), .. } => special.for_each_arg_mut(code, inst, lambda),
        }
    }

    pub fn is_valid(&self, code: &Code<'_>, inst: &Inst) -> bool {
        match self {
            Special { kind: SpecialKind::CCall(special), .. } => special.is_valid(code, inst),
            Special { kind: SpecialKind::Patchpoint(special), .. } => special.is_valid(code, inst),
            Special { kind: SpecialKind::Check(special), .. } => special.is_valid(code, inst),
        }
    }

    pub fn admits_stack(&self, code: &Code<'_>, inst: &Inst, arg_index: usize) -> bool {
        match self {
            Special { kind: SpecialKind::CCall(special), .. } => special.admits_stack(code, inst, arg_index),
            Special { kind: SpecialKind::Patchpoint(special), .. } => special.admits_stack(code, inst, arg_index),
            Special { kind: SpecialKind::Check(special), .. } => special.admits_stack(code, inst, arg_index),
        }
    }
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
