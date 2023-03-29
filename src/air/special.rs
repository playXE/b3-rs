use macroassembler::assembler::{abstract_macro_assembler::Jump, TargetMacroAssembler};

use crate::{sparse_collection::SparseElement, stackmap_special::StackMapSpecial, bank::Bank, width::Width, patchpoint_special::PatchpointSpecial, check_special::CheckSpecial, jit::register_set::RegisterSetBuilder};

use super::{arg::{Arg, ArgRole}, inst::Inst, code::Code, ccall_special::CCallSpecial, generation_context::GenerationContext};

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

    pub fn generate<'a>(&self, inst: &Inst, jit: &mut TargetMacroAssembler, gc: &'a mut GenerationContext<'a>) -> Jump {
        match self.kind {
            SpecialKind::CCall(ref _special) => CCallSpecial::generate(inst, jit, gc),
            SpecialKind::Patchpoint(ref _special) => PatchpointSpecial::generate(inst, jit, gc),
            SpecialKind::Check(ref _special) => todo!(),
        }
    }

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

    pub fn is_terminal(&self, code: &Code<'_>, inst: &Inst) -> bool {
        match self {
            Special { kind: SpecialKind::CCall(_special), .. } => false,
            Special { kind: SpecialKind::Patchpoint(special), .. } => special.is_terminal(code, inst),
            Special { kind: SpecialKind::Check(_special), .. } => false,
        }
    }

    pub fn has_non_arg_effects(&self, _code: &Code<'_>, _inst: &Inst) -> bool {
        true
    }

    pub fn has_non_arg_non_control_effects(&self, _code: &Code<'_>, _inst: &Inst) -> bool {
        true
    }

    /// This gets called on for each Inst that uses this Special. Note that there is no way to
    /// guarantee that a Special gets used from just one Inst, because Air might taildup late. So,
    /// if you want to pass this information down to generate(), then you have to either:
    ///
    /// 1) Generate Air that starts with a separate Special per Patch Inst, and then merge
    ///    usedRegister sets. This is probably not great, but it optimizes for the common case that
    ///    Air didn't duplicate code or that such duplication didn't cause any interesting changes to
    ///    register assignment.
    ///
    /// 2) Have the Special maintain a HashMap<Inst*, RegisterSetBuilder>. This works because the analysis
    ///    that feeds into this call is performed just before code generation and there is no way
    ///    for the Vec<>'s that contain the Insts to be reallocated. This allows generate() to
    ///    consult the HashMap.
    ///
    /// 3) Hybrid: you could use (1) and fire up a HashMap if you see multiple calls.
    ///
    /// Note that it's not possible to rely on reportUsedRegisters() being called in the same order
    /// as generate(). If we could rely on that, then we could just have each Special instance
    /// maintain a Vector of RegisterSetBuilder's and then process that vector in the right order in
    /// generate(). But, the ordering difference is unlikely to change since it would harm the
    /// performance of the liveness analysis.
    ///
    /// Currently, we do (1) for B3 stackmaps.
    pub fn report_used_registers(&self, code: &mut Code<'_>, inst: &Inst, used_registers: &RegisterSetBuilder) {
        match self.kind {
            SpecialKind::Check(_) => StackMapSpecial::report_used_registers(code, inst, used_registers),
            SpecialKind::Patchpoint(_) => StackMapSpecial::report_used_registers(code, inst, used_registers),
            _ => ()
        }
    }

    pub fn extra_early_clobbered_regs(&self, code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        match self.kind {
            SpecialKind::CCall(_) => RegisterSetBuilder::new(),
            SpecialKind::Check(_) | SpecialKind::Patchpoint(_) => StackMapSpecial::extra_early_clobbered_regs(code, inst),
        }
    }

    pub fn extra_clobbered_regs(&self, code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        match self.kind {
            SpecialKind::CCall(ref call) => call.clobbered_regs,
            SpecialKind::Check(_) | SpecialKind::Patchpoint(_) => StackMapSpecial::extra_clobbered_regs(code, inst),
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
