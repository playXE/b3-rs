use tinyvec::TinyVec;

use crate::{
    bank::Bank,
    jit::{reg::Reg, register_set::RegisterSetBuilder},
    sparse_collection::SparseElement,
    value::ValueId,
    width::Width,
};

use super::{
    arg::{Arg, ArgRole},
    code::Code,
    custom::{CCallCustom, ColdCCallCustom, EntrySwitchCustom, PatchCustom, ShuffleCustom},
    form_table::{decode_form_bank, decode_form_role, decode_form_width, is_x86},
    kind::Kind,
    opcode::Opcode,
    opcode_generated::G_FORM_TABLE,
    stack_slot::StackSlotId,
    tmp::Tmp,
};

#[derive(Clone, PartialEq, Eq)]
pub struct Inst {
    pub args: TinyVec<[Arg; 3]>,
    pub origin: ValueId,
    pub kind: Kind,
    pub index: usize,
}

impl std::fmt::Debug for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Default for Inst {
    fn default() -> Self {
        Self {
            args: TinyVec::new(),
            origin: ValueId::default(),
            kind: Kind::default(),
            index: usize::MAX,
        }
    }
}

impl Inst {
    pub fn extra_clobbered_regs(&self, code: &Code<'_>) -> RegisterSetBuilder {
        debug_assert!(self.kind.opcode == Opcode::Patch);
        code.special(self.args[0].special())
            .extra_clobbered_regs(code, self)
    }

    pub fn extra_early_clobbered_regs(&self, code: &Code<'_>) -> RegisterSetBuilder {
        debug_assert!(self.kind.opcode == Opcode::Patch);
        code.special(self.args[0].special())
            .extra_early_clobbered_regs(code, self)
    }

    pub fn has_late_use_or_def(&self, code: &Code<'_>) -> bool {
        if self.kind.opcode == Opcode::Patch && !self.extra_clobbered_regs(code).is_empty() {
            return true;
        }
        let mut result = false;

        self.for_each_arg(code, |_, _arg, role, _, _| {
            result |= role.is_late_use() || role.is_late_def();
        });

        result
    }

    pub fn has_early_def(&self, code: &Code<'_>) -> bool {
        if self.kind.opcode == Opcode::Patch && !self.extra_early_clobbered_regs(code).is_empty() {
            return true;
        }
        let mut result = false;

        self.for_each_arg(code, |_, _arg, role, _, _| {
            result |= role.is_early_def();
        });

        result
    }

    pub fn needs_padding(code: &Code<'_>, prev: &Self, next: &Self) -> bool {
        prev.has_late_use_or_def(code) && next.has_early_def(code)
    }

    /*pub fn for_each_arg_simple<F>(&self, mut f: F)
    where
        F: FnMut(&Arg, ArgRole, Bank, Width),
    {
        let num_operands = self.args.len();

        let form_offset = num_operands.wrapping_sub(1) * num_operands / 2;
        let form_base = &G_FORM_TABLE[self.kind.opcode as usize * 21 + form_offset..];

        for i in 0..num_operands {
            let form = form_base[i];
            f(
                &self.args[i],
                decode_form_role(form),
                decode_form_bank(form),
                decode_form_width(form),
            )
        }
    }*/

    pub fn for_each_arg_simple_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    {
        let num_operands = self.args.len();

        let form_offset = (num_operands.wrapping_sub(1)) * num_operands / 2;
        let form_base = &G_FORM_TABLE[self.kind.opcode as usize * 21 + form_offset..];

        for i in 0..num_operands {
            let form = form_base[i];
            f(
                i,
                &mut self.args[i],
                decode_form_role(form),
                decode_form_bank(form),
                decode_form_width(form),
            )
        }
    }

    pub fn new(kind: Kind, origin: ValueId, arguments: &[Arg]) -> Self {
        Inst {
            args: arguments.iter().copied().collect(),
            origin,
            kind,
            index: 0,
        }
    }

    pub fn for_each_arg<F>(&self, code: &Code<'_>, f: F)
    where
        F: FnMut(usize, &Arg, ArgRole, Bank, Width),
    {
        match self.kind.opcode {
            Opcode::EntrySwitch => EntrySwitchCustom::for_each_arg(code, self, f),

            Opcode::Shuffle => ShuffleCustom::for_each_arg(code, self, f),

            Opcode::Patch => PatchCustom::for_each_arg(code, self, f),

            Opcode::CCall => CCallCustom::for_each_arg(code, self, f),

            Opcode::ColdCCall => ColdCCallCustom::for_each_arg(code, self, f),

            _ => self.for_each_arg_simple(f),
        }
    }

    pub fn for_each_arg_mut(
        &mut self,
        code: &mut Code<'_>,
        f: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        match self.kind.opcode {
            Opcode::EntrySwitch => EntrySwitchCustom::for_each_arg_mut(code, self, f),

            Opcode::Shuffle => ShuffleCustom::for_each_arg_mut(code, self, f),

            Opcode::Patch => PatchCustom::for_each_arg_mut(code, self, f),

            Opcode::CCall => CCallCustom::for_each_arg_mut(code, self, f),

            Opcode::ColdCCall => ColdCCallCustom::for_each_arg_mut(code, self, f),

            _ => self.for_each_arg_simple_mut(f),
        }
    }

    pub fn for_each_tmp(&self, code: &Code<'_>, mut f: impl FnMut(Tmp, ArgRole, Bank, Width)) {
        self.for_each_arg(code, |_, arg, role, bank, width| {
            arg.for_each_tmp(role, bank, width, |tmp, role, bank, width| {
                f(tmp, role, bank, width)
            })
        })
    }

    pub fn for_each_tmp_mut(
        &mut self,
        code: &mut Code<'_>,
        mut f: impl FnMut(&mut Tmp, ArgRole, Bank, Width),
    ) {
        self.for_each_arg_mut(code, |_, arg, role, bank, width| {
            arg.for_each_tmp_mut(role, bank, width, |tmp, role, bank, width| {
                f(tmp, role, bank, width)
            })
        })
    }

    pub fn for_each_tmp_fast(&self, code: &Code<'_>, mut f: impl FnMut(Tmp)) {
        self.for_each_arg(code, |_, arg, _, _, _| arg.for_each_tmp_fast(|tmp| f(tmp)))
    }

    pub fn for_each_tmp_fast_mut(&mut self, code: &mut Code<'_>, mut f: impl FnMut(&mut Tmp)) {
        self.for_each_arg_mut(code, |_, arg, _, _, _| {
            arg.for_each_tmp_fast_mut(|tmp| f(tmp))
        })
    }

    pub fn for_each_reg(&self, code: &Code<'_>, mut f: impl FnMut(Reg, ArgRole, Bank, Width)) {
        self.for_each_arg(code, |_, arg, role, bank, width| {
            arg.for_each_reg(role, bank, width, |reg, role, bank, width| {
                f(reg, role, bank, width)
            })
        })
    }

    pub fn for_each_reg_mut(
        &mut self,
        code: &mut Code<'_>,
        mut f: impl FnMut(&mut Reg, ArgRole, Bank, Width),
    ) {
        self.for_each_arg_mut(code, |_, arg, role, bank, width| {
            arg.for_each_reg_mut(role, bank, width, |reg, role, bank, width| {
                f(reg, role, bank, width)
            })
        })
    }

    pub fn for_each_reg_fast(&self, code: &Code<'_>, mut f: impl FnMut(Reg)) {
        self.for_each_arg(code, |_, arg, _, _, _| arg.for_each_reg_fast(|reg| f(reg)))
    }

    pub fn for_each_reg_fast_mut(&mut self, code: &mut Code<'_>, mut f: impl FnMut(&mut Reg)) {
        self.for_each_arg_mut(code, |_, arg, _, _, _| {
            arg.for_each_reg_fast_mut(|reg| f(reg))
        })
    }

    pub fn for_each_stack_slot(
        &self,
        code: &Code<'_>,
        mut f: impl FnMut(StackSlotId, ArgRole, Bank, Width),
    ) {
        self.for_each_arg(code, |_, arg, role, bank, width| {
            arg.for_each_stack_slot(role, bank, width, |stack_slot, role, bank, width| {
                f(stack_slot, role, bank, width)
            })
        })
    }

    pub fn for_each_stack_slot_mut(
        &mut self,
        code: &mut Code<'_>,
        mut f: impl FnMut(&mut StackSlotId, ArgRole, Bank, Width),
    ) {
        self.for_each_arg_mut(code, |_, arg, role, bank, width| {
            arg.for_each_stack_slot_mut(role, bank, width, |stack_slot, role, bank, width| {
                f(stack_slot, role, bank, width)
            })
        })
    }

    pub fn for_each_stack_slot_fast(&self, code: &Code<'_>, mut f: impl FnMut(StackSlotId)) {
        self.for_each_arg(code, |_, arg, _, _, _| {
            arg.for_each_stack_slot_fast(|stack_slot| f(stack_slot))
        })
    }

    pub fn for_each_stack_slot_fast_mut(
        &mut self,
        code: &mut Code<'_>,
        mut f: impl FnMut(&mut StackSlotId),
    ) {
        self.for_each_arg_mut(code, |_, arg, _, _, _| {
            arg.for_each_stack_slot_fast_mut(|stack_slot| f(stack_slot))
        })
    }

    pub fn for_each_def_stack(
        prev_inst: Option<&Self>,
        next_inst: Option<&Self>,
        code: &Code,
        mut functor: impl FnMut(StackSlotId, ArgRole, Bank, Width),
    ) {
        if let Some(prev_inst) = prev_inst {
            prev_inst.for_each_stack_slot(code, |tmp, role, bank, width| {
                if role.is_late_def() {
                    functor(tmp, role, bank, width)
                }
            });
        }

        if let Some(next_inst) = next_inst {
            next_inst.for_each_stack_slot(code, |tmp, role, bank, width| {
                if role.is_early_def() {
                    functor(tmp, role, bank, width)
                }
            });
        }
    }

    pub fn for_each_def_arg(
        prev_inst: Option<&Self>,
        next_inst: Option<&Self>,
        code: &Code,
        mut functor: impl FnMut(&Arg, ArgRole, Bank, Width),
    ) {
        if let Some(prev_inst) = prev_inst {
            prev_inst.for_each_arg(code, |_, arg, role, bank, width| {
                if role.is_late_def() {
                    functor(arg, role, bank, width)
                }
            });
        }

        if let Some(next_inst) = next_inst {
            next_inst.for_each_arg(code, |_, arg, role, bank, width| {
                if role.is_early_def() {
                    functor(arg, role, bank, width)
                }
            });
        }
    }

    pub fn for_each_def(
        prev_inst: Option<&Self>,
        next_inst: Option<&Self>,
        code: &Code,
        mut functor: impl FnMut(Tmp, ArgRole, Bank, Width),
    ) {
        if let Some(prev_inst) = prev_inst {
            prev_inst.for_each_tmp(code, |tmp, role, bank, width| {
                if role.is_late_def() {
                    functor(tmp, role, bank, width)
                }
            });
        }

        if let Some(next_inst) = next_inst {
            next_inst.for_each_tmp(code, |tmp, role, bank, width| {
                if role.is_early_def() {
                    functor(tmp, role, bank, width)
                }
            });
        }
    }

    pub fn for_each_def_with_extra_clobbered_regs(
        prev_inst: Option<&Self>,
        next_inst: Option<&Self>,
        code: &Code<'_>,
        mut functor: impl FnMut(Tmp, ArgRole, Bank, Width, bool),
    ) {
        Self::for_each_def(prev_inst, next_inst, code, |tmp, role, bank, width| {
            functor(tmp, role, bank, width, false)
        });

        if let Some(prev_inst) = prev_inst.filter(|x| x.kind.opcode == Opcode::Patch) {
            let report_reg = |reg: Reg, width, preserved64: bool| {
                let bank = if reg.is_gpr() { Bank::GP } else { Bank::FP };

                functor(Tmp::from_reg(reg), ArgRole::Def, bank, width, preserved64)
            };

            prev_inst
                .extra_clobbered_regs(code)
                .to_register_set()
                .for_each_with_width_and_preserved(report_reg);
        }

        if let Some(next_inst) = next_inst.filter(|x| x.kind.opcode == Opcode::Patch) {
            let report_reg = |reg: Reg, width, preserved64: bool| {
                let bank = if reg.is_gpr() { Bank::GP } else { Bank::FP };

                functor(
                    Tmp::from_reg(reg),
                    ArgRole::EarlyDef,
                    bank,
                    width,
                    preserved64,
                )
            };

            next_inst
                .extra_early_clobbered_regs(code)
                .to_register_set()
                .for_each_with_width_and_preserved(report_reg);
        }
    }

    pub fn should_try_aliasing_def(&self, _code: &Code) -> Option<usize> {
        if !is_x86() {
            return None;
        }

        match self.kind.opcode {
            Opcode::Add32
            | Opcode::Add64
            | Opcode::And32
            | Opcode::And64
            | Opcode::Mul32
            | Opcode::Mul64
            | Opcode::Or32
            | Opcode::Or64
            | Opcode::Xor32
            | Opcode::Xor64
            | Opcode::AndFloat
            | Opcode::AndDouble
            | Opcode::OrFloat
            | Opcode::OrDouble
            | Opcode::XorFloat
            | Opcode::XorDouble => {
                if self.args.len() == 3 {
                    return Some(2);
                }

                return None;
            }

            Opcode::BranchAdd32 | Opcode::BranchAdd64 => {
                if self.args.len() == 4 {
                    return Some(3);
                }
            }

            Opcode::MoveConditionally32
            | Opcode::MoveConditionally64
            | Opcode::MoveConditionallyTest32
            | Opcode::MoveConditionallyTest64
            | Opcode::MoveConditionallyFloat
            | Opcode::MoveConditionallyDouble
            | Opcode::MoveDoubleConditionally32
            | Opcode::MoveDoubleConditionally64
            | Opcode::MoveDoubleConditionallyTest32
            | Opcode::MoveDoubleConditionallyTest64
            | Opcode::MoveDoubleConditionallyFloat => {
                if self.args.len() == 6 {
                    return Some(5);
                }
            }

            Opcode::Patch => {
                return None;
            }

            _ => (),
        }

        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct InstId(pub usize);

impl From<usize> for InstId {
    fn from(index: usize) -> Self {
        InstId(index)
    }
}

impl From<InstId> for usize {
    fn from(id: InstId) -> Self {
        id.0
    }
}

impl SparseElement for Inst {
    type Id = InstId;

    fn id(&self) -> Self::Id {
        InstId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.kind)?;
        for (i, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;

            if i != self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}
