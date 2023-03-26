use macroassembler::assembler::{TargetAssembler, x86assembler::{ebx, r15, r14, r13, ebp}, TargetMacroAssembler};

use crate::{width::Width, bitmap};

use super::reg::Reg;
/*
pub type RegisterBitmap =
    BitMap<{ TargetAssembler::number_of_registers() + TargetAssembler::number_of_fp_registers() }>;
*/
bitmap!(RegisterBitmap, TargetAssembler::number_of_registers() + TargetAssembler::number_of_fp_registers());

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct RegisterSetBuilder {
    bits: RegisterBitmap,
    upper_bits: RegisterBitmap,
}

impl RegisterSetBuilder {
    pub fn from_regs(set: &RegisterSet) -> Self {
        Self {
            bits: set.bits,
            upper_bits: set.upper_bits
        }
    }

    pub fn new() -> Self {
        Self {
            bits: RegisterBitmap::new(),
            upper_bits: RegisterBitmap::new(),
        }
    }

    pub fn include_whole_register_width(&mut self) -> &mut Self {
        self.upper_bits.merge(&self.bits);
        self
    }

    pub fn add(&mut self, reg: Reg, width: Width) -> &mut Self {
        self.bits.set(reg.index());

        if width > reg.conservative_width_without_vectors() {
            self.upper_bits.set(reg.index());
        }

        self
    }

    pub fn add_many(&mut self, regs: &[Reg], width: Width) -> &mut Self {
        for reg in regs {
            self.add(*reg, width);
        }

        self
    }

    pub fn remove(&mut self, reg: Reg) -> &mut Self {
        self.bits.clear(reg.index());
        self.upper_bits.clear(reg.index());
        self
    }

    pub fn has_any_wide_registers(&self) -> bool {
        self.upper_bits.count() > 0
    }

    pub fn is_empty(&self) -> bool {
        self.bits.count() == 0 && self.upper_bits.count() == 0
    }

    pub fn merge(&mut self, other: &Self) -> &mut Self {
        self.bits.merge(&other.bits);
        self.upper_bits.merge(&other.upper_bits);
        self
    }

    pub fn exclude(&mut self, other: &Self) -> &mut Self {
        self.bits.exclude(&other.bits);
        self.upper_bits.exclude(&other.upper_bits);
        self
    }

    pub fn exclude_regs(&mut self, other: &RegisterSet) -> &mut Self {
        self.bits.exclude(&other.bits);
        self.upper_bits.exclude(&other.upper_bits);
        self
    }

    pub fn filter(&mut self, other: &Self) -> &mut Self {
        self.bits.filter(&other.bits);
        self.upper_bits.filter(&other.upper_bits);
        self
    }
    

    pub fn filter_regs(&mut self, other: &RegisterSet) -> &mut Self {
        self.bits.filter(&other.bits);
        self.upper_bits.filter(&other.upper_bits);
        self
    }

    pub fn to_register_set(&self) -> RegisterSet {
        RegisterSet {
            bits: self.bits,
            upper_bits: self.upper_bits,
        }
    }

    pub fn all_gprs() -> RegisterSet {
        let mut result = RegisterSet::default();

        for reg in TargetAssembler::first_register()..=TargetAssembler::last_register() {
            result.add(Reg::new_gpr(reg), Width::W64);
        }

        result
    }

    pub fn all_fprs() -> RegisterSet {
        let mut result = RegisterSet::default();

        for reg in TargetAssembler::first_fp_register()..=TargetAssembler::last_fp_register() {
            result.add(Reg::new_fpr(reg), Width::W64);
        }

        result
    }

    pub fn all_registers() -> RegisterSet {
        let mut result = RegisterSet::default();

        result.merge(&Self::all_gprs());
        result.merge(&Self::all_fprs());

        result
    }

    pub fn all_scalar_registers() -> RegisterSet {
        let mut result = RegisterSet::default();

        result.merge(&Self::all_gprs());
        result.merge(&Self::all_fprs());
        result.upper_bits.clear_all();

        result
    }

    pub fn callee_saved_registers() -> RegisterSet {
        let mut result = RegisterSet::default();
        use macroassembler::assembler::x86assembler::r12;
        result.add(Reg::new_gpr(ebx), Width::W64);
        result.add(Reg::new_gpr(ebp), Width::W64);
        result.add(Reg::new_gpr(r12), Width::W64);
        result.add(Reg::new_gpr(r13), Width::W64);
        result.add(Reg::new_gpr(r14), Width::W64);
        result.add(Reg::new_gpr(r15), Width::W64);

        result
    }

    pub fn build_and_validate(&self) -> RegisterSet {
        RegisterSet { bits: self.bits, upper_bits: self.upper_bits }
    }

    pub fn stack_registers() -> RegisterSet {
        let mut result = RegisterSet::default();

        result.add(Reg::new_gpr(TargetMacroAssembler::STACK_POINTER_REGISTER), Width::W64);
        result.add(Reg::new_gpr(TargetMacroAssembler::FRAME_POINTER_REGISTER), Width::W64);

        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct RegisterSet {
    bits: RegisterBitmap,
    upper_bits: RegisterBitmap,
}

impl std::fmt::Display for RegisterSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{{\n bits: {},\n upper_bits: {}}}", self.bits, self.upper_bits)
    }
}

impl Default for RegisterSet {
    fn default() -> Self {
        Self {
            bits: RegisterBitmap::new(),
            upper_bits: RegisterBitmap::new(),
        }
    }
}

impl RegisterSet {
    pub fn new(builder: &RegisterSetBuilder) -> Self {
        Self {
            bits: builder.bits,
            upper_bits: builder.upper_bits,
        }
    }

    pub fn subsumes(&self, other: &Self) -> bool {
        self.bits.subsumes(&other.bits) && self.upper_bits.subsumes(&other.upper_bits)
    }

    pub fn for_each(&self, mut f: impl FnMut(Reg)) {
        self.bits.for_each_set_bit(|index| {
            f(Reg::from_index(index as _));

            false
        })
    }

    pub fn include_whole_register_width(&mut self) -> &mut Self {
        self.upper_bits.merge(&self.bits);
        self
    }

    pub fn remove(&mut self, reg: Reg) {
        self.bits.clear(reg.index());
        self.upper_bits.clear(reg.index());
    }

    pub fn contains(&self, reg: Reg, width: Width) -> bool {
        if width < reg.conservative_width_without_vectors() {
            self.bits.get(reg.index())
        } else {
            self.upper_bits.get(reg.index()) && self.bits.get(reg.index())
        }
    }

    pub fn add(&mut self, reg: Reg, width: Width) {
        self.bits.set(reg.index());

        if width >= reg.conservative_width_without_vectors() {
            self.upper_bits.set(reg.index());
        }
    }

    pub fn merge(&mut self, other: &Self) {
        self.bits.merge(&other.bits);
        self.upper_bits.merge(&other.upper_bits);
    }

    pub fn exclude(&mut self, other: &Self) {
        self.bits.exclude(&other.bits);
        self.upper_bits.exclude(&other.upper_bits);
    }

    pub fn filter(&mut self, other: &Self) {
        self.bits.filter(&other.bits);
        self.upper_bits.filter(&other.upper_bits);
    }

    pub fn build_scalar_register_set(&self) -> ScalarRegisterSet {
        ScalarRegisterSet {
            bits: self.bits,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.bits.count() == 0 && self.upper_bits.count() == 0
    }
}

#[derive(Default)]
pub struct ScalarRegisterSet {
    bits: RegisterBitmap,
}

impl ScalarRegisterSet {
    pub fn empty() -> Self {
        Self {
            bits: RegisterBitmap::new(),
        }
    }

    pub fn new(set: &RegisterSet) -> Self {
        Self { bits: set.bits }
    }

    pub fn add(&mut self, reg: Reg) {
        self.bits.set(reg.index());
    }

    pub fn remove(&mut self, reg: Reg) {
        self.bits.clear(reg.index());
    }

    pub fn contains(&self, reg: Reg) -> bool {
        self.bits.get(reg.index())
    }

    pub fn number_of_set_gprs(&self) -> usize {
        let mut temp = self.bits;
        temp.filter(&RegisterSetBuilder::all_gprs().bits);
        temp.count()
    }

    pub fn number_of_set_fprs(&self) -> usize {
        let mut temp = self.bits;
        temp.filter(&RegisterSetBuilder::all_fprs().bits);
        temp.count()
    }

    pub fn to_register_set(&self) -> RegisterSet {
        let mut result = RegisterSet::default();

        self.bits.for_each_set_bit(|index| {
            result.add(
                Reg::from_index(index as _),
                Reg::from_index(index as _).conservative_width_without_vectors(),
            );
            false
        });

        result
    }
}
