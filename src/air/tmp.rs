use std::ops::{Deref, DerefMut};

use macroassembler::assembler::TargetAssembler;

use crate::{bank::Bank, jit::reg::Reg, utils::index_set::KeyIndex};

use super::code::Code;

/// A Tmp is a generalization of a register. It can be used to refer to any GPR or FPR. It can also
/// be used to refer to an unallocated register (i.e. a temporary). Like many Air classes, we use
/// deliberately terse naming since we will have to use this name a lot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tmp {
    value: i32,
}



impl Tmp {
    pub fn bank(self) -> Bank {
        if self.is_gp() {
            Bank::GP
        } else {
            Bank::FP
        }
    }

    const fn encode_gp(index: usize) -> i32 {
        1 + index as i32
    }

    const fn encode_fp(index: usize) -> i32 {
        -1 - (index as i32)
    }

    const fn encode_gpr(r: u8) -> i32 {
        Self::encode_gp(r as usize - TargetAssembler::first_register() as usize)
    }

    const fn encode_fpr(r: u8) -> i32 {
        Self::encode_fp(r as usize - TargetAssembler::first_fp_register() as usize)
    }

    const fn encode_gp_tmp(index: usize) -> i32 {
        Self::encode_gp(TargetAssembler::last_register() as _) + 1 + index as i32
    }

    const fn encode_fp_tmp(index: usize) -> i32 {
        Self::encode_fp(TargetAssembler::last_fp_register() as _) - 1 - index as i32
    }

    const fn is_encoded_gp(value: i32) -> bool {
        value > 0
    }

    const fn is_encoded_fp(value: i32) -> bool {
        value < 0
    }

    const fn is_encoded_gpr(value: i32) -> bool {
        Self::is_encoded_gp(value)
            && value <= Self::encode_gp(TargetAssembler::last_register() as _)
    }

    const fn is_encoded_fpr(value: i32) -> bool {
        Self::is_encoded_fp(value)
            && value >= Self::encode_fp(TargetAssembler::last_fp_register() as _)
    }

    const fn is_encoded_gp_tmp(value: i32) -> bool {
        Self::is_encoded_gp(value) && !Self::is_encoded_gpr(value)
    }

    const fn is_encoded_fp_tmp(value: i32) -> bool {
        Self::is_encoded_fp(value) && !Self::is_encoded_fpr(value)
    }

    const fn decode_gpr(value: i32) -> u8 {
        assert!(Self::is_encoded_gpr(value));
        ((value as i32 - Self::encode_gpr(TargetAssembler::first_register() as _))
            + TargetAssembler::first_register() as i32) as _
    }

    const fn decode_fpr(value: i32) -> u8 {
        assert!(Self::is_encoded_fpr(value));
        /*((value as i32 - Self::encode_fpr(TargetAssembler::first_fp_register() as _))
            + TargetAssembler::first_fp_register() as i32) as _*/

        ((Self::encode_fpr(TargetAssembler::first_fp_register() as _) - value)
            + TargetAssembler::first_fp_register() as i32) as _
    }

    const fn decode_gp_tmp(value: i32) -> usize {
        assert!(Self::is_encoded_gp_tmp(value));
        (value as i32 - (Self::encode_gpr(TargetAssembler::last_register()) + 1)) as usize
    }

    const fn decode_fp_tmp(value: i32) -> usize {
        assert!(Self::is_encoded_fp_tmp(value));
        ((Self::encode_fpr(TargetAssembler::last_fp_register()) - 1) - value) as usize
    }

    pub const fn empty() -> Self {
        Self { value: 0 }
    }

    pub const fn from_reg(reg: Reg) -> Self {
        if reg.is_gpr() {
            Self {
                value: Self::encode_gpr(reg.gpr()),
            }
        } else {
            Self {
                value: Self::encode_fpr(reg.fpr()),
            }
        }
    }

    pub const fn gp_tmp_for_index(index: usize) -> Self {
        Self {
            value: Self::encode_gp_tmp(index),
        }
    }

    pub const fn fp_tmp_for_index(index: usize) -> Self {
        Self {
            value: Self::encode_fp_tmp(index),
        }
    }

    pub const fn tmp_for_index(bank: Bank, index: usize) -> Self {
        match bank {
            Bank::GP => Self::gp_tmp_for_index(index),
            Bank::FP => Self::fp_tmp_for_index(index),
        }
    }

    pub fn is_set(&self) -> bool {
        self.value != 0
    }

    pub const fn is_gp(&self) -> bool {
        Self::is_encoded_gp(self.value)
    }

    pub const fn is_fp(&self) -> bool {
        Self::is_encoded_fp(self.value)
    }

    pub const fn is_gpr(&self) -> bool {
        Self::is_encoded_gpr(self.value)
    }

    pub const fn is_fpr(&self) -> bool {
        Self::is_encoded_fpr(self.value)
    }

    pub const fn gpr(&self) -> u8 {
        Self::decode_gpr(self.value)
    }

    pub const fn fpr(&self) -> u8 {
        Self::decode_fpr(self.value)
    }

    pub fn reg(&self) -> Reg {
        if self.is_gpr() {
            Reg::new_gpr(self.gpr())
        } else {
            Reg::new_fpr(self.fpr())
        }
    }

    pub fn is_reg(&self) -> bool {
        self.is_gpr() || self.is_fpr()
    }

    pub fn has_tmp_index(&self) -> bool {
        !self.is_reg()
    }

    pub const fn gp_tmp_index(&self) -> usize {
        Self::decode_gp_tmp(self.value)
    }

    pub const fn fp_tmp_index(&self) -> usize {
        Self::decode_fp_tmp(self.value)
    }

    pub const fn tmp_index_bank(&self, bank: Bank) -> usize {
        match bank {
            Bank::GP => self.gp_tmp_index(),
            Bank::FP => self.fp_tmp_index(),
        }
    }

    pub const fn tmp_index(&self) -> usize {
        if self.is_gp() {
            self.gp_tmp_index()
        } else {
            self.fp_tmp_index()
        }
    }

    pub const fn internal_value(&self) -> i32 {
        self.value
    }

    pub fn linearly_indexed<'a>(&'a self, code: &'a Code<'a>) -> LinearlyIndexed<'a> {
        LinearlyIndexed::new(code, *self)
    }

    pub fn tmp_for_linear_index(code: &Code<'_>, index: usize) -> Self {
        let gp_end = AbsoluteIndexed::<{Bank::GP}>::absolute_index(&Tmp::gp_tmp_for_index(code.num_gp_tmps));

        if index < gp_end {
            AbsoluteIndexed::<{Bank::GP}>::tmp_for_absolute_index(index)
        } else {
            AbsoluteIndexed::<{Bank::FP}>::tmp_for_absolute_index(index - gp_end)
        }
    }
}

impl std::fmt::Display for Tmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_reg() {
            write!(f, "{}", self.reg())
        } else if self.is_gp() {
            write!(f,"%tmp{}", self.gp_tmp_index())
        } else {
            write!(f,"%ftmp{}", self.fp_tmp_index())
        }
    }
}


pub struct AbsoluteIndexed<const BANK: Bank>(pub Tmp);
/*pub trait AbsoluteIndexed<const BANK: Bank> {
    fn absolute_index(tmp: &Tmp) -> usize;
    fn absolute_index_from_usize(index: usize) -> usize;
    fn last_machine_register_index() -> usize;
    fn tmp_for_absolute_index(index: usize) -> Tmp;
}*/

impl<const BANK: Bank> Clone for AbsoluteIndexed<{BANK}> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<const BANK: Bank> Copy for AbsoluteIndexed<{BANK}> {}


impl<const BANK: Bank> KeyIndex for AbsoluteIndexed<BANK> {
    fn index(&self) -> usize {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{Bank::GP}>::absolute_index(&self.0)
        } else {
            AbsoluteIndexed::<{Bank::FP}>::absolute_index(&self.0)
        }
    }
}

impl AbsoluteIndexed<{Bank::GP}> {
    pub fn absolute_index(tmp: &Tmp) -> usize {
        assert!(tmp.is_gp());
        assert!(tmp.internal_value() > 0);
        tmp.internal_value() as _ 
    }

    pub fn absolute_index_from_usize(index: usize) -> usize {
        Self::absolute_index(&Tmp::gp_tmp_for_index(index))
    }

    pub fn last_machine_register_index() -> usize {
        Self::absolute_index(&Tmp::from_reg(Reg::new_gpr(TargetAssembler::last_register())))
    }

    pub fn tmp_for_absolute_index(index: usize) -> Tmp {
        Tmp {
            value: index as i32,
        }
    }
}

impl AbsoluteIndexed<{Bank::FP}> {
    pub fn absolute_index(tmp: &Tmp) -> usize {
        assert!(tmp.is_fp());
        assert!(tmp.internal_value() < 0);
        (-tmp.internal_value()) as _
    }

    pub fn absolute_index_from_usize(index: usize) -> usize {
        Self::absolute_index(&Tmp::fp_tmp_for_index(index))
    }

    pub fn last_machine_register_index() -> usize {
        Self::absolute_index(&Tmp::from_reg(Reg::new_fpr(TargetAssembler::last_fp_register())))
    }

    pub fn tmp_for_absolute_index(index: usize) -> Tmp {
        Tmp {
            value: -(index as i32),
        }
    }
}

pub struct LinearlyIndexed<'a> {
    pub code: &'a Code<'a>,
    pub tmp: Tmp 
}

impl<'a> Deref for LinearlyIndexed<'a> {
    type Target = Tmp;

    fn deref(&self) -> &Self::Target {
        &self.tmp
    }
}

impl<'a> DerefMut for LinearlyIndexed<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tmp
    }
}

impl<'a> LinearlyIndexed<'a> {
    pub fn new(code: &'a Code<'a>, tmp: Tmp) -> Self {
        Self {
            code,
            tmp,
        }
    }

    pub fn index(&self) -> usize {
        if self.is_gp() {
            AbsoluteIndexed::<{Bank::GP}>::absolute_index(&self.tmp)
        } else {
            AbsoluteIndexed::<{Bank::GP}>::absolute_index(&Tmp::gp_tmp_for_index(self.code.num_gp_tmps)) + AbsoluteIndexed::<{Bank::FP}>::absolute_index(&self.tmp)
        }
    }
}