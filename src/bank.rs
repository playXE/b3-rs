use std::mem::size_of;

use macroassembler::jit::{fpr_info, gpr_info};

use crate::width::{width_for_bytes, Width};

use super::typ::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(i8)]
pub enum Bank {
    GP,
    FP,
}

impl Bank {
    pub const fn from(value: i8) -> Self {
        assert!(value < 2);
        unsafe { std::mem::transmute::<i8, Bank>(value) }
    }
}

impl Bank {
    pub fn num_of_argument_registers(&self) -> usize {
        match self {
            Self::GP => gpr_info::NUMBER_OF_ARGUMENT_REGISTERS,
            Self::FP => fpr_info::NUMBER_OF_ARGUMENT_REGISTERS,
        }
    }

    pub fn to_argument_register(&self, index: usize) -> u8 {
        match self {
            Self::GP => gpr_info::to_argument_register(index),
            Self::FP => fpr_info::to_argument_register(index),
        }
    }
}

pub fn bank_for_type(typ: Type) -> Bank {
    match typ.kind() {
        TypeKind::Int32 | TypeKind::Int64 => Bank::GP,
        TypeKind::V128 | TypeKind::Float | TypeKind::Double => Bank::FP,
        _ => panic!("invalid type for bank: {}", typ),
    }
}

pub fn minimum_width(bank: Bank) -> Width {
    match bank {
        Bank::GP => Width::W8,
        Bank::FP => Width::W32,
    }
}

impl std::fmt::Display for Bank {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Bank::GP => write!(f, "GP"),
            Bank::FP => write!(f, "FP"),
        }
    }
}

pub fn conservative_width_without_vectors(bank: Bank) -> Width {
    if bank == Bank::FP {
        Width::W64
    } else {
        width_for_bytes(size_of::<usize>())
    }
}

pub fn for_each_bank<F: FnMut(Bank)>(mut f: F) {
    f(Bank::GP);
    f(Bank::FP);
}
