

use crate::width::Width;

use super::typ::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(i8)]
pub enum Bank {
    GP,
    FP,
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