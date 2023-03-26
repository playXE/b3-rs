use std::{mem::size_of, ops::Range};

use macroassembler::assembler::TargetAssembler;

use crate::width::{width_for_bytes, Width};

/// Reg is a polymorphic register structure. It can refer to either integer or float registers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg {
    index: u8,
}

impl Reg {
    pub fn conservative_width_without_vectors(&self) -> Width {
        if self.is_fpr() {
            Width::W64
        } else {
            width_for_bytes(size_of::<usize>())
        }
    }

    pub fn index(self) -> usize {
        self.index as _
    }

    pub const fn is_gpr(self) -> bool {
        self.index < TargetAssembler::number_of_registers() as u8
    }

    pub const fn is_fpr(self) -> bool {
        ((self.index as usize).wrapping_sub(TargetAssembler::number_of_registers()))
            < TargetAssembler::number_of_fp_registers()
    }

    pub const fn gpr(self) -> u8 {
        TargetAssembler::first_register() + self.index
    }

    pub const fn fpr(self) -> u8 {
        TargetAssembler::first_fp_register()
            + (self.index - TargetAssembler::number_of_registers() as u8)
    }

    pub const fn last() -> Reg {
        Reg {
            index: TargetAssembler::number_of_registers() as u8
                + TargetAssembler::number_of_fp_registers() as u8
                - 1,
        }
    }

    pub const fn first() -> Reg {
        Reg { index: 0 }
    }

    pub const fn next(self) -> Reg {
        assert!(self.is_set());
        if self.index == Reg::last().index {
            Reg {
                index: Self::invalid(),
            }
        } else {
            Reg {
                index: self.index + 1,
            }
        }
    }

    pub const fn is_set(self) -> bool {
        self.index != Self::invalid()
    }

    const fn invalid() -> u8 {
        (1 << 7) - 1
    }

    #[allow(dead_code)]
    const fn deleted() -> u8 {
        Self::invalid() - 1
    }

    pub const fn new_fpr(index: u8) -> Reg {
        Reg {
            index: index - TargetAssembler::first_fp_register()
                + TargetAssembler::number_of_registers() as u8,
        }
    }

    pub const fn new_gpr(index: u8) -> Reg {
        Reg {
            index: index - TargetAssembler::first_register(),
        }
    }

    pub const fn from_index(index: u8) -> Reg {
        Reg { index }
    }
}

impl Default for Reg {
    fn default() -> Self {
        Reg {
            index: Self::invalid(),
        }
    }
}

pub const ALL_REGS: Range<Reg> = Range {
    start: Reg::first(),
    end: Reg::last(),
};

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_gpr() {
            write!(f, "{}", TargetAssembler::gpr_name(self.gpr()))
        } else {
            write!(f, "{}", TargetAssembler::fpr_name(self.fpr()))
        }
    }
}