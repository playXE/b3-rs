use std::{mem::size_of, ops::{Deref, DerefMut}};

use crate::width::Width;

use super::{reg::Reg, register_set::RegisterSet};

#[derive(Debug, Clone, Hash, Copy, Default)]
pub struct RegisterAtOffset {
    reg_index: u8,
    width: bool,
    offset_bits: isize,
}

impl RegisterAtOffset {
    pub fn new(reg: Reg, offset: isize, width: Width) -> Self {
        Self {
            reg_index: reg.index() as _,
            width: width == Width::W128,
            offset_bits: offset,
        }
    }

    pub fn reg(&self) -> Reg {
        Reg::from_index(self.reg_index as _)
    }

    pub fn offset(&self) -> isize {
        self.offset_bits
    }

    pub fn byte_size(&self) -> usize {
        if self.width {
            16
        } else {
            8
        }
    }

    pub fn width(&self) -> Width {
        if self.width {
            Width::W128
        } else {
            Width::W64
        }
    }

    pub fn offset_as_index(&self) -> isize {
        self.offset_bits / size_of::<usize>() as isize 
    }


}

impl PartialEq for RegisterAtOffset {
    fn eq(&self, other: &Self) -> bool {
        self.reg() == other.reg() && self.offset() == other.offset() && self.width() == other.width()
    }
}

impl Eq for RegisterAtOffset {}

impl PartialOrd for RegisterAtOffset {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.reg() != other.reg() {
            return self.reg().partial_cmp(&other.reg())
        }

        self.offset().partial_cmp(&other.offset())
    }
}
impl Ord for RegisterAtOffset {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Clone, Default)]
pub struct RegisterAtOffsetList {
    registers: Vec<RegisterAtOffset>,
    size_of_area_in_bytes: usize,
}

impl Deref for RegisterAtOffsetList {
    type Target = [RegisterAtOffset];

    fn deref(&self) -> &Self::Target {
        &self.registers
    }
}

impl DerefMut for RegisterAtOffsetList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.registers
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OffsetBaseType {
    FramePointerBased,
    ZeroBased,
}

impl RegisterAtOffsetList {
    pub fn new(set: RegisterSet, offset_base_type: OffsetBaseType) -> Self {
        let mut registers = vec![RegisterAtOffset::default(); set.number_of_set_registers()];
        let size_of_area_in_bytes = set.byte_size_of_set_registers();

        let mut start_offset = 0isize;

        if offset_base_type == OffsetBaseType::FramePointerBased {
            start_offset -= size_of_area_in_bytes as isize;
        }

        let mut offset = start_offset;
        let mut index = 0;

        set.for_each_with_width(|reg, width| {
            offset = round_up_to_multiple_of(width.alignment() as _, offset);
            registers[index] = RegisterAtOffset::new(reg, offset, width);
            index += 1;
            offset += width.bytes() as isize;
        });

        Self {
            registers,
            size_of_area_in_bytes,
        }
    }

    pub fn find(&self, reg: Reg) -> Option<RegisterAtOffset> {
        self.registers.iter().find(|r| r.reg() == reg).copied()
    }

    pub fn index_of(&self, reg: Reg) -> Option<usize> {
        self.registers.iter().position(|r| r.reg() == reg)
    }

    pub fn adjust_offsets(&mut self, addend: isize) {
        for item in self.registers.iter_mut() {
            item.offset_bits += addend;
        }
    }

    pub fn register_count(&self) -> usize {
        self.registers.len()
    }

    pub fn size_of_area_in_bytes(&self) -> usize {
        self.size_of_area_in_bytes
    }
}

pub const fn round_up_to_multiple_of(divisor: isize, x: isize) -> isize {
    (x + (divisor - 1)) & !(divisor - 1)
}