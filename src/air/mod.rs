pub mod arg;
pub mod basic_block;
pub mod code;
pub mod custom;
pub mod eliminate_dead_code;
pub mod fix_spills_after_terminals;
pub mod form_table;
pub mod ccall_special;
pub mod generate;
pub mod handle_callee_saves;
pub mod helpers;
pub mod generation_context;
pub mod insertion_set;
pub mod inst;
pub mod kind;
pub mod liveness_adapter;
pub mod lsra;
pub mod pad_interference;
pub mod reg_liveness;
pub mod rpo;
pub mod simplify_cfg;
pub mod special;
pub mod stack_allocation;
pub mod stack_slot;
pub mod tmp;
pub mod tmp_set;
pub mod tmp_width;

pub mod opcode {
    include!(concat!(env!("OUT_DIR"), "/opcode.rs"));
}

pub mod opcode_utils {
    #![allow(unused_imports, unused_braces, unreachable_patterns)]
    use super::arg::*;
    use super::code::*;
    use super::custom::*;
    use super::opcode::*;
    use super::opcode_generated::*;
    use crate::bank::*;
    use crate::width::*;
    use Bank::*;
    use Width::*;

    include!(concat!(env!("OUT_DIR"), "/opcode_utils.rs"));
}

pub mod opcode_generated {
    #![allow(unused_imports, unused_braces, unreachable_code, unreachable_patterns)]
    use super::arg::*;
    use super::code::*;
    use super::custom::*;
    use super::inst::*;
    use super::opcode::*;
    use crate::bank::*;
    use crate::width::*;
    use Bank::*;
    use Width::*;

    pub const fn is_x86() -> bool {
        #[cfg(target_arch = "x86")]
        {
            return true;
        }
        #[cfg(target_arch = "x86_64")]
        {
            return true;
        }
        #[cfg(not(target_arch = "x86"))]
        {
            return false;
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            return false;
        }
    }

    pub const fn is_x86_64() -> bool {
        #[cfg(target_arch = "x86_64")]
        {
            return true;
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            return false;
        }
    }

    pub const fn is_arm64() -> bool {
        #[cfg(target_arch = "aarch64")]
        {
            return true;
        }
        #[cfg(not(target_arch = "aarch64"))]
        {
            return false;
        }
    }

    pub fn is_x86_64_avx() -> bool {
        #[cfg(any(target_arch = "x86_64", target_feature = "avx"))]
        {
            return true;
        }
        #[cfg(not(any(target_arch = "x86_64", target_feature = "avx")))]
        {
            return false;
        }
    }

    pub const fn is_arm_thumb2() -> bool {
        #[cfg(any(target_arch = "arm", target_feature = "thumb-mode"))]
        {
            return true;
        }
        #[cfg(not(any(target_arch = "arm", target_feature = "thumb-mode")))]
        {
            return false;
        }
    }

    const FORM_ROLE_SHIFT: u8 = 0;
    const FORM_ROLE_MASK: u8 = 0b1111;
    const FORM_BANK_SHIFT: u8 = 4;
    const FORM_BANK_MASK: u8 = 0b01;
    const FORM_WIDTH_SHIFT: u8 = 5;
    const FORM_WIDTH_MASK: u8 = 0b111;

    pub const fn encode_form_width(width: Width) -> u8 {
        match width {
            Width::W8 => 0b001,
            Width::W16 => 0b010,
            Width::W32 => 0b011,
            Width::W64 => 0b100,
            Width::W128 => 0b101,
        }
    }

    pub const fn decode_form_role(value: u8) -> ArgRole {
        unsafe { std::mem::transmute((value >> FORM_ROLE_SHIFT) & FORM_ROLE_MASK) }
    }

    pub const fn decode_form_bank(value: u8) -> Bank {
        unsafe { std::mem::transmute((value >> FORM_BANK_SHIFT) & FORM_BANK_MASK) }
    }

    pub const fn decode_form_width(value: u8) -> Width {
        match (value >> FORM_WIDTH_SHIFT) & FORM_WIDTH_MASK {
            0b001 => Width::W8,
            0b010 => Width::W16,
            0b011 => Width::W32,
            0b100 => Width::W64,
            0b101 => Width::W128,
            _ => unreachable!(),
        }
    }

    #[allow(non_snake_case)]
    const fn encode_inst_form(role: ArgRole, bank: Bank, width: Width) -> u8 {
        (role as u8) << FORM_ROLE_SHIFT
            | ((bank as u8) << FORM_BANK_SHIFT)
            | (encode_form_width(width) << FORM_WIDTH_SHIFT)
    }

    const INVALID_INST_FORM: u8 = 0;
    include!(concat!(env!("OUT_DIR"), "/opcode_generated.rs"));
}
