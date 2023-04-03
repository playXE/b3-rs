//! # Assembl IR (AIR)
//!
//! Air is an instruction superset: it recognizes all of the instructions from all CPUs that Air may target.
//! In its lowest-level form, Air is simply a way of describing an assembly instruction sequence,
//! and this includes CPU concepts like registers and direct accesses to the stack.
//! Air also has a higher-level form in which the assembly has not yet undergone register or stack allocation.
//! Therefore, Air also supports abstract registers (called Tmps) and abstract stack slots.
//! A Tmp object can either hold an unallocated temporary or a register.
//! ## Air as an Instruction Superset
//! Air has syntax to speak of all of the CPU instructions we know about.
//! It is possible to speak of an x86-64 instruction while compiling for ARM64, for example. Clients of Air,
//! such as the B3 to Air lowering phase, are allowed to pick any Air opcode and ask if
//! that opcode would be valid on the current CPU. They are also allowed to check if specific forms of any given opcode are valid.
//! This allows clients to optimize for multiple instruction sets by cascading through the possible opcodes that they know of, 
//! starting with the one they think is most efficient. Some of those opcodes may only be available on one CPU while others are 
//! available everywhere. Instruction selection does not need to know which instructions work on which CPUs; 
//! Air will tell you if some instruction happens to not be valid right now for whatever reason.
//!
//! Air opcodes support overloading. For example, the Add32 opcode has both two-operand and three-operand overloads,
//!  and those overloads have multiple forms: the first operand may or may not be permitted to be an immediate and depending 
//! on the CPU and some of the other operands may or may not be allowed to be memory addresses. 
//! We use opcode overload to refer to all forms of an opcode that share the same number of arguments, 
//! and opcode form to mean the number of arguments and their types. A fundamental Air operation is Inst::is_valid_form(), 
//! which tells the client if the instruction's current form is valid on the current CPU. 
//! This may return false either because the Inst is not well-formed for any CPU or because it is not valid for 
//! the current CPU even though it may be valid on some other CPU. There is also [is_valid_form()](is_valid_form), which can answer
//!  if the form you are intending to use will be valid even if you have not created an Inst yet. 
//! This allows clients to generate Air by experimenting with different forms before settling on the one that the current CPU supports.

pub mod allocate_registers_and_stack_by_linear_scan;
pub mod allocate_registers_by_graph_coloring;
pub mod allocate_stack_by_graph_coloring;
pub mod arg;
pub mod basic_block;
pub mod block_order;
pub mod ccall_special;
pub mod ccalling_convention;
pub mod code;
pub mod custom;
pub mod eliminate_dead_code;
pub mod emit_shuffle;
pub mod fix_obvious_spills;
pub mod fix_spills_after_terminals;
pub mod form_table;
pub mod generate;
pub mod generation_context;
pub mod handle_callee_saves;
pub mod helpers;
pub mod insertion_set;
pub mod inst;
pub mod kind;
pub mod liveness_adapter;
pub mod lower_after_regalloc;
pub mod lower_entry_switch;
pub mod lower_macros;
pub mod lower_stack_args;
pub mod pad_interference;
pub mod reg_liveness;
pub mod report_used_registers;
pub mod simplify_cfg;
pub mod special;
pub mod stack_allocation;
pub mod stack_slot;
pub mod tmp;
pub mod tmp_set;
pub mod tmp_width;
pub mod use_counts;

pub mod opcode {
    include!(concat!(env!("OUT_DIR"), "/opcode.rs"));
}

pub mod opcode_utils {
    #![allow(unused_imports, unused_braces, unreachable_patterns)]
    use super::arg::*;
    use super::code::*;
    use super::custom::*;
    use super::inst::*;
    use super::opcode::*;
    use super::opcode_generated::*;
    use crate::bank::*;
    use crate::width::*;
    use Bank::*;
    use Width::*;

    include!(concat!(env!("OUT_DIR"), "/opcode_utils.rs"));
}

pub use opcode_utils::is_valid_form;

pub mod opcode_generated {
    #![allow(unused_imports, unused_braces, unreachable_code, unreachable_patterns)]
    use super::arg::*;
    use super::code::*;
    use super::custom::*;
    use super::generation_context::*;
    use super::inst::*;
    use super::opcode::*;
    use crate::bank::*;
    use crate::width::*;
    use macroassembler::assembler::abstract_macro_assembler::*;
    use macroassembler::assembler::TargetMacroAssembler;
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

    pub fn decode_form_width(value: u8) -> Width {
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
