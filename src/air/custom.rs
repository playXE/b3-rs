#![allow(unused_variables)]
use super::{arg::ArgKind, code::Code, inst::Inst};


pub struct PatchCustom {}

impl PatchCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}

pub struct CCallCustom {}

impl CCallCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}

pub struct ColdCCallCustom {}

impl ColdCCallCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}

pub struct ShuffleCustom {}

impl ShuffleCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}

pub struct EntrySwitchCustom {}

impl EntrySwitchCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        args.len() != 0
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}

pub struct WasmBoundsCheckCustom {}

impl WasmBoundsCheckCustom {
    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }
}
