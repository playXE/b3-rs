#![allow(unused_variables)]
use std::collections::HashSet;

use macroassembler::assembler::{abstract_macro_assembler::Jump, TargetMacroAssembler};

use crate::{
    bank::{bank_for_type, Bank},
    jit::register_set::RegisterSetBuilder,
    width::{width_for_type, Width},
};

use super::{
    arg::{Arg, ArgKind, ArgRole},
    ccalling_convention::{ccall_argument_register_count, ccall_result_count},
    code::Code,
    generation_context::GenerationContext,
    inst::Inst,
};

pub struct PatchCustom {}

impl PatchCustom {
    pub fn for_each_arg(
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
        lambda(0, &inst.args[0], ArgRole::Use, Bank::GP, Width::W64);

        let special = inst.args[0].special();

        code.special(special).for_each_arg(code, inst, lambda);
    }

    pub fn for_each_arg_mut(
        code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        lambda(0, &mut inst.args[0], ArgRole::Use, Bank::GP, Width::W64);

        let special = inst.args[0].special();

        let code2 = unsafe { &mut *(code as *const _ as *mut _) };
        code.special_mut(special)
            .for_each_arg_mut(code2, inst, lambda);
    }

    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        if inst.args.len() < 1 {
            return false;
        }

        if !inst.args[0].is_special() {
            return false;
        }

        if !code.special(inst.args[0].special()).is_valid(code, inst) {
            return false;
        }

        let clobbered_early = inst
            .extra_early_clobbered_regs(code)
            .filter_regs(&RegisterSetBuilder::all_scalar_registers())
            .build_and_validate();
        let clobbered_late = inst
            .extra_clobbered_regs(code)
            .filter_regs(&RegisterSetBuilder::all_scalar_registers())
            .build_and_validate();

        let mut ok = true;

        inst.for_each_tmp(code, |tmp, role, _, _| {
            if !tmp.is_reg() {
                return;
            }

            if role.is_late_def() || role.is_late_use() {
                ok &= !clobbered_late.contains(tmp.reg(), Width::W64);
                if !ok {
                    panic!("late clobbered: {}", tmp.reg());
                }
            } else {
                ok &= !clobbered_early.contains(tmp.reg(), Width::W64);
            }
        });

        ok
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        if arg_index == 0 {
            return false;
        }

        code.special(inst.args[0].special())
            .admits_stack(code, inst, arg_index)
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        if arg_index == 0 {
            return false;
        }

        code.special(inst.args[0].special())
            .admits_stack(code, inst, arg_index)
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        code.special(inst.args[0].special()).is_terminal(code, inst)
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        code.special(inst.args[0].special())
            .has_non_arg_effects(code, inst)
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        code.special(inst.args[0].special())
            .has_non_arg_non_control_effects(code, inst)
    }

    pub fn generate(
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        let special = inst.args[0].special();
        let context2 = unsafe { &mut *(context as *const _ as *mut _) };

        context.code.special(special).generate(inst, jit, context2)
    }
}

pub struct CCallCustom {}

impl CCallCustom {
    pub fn for_each_arg(
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
        // Skip CCallSpecial arg
        let mut index = 1;

        let mut next = |role, bank, width| {
            lambda(index, &inst.args[index], role, bank, width);
            index += 1;
        };

        next(ArgRole::Use, Bank::GP, Width::W64);

        let result_count = ccall_result_count(code, inst.origin);

        for n in 0..result_count {
            let typ = code.proc.value(inst.origin).typ();

            next(ArgRole::Def, bank_for_type(typ), width_for_type(typ));
        }

        for i in 1..code.proc.value(inst.origin).children.len() {
            let child = code.proc.value(inst.origin).children[i];

            for j in 0..ccall_argument_register_count(code, child) {
                next(
                    ArgRole::Use,
                    bank_for_type(code.proc.value(child).typ()),
                    width_for_type(code.proc.value(child).typ()),
                );
            }
        }

        assert!(index == inst.args.len());
    }

    pub fn for_each_arg_mut(
        code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        // Skip CCallSpecial arg
        let mut index = 1;

        let mut next = |role, bank, width| {
            lambda(index, &mut inst.args[index], role, bank, width);
            index += 1;
        };

        next(ArgRole::Use, Bank::GP, Width::W64);

        let result_count = ccall_result_count(code, inst.origin);

        for n in 0..result_count {
            let typ = code.proc.value(inst.origin).typ();

            next(ArgRole::Def, bank_for_type(typ), width_for_type(typ));
        }

        for i in 1..code.proc.value(inst.origin).children.len() {
            let child = code.proc.value(inst.origin).children[i];

            for j in 0..ccall_argument_register_count(code, child) {
                next(
                    ArgRole::Use,
                    bank_for_type(code.proc.value(child).typ()),
                    width_for_type(code.proc.value(child).typ()),
                );
            }
        }

        assert!(index == inst.args.len());
    }

    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        if !inst.args[0].is_special() {
            return false;
        }

        let special = code.special(inst.args[0].special());

        let result_count = ccall_result_count(code, inst.origin);
        let mut expected_arg_count = result_count + 1; // first arg is always CCallSpecial

        for child in code.proc.value(inst.origin).children.iter() {
            expected_arg_count += ccall_argument_register_count(code, *child);
        }

        if inst.args.len() != expected_arg_count {
            return false;
        }

        // The arguments can only refer to the stack, tmps, or immediates.
        for i in (0..inst.args.len() - 1).rev() {
            let arg = inst.args[i];

            if !arg.is_tmp() || !arg.is_stack_memory() && !arg.is_some_imm() {
                return false;
            }
        }

        // callee
        if !inst.args[1].is_gp() {
            return false;
        }

        let mut offset = 2;

        // If there is a result then it cannot be an immediate.
        for i in 0..result_count {
            if inst.args[offset].is_some_imm() {
                return false;
            }

            if !inst.args[offset].can_represent(code.proc.value(inst.origin).typ()) {
                return false;
            }

            offset += 1;
        }

        let mut check_next_arg = |child| {
            let res = inst.args[offset].can_represent(code.proc.value(inst.origin).typ());
            offset += 1;
            res
        };

        for i in 1..code.proc.value(inst.origin).children.len() {
            let child = code.proc.value(inst.origin).children[i];

            for j in 0..ccall_argument_register_count(code, child) {
                if !check_next_arg(child) {
                    return false;
                }
            }
        }

        true
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        true
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        true
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        true
    }

    pub fn generate(
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        todo!()
    }
}

pub struct ColdCCallCustom {}

impl ColdCCallCustom {
    pub fn for_each_arg(
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
        CCallCustom::for_each_arg(code, inst, |ix, arg, role, bank, width| {
            lambda(ix, arg, role, bank, width)
        });
    }

    pub fn for_each_arg_mut(
        code: &mut Code<'_>,
        inst: &mut Inst,
        lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        CCallCustom::for_each_arg_mut(code, inst, lambda);
    }

    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        CCallCustom::is_valid_form(inst, code)
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        CCallCustom::admits_stack(inst, arg_index, code)
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        true
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        true
    }

    pub fn generate(
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        todo!()
    }
}

pub struct ShuffleCustom {}

impl ShuffleCustom {
    pub fn for_each_arg(
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
        let limit = inst.args.len() / 3 * 3;

        let mut i = 0;

        while i < limit {
            let src = &inst.args[i + 0];
            let dst = &inst.args[i + 1];
            let width_arg = &inst.args[i + 2];
            let width = width_arg.width();

            let bank = if src.is_gp() && dst.is_gp() {
                Bank::GP
            } else {
                Bank::FP
            };

            lambda(i, src, ArgRole::Use, bank, width);
            lambda(i + 1,dst, ArgRole::Def, bank, width);
            lambda(i + 2,width_arg, ArgRole::Use, bank, width);

            i += 3;
        }
    }

    pub fn for_each_arg_mut(
        code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        let limit = inst.args.len() / 3 * 3;

        let mut i = 0;

        while i < limit {
            let src = &inst.args[i + 0];
            let dst = &inst.args[i + 1];
            let width_arg = &inst.args[i + 2];
            let width = width_arg.width();

            let bank = if src.is_gp() && dst.is_gp() {
                Bank::GP
            } else {
                Bank::FP
            };

            lambda(i, &mut inst.args[i + 0], ArgRole::Use, bank, width);
            lambda(i + 1, &mut inst.args[i + 1], ArgRole::Def, bank, width);
            lambda(i + 2, &mut inst.args[i + 2], ArgRole::Use, bank, width);

            i += 3;
        }
    }

    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        let _ = args;
        false
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        if inst.args.len() % 3 != 0 {
            return false;
        }

        // A destination may only appear once. This requirement allows us to avoid the undefined behavior
        // of having a destination that is supposed to get multiple inputs simultaneously. It also
        // imposes some interesting constraints on the "shape" of the shuffle. If we treat a shuffle pair
        // as an edge and the Args as nodes, then the single-destination requirement means that the
        // shuffle graph consists of two kinds of subgraphs:
        //
        // - Spanning trees. We call these shifts. They can be executed as a sequence of Move
        //   instructions and don't usually require scratch registers.
        //
        // - Closed loops. These loops consist of nodes that have one successor and one predecessor, so
        //   there is no way to "get into" the loop from outside of it. These can be executed using swaps
        //   or by saving one of the Args to a scratch register and executing it as a shift.

        let mut dsts = HashSet::new();

        for i in 0..inst.args.len() {
            let arg = inst.args[i];
            let mode = i % 3;

            if mode == 2 {
                if !arg.is_width_arg() {
                    return false;
                }

                continue;
            }

            if mode == 0 {
                if arg.is_some_imm() {
                    continue;
                }

                if !arg.is_compatible_bank(&inst.args[i + 1]) {
                    return false;
                }
            } else {
                if !dsts.insert(arg) {
                    return false;
                }
            }

            if arg.is_tmp() || arg.is_memory() {
                continue;
            }

            return false;
        }
        // No destination register may appear in any address expressions. The lowering can't handle it
        // and it's not useful for the way we end up using Shuffles. Normally, Shuffles only used for
        // stack addresses and non-stack registers.

        for arg in inst.args.iter() {
            if !arg.is_memory() {
                continue;
            }

            let mut ok = true;

            arg.for_each_tmp_fast(|tmp| {
                if dsts.contains(&Arg::new_tmp(tmp)) {
                    ok = false;
                }
            });

            if !ok {
                return false;
            }
        }

        true
    }

    pub fn admits_stack(_inst: &Inst, arg_index: usize, _code: &Code<'_>) -> bool {
        match arg_index % 3 {
            0 | 1 => true,
            _ => false,
        }
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

    pub fn generate(
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        todo!()
    }
}

pub struct EntrySwitchCustom {}

impl EntrySwitchCustom {
    pub fn for_each_arg(
        _code: &Code<'_>,
        _inst: &Inst,
        _lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
    }

    pub fn for_each_arg_mut(
        _code: &mut Code<'_>,
        _inst: &mut Inst,
        _lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
    }

    pub fn is_valid_form_static(args: &[ArgKind]) -> bool {
        args.len() == 0
    }

    pub fn is_valid_form(inst: &Inst, code: &Code<'_>) -> bool {
        inst.args.is_empty()
    }

    pub fn admits_stack(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn admits_extended_offset_addr(inst: &Inst, arg_index: usize, code: &Code<'_>) -> bool {
        false
    }

    pub fn is_terminal(inst: &Inst, code: &Code<'_>) -> bool {
        true
    }

    pub fn has_non_arg_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn has_non_arg_non_control_effects(inst: &Inst, code: &Code<'_>) -> bool {
        false
    }

    pub fn generate(
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        todo!()
    }
}
