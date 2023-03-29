use macroassembler::jit::{gpr_info::{RETURN_VALUE_GPR, RETURN_VALUE_GPR2}, fpr_info::RETURN_VALUE_FPR};

use crate::{
    bank::{bank_for_type, Bank},
    jit::{reg::Reg, register_at_offset::round_up_to_multiple_of},
    typ::{size_of_type, TypeKind},
    value::ValueId,
    width::{bytes_for_width, width_for_type},
};

use super::{arg::Arg, ccall_special::CCallSpecial, code::Code, tmp::Tmp, inst::Inst, opcode::Opcode};

pub fn ccall_result_count(code: &Code<'_>, value: ValueId) -> usize {
    match code.proc.value(value).typ().kind() {
        TypeKind::Void => 0,
        _ => 1,
    }
}

pub fn ccall_argument_register_count(code: &Code<'_>, value: ValueId) -> usize {
    match code.proc.value(value).typ().kind() {
        TypeKind::Void => 0,
        _ => 1,
    }
}

fn marshall_ccall_argument_impl<const BANK: Bank>(
    code: &Code,
    result: &mut Vec<Arg>,
    argument_count: &mut usize,
    stack_offset: &mut usize,
    child: ValueId,
) {
    let register_count = ccall_argument_register_count(code, child);

    if *argument_count < BANK.num_of_argument_registers() {
        for i in 0..register_count {
            result.push(Arg::new_tmp(Tmp::from_reg(match BANK {
                Bank::GP => Reg::new_gpr(BANK.to_argument_register(i)),
                Bank::FP => Reg::new_fpr(BANK.to_argument_register(i)),
            })));
        }

        return;
    }

    let slot_size;
    let slot_alignment;

    if cfg!(target_arch = "aarch64") && cfg!(any(target_os = "macos", target_os = "ios")) {
        slot_size = bytes_for_width(width_for_type(code.proc.value(child).typ()));
        slot_alignment = size_of_type(code.proc.value(child).typ());
    } else {
        slot_size = 8;
        slot_alignment = slot_size as _;
    }

    *stack_offset = round_up_to_multiple_of(slot_alignment as _, *stack_offset as _) as usize;
    for _ in 0..register_count {
        result.push(Arg::new_call_arg(*stack_offset as _));
        *stack_offset += slot_size;
    }
}

fn marshall_ccall_argument(
    code: &mut Code,
    result: &mut Vec<Arg>,
    gp_argument_count: &mut usize,
    fp_argument_count: &mut usize,
    stack_offset: &mut usize,
    child: ValueId,
) {
    match bank_for_type(code.proc.value(child).typ()) {
        Bank::FP => marshall_ccall_argument_impl::<{ Bank::FP }>(
            code,
            result,
            fp_argument_count,
            stack_offset,
            child,
        ),
        Bank::GP => marshall_ccall_argument_impl::<{ Bank::GP }>(
            code,
            result,
            gp_argument_count,
            stack_offset,
            child,
        ),
    }
}

pub fn compute_ccalling_convention(code: &mut Code, value: ValueId) -> Vec<Arg> {
    let mut result = vec![];

    result.push(Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(
        CCallSpecial::SCRATCH_REGISTER,
    )))); // for callee

    let mut gp_argument_count = 0;
    let mut fp_argument_count = 0;
    let mut stack_offset = 0;

    for i in 0..code.proc.value(value).children.len() {
        let child = code.proc.value(value).children[i];
        marshall_ccall_argument(
            code,
            &mut result,
            &mut gp_argument_count,
            &mut fp_argument_count,
            &mut stack_offset,
            child,
        );
    }

    code.call_arg_area_size = code
        .call_arg_area_size
        .max(round_up_to_multiple_of(16, stack_offset as _) as _);
    result
}

pub fn ccall_result(code: &mut Code, value: ValueId, _index: usize) -> Tmp {
    match code.proc.value(value).typ().kind() {
        TypeKind::Void => Tmp::empty(),
        TypeKind::Int32 | TypeKind::Int64 => Tmp::from_reg(Reg::new_gpr(RETURN_VALUE_GPR)),
        TypeKind::Float | TypeKind::Double => Tmp::from_reg(Reg::new_fpr(RETURN_VALUE_FPR)),

        _ => todo!()
    }
}

pub fn build_ccall(code: &mut Code, origin: ValueId, arguments: &[Arg]) -> Inst {
    let special = code.ccall_special();
    let mut inst = Inst::new(Opcode::Patch.into(), origin, &[Arg::new_special(special)]);

    inst.args.push(arguments[0]);
    inst.args.push(Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(
        RETURN_VALUE_GPR
    ))));
    inst.args.push(Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(
        RETURN_VALUE_GPR2
    ))));
    inst.args.push(Arg::new_tmp(Tmp::from_reg(Reg::new_fpr(
        RETURN_VALUE_FPR
    ))));

    for i in 1..arguments.len() {
        if arguments[i].is_tmp() {
            inst.args.push(arguments[i]);
        }
    }

    inst
}