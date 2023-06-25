use macroassembler::{assembler::TargetMacroAssembler, jit::gpr_info::CALL_FRAME_REGISTER};

use crate::{
    air::{
        arg::{Arg, ArgKind, ArgRole},
        code::Code,
        generation_context::GenerationContext,
        inst::Inst,
        tmp::Tmp,
    },
    bank::{bank_for_type, Bank},
    jit::{reg::Reg, register_set::RegisterSetBuilder},
    typ::Type,
    value::ValueRep,
    width::{width_for_type, Width},
};

pub struct StackMapSpecial {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum RoleMode {
    SameAsRep,
    ForceLateUseRecoverable,
    ForceLateUse,
}

impl StackMapSpecial {
    pub fn report_used_registers(
        code: &mut Code<'_>,
        inst: &Inst,
        used_registers: &RegisterSetBuilder,
    ) {
        let value = code.proc.value_mut(inst.origin);

        value
            .stackmap_mut()
            .unwrap()
            .used_registers
            .merge(used_registers);
    }

    pub fn extra_clobbered_regs(code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        let value = code.proc.value(inst.origin);

        value.stackmap().unwrap().late_clobbered
    }

    pub fn extra_early_clobbered_regs(code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        let value = code.proc.value(inst.origin);

        value.stackmap().unwrap().early_clobbered
    }

    pub fn for_each_arg_impl_mut(
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &mut Inst,
        mut role_mode: RoleMode,
        first_recoverable_index: Option<usize>,
        mut callback: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
        optional_def_arg_width: Option<Width>,
        code: &Code<'_>,
    ) {
        assert!(inst.args.len() >= num_ignored_air_args);
        assert!(code.proc.value(inst.origin).children.len() >= num_ignored_b3_args);
        assert!(
            inst.args.len() - num_ignored_air_args
                >= code.proc.value(inst.origin).children.len() - num_ignored_b3_args
        );
        assert!(inst.args[0].kind() == ArgKind::Special);

        for i in 0..code.proc.value(inst.origin).children.len() - num_ignored_b3_args {
            let mut arg = inst.args[i + num_ignored_air_args];
            let child = code
                .proc
                .value(inst.origin)
                .constrained_child(i + num_ignored_b3_args);
            let mut role = ArgRole::LateColdUse;
            let mut found = false;
            if role_mode == RoleMode::ForceLateUseRecoverable
                && arg != inst.args[first_recoverable_index.unwrap()]
                && arg != inst.args[first_recoverable_index.unwrap() + 1]
            {
                role = ArgRole::LateColdUse;
                found = true;
            }

            if !found {
                role_mode = RoleMode::SameAsRep;

                match role_mode {
                    RoleMode::SameAsRep => {
                        match child.rep {
                            ValueRep::WarmAny
                            | ValueRep::SomeRegister
                            | ValueRep::Register(_)
                            | ValueRep::Stack(_)
                            | ValueRep::StackArgument(_)
                            | ValueRep::Constant(_) => {
                                role = ArgRole::Use;
                            }

                            ValueRep::SomeRegisterWithClobber => {
                                role = ArgRole::UseDef;
                            }

                            ValueRep::SomeLateRegister(_) | ValueRep::LateRegister(_) => {
                                role = ArgRole::LateUse;
                            }

                            ValueRep::ColdAny => {
                                role = ArgRole::ColdUse;
                            }

                            ValueRep::LateColdAny => {
                                role = ArgRole::LateColdUse;
                            }

                            _ => unreachable!(),
                        }

                        if !role.is_late_use()
                            && optional_def_arg_width
                                .map(|x| x < code.proc.value(child.value).result_width())
                                .unwrap_or(false)
                        {
                            if role.is_warm_use() {
                                role = ArgRole::LateUse;
                            } else {
                                role = ArgRole::LateColdUse;
                            }
                        }
                    }
                    RoleMode::ForceLateUse => {
                        role = ArgRole::LateColdUse;
                    }

                    _ => unreachable!(),
                }
            }

            let typ = code.proc.value(child.value).typ();

            callback(
                i + num_ignored_air_args,
                &mut arg,
                role,
                bank_for_type(typ),
                width_for_type(typ),
            );
            inst.args[i + num_ignored_air_args] = arg;
        }
    }

    pub fn for_each_arg_impl(
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &Inst,
        mut role_mode: RoleMode,
        first_recoverable_index: Option<usize>,
        mut callback: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
        optional_def_arg_width: Option<Width>,
        code: &Code<'_>,
    ) {
        assert!(inst.args.len() >= num_ignored_air_args);
        assert!(code.proc.value(inst.origin).children.len() >= num_ignored_b3_args);
        assert!(
            inst.args.len() - num_ignored_air_args
                >= code.proc.value(inst.origin).children.len() - num_ignored_b3_args
        );
        assert!(inst.args[0].kind() == ArgKind::Special);

        for i in 0..code.proc.value(inst.origin).children.len() - num_ignored_b3_args {
            let arg = inst.args[i + num_ignored_air_args];
            let child = code
                .proc
                .value(inst.origin)
                .constrained_child(i + num_ignored_b3_args);
            let mut role = ArgRole::LateColdUse;
            let mut found = false;
            if role_mode == RoleMode::ForceLateUseRecoverable
                && arg != inst.args[first_recoverable_index.unwrap()]
                && arg != inst.args[first_recoverable_index.unwrap() + 1]
            {
                role = ArgRole::LateColdUse;
                found = true;
            }

            if !found {
                role_mode = RoleMode::SameAsRep;

                match role_mode {
                    RoleMode::SameAsRep => {
                        match child.rep {
                            ValueRep::WarmAny
                            | ValueRep::SomeRegister
                            | ValueRep::Register(_)
                            | ValueRep::Stack(_)
                            | ValueRep::StackArgument(_)
                            | ValueRep::Constant(_) => {
                                role = ArgRole::Use;
                            }

                            ValueRep::SomeRegisterWithClobber => {
                                role = ArgRole::UseDef;
                            }

                            ValueRep::SomeLateRegister(_) | ValueRep::LateRegister(_) => {
                                role = ArgRole::LateUse;
                            }

                            ValueRep::ColdAny => {
                                role = ArgRole::ColdUse;
                            }

                            ValueRep::LateColdAny => {
                                role = ArgRole::LateColdUse;
                            }

                            _ => unreachable!(),
                        }

                        if !role.is_late_use()
                            && optional_def_arg_width
                                .map(|x| x < code.proc.value(child.value).result_width())
                                .unwrap_or(false)
                        {
                            if role.is_warm_use() {
                                role = ArgRole::LateUse;
                            } else {
                                role = ArgRole::LateColdUse;
                            }
                        }
                    }
                    RoleMode::ForceLateUse => {
                        role = ArgRole::LateColdUse;
                    }

                    _ => unreachable!(),
                }
            }

            let typ = code.proc.value(child.value).typ();

            callback(
                i + num_ignored_air_args,
                &arg,
                role,
                bank_for_type(typ),
                width_for_type(typ),
            );
        }
    }

    pub fn is_valid_impl(
        code: &Code<'_>,
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &Inst,
    ) -> bool {
        let value = code.proc.value(inst.origin);
        let stackmap_value = code.proc.value(inst.origin).stackmap().unwrap();

        // For the Inst to be valid, it needs to have the right number of arguments.
        if inst.args.len() - num_ignored_air_args < value.children.len() - num_ignored_b3_args {
            return false;
        }

        for i in 0..value.children.len() - num_ignored_b3_args {
            let child = code.proc.value(value.children[i + num_ignored_b3_args]);
            let arg = &inst.args[i + num_ignored_air_args];

            if !Self::is_arg_valid_for_type(arg, child.typ()) {
                return false;
            }
        }

        for i in num_ignored_b3_args..stackmap_value.reps.len() {
            let rep = &stackmap_value.reps[i];
            let arg = &inst.args[i - num_ignored_b3_args + num_ignored_air_args];

            if !Self::is_arg_valid_for_rep(code, arg, rep) {
                return false;
            }
        }

        true
    }

    pub fn admits_stack_impl(
        code: &Code<'_>,
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &Inst,
        arg_index: usize,
    ) -> bool {
        let value = code.proc.value(inst.origin);
        let stackmap_value = value.stackmap().unwrap();

        let stackmap_arg_index = arg_index - num_ignored_air_args + num_ignored_b3_args;

        if stackmap_arg_index >= value.children.len() {
            // It's not a stackmap argument, so as far as we are concerned, it doesn't admit stack.
            return false;
        }

        if stackmap_arg_index >= stackmap_value.reps.len() {
            // This means that there was no constraint.
            return true;
        }

        // We only admit stack for Any's, since Stack is not a valid input constraint, and StackArgument
        // translates to a CallArg in Air.
        if stackmap_value.reps[stackmap_arg_index].is_any() {
            return true;
        }

        false
    }

    pub fn reps_impl(
        context: &mut GenerationContext<'_, '_>,
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &Inst,
    ) -> Vec<ValueRep> {
        let mut result = vec![];

        for i in 0..context.code.proc.value(inst.origin).children.len() - num_ignored_b3_args {
            result.push(Self::rep_for_arg(
                context.code,
                &inst.args[i + num_ignored_air_args],
            ));
        }

        result
    }

    pub fn rep_for_arg(code: &Code<'_>, arg: &Arg) -> ValueRep {
        match arg.kind() {
            ArgKind::Tmp => ValueRep::Register(arg.reg()),
            ArgKind::Imm | ArgKind::BigImm => ValueRep::Constant(arg.value()),
            ArgKind::ExtendedOffsetAddr | ArgKind::Addr => {
                if arg.base() == Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER)) {
                    return ValueRep::Stack(arg.offset() as _);
                }

                ValueRep::Stack(arg.offset() as isize - code.frame_size as isize)
            }
            _ => unreachable!("{}", arg),
        }
    }

    pub fn is_arg_valid_for_type(arg: &Arg, typ: Type) -> bool {
        match arg.kind() {
            ArgKind::Tmp | ArgKind::Imm | ArgKind::BigImm => {}

            _ => {
                if !arg.is_stack_memory() {
                    return false;
                }
            }
        }

        arg.can_represent(typ)
    }

    pub fn is_arg_valid_for_rep(code: &Code<'_>, arg: &Arg, rep: &ValueRep) -> bool {
        match rep {
            ValueRep::WarmAny | ValueRep::ColdAny | ValueRep::LateColdAny => {
                return true;
            }

            ValueRep::SomeRegister
            | ValueRep::SomeRegisterWithClobber
            | ValueRep::SomeEarlyRegister
            | ValueRep::SomeLateRegister(_) => arg.is_tmp(),
            ValueRep::LateRegister(_) | ValueRep::Register(_) => {
                arg == &Arg::new_tmp(Tmp::from_reg(rep.get_reg()))
            }

            ValueRep::StackArgument(_) => {
                if arg == &Arg::new_call_arg(rep.offset_from_fp() as _) && code.frame_size != 0 {
                    if arg.base() == Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER))
                        && arg.offset() == rep.offset_from_fp() as i64 - code.frame_size as i64
                    {
                        return true;
                    }

                    if arg.base()
                        == Tmp::from_reg(Reg::new_gpr(TargetMacroAssembler::STACK_POINTER_REGISTER))
                        && arg.offset() == rep.offset_from_fp() as i64
                    {
                        return true;
                    }
                }

                false
            }

            _ => unreachable!(),
        }
    }
}
