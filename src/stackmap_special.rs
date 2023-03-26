use crate::{
    air::{
        arg::{Arg, ArgKind, ArgRole},
        code::Code,
        inst::Inst,
    },
    bank::{bank_for_type, Bank},
    jit::register_set::RegisterSetBuilder,
    value::ValueRepKind,
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
        &self,
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

    pub fn extra_clobbered_regs(&self, code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        let value = code.proc.value(inst.origin);

        value.stackmap().unwrap().late_clobbered
    }

    pub fn extra_early_clobbered_regs(&self, code: &Code<'_>, inst: &Inst) -> RegisterSetBuilder {
        let value = code.proc.value(inst.origin);

        value.stackmap().unwrap().early_clobbered
    }

    pub fn for_each_arg_impl(
        &self,
        num_ignored_b3_args: usize,
        num_ignored_air_args: usize,
        inst: &Inst,
        mut role_mode: RoleMode,
        first_recoverable_index: Option<usize>,
        mut callback: impl FnMut(Arg, ArgRole, Bank, Width),
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

        for i in 0..code.proc.value(inst.origin).children.len() {
            let arg = inst.args[i + num_ignored_air_args];
            let child = code
                .proc
                .value(inst.origin)
                .constrained_child(i + num_ignored_b3_args);
            let mut role = ArgRole::LateColdUse;
            let mut found = false;
            if role_mode == RoleMode::ForceLateUseRecoverable {
                if arg != inst.args[first_recoverable_index.unwrap()]
                    && arg != inst.args[first_recoverable_index.unwrap() + 1]
                {
                    role = ArgRole::LateColdUse;
                    found = true;
                }
            }

            if !found {
                role_mode = RoleMode::SameAsRep;

                match role_mode {
                    RoleMode::SameAsRep => {
                        match child.rep.kind() {
                            ValueRepKind::WarmAny
                            | ValueRepKind::SomeRegister
                            | ValueRepKind::Register
                            | ValueRepKind::Stack
                            | ValueRepKind::StackArgument
                            | ValueRepKind::Constant => {
                                role = ArgRole::Use;
                            }

                            ValueRepKind::SomeRegisterWithClobber => {
                                role = ArgRole::UseDef;
                            }

                            ValueRepKind::SomeLateRegister | ValueRepKind::LateRegister => {
                                role = ArgRole::LateUse;
                            }

                            ValueRepKind::ColdAny => {
                                role = ArgRole::ColdUse;
                            }

                            ValueRepKind::LateColdAny => {
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

            callback(arg, role, bank_for_type(typ), width_for_type(typ));
        }
    }
}
