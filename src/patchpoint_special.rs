use crate::{
    air::{
        arg::{Arg, ArgRole},
        code::Code,
        inst::Inst,
    },
    bank::{bank_for_type, Bank},
    stackmap_special::{RoleMode, StackMapSpecial},
    value::ValueRepKind,
    width::{width_for_type, Width},
};

/// This is a special that recognizes that there are two uses of Patchpoint: Void and and non-Void.
/// In the Void case, the syntax of the Air Patch instruction is:
///
///     Patch &patchpoint, args...
///
/// Where "args..." are the lowered arguments to the Patchpoint instruction. In the non-Void case
/// we will have:
///
///     Patch &patchpoint, result, args...

pub struct PatchpointSpecial;

impl PatchpointSpecial {
    pub fn for_each_arg(
        &self,
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(&Arg, ArgRole, Bank, Width),
    ) {
        let procedure = &code.proc;

        let value = procedure.value(inst.origin);
        let patchpoint = value.patchpoint().unwrap();

        let typ = value.typ();

        let mut arg_index = 1;
        while arg_index <= (typ.is_numeric() as usize) {
            let role = if patchpoint.result_constraints[arg_index - 1].kind()
                == ValueRepKind::SomeEarlyRegister
            {
                ArgRole::EarlyDef
            } else {
                ArgRole::Def
            };

            lambda(
                &inst.args[arg_index],
                role,
                bank_for_type(typ),
                width_for_type(typ),
            );
            arg_index += 1;
        }

        StackMapSpecial::for_each_arg_impl(
            0,
            arg_index,
            inst,
            RoleMode::SameAsRep,
            None,
            |arg, role, bank, width| lambda(&arg, role, bank, width),
            None,
            code,
        );
        arg_index += code.proc.value(inst.origin).children.len();
        let value = code.proc.value(inst.origin);
        let patchpoint = value.patchpoint().unwrap();

        for i in (0..patchpoint.num_gp_scratch_registers).rev() {
            lambda(
                &inst.args[arg_index],
                ArgRole::Scratch,
                Bank::GP,
                Width::W64,
            );
            arg_index += 1;
        }

        for i in (0..patchpoint.num_fp_scratch_registers).rev() {
            lambda(
                &inst.args[arg_index],
                ArgRole::Scratch,
                Bank::FP,
                Width::W64,
            );
            arg_index += 1;
        }
    }

    pub fn for_each_arg_mut(
        &mut self,
        code: &Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(&mut Arg, ArgRole, Bank, Width),
    ) {
        let procedure = &code.proc;

        let value = procedure.value(inst.origin);
        let patchpoint = value.patchpoint().unwrap();

        let typ = value.typ();

        let mut arg_index = 1;
        while arg_index <= (typ.is_numeric() as usize) {
            let role = if patchpoint.result_constraints[arg_index - 1].kind()
                == ValueRepKind::SomeEarlyRegister
            {
                ArgRole::EarlyDef
            } else {
                ArgRole::Def
            };

            lambda(
                &mut inst.args[arg_index],
                role,
                bank_for_type(typ),
                width_for_type(typ),
            );
            arg_index += 1;
        }

        StackMapSpecial::for_each_arg_impl_mut(
            0,
            arg_index,
            inst,
            RoleMode::SameAsRep,
            None,
            |arg, role, bank, width| lambda(arg, role, bank, width),
            None,
            code,
        );
        arg_index += code.proc.value(inst.origin).children.len();
        let value = code.proc.value(inst.origin);
        let patchpoint = value.patchpoint().unwrap();

        for _ in (0..patchpoint.num_gp_scratch_registers).rev() {
            lambda(
                &mut inst.args[arg_index],
                ArgRole::Scratch,
                Bank::GP,
                Width::W64,
            );
            arg_index += 1;
        }

        for _ in (0..patchpoint.num_fp_scratch_registers).rev() {
            lambda(
                &mut inst.args[arg_index],
                ArgRole::Scratch,
                Bank::FP,
                Width::W64,
            );
            arg_index += 1;
        }
    }

    pub fn is_valid(&self, code: &Code<'_>, inst: &Inst) -> bool {
        let procedure = &code.proc;

        let value = procedure.value(inst.origin);
        let patchpoint = value.patchpoint().unwrap();

        let mut arg_index = 1;

        while arg_index <= (value.typ().is_numeric() as usize) {
            if arg_index >= inst.args.len() {
                return false;
            }

            if !StackMapSpecial::is_arg_valid_for_type(&inst.args[arg_index], value.typ()) {
                return false;
            }

            if !StackMapSpecial::is_arg_valid_for_rep(
                code,
                &inst.args[arg_index],
                &patchpoint.result_constraints[arg_index - 1],
            ) {
                return false;
            }
            arg_index += 1;
        }

        if StackMapSpecial::is_valid_impl(code, 0, arg_index, inst) {
            return false;
        }

        arg_index += value.children.len();

        if arg_index
            + patchpoint.num_gp_scratch_registers as usize
            + patchpoint.num_fp_scratch_registers as usize
            != inst.args.len()
        {
            return false;
        }

        for _ in (0..patchpoint.num_gp_scratch_registers as usize).rev() {
            if !inst.args[arg_index].is_gp_tmp() {
                return false;
            }

            arg_index += 1;
        }

        for _ in (0..patchpoint.num_fp_scratch_registers as usize).rev() {
            if !inst.args[arg_index].is_fp_tmp() {
                return false;
            }

            arg_index += 1;
        }

        true
    }

    pub fn admits_stack(&self, code: &Code<'_>, inst: &Inst, arg_index: usize) -> bool {
        assert!(arg_index != 0);

        let typ = code.proc.value(inst.origin).typ();

        let return_count = typ.is_numeric() as usize;

        if arg_index <= return_count {
            let value = code.proc.value(inst.origin);
            let patchpoint = value.patchpoint().unwrap();
            match patchpoint.result_constraints[arg_index - 1].kind() {
                ValueRepKind::WarmAny | ValueRepKind::StackArgument => return true,
                _ => return false,
            }
        }

        StackMapSpecial::admits_stack_impl(code, 0, return_count + 1, inst, arg_index)
    }

    pub fn admits_extended_offset_addr(&self, code: &Code<'_>, inst: &Inst, arg_index: usize) -> bool {
        self.admits_stack(code, inst, arg_index)
    }

    pub fn is_terminal(&self, code: &Code<'_>, inst: &Inst) -> bool {
        code.proc.value(inst.origin).patchpoint().unwrap().effects.terminal
    }
}
