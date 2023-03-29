use crate::{
    air::{
        arg::{Arg, ArgRole},
        code::Code,
        inst::Inst,
        kind::Kind,
        opcode::Opcode as AirOpcode,
    },
    bank::Bank,
    opcode::Opcode,
    stackmap_special::{RoleMode, StackMapSpecial},
    width::Width,
};

/// We want to lower Check instructions to a branch, but then we want to route that branch to our
/// out-of-line code instead of doing anything else. For this reason, a CheckSpecial will remember
/// which branch opcode we have selected along with the number of args in the overload we want. It
/// will create an Inst with that opcode plus the appropriate args from the owning Inst whenever you
/// call any of the callbacks.
///
/// Note that for CheckAdd, CheckSub, and CheckMul we expect that the B3 arguments are the reverse
/// of the Air arguments (Add(a, b) => Add32 b, a). Except:
/// - CheckSub(0, x), which turns into BranchNeg32 x.
/// - CheckMul(a, b), which turns into Mul32 b, a but we pass Any for a's ValueRep.
pub struct CheckSpecial {
    pub check_kind: Kind,
    pub stackmap_role: RoleMode,
    pub num_check_args: usize,
}

pub const fn num_b3_args(kind: Opcode) -> usize {
    match kind {
        Opcode::CheckAdd | Opcode::CheckSub | Opcode::CheckMul => 2,
        Opcode::Check => 1,
        _ => unreachable!(),
    }
}

impl CheckSpecial {
    pub fn hidden_branch(&self, code: &Code<'_>, inst: &Inst) -> Inst {
        let hidden_branch = Inst::new(
            self.check_kind,
            inst.origin,
            &inst.args[1..self.num_check_args],
        );

        assert!(hidden_branch.is_terminal(code));

        hidden_branch
    }

    pub fn for_each_arg(
        &self,
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(&Arg, ArgRole, Bank, Width),
    ) {
        let mut optional_def_arg_width = None;
        let hidden = self.hidden_branch(code, inst);
        let lambda: &mut dyn FnMut(&Arg, ArgRole, Bank, Width) = &mut lambda;

        let hidden_lambda: &mut dyn FnMut(&Arg, ArgRole, Bank, Width) = &mut |arg, role: ArgRole, bank, width| {
            if role.is_any_def() && role != ArgRole::Scratch {
                assert!(optional_def_arg_width.is_none(), "only one def arg allowed");
                optional_def_arg_width = Some(width);
            }
            let index = (arg as *const Arg as usize - hidden.args.as_ptr() as usize)
                / std::mem::size_of::<Arg>();
            lambda(&inst.args[1 + index], role, bank, width);
        };

        hidden.for_each_arg(code, hidden_lambda);

        let mut first_recoverable_index = None;
        if self.check_kind.opcode == AirOpcode::BranchAdd32
            || self.check_kind.opcode == AirOpcode::BranchAdd64
        {
            first_recoverable_index = Some(1);
        }
        let b3_op = code.proc.value(inst.origin).kind.opcode();
        StackMapSpecial::for_each_arg_impl(
            num_b3_args(b3_op),
            self.num_check_args + 1,
            inst,
            self.stackmap_role,
            first_recoverable_index,
            |arg, role, bank, width| lambda(arg, role, bank, width),
            optional_def_arg_width,
            code,
        );
    }

    pub fn for_each_arg_mut(
        &self,
        code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(&mut Arg, ArgRole, Bank, Width),
    ) {
        let mut optional_def_arg_width = None;
        let hidden = self.hidden_branch(code, inst);

        hidden.for_each_arg(code, |arg, role, bank, width| {
            if role.is_any_def() && role != ArgRole::Scratch {
                assert!(optional_def_arg_width.is_none(), "only one def arg allowed");
                optional_def_arg_width = Some(width);
            }
            let index = (arg as *const Arg as usize - hidden.args.as_ptr() as usize)
                / std::mem::size_of::<Arg>();
            lambda(&mut inst.args[1 + index], role, bank, width);
        });

        let mut first_recoverable_index = None;
        if self.check_kind.opcode == AirOpcode::BranchAdd32
            || self.check_kind.opcode == AirOpcode::BranchAdd64
        {
            first_recoverable_index = Some(1);
        }
        let b3_op = code.proc.value(inst.origin).kind.opcode();
        StackMapSpecial::for_each_arg_impl_mut(
            num_b3_args(b3_op),
            self.num_check_args + 1,
            inst,
            self.stackmap_role,
            first_recoverable_index,
            |arg, role, bank, width| lambda(arg, role, bank, width),
            optional_def_arg_width,
            code,
        );
    }

    pub fn is_valid(&self, code: &Code<'_>, inst: &Inst) -> bool {
        self.hidden_branch(code, inst).is_valid_form(code)
            && StackMapSpecial::is_valid_impl(
                code,
                num_b3_args(code.proc.value(inst.origin).kind.opcode()),
                self.num_check_args + 1,
                inst,
            )
            && inst.args.len() - self.num_check_args - 1
                == code.proc.value(inst.origin).children.len()
                    - num_b3_args(code.proc.value(inst.origin).kind.opcode())
    }

    pub fn admits_stack(&self, code: &Code<'_>, inst: &Inst, arg_index: usize) -> bool {
        if arg_index >= 1 && arg_index < 1 + self.num_check_args {
            return self
                .hidden_branch(code, inst)
                .admits_stack(arg_index - 1, code);
        }

        StackMapSpecial::admits_stack_impl(
            code,
            num_b3_args(code.proc.value(inst.origin).kind.opcode()),
            self.num_check_args + 1,
            inst,
            arg_index,
        )
    }

    /// TODO: Implement this once we have Graph Coloring RA implemented.
    pub fn should_try_aliasing_def(&self, code: &Code<'_>, inst: &Inst) -> Option<usize> {
        let _ = code;
        let _ = inst;

        None
        //if let Some(branch_def) = self.hidden_branch(code, inst).s
    }
}
