use macroassembler::assembler::{abstract_macro_assembler::Jump, TargetMacroAssembler};
use tinyvec::TinyVec;

use crate::{
    air::{
        arg::{Arg, ArgRole,},
        code::Code,
        generation_context::GenerationContext,
        inst::Inst,
        kind::Kind,
        opcode::Opcode as AirOpcode, generate::select_scratch_gpr_for_gpr,
    },
    bank::Bank,
    opcode::Opcode,
    stackmap_special::{RoleMode, StackMapSpecial},
    width::Width, ValueId, stackmap_generation_params::StackmapGenerationParams,
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
    pub fn new(kind: Kind, num_args: usize, stackmap_role: RoleMode) -> Self {
        Self {
            check_kind: kind,
            stackmap_role,
            num_check_args: num_args,
        }
    }

    pub fn hidden_branch(&self, code: &Code<'_>, inst: &Inst) -> Inst {
        let mut hidden_branch = Inst::new(
            self.check_kind,
            inst.origin,
            &[]
        );

        for i in 0..self.num_check_args {
            hidden_branch.args.push(inst.args[i + 1]);
        }

        assert!(hidden_branch.is_terminal(code));

        hidden_branch
    }

    pub fn for_each_arg(
        &self,
        code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(usize, &Arg, ArgRole, Bank, Width),
    ) {
        let mut optional_def_arg_width = None;
        let hidden = self.hidden_branch(code, inst);
        let lambda: &mut dyn FnMut(usize, &Arg, ArgRole, Bank, Width) = &mut lambda;

        let hidden_lambda: &mut dyn FnMut(usize, &Arg, ArgRole, Bank, Width) =
            &mut |index, _arg, role: ArgRole, bank, width| {
                if role.is_any_def() && role != ArgRole::Scratch {
                    assert!(optional_def_arg_width.is_none(), "only one def arg allowed");
                    optional_def_arg_width = Some(width);
                }
                
                lambda(index + 1, &inst.args[1 + index], role, bank, width);
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
            |index, arg, role, bank, width| lambda(index, arg, role, bank, width),
            optional_def_arg_width,
            code,
        );
    }

    pub fn for_each_arg_mut(
        &self,
        code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(usize, &mut Arg, ArgRole, Bank, Width),
    ) {
        let mut optional_def_arg_width = None;
        let hidden = self.hidden_branch(code, inst);

        hidden.for_each_arg(code, |index, _arg, role, bank, width| {
            if role.is_any_def() && role != ArgRole::Scratch {
                assert!(optional_def_arg_width.is_none(), "only one def arg allowed");
                optional_def_arg_width = Some(width);
            }
            
            lambda(1 + index, &mut inst.args[1 + index], role, bank, width);
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
            |ix, arg, role, bank, width| lambda(ix, arg, role, bank, width),
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

    pub fn generate(
        &self,
        inst: &Inst,
        jit: &mut TargetMacroAssembler,
        context: &mut GenerationContext,
    ) -> Jump {
        let fail = self
            .hidden_branch(context.code, inst)
            .generate(jit, context);

        let value = context.code.proc.value(inst.origin);
        let stackmap = value.stackmap().unwrap();
        let generator = stackmap.generator.clone();
        let reps = StackMapSpecial::reps_impl(
            context,
            num_b3_args(value.kind.opcode()),
            self.num_check_args + 1,
            inst,
        );

        let mut args = TinyVec::<[Arg; 3]>::new();

        for i in 0..self.num_check_args {
            args.push(inst.args[1 + i]);
        }

        let check_kind = self.check_kind;
        let num_check_args = self.num_check_args;
        
        let origin = inst.origin;
        context.late_paths.push(Box::new(move |jit, context| {
            fail.link(jit);

            // If necessary, undo the operation.
            match check_kind.opcode {
                AirOpcode::BranchAdd32 => {
                    // this instruction happens not to be used (and requires unimplemented assembler instructions) in 32-bit
                    if cfg!(target_pointer_width = "64") {
                        if (num_check_args == 4 && args[1] == args[2] && args[2] == args[3])
                        || (num_check_args == 3 && args[1] == args[2]) {
                            assert!(args[1].is_gpr());

                            let value_gpr = args[1].gpr();
                            let scratch_gpr = select_scratch_gpr_for_gpr(value_gpr);
                            jit.push_to_save_gpr(scratch_gpr);
                            jit.set_carry(scratch_gpr);
                            jit.lshift32(31i32, scratch_gpr);
                            jit.urshift32(1i32, value_gpr);
                            jit.or32(scratch_gpr, value_gpr);
                            jit.pop_to_restore_gpr(scratch_gpr);
                        } else if num_check_args == 4 {
                            if args[1] == args[3] {
                                Inst::new(AirOpcode::Sub32.into(), ValueId(usize::MAX), &[args[2], args[3]]).generate(jit, context);
                            } else if args[2] == args[3] {
                                Inst::new(AirOpcode::Sub32.into(), ValueId(usize::MAX), &[args[1], args[3]]).generate(jit, context);
                            }
                        } else if num_check_args == 3 {
                            Inst::new(AirOpcode::Sub32.into(), ValueId(usize::MAX), &[args[1], args[2]]).generate(jit, context);
                        }
                    } else {
                        unreachable!("Unreachable for current platform");
                    }
                }

                AirOpcode::BranchAdd64 => {
                    if cfg!(target_pointer_width = "64") {
                        if (num_check_args == 4 && args[1] == args[2] && args[2] == args[3])
                        || (num_check_args == 3 && args[1] == args[2]) {
                            assert!(args[1].is_gpr());

                            let value_gpr = args[1].gpr();
                            let scratch_gpr = select_scratch_gpr_for_gpr(value_gpr);
                            jit.push_to_save_gpr(scratch_gpr);
                            jit.set_carry(scratch_gpr);
                            jit.lshift64(63i32, scratch_gpr);
                            jit.urshift64(1i32, value_gpr);
                            jit.or64(scratch_gpr, value_gpr);
                            jit.pop_to_restore_gpr(scratch_gpr);
                        } else if num_check_args == 4 {
                            if args[1] == args[3] {
                                Inst::new(AirOpcode::Sub64.into(), ValueId(usize::MAX), &[args[2], args[3]]).generate(jit, context);
                            } else if args[2] == args[3] {
                                Inst::new(AirOpcode::Sub64.into(), ValueId(usize::MAX), &[args[1], args[3]]).generate(jit, context);
                            }
                        } else if num_check_args == 3 {
                            Inst::new(AirOpcode::Sub64.into(), ValueId(usize::MAX), &[args[1], args[2]]).generate(jit, context);
                        }
                    } else {
                        unreachable!("Unreachable for current platform");
                    }
                }

                AirOpcode::BranchSub32 => {
                    Inst::new(AirOpcode::Add32.into(), ValueId(usize::MAX), &[args[1], args[2]]).generate(jit, context);
                }

                AirOpcode::BranchSub64 => {
                    Inst::new(AirOpcode::Add64.into(), ValueId(usize::MAX), &[args[1], args[2]]).generate(jit, context);
                }

                AirOpcode::BranchNeg32 => {
                    Inst::new(AirOpcode::Neg32.into(), ValueId(usize::MAX), &[args[1]]).generate(jit, context);
                }

                AirOpcode::BranchNeg64 => {
                    Inst::new(AirOpcode::Neg64.into(), ValueId(usize::MAX), &[args[1]]).generate(jit, context);
                }

                _ => ()
            }

            // Generate the handler
            let generator = generator.unwrap();
            let params = StackmapGenerationParams::new(origin, reps, context);
            generator(jit, &params);
        }));

        Jump::default()  // As far as Air thinks, we are not a terminal.
    }
}
