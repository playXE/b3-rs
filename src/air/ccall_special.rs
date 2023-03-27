use macroassembler::{
    assembler::x86assembler::xmm0,
    jit::gpr_info::{NON_PRESERVED_NON_ARGUMENT_GPR0, RETURN_VALUE_GPR, RETURN_VALUE_GPR2},
};

use crate::{
    bank::Bank,
    jit::{reg::Reg, register_set::RegisterSetBuilder},
    width::Width,
};

use super::{
    arg::{Arg, ArgKind, ArgRole},
    code::Code,
    form_table::is_arm64,
    inst::Inst,
    tmp::Tmp,
};

/// Use this special for constructing a C call. Arg 0 is of course a Special arg that refers to the
/// CCallSpecial object. Arg 1 is the callee, and it can be an ImmPtr, a register, or an address. The
/// next three args - arg 2, arg 3, and arg 4 - hold the return value GPRs and FPR. The remaining args
/// are just the set of argument registers used by this call. For arguments that go to the stack, you
/// have to do the grunt work of doing those stack stores. In fact, the only reason why we specify the
/// argument registers as arguments to a call is so that the liveness analysis can see that they get
/// used here. It would be wrong to automagically report all argument registers as being used because
/// if we had a call that didn't pass them, then they'd appear to be live until some clobber point or
/// the prologue, whichever happened sooner.
pub struct CCallSpecial {
    pub clobbered_regs: RegisterSetBuilder,
}

impl CCallSpecial {
    /// You cannot use this register to pass arguments. It just so happens that this register is not
    /// used for arguments in the C calling convention. By the way, this is the only thing that causes
    /// this special to be specific to C calls.
    pub const SCRATCH_REGISTER: u8 = NON_PRESERVED_NON_ARGUMENT_GPR0;

    pub const SPECIAL_ARG_OFFSET: usize = 0;
    pub const NUM_SPECIAL_ARGS: usize = 1;
    pub const CALLEE_ARG_OFFSET: usize = Self::NUM_SPECIAL_ARGS;
    pub const NUM_CALLEE_ARGS: usize = 1;
    pub const RETURN_GP_ARG_OFFSET: usize = Self::NUM_SPECIAL_ARGS + Self::NUM_CALLEE_ARGS;
    pub const NUM_RETURN_GP_ARGS: usize = 2;
    pub const RETURN_FP_ARG_OFFSET: usize =
        Self::NUM_SPECIAL_ARGS + Self::NUM_CALLEE_ARGS + Self::NUM_RETURN_GP_ARGS;
    pub const NUM_RETURN_FP_ARGS: usize = 1;
    pub const ARG_ARG_OFFSET: usize = Self::NUM_SPECIAL_ARGS
        + Self::NUM_CALLEE_ARGS
        + Self::NUM_RETURN_GP_ARGS
        + Self::NUM_RETURN_FP_ARGS;

    pub fn new() -> Self {
        let mut clobbered =
            RegisterSetBuilder::registers_to_save_for_ccall(RegisterSetBuilder::all_registers());

        clobbered.remove(Reg::new_gpr(RETURN_VALUE_GPR));
        clobbered.remove(Reg::new_gpr(RETURN_VALUE_GPR2));
        clobbered.remove(Reg::new_gpr(xmm0));

        Self {
            clobbered_regs: clobbered,
        }
    }

    pub fn for_each_arg(
        &self,
        _code: &Code<'_>,
        inst: &Inst,
        mut lambda: impl FnMut(Arg, ArgRole, Bank, Width),
    ) {
        for i in 0..Self::NUM_CALLEE_ARGS {
            lambda(
                inst.args[Self::CALLEE_ARG_OFFSET + i],
                ArgRole::Use,
                Bank::GP,
                Width::W64,
            )
        }

        for i in 0..Self::NUM_RETURN_GP_ARGS {
            lambda(
                inst.args[Self::RETURN_GP_ARG_OFFSET + i],
                ArgRole::Def,
                Bank::GP,
                Width::W64,
            )
        }

        for i in 0..Self::NUM_RETURN_FP_ARGS {
            lambda(
                inst.args[Self::RETURN_FP_ARG_OFFSET + i],
                ArgRole::Def,
                Bank::FP,
                Width::W64,
            )
        }

        for i in Self::ARG_ARG_OFFSET..inst.args.len() {
            let bank = inst.args[i].bank();
            lambda(inst.args[i], ArgRole::Use, bank, Width::W64)
        }
    }

    pub fn for_each_arg_mut(
        &mut self,
        _code: &mut Code<'_>,
        inst: &mut Inst,
        mut lambda: impl FnMut(&mut Arg, ArgRole, Bank, Width),
    ) {
        for i in 0..Self::NUM_CALLEE_ARGS {
            lambda(
                &mut inst.args[Self::CALLEE_ARG_OFFSET + i],
                ArgRole::Use,
                Bank::GP,
                Width::W64,
            )
        }

        for i in 0..Self::NUM_RETURN_GP_ARGS {
            lambda(
                &mut inst.args[Self::RETURN_GP_ARG_OFFSET + i],
                ArgRole::Def,
                Bank::GP,
                Width::W64,
            )
        }

        for i in 0..Self::NUM_RETURN_FP_ARGS {
            lambda(
                &mut inst.args[Self::RETURN_FP_ARG_OFFSET + i],
                ArgRole::Def,
                Bank::FP,
                Width::W64,
            )
        }

        for i in Self::ARG_ARG_OFFSET..inst.args.len() {
            let bank = inst.args[i].bank();
            lambda(&mut inst.args[i], ArgRole::Use, bank, Width::W64)
        }
    }

    pub fn is_valid(&self, _code: &Code<'_>, inst: &Inst) -> bool {
        if inst.args.len() < Self::ARG_ARG_OFFSET {
            return false;
        }

        for i in 0..Self::NUM_CALLEE_ARGS {
            let arg = inst.args[Self::CALLEE_ARG_OFFSET + i];

            if !arg.is_gp() {
                return false;
            }

            match arg.kind() {
                ArgKind::Imm => {
                    #[cfg(target_pointer_width = "32")]
                    {}
                    #[cfg(target_pointer_width = "64")]
                    {
                        return false;
                    }
                }

                ArgKind::BigImm => {
                    #[cfg(target_pointer_width = "32")]
                    {
                        return false;
                    }
                    #[cfg(target_pointer_width = "64")]
                    {}
                }

                ArgKind::Addr
                | ArgKind::Tmp
                | ArgKind::ExtendedOffsetAddr
                | ArgKind::Stack
                | ArgKind::CallArg => {}
                _ => return false,
            }
        }

        if inst.args[Self::RETURN_GP_ARG_OFFSET + 0]
            != Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(RETURN_VALUE_GPR)))
        {
            return false;
        }

        if inst.args[Self::RETURN_GP_ARG_OFFSET + 1]
            != Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(RETURN_VALUE_GPR2)))
        {
            return false;
        }

        if inst.args[Self::RETURN_FP_ARG_OFFSET + 0]
            != Arg::new_tmp(Tmp::from_reg(Reg::new_fpr(xmm0)))
        {
            return false;
        }

        for i in Self::ARG_ARG_OFFSET..inst.args.len() {
            let arg = inst.args[i];

            if !arg.is_reg() {
                return false;
            }

            if arg == Arg::new_tmp(Tmp::from_reg(Reg::new_gpr(Self::SCRATCH_REGISTER))) {
                return false;
            }
        }

        true
    }

    pub fn admits_stack(&self, _code: &Code<'_>, _inst: &Inst, arg_index: usize) -> bool {
        // The callee can be on the stack unless targeting ARM64, where we can't later properly
        // handle an Addr callee argument in generate() due to disallowed scratch register usage.
        if arg_index == Self::CALLEE_ARG_OFFSET {
            !is_arm64()
        } else {
            false
        }
    }
}
