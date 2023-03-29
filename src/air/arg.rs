use macroassembler::{
    assembler::{
        abstract_macro_assembler::{Address, BaseIndex, Extend, PreIndexAddress, Scale},
        macro_assembler_x86_common::{
            DoubleCondition, RelationalCondition, ResultCondition, StatusCondition,
        },
        TargetMacroAssembler,
    },
    jit::gpr_info::CALL_FRAME_REGISTER,
};

use crate::{
    bank::{Bank, bank_for_type},
    jit::reg::Reg,
    width::{bytes_for_width, width_for_bytes, Width}, typ::Type,
};

use super::{
    opcode::Opcode,
    special::{Special, SpecialId},
    stack_slot::StackSlotId,
    tmp::Tmp,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ArgKind {
    Invalid,

    // This is either an unassigned temporary or a register. All unassigned temporaries
    // eventually become registers.
    Tmp,

    // This is an immediate that the instruction will materialize. Imm is the immediate that can be
    // inlined into most instructions, while BigImm indicates a constant materialization and is
    // usually only usable with Move. Specials may also admit it, for example for stackmaps used for
    // OSR exit and tail calls.
    // BitImm is an immediate for Bitwise operation (And, Xor, etc).
    Imm,
    BigImm,
    BitImm,
    BitImm64,

    // These are the addresses. Instructions may load from (Use), store to (Def), or evaluate
    // (UseAddr) addresses.
    SimpleAddr,
    Addr,
    ExtendedOffsetAddr,
    Stack,
    CallArg,
    Index,
    PreIndex,
    PostIndex,

    // Immediate operands that customize the behavior of an operation. You can think of them as
    // secondary opcodes. They are always "Use"'d.
    RelCond,
    ResCond,
    DoubleCond,
    StatusCond,
    Special,
    WidthArg,

    // ZeroReg is interpreted as a zero register in ARM64
    ZeroReg,

    SIMDInfo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ArgTemperature {
    Cold,
    Warm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ArgPhase {
    Early,
    Late,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ArgTiming {
    OnlyEarly,
    OnlyLate,
    EarlyAndLate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ArgRole {
    // Use means that the Inst will read from this value before doing anything else.
    //
    // For Tmp: The Inst will read this Tmp.
    // For Arg::addr and friends: The Inst will load from this address.
    // For Arg::imm and friends: The Inst will materialize and use this immediate.
    // For RelCond/ResCond/Special: This is the only valid role for these kinds.
    //
    // Note that Use of an address does not mean escape. It only means that the instruction will
    // load from the address before doing anything else. This is a bit tricky; for example
    // Specials could theoretically squirrel away the address and effectively escape it. However,
    // this is not legal. On the other hand, any address other than Stack is presumed to be
    // always escaping, and Stack is presumed to be always escaping if it's Locked.
    Use,

    // Exactly like Use, except that it also implies that the use is cold: that is, replacing the
    // use with something on the stack is free.
    ColdUse,

    // LateUse means that the Inst will read from this value after doing its Def's. Note that LateUse
    // on an Addr or Index still means Use on the internal temporaries. Note that specifying the
    // same Tmp once as Def and once as LateUse has undefined behavior: the use may happen before
    // the def, or it may happen after it.
    LateUse,

    // Combination of LateUse and ColdUse.
    LateColdUse,

    // Def means that the Inst will write to this value after doing everything else.
    //
    // For Tmp: The Inst will write to this Tmp.
    // For Arg::addr and friends: The Inst will store to this address.
    // This isn't valid for any other kinds.
    //
    // Like Use of address, Def of address does not mean escape.
    Def,

    // This is a special variant of Def that implies that the upper bits of the target register are
    // zero-filled. Specifically, if the Width of a ZDef is less than the largest possible width of
    // the argument (for example, we're on a 64-bit machine and we have a Width32 ZDef of a GPR) then
    // this has different implications for the upper bits (i.e. the top 32 bits in our example)
    // depending on the kind of the argument:
    //
    // For register: the upper bits are zero-filled.
    // For anonymous stack slot: the upper bits are zero-filled.
    // For address: the upper bits are not touched (i.e. we do a 32-bit store in our example).
    // For tmp: either the upper bits are not touched or they are zero-filled, and we won't know
    // which until we lower the tmp to either a StackSlot or a Reg.
    //
    // The behavior of ZDef is consistent with what happens when you perform 32-bit operations on a
    // 64-bit GPR. It's not consistent with what happens with 8-bit or 16-bit Defs on x86 GPRs, or
    // what happens with float Defs in ARM NEON or X86 SSE. Hence why we have both Def and ZDef.
    ZDef,

    // This is a combined Use and Def. It means that both things happen.
    UseDef,

    // This is a combined Use and ZDef. It means that both things happen.
    UseZDef,

    // This is like Def, but implies that the assignment occurs before the start of the Inst's
    // execution rather than after. Note that specifying the same Tmp once as EarlyDef and once
    // as Use has undefined behavior: the use may happen before the def, or it may happen after
    // it.
    EarlyDef,

    EarlyZDef,

    // Some instructions need a scratch register. We model this by saying that the temporary is
    // defined early and used late. This role implies that.
    Scratch,

    // This is a special kind of use that is only valid for addresses. It means that the
    // instruction will evaluate the address expression and consume the effective address, but it
    // will neither load nor store. This is an escaping use, because now the address may be
    // passed along to who-knows-where. Note that this isn't really a Use of the Arg, but it does
    // imply that we're Use'ing any registers that the Arg contains.
    UseAddr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ArgSignedness {
    Signed,
    Unsigned,
}

impl ArgRole {
    pub const fn is_any_use(self) -> bool {
        matches!(
            self,
            ArgRole::Use
                | ArgRole::ColdUse
                | ArgRole::LateUse
                | ArgRole::LateColdUse
                | ArgRole::UseDef
                | ArgRole::UseZDef
                | ArgRole::Scratch
        )
    }

    pub const fn is_cold_use(self) -> bool {
        matches!(self, ArgRole::ColdUse | ArgRole::LateColdUse)
    }

    pub const fn is_warm_use(self) -> bool {
        self.is_any_use() && !self.is_cold_use()
    }

    pub const fn temperature(self) -> ArgTemperature {
        if self.is_cold_use() {
            ArgTemperature::Cold
        } else {
            ArgTemperature::Warm
        }
    }

    pub const fn active_at(self, phase: ArgPhase) -> bool {
        if matches!(
            self,
            Self::Use | Self::ColdUse | Self::EarlyDef | Self::EarlyZDef | Self::UseAddr
        ) {
            phase as u8 == ArgPhase::Early as u8
        } else if matches!(
            self,
            Self::LateUse | Self::LateColdUse | Self::Def | Self::ZDef
        ) {
            phase as u8 == ArgPhase::Late as u8
        } else {
            true
        }
    }

    pub const fn timing(self) -> ArgTiming {
        if matches!(
            self,
            Self::Use | Self::ColdUse | Self::EarlyDef | Self::EarlyZDef | Self::UseAddr
        ) {
            ArgTiming::OnlyEarly
        } else if matches!(
            self,
            Self::Def | Self::ZDef | Self::LateColdUse | Self::LateUse
        ) {
            ArgTiming::OnlyLate
        } else {
            ArgTiming::EarlyAndLate
        }
    }

    pub const fn is_early_use(self) -> bool {
        matches!(
            self,
            Self::Use | Self::ColdUse | Self::UseDef | Self::UseZDef
        )
    }

    pub const fn is_late_use(self) -> bool {
        matches!(self, Self::LateUse | Self::LateColdUse | Self::Scratch)
    }

    pub const fn is_any_def(self) -> bool {
        matches!(
            self,
            Self::Def
                | Self::UseDef
                | Self::ZDef
                | Self::UseZDef
                | Self::EarlyDef
                | Self::EarlyZDef
                | Self::Scratch
        )
    }

    pub const fn is_early_def(self) -> bool {
        matches!(self, Self::EarlyDef | Self::EarlyZDef | Self::Scratch)
    }

    pub const fn is_late_def(self) -> bool {
        matches!(self, Self::Def | Self::UseDef | Self::ZDef | Self::UseZDef)
    }

    pub const fn is_zdef(self) -> bool {
        matches!(self, Self::ZDef | Self::UseZDef | Self::EarlyZDef)
    }
}

impl ArgTiming {
    pub const fn active_at(self, phase: ArgPhase) -> bool {
        match self {
            Self::OnlyEarly => phase as u8 == ArgPhase::Early as u8,
            Self::OnlyLate => phase as u8 == ArgPhase::Late as u8,
            Self::EarlyAndLate => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Arg {
    offset: i64,
    kind: ArgKind,
    extend: Extend,
    scale: i32,
    base: Tmp,
    index: Tmp,
}

impl Default for Arg {
    fn default() -> Self {
        Self {
            offset: 0,
            kind: ArgKind::Invalid,
            extend: Extend::None,
            scale: 1,
            base: Tmp::empty(),
            index: Tmp::empty(),
        }
    }
}

impl Arg {
    pub fn can_represent(&self, typ: Type) -> bool {
        self.bank() == bank_for_type(typ)
    }

    

    /// This is smart enough to know that an address arg in a Def or UseDef rule will use its
    /// tmps and never def them. For example, this:
    ///
    /// mov %rax, (%rcx)
    ///
    /// This defs (%rcx) but uses %rcx.
    pub fn for_each_tmp(
        &self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(Tmp, ArgRole, Bank, Width),
    ) {
        match self.kind {
            ArgKind::Tmp => {
                f(self.base, arg_role, arg_bank, arg_width);
            }

            ArgKind::SimpleAddr | ArgKind::Addr | ArgKind::ExtendedOffsetAddr => f(
                self.base,
                ArgRole::Use,
                Bank::GP,
                if arg_role == ArgRole::UseAddr {
                    arg_width
                } else {
                    Width::W64
                },
            ),

            ArgKind::PostIndex | ArgKind::PreIndex => {
                f(
                    self.base,
                    ArgRole::UseDef,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
            }

            ArgKind::Index => {
                f(
                    self.base,
                    ArgRole::Use,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
                f(
                    self.index,
                    ArgRole::Use,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
            }

            _ => (),
        }
    }

    pub fn for_each_tmp_mut(
        &mut self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(&mut Tmp, ArgRole, Bank, Width),
    ) {
        match self.kind {
            ArgKind::Tmp => {
                f(&mut self.base, arg_role, arg_bank, arg_width);
            }

            ArgKind::SimpleAddr | ArgKind::Addr | ArgKind::ExtendedOffsetAddr => f(
                &mut self.base,
                ArgRole::Use,
                Bank::GP,
                if arg_role == ArgRole::UseAddr {
                    arg_width
                } else {
                    Width::W64
                },
            ),

            ArgKind::PostIndex | ArgKind::PreIndex => {
                f(
                    &mut self.base,
                    ArgRole::UseDef,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
            }

            ArgKind::Index => {
                f(
                    &mut self.base,
                    ArgRole::Use,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
                f(
                    &mut self.index,
                    ArgRole::Use,
                    Bank::GP,
                    if arg_role == ArgRole::UseAddr {
                        arg_width
                    } else {
                        Width::W64
                    },
                );
            }

            _ => (),
        }
    }

    pub fn for_each_arg(
        &self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(&Self, ArgRole, Bank, Width),
    ) {
        f(self, arg_role, arg_bank, arg_width);
    }

    pub fn for_each_arg_mut(
        &mut self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(&mut Self, ArgRole, Bank, Width),
    ) {
        f(self, arg_role, arg_bank, arg_width);
    }

    pub fn for_each_stack_slot(
        &self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(StackSlotId, ArgRole, Bank, Width),
    ) {
        if !self.is_stack() {
            return;
        }

        let stack_slot = self.stack_slot();

        f(stack_slot, arg_role, arg_bank, arg_width);
    }

    pub fn for_each_stack_slot_mut(
        &mut self,
        arg_role: ArgRole,
        arg_bank: Bank,
        arg_width: Width,
        mut f: impl FnMut(&mut StackSlotId, ArgRole, Bank, Width),
    ) {
        if !self.is_stack() {
            return;
        }

        let mut stack_slot = self.stack_slot();

        f(&mut stack_slot, arg_role, arg_bank, arg_width);

        self.offset = stack_slot.0 as _;
    }

    pub fn for_each_reg(&self, arg_role: ArgRole, arg_bank: Bank, arg_width: Width, mut f: impl FnMut(Reg, ArgRole, Bank, Width)) {
        self.for_each_tmp(arg_role, arg_bank, arg_width, |tmp, role, bank, width| {
            if !tmp.is_reg() {
                return;
            }

            let reg = tmp.reg();
            f(reg, role, bank, width);
        });
    }

    pub fn for_each_reg_mut(&mut self, arg_role: ArgRole, arg_bank: Bank, arg_width: Width, mut f: impl FnMut(&mut Reg, ArgRole, Bank, Width)) {
        self.for_each_tmp_mut(arg_role, arg_bank, arg_width, |tmp, role, bank, width| {
            if !tmp.is_reg() {
                return;
            }

            let mut reg = tmp.reg();
            f(&mut reg, role, bank, width);
            *tmp = Tmp::from_reg(reg);
        });
    }

    pub fn for_each_tmp_fast(&self, mut f: impl FnMut(Tmp)) {
        match self.kind {
            ArgKind::Tmp => f(self.base),
            ArgKind::SimpleAddr | ArgKind::Addr | ArgKind::ExtendedOffsetAddr => f(self.base),
            ArgKind::PostIndex | ArgKind::PreIndex => f(self.base),
            ArgKind::Index => {
                f(self.base);
                f(self.index);
            }
            _ => (),
        }
    }

    pub fn for_each_arg_fast(&self, mut f: impl FnMut(&Self)) {
        f(self);
    }

    pub fn for_each_arg_fast_mut(&mut self, mut f: impl FnMut(&mut Self)) {
        f(self);
    }

    pub fn for_each_stack_slot_fast(&self, mut f: impl FnMut(StackSlotId)) {
        if !self.is_stack() {
            return;
        }

        let stack_slot = self.stack_slot();

        f(stack_slot);
    }

    pub fn for_each_stack_slot_fast_mut(&mut self, mut f: impl FnMut(&mut StackSlotId)) {
        if !self.is_stack() {
            return;
        }

        let mut stack_slot = self.stack_slot();

        f(&mut stack_slot);

        self.offset = stack_slot.0 as _;
    }

    pub fn for_each_reg_fast(&self, mut f: impl FnMut(Reg)) {
        self.for_each_tmp_fast(|tmp| {
            if !tmp.is_reg() {
                return;
            }

            let reg = tmp.reg();
            f(reg);
        });
    }

    pub fn for_each_reg_fast_mut(&mut self, mut f: impl FnMut(&mut Reg)) {
        self.for_each_tmp_fast_mut(|tmp| {
            if !tmp.is_reg() {
                return;
            }

            let mut reg = tmp.reg();
            f(&mut reg);
            *tmp = Tmp::from_reg(reg);
        });
    }

    pub fn for_each_tmp_fast_mut(&mut self, mut f: impl FnMut(&mut Tmp)) {
        match self.kind {
            ArgKind::Tmp => f(&mut self.base),
            ArgKind::SimpleAddr | ArgKind::Addr | ArgKind::ExtendedOffsetAddr => f(&mut self.base),
            ArgKind::PostIndex | ArgKind::PreIndex => f(&mut self.base),
            ArgKind::Index => {
                f(&mut self.base);
                f(&mut self.index);
            }
            _ => (),
        }
    }

    pub fn new_imm(value: i64) -> Self {
        Self {
            kind: ArgKind::Imm,
            offset: value,
            ..Default::default()
        }
    }

    pub fn new_bigimm(value: i64) -> Self {
        Self {
            kind: ArgKind::BigImm,
            offset: value,
            ..Default::default()
        }
    }

    pub fn new_bigimm_lo32(value: i64) -> Self {
        Self::new_bigimm(value & 0xffff_ffff)
    }

    pub fn new_bigimm_hi32(value: i64) -> Self {
        Self::new_bigimm(value >> 32)
    }

    pub fn new_bitimm(value: i64) -> Self {
        Self {
            kind: ArgKind::BitImm,
            offset: value,
            ..Default::default()
        }
    }

    pub fn new_bitimm64(value: i64) -> Self {
        Self {
            kind: ArgKind::BitImm64,
            offset: value,
            ..Default::default()
        }
    }

    pub fn new_simple_addr(base: Tmp, offset: i64) -> Self {
        Self {
            kind: ArgKind::SimpleAddr,
            base,
            offset,
            ..Default::default()
        }
    }

    pub fn new_addr(base: Tmp, offset: i64) -> Self {
        Self {
            kind: ArgKind::Addr,
            base,
            offset,
            ..Default::default()
        }
    }

    pub fn extended_offset_addr(offset_from_fp: isize) -> Self {
        Self {
            kind: ArgKind::ExtendedOffsetAddr,
            base: Tmp::from_reg(Reg::new_gpr(TargetMacroAssembler::FRAME_POINTER_REGISTER)),
            offset: offset_from_fp as _,
            ..Default::default()
        }
    }

    pub fn new_stack(stack_slot: StackSlotId, offset: i32) -> Self {
        Self {
            kind: ArgKind::Stack,
            offset: stack_slot.0 as _,
            scale: offset as _, // very hacky
            ..Default::default()
        }
    }

    pub fn new_call_arg(offset: i32) -> Self {
        Self {
            kind: ArgKind::CallArg,
            offset: offset as _,
            ..Default::default()
        }
    }

    pub fn new_index(base: Tmp, index: Tmp, scale: usize, offset: isize, extend: Extend) -> Self {
        Self {
            kind: ArgKind::Index,
            base,
            index,
            scale: scale as _,
            offset: offset as _,
            extend,
        }
    }

    pub fn new_pre_index(base: Tmp, index: isize) -> Self {
        Self {
            kind: ArgKind::PreIndex,
            base,
            offset: index as _,
            ..Default::default()
        }
    }

    pub fn new_post_index(base: Tmp, index: isize) -> Self {
        Self {
            kind: ArgKind::PostIndex,
            base,
            offset: index as _,
            ..Default::default()
        }
    }

    pub fn new_res_cond(cond: ResultCondition) -> Self {
        Self {
            kind: ArgKind::ResCond,
            offset: cond as _,
            ..Default::default()
        }
    }

    pub fn new_rel_cond(cond: RelationalCondition) -> Self {
        Self {
            kind: ArgKind::RelCond,
            offset: cond as _,
            ..Default::default()
        }
    }

    pub fn new_double_cond(cond: DoubleCondition) -> Self {
        Self {
            kind: ArgKind::DoubleCond,
            offset: cond as _,
            ..Default::default()
        }
    }

    pub fn new_status_cond(cond: StatusCondition) -> Self {
        Self {
            kind: ArgKind::StatusCond,
            offset: cond as _,
            ..Default::default()
        }
    }

    pub fn new_special(special: SpecialId) -> Self {
        Self {
            kind: ArgKind::Special,
            offset: special.0 as _,
            ..Default::default()
        }
    }

    pub fn new_width_arg(width: Width) -> Self {
        Self {
            kind: ArgKind::WidthArg,
            offset: bytes_for_width(width) as _,
            ..Default::default()
        }
    }

    pub fn zero_reg() -> Self {
        Self {
            kind: ArgKind::ZeroReg,
            ..Default::default()
        }
    }

    pub fn new_tmp(tmp: Tmp) -> Self {
        Self {
            kind: ArgKind::Tmp,
            base: tmp,
            ..Default::default()
        }
    }

    pub fn is_valid_scale(scale: usize, width: Option<Width>) -> bool {
        match scale {
            1 => {
                if cfg!(any(
                    target_arch = "x86",
                    target_arch = "x86_64",
                    target_arch = "aarch64"
                )) {
                    true
                } else {
                    false
                }
            }

            2 | 4 | 8 => {
                if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
                    return true;
                }

                if cfg!(target_arch = "aarch64") {
                    if width.is_none() {
                        return true;
                    }

                    return scale == 1 || scale == bytes_for_width(width.unwrap());
                }

                false
            }

            _ => false,
        }
    }

    pub const fn is_valid_imm_form(value: i64) -> bool {
        value as i32 as i64 == value
    }

    pub const fn is_valid_bit_imm_form(value: i64) -> bool {
        value as i32 as i64 == value
    }

    pub const fn is_valid_bit_imm64_form(value: i64) -> bool {
        value as i32 as i64 == value
    }

    pub const fn is_valid_addr_form(opcode: Opcode, offset: isize) -> bool {
        let _ = opcode;
        let _ = offset;
        true
    }

    pub const fn is_valid_index_form(
        opcode: Opcode,
        scale: i32,
        _offset: i64,
        _width: Option<Width>,
    ) -> bool {
        let _ = opcode;
        let _ = scale;
        true
    }

    pub const fn is_valid_increment_index_form(_offset: isize) -> bool {
        false
    }

    pub fn is_valid_form(&self, opcode: Opcode, width: Option<Width>) -> bool {
        match self.kind() {
            ArgKind::Invalid => false,
            ArgKind::Tmp => true,
            ArgKind::Imm => Self::is_valid_imm_form(self.value()),
            ArgKind::BigImm => true,
            ArgKind::BitImm => Self::is_valid_bit_imm_form(self.value()),
            ArgKind::BitImm64 => Self::is_valid_bit_imm64_form(self.value()),
            ArgKind::Addr | ArgKind::CallArg | ArgKind::Stack => {
                Self::is_valid_addr_form(opcode, self.offset() as _)
            }
            ArgKind::Index => {
                Self::is_valid_index_form(opcode, self.scale() as _, self.offset() as _, width)
            }

            _ => true,
        }
    }


    pub fn uses_tmp(&self, tmp: Tmp) -> bool {
        match self.kind {
            ArgKind::Tmp
            | ArgKind::SimpleAddr
            | ArgKind::ExtendedOffsetAddr
            | ArgKind::PreIndex
            | ArgKind::PostIndex => self.base == tmp,

            ArgKind::Index => self.base == tmp || self.index == tmp,
            _ => false,
        }
    }

    pub fn is_stack_memory(&self) -> bool {
        match self.kind {
            ArgKind::Addr => {
                self.base == Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER))
                    || self.base
                        == Tmp::from_reg(Reg::new_gpr(TargetMacroAssembler::STACK_POINTER_REGISTER))
            }

            ArgKind::ExtendedOffsetAddr | ArgKind::Stack | ArgKind::CallArg => true,
            _ => false,
        }
    }

    pub fn kind(&self) -> ArgKind {
        self.kind
    }

    pub fn extend(&self) -> Extend {
        self.extend
    }

    pub fn set_kind(&mut self, kind: ArgKind) {
        self.kind = kind;
    }

    pub fn set_extend(&mut self, extend: Extend) {
        self.extend = extend;
    }

    pub fn set_scale(&mut self, scale: i32) {
        self.scale = scale;
    }

    pub fn set_base(&mut self, base: Tmp) {
        self.base = base;
    }

    pub fn set_index(&mut self, index: Tmp) {
        self.index = index;
    }

    pub fn set_offset(&mut self, offset: i64) {
        self.offset = offset;
    }

    pub fn set_kind_and_base(&mut self, kind: ArgKind, base: Tmp) {
        self.kind = kind;
        self.base = base;
    }

    pub fn is_tmp(&self) -> bool {
        self.kind == ArgKind::Tmp
    }

    pub fn is_imm(&self) -> bool {
        self.kind == ArgKind::Imm
    }

    pub fn is_big_imm(&self) -> bool {
        self.kind == ArgKind::BigImm
    }

    pub fn is_bit_imm(&self) -> bool {
        self.kind == ArgKind::BitImm
    }

    pub fn is_bit_imm64(&self) -> bool {
        self.kind == ArgKind::BitImm64
    }

    pub fn is_zero_reg(&self) -> bool {
        self.kind == ArgKind::ZeroReg
    }

    pub fn is_some_imm(&self) -> bool {
        self.is_imm() || self.is_big_imm() || self.is_bit_imm() || self.is_bit_imm64()
    }

    pub fn is_simple_addr(&self) -> bool {
        self.kind == ArgKind::SimpleAddr
    }

    pub fn is_addr(&self) -> bool {
        self.kind == ArgKind::Addr
    }

    pub fn is_stack(&self) -> bool {
        self.kind == ArgKind::Stack
    }

    pub fn is_call_arg(&self) -> bool {
        self.kind == ArgKind::CallArg
    }

    pub fn is_index(&self) -> bool {
        self.kind == ArgKind::Index
    }

    pub fn is_pre_index(&self) -> bool {
        self.kind == ArgKind::PreIndex
    }

    pub fn is_post_index(&self) -> bool {
        self.kind == ArgKind::PostIndex
    }

    pub fn is_memory(&self) -> bool {
        self.is_stack()
            || self.is_addr()
            || self.is_simple_addr()
            || self.is_index()
            || self.is_pre_index()
            || self.is_post_index()
    }

    pub fn is_rel_cond(&self) -> bool {
        self.kind == ArgKind::RelCond
    }

    pub fn is_res_cond(&self) -> bool {
        self.kind == ArgKind::ResCond
    }

    pub fn is_double_cond(&self) -> bool {
        self.kind == ArgKind::DoubleCond
    }

    pub fn is_status_cond(&self) -> bool {
        self.kind == ArgKind::StatusCond
    }

    pub fn is_condition(&self) -> bool {
        self.is_rel_cond() || self.is_res_cond() || self.is_double_cond() || self.is_status_cond()
    }

    pub fn is_special(&self) -> bool {
        self.kind == ArgKind::Special
    }

    pub fn is_width_arg(&self) -> bool {
        self.kind == ArgKind::WidthArg
    }

    pub fn is_alive(&self) -> bool {
        self.is_tmp() || self.is_stack()
    }

    pub fn is_simd_info(&self) -> bool {
        self.kind == ArgKind::SIMDInfo
    }

    pub fn tmp(&self) -> Tmp {
        assert!(self.is_tmp());
        self.base
    }

    pub fn value(&self) -> i64 {
        assert!(self.is_some_imm());
        self.offset
    }

    pub fn pointer_value(&self) -> *const u8 {
        assert!(self.is_some_imm());
        self.offset as *const u8
    }

    pub fn ptr(&self) -> Tmp {
        assert!(self.is_simple_addr());
        self.base
    }

    pub fn base(&self) -> Tmp {
        assert!(
            self.is_addr()
                || self.is_index()
                || self.is_pre_index()
                || self.is_post_index()
                || self.kind == ArgKind::ExtendedOffsetAddr
                || self.is_simple_addr()
        );
        self.base
    }

    pub fn has_offset(&self) -> bool {
        self.is_memory()
    }

    pub fn offset(&self) -> i64 {
        if self.kind() == ArgKind::Stack {
            return self.scale as _;
        }
        assert!(self.has_offset());
        self.offset
    }

    pub fn stack_slot(&self) -> StackSlotId {
        assert!(self.is_stack());
        // FIXME: This code is direct copy from the original code.
        // It is not a safe version, rewrite it to a safe version in future.

        // SAFETY: Safe because we check if the argument is a stack slot before we transmute it.
        StackSlotId(self.offset as _)
    }

    pub fn special(&self) -> SpecialId {
        assert!(self.is_special());

        SpecialId(self.offset as _)
    }

    pub fn index(&self) -> Tmp {
        assert!(self.kind == ArgKind::Index);
        self.index
    }

    pub fn scale(&self) -> usize {
        assert!(self.kind == ArgKind::Index);
        self.scale as usize
    }

    pub fn log_scale(&self) -> usize {
        assert!(self.kind == ArgKind::Index);
        log_scale(self.scale as usize)
    }

    pub fn width(&self) -> Width {
        assert!(self.is_width_arg());
        width_for_bytes(self.offset as _)
    }

    pub fn special_mut<'a>(&'a mut self) -> &'a mut Special {
        assert!(self.is_special());

        unsafe {
            // SAFETY: Safe because we check if the argument is a special before we transmute it.
            std::mem::transmute::<usize, _>(self.offset as usize)
        }
    }

    pub fn is_gp_tmp(&self) -> bool {
        self.is_tmp() && self.tmp().is_gp()
    }

    pub fn is_fp_tmp(&self) -> bool {
        self.is_tmp() && self.tmp().is_fp()
    }

    pub fn is_gp(&self) -> bool {
        if matches!(
            self.kind(),
            ArgKind::Imm
                | ArgKind::BigImm
                | ArgKind::BitImm
                | ArgKind::BitImm64
                | ArgKind::ZeroReg
                | ArgKind::SimpleAddr
                | ArgKind::Addr
                | ArgKind::ExtendedOffsetAddr
                | ArgKind::Index
                | ArgKind::PreIndex
                | ArgKind::PostIndex
                | ArgKind::Stack
                | ArgKind::CallArg
                | ArgKind::RelCond
                | ArgKind::ResCond
                | ArgKind::DoubleCond
                | ArgKind::StatusCond
                | ArgKind::Special
                | ArgKind::WidthArg
        ) {
            return true;
        } else if self.is_tmp() && self.is_gp_tmp() {
            return true;
        } else {
            false
        }
    }

    pub fn is_fp(&self) -> bool {
        if self.is_fp_tmp() {
            return true;
        }

        matches!(
            self.kind(),
            ArgKind::SimpleAddr
                | ArgKind::Addr
                | ArgKind::ExtendedOffsetAddr
                | ArgKind::Index
                | ArgKind::PreIndex
                | ArgKind::PostIndex
                | ArgKind::Stack
                | ArgKind::CallArg
                | ArgKind::BigImm // Yes, we allow BigImm as a double immediate. We use this for implementing stackmaps.
        )
    }

    pub fn has_bank(&self) -> bool {
        matches!(
            self.kind(),
            ArgKind::Imm | ArgKind::BitImm | ArgKind::BitImm64 | ArgKind::Special | ArgKind::Tmp
        )
    }

    pub fn bank(&self) -> Bank {
        if self.is_gp() {
            Bank::GP
        } else {
            Bank::FP
        }
    }

    pub fn is_compatible_bank(&self, other: &Self) -> bool {
        self.bank() == other.bank()
    }

    pub fn is_bank(&self, bank: Bank) -> bool {
        self.bank() == bank
    }

    pub fn is_gpr(&self) -> bool {
        self.is_tmp() && self.tmp().is_gpr()
    }

    pub fn gpr(&self) -> u8 {
        assert!(self.is_gpr());
        self.tmp().gpr()
    }

    pub fn is_fpr(&self) -> bool {
        self.is_tmp() && self.tmp().is_fpr()
    }

    pub fn fpr(&self) -> u8 {
        assert!(self.is_fpr(), "not fpr: {}", self);
        self.tmp().fpr()
    }

    pub fn is_reg(&self) -> bool {
        self.is_tmp() && self.tmp().is_reg()
    }

    pub fn reg(&self) -> Reg {
        assert!(self.is_reg());
        self.tmp().reg()
    }

    pub fn gp_tmp_index(&self) -> usize {
        assert!(self.is_gp_tmp());
        self.tmp().gp_tmp_index()
    }

    pub fn fp_tmp_index(&self) -> usize {
        assert!(self.is_fp_tmp());
        self.tmp().fp_tmp_index()
    }

    pub fn tmp_index(&self) -> usize {
        assert!(self.is_tmp());
        self.tmp().tmp_index()
    }

    pub fn as_imm32(&self) -> i32 {
        assert!(self.is_imm() || self.is_bit_imm());
        self.offset as _
    }

    pub fn as_imm64(&self) -> i64 {
        assert!(self.is_big_imm() || self.is_bit_imm64());
        self.offset as _
    }

    #[cfg(target_pointer_width = "32")]
    pub fn as_big_imm(&self) -> i32 {
        assert!(self.is_big_imm());
        self.offset as _
    }

    #[cfg(target_pointer_width = "64")]
    pub fn as_big_imm(&self) -> i64 {
        assert!(self.is_big_imm());
        self.offset as _
    }

    #[cfg(target_arch = "aarch64")]
    pub fn as_zero_reg(&self) -> u8 {
        zr
    }

    pub fn as_imm_ptr(&self) -> i64 {
        self.pointer_value() as _
    }

    pub fn as_address(&self) -> Address {
        if self.is_simple_addr() {
            return Address::new(self.base.gpr(), 0);
        }
        Address::new(self.base.gpr(), self.offset as _)
    }

    pub fn as_base_index(&self) -> BaseIndex {
        BaseIndex::new(
            self.base.gpr(),
            self.index.gpr(),
            unsafe { std::mem::transmute::<u8, Scale>(self.log_scale() as _) },
            self.offset as _,
            self.extend,
        )
    }

    pub fn as_pre_index_address(&self) -> PreIndexAddress {
        PreIndexAddress::new(self.base.gpr(), self.offset as _)
    }

    pub fn as_post_index_address(&self) -> Address {
        Address::new(self.base.gpr(), self.offset as _)
    }

    pub fn as_relational_condition(&self) -> RelationalCondition {
        unsafe { std::mem::transmute::<u8, RelationalCondition>(self.offset as _) }
    }

    pub fn as_result_condition(&self) -> ResultCondition {
        unsafe { std::mem::transmute::<u8, ResultCondition>(self.offset as _) }
    }

    pub fn as_double_condition(&self) -> DoubleCondition {
        unsafe { std::mem::transmute::<u8, DoubleCondition>(self.offset as _) }
    }

    pub fn as_status_condition(&self) -> StatusCondition {
        unsafe { std::mem::transmute::<u8, StatusCondition>(self.offset as _) }
    }

    pub fn is_invertible(&self) -> bool {
        match self.kind {
            ArgKind::RelCond | ArgKind::DoubleCond | ArgKind::StatusCond => true,
            ArgKind::ResCond => TargetMacroAssembler::is_invertible(self.as_result_condition()),
            _ => false,
        }
    }

    pub fn inverted(&self, inverted: bool) -> Self {
        if !inverted {
            return self.clone();
        }

        match self.kind {
            ArgKind::RelCond => {
                Self::new_rel_cond(TargetMacroAssembler::invert(self.as_relational_condition()))
            }
            ArgKind::ResCond => Self::new_res_cond(TargetMacroAssembler::invert_result(
                self.as_result_condition(),
            )),
            ArgKind::DoubleCond => {
                Self::new_double_cond(TargetMacroAssembler::invert_fp(self.as_double_condition()))
            }
            ArgKind::StatusCond => Self::new_status_cond(match self.as_status_condition() {
                StatusCondition::Failure => StatusCondition::Success,
                StatusCondition::Success => StatusCondition::Failure,
            }),
            _ => unreachable!(),
        }
    }

    pub fn flipped(&self, flipped: bool) -> Option<Self> {
        if !flipped {
            return None;
        }

        Some(Self::new_rel_cond(match self.as_relational_condition() {
            RelationalCondition::Above => RelationalCondition::Below,
            RelationalCondition::AboveOrEqual => RelationalCondition::BelowOrEqual,
            RelationalCondition::Below => RelationalCondition::Above,
            RelationalCondition::BelowOrEqual => RelationalCondition::AboveOrEqual,
            RelationalCondition::Equal => RelationalCondition::Equal,
            RelationalCondition::NotEqual => RelationalCondition::NotEqual,
            RelationalCondition::GreaterThan => RelationalCondition::LessThan,
            RelationalCondition::GreaterThanOrEqual => RelationalCondition::LessThanOrEqual,
            RelationalCondition::LessThan => RelationalCondition::GreaterThan,
            RelationalCondition::LessThanOrEqual => RelationalCondition::GreaterThanOrEqual,
        }))
    }

    pub fn is_signed(cond: RelationalCondition) -> bool {
        match cond {
            RelationalCondition::Equal
            | RelationalCondition::NotEqual
            | RelationalCondition::GreaterThan
            | RelationalCondition::GreaterThanOrEqual
            | RelationalCondition::LessThan
            | RelationalCondition::LessThanOrEqual => true,

            _ => false,
        }
    }

    pub fn is_unsigned(cond: RelationalCondition) -> bool {
        match cond {
            RelationalCondition::Equal
            | RelationalCondition::NotEqual
            | RelationalCondition::Above
            | RelationalCondition::AboveOrEqual
            | RelationalCondition::Below
            | RelationalCondition::BelowOrEqual => true,

            _ => false,
        }
    }

    pub fn is_signed_cond(&self) -> bool {
        self.is_rel_cond() && Self::is_signed(self.as_relational_condition())
    }

    pub fn is_unsigned_cond(&self) -> bool {
        self.is_rel_cond() && Self::is_unsigned(self.as_relational_condition())
    }
}

pub const fn log_scale(scale: usize) -> usize {
    match scale {
        1 => 0,
        2 => 1,
        4 => 2,
        8 => 3,
        16 => 4,

        _ => unreachable!(),
    }
}

pub fn is_representable_as(width: Width, signedness: ArgSignedness, value: i64) -> bool {
    match signedness {
        ArgSignedness::Signed => match width {
            Width::W8 => value as i8 as i64 == value,
            Width::W16 => value as i16 as i64 == value,
            Width::W32 => value as i32 as i64 == value,
            Width::W64 => true,
            _ => false,
        },

        ArgSignedness::Unsigned => match width {
            Width::W8 => value as u8 as i64 == value,
            Width::W16 => value as u16 as i64 == value,
            Width::W32 => value as u32 as i64 == value,
            Width::W64 => true,
            _ => false,
        },
    }
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ArgKind::Invalid => write!(f, "<invalid>"),
            ArgKind::Tmp => self.tmp().fmt(f),
            ArgKind::Imm => write!(f, "${}", self.offset),
            ArgKind::BigImm => write!(f, "${:#x}", self.offset),
            ArgKind::BitImm => write!(f, "${}", self.offset),
            ArgKind::BitImm64 => write!(f, "${:#x}", self.offset),
            ArgKind::ZeroReg => write!(f, "%xzr"),
            ArgKind::SimpleAddr => write!(f, "({})", self.base),
            ArgKind::ExtendedOffsetAddr | ArgKind::Addr => {
                if self.offset == 0 {
                    write!(f, "({})", self.base)
                } else {
                    write!(f, "{}({})", self.offset, self.base)
                }
            }

            ArgKind::Index => {
                if self.offset != 0 {
                    write!(f, "{}", self.offset)?;
                }
                write!(f, "({},{}", self.base, self.index)?;
                if self.scale() != 1 {
                    write!(f, ",{}", self.scale())?;
                }
                write!(f, ")")
            }

            ArgKind::Stack => {
                if self.offset != 0 {
                    write!(f, "{}", self.offset)?;
                }

                write!(f, "($stack{})", self.stack_slot().0)
            }

            ArgKind::CallArg => {
                if self.offset != 0 {
                    write!(f, "{}", self.offset)?;
                }

                write!(f, "(call_arg)")
            }

            ArgKind::RelCond => write!(f, "{:?}", self.as_relational_condition()),
            ArgKind::ResCond => write!(f, "{:?}", self.as_result_condition()),
            ArgKind::DoubleCond => write!(f, "{:?}", self.as_double_condition()),
            ArgKind::StatusCond => write!(f, "{:?}", self.as_status_condition()),
            ArgKind::Special => write!(f, "$special{}", self.special().0),
            ArgKind::WidthArg => write!(f, "{}", self.width()),
            _ => todo!(),
        }
    }
}
