use std::collections::HashMap;

use crate::air;
use crate::air::arg::{is_representable_as, ArgSignedness};
use crate::air::form_table::{is_arm64, is_valid_form, is_x86};
use crate::air::helpers::{move_for_type, relaxed_move_for_type};
use crate::air::insertion_set::InsertionSet;
use crate::air::kind::Kind;
use crate::air::opcode::Opcode as AirOpcode;
use crate::bank::Bank;
use crate::block::{blocks_in_pre_order, Frequency};
use crate::typ::TypeKind;
use crate::value::{Value, ValueData};
use crate::width::Width;
use crate::{
    air::{
        arg::{Arg, ArgKind},
        basic_block::BasicBlockId,
        code::Code,
        inst::Inst,
        tmp::Tmp,
    },
    bank::bank_for_type,
    block::BlockId,
    dominators::{Dominators, GraphNodeWorklist},
    jit::reg::Reg,
    opcode::Opcode,
    phi_children::PhiChildren,
    procedure::Procedure,
    typ::Type,
    use_counts::UseCounts,
    value::ValueId,
    variable::VariableId,
};
use macroassembler::assembler::macro_assembler_x86_common::{
    DoubleCondition, RelationalCondition, ResultCondition,
};
use macroassembler::jit::gpr_info::RETURN_VALUE_GPR;
use macroassembler::{
    assembler::abstract_macro_assembler::Extend, jit::gpr_info::CALL_FRAME_REGISTER,
};

/// This lowers the current B3 procedure to an Air code.
pub fn lower_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    let code = Code::new(proc);

    let mut lower_to_air = LowerToAir::new(code);
    lower_to_air.run();

    lower_to_air.code
}

macro_rules! opcode_for_width {
    ($opcode: ident, $width: expr) => {
        match $width {
            Width::W8 => paste::paste! { AirOpcode::[< $opcode 8>] },
            Width::W16 => paste::paste! { AirOpcode::[< $opcode 16>] },
            Width::W32 => paste::paste! { AirOpcode::[< $opcode 32>] },
            _ => paste::paste! { AirOpcode::[< $opcode 64>] },
        }
    };
}

#[allow(dead_code)]
struct MoveConditionallyConfig {
    move_conditionally32: AirOpcode,
    move_conditionally64: AirOpcode,
    move_conditionally_test32: AirOpcode,
    move_conditionally_test64: AirOpcode,
    move_conditionally_double: AirOpcode,
    move_conditionally_float: AirOpcode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AddrRequestMode {
    NoRestriction,
    PreferSimpleAddr,
}

struct LowerToAir<'a> {
    locked: Vec<ValueId>,
    value_to_tmp: Vec<Tmp>,
    phi_to_tmp: Vec<Tmp>,
    block_to_block: Vec<BasicBlockId>,
    variable_to_tmps: HashMap<VariableId, Vec<Tmp>>,
    phi_children: PhiChildren,
    use_counts: UseCounts,
    dominators: Dominators<Procedure>,
    code: Code<'a>,
    fast_worklist: GraphNodeWorklist<BlockId>,
    insts: Vec<Vec<Inst>>,
    prologue: Vec<Inst>,

    block: BlockId,
    is_rare: bool,
    index: usize,
    value: ValueId,

    eax: Tmp,
    ecx: Tmp,
    edx: Tmp,
}

use macroassembler::assembler::x86assembler::*;

#[allow(dead_code)]
impl<'a> LowerToAir<'a> {
    pub fn new(code: Code<'a>) -> Self {
        code.proc.dominators_or_compute();
        Self {
            value_to_tmp: vec![Tmp::empty(); code.proc.values.size()],
            phi_to_tmp: vec![Tmp::empty(); code.proc.values.size()],
            block_to_block: vec![BasicBlockId(0); code.proc.blocks.len()],
            use_counts: UseCounts::new(code.proc),
            dominators: code.proc.dominators().clone(),
            phi_children: PhiChildren::new(code.proc),
            variable_to_tmps: HashMap::new(),
            code,
            fast_worklist: GraphNodeWorklist::new(),
            insts: vec![],
            prologue: vec![],
            block: BlockId(0),
            is_rare: false,
            index: 0,
            value: ValueId(usize::MAX),
            eax: Tmp::from_reg(Reg::new_gpr(eax)),
            ecx: Tmp::from_reg(Reg::new_gpr(ecx)),
            edx: Tmp::from_reg(Reg::new_gpr(edx)),
            locked: vec![],
        }
    }

    pub fn run(&mut self) {
        for block in (0..self.code.proc.blocks.len()).map(BlockId) {
            self.block_to_block[block.0] =
                self.code.add_block(self.code.proc.block(block).frequency);
        }

        for value in (0..self.code.proc.values.size()).map(ValueId) {
            match self.code.proc.value(value).kind.opcode() {
                Opcode::Phi => {
                    let temp = self.code.new_tmp(self.code.proc.value(value).result_bank());
                    self.phi_to_tmp[value.0] = temp;
                }

                _ => continue,
            }
        }

        for i in 0..self.code.proc.variables.size() {
            let variable = self.code.proc.variable(VariableId(i));
            let index = variable.index();
            let typ = variable.typ;
            let tmp = self.tmp_for_type(typ);
            self.variable_to_tmps.insert(VariableId(index), vec![tmp]);
        }

        self.fast_worklist.push(BlockId(0));

        while let Some(block) = self.fast_worklist.pop() {
            for i in 0..self.code.proc.block(block).successor_list.len() {
                let successor = self.code.proc.block(block).successor_list[i];
                if successor.1 != Frequency::Rare {
                    self.fast_worklist.push(successor.0);
                }
            }
        }

        self.code.proc.reset_value_owners();

        for block in blocks_in_pre_order(BlockId(0), &self.code.proc) {
            self.block = block;
            self.is_rare = !self.fast_worklist.saw(block);

            for i in 0..self.code.proc.block(block).successor_list.len() {
                let successor = self.code.proc.block(block).successor_list[i];
                let successor_to_block = self.block_to_block[successor.0 .0];
                let to_block = self.block_to_block[block.0];
                self.code
                    .block_mut(to_block)
                    .successors
                    .push((successor_to_block, successor.1));
            }

            for i in (0..self.code.proc.block(block).len()).rev() {
                self.index = i;
                self.value = self.code.proc.block(block)[i];

                if self.locked.contains(&self.value) {
                    continue;
                }

                self.insts.push(vec![]);

                self.lower();
            }

            let target = self.block_to_block[block.0];
            self.finish_appending_instructions(target);
        }
        let mut insertion_set = InsertionSet::new();
        for inst in self.prologue.drain(..) {
            insertion_set.insert_inst(0, inst);
            //self.code.block_mut(BasicBlockId(0)).insts.insert(0, inst);
        }

        insertion_set.execute(&mut self.code, BasicBlockId(0));
    }

    pub fn tmp(&mut self, mut value: ValueId) -> Tmp {
        let mut tmp = self.value_to_tmp[value.0];

        if tmp == Tmp::empty() {
            while self.should_copy_propagate(value) {
                value = self.code.proc.value(value).children[0];
            }

            if self.code.proc.value(value).kind.opcode() == Opcode::FramePointer {
                return Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER));
            }

            let real_tmp = self.value_to_tmp[value.0];

            if real_tmp == Tmp::empty() {
                self.value_to_tmp[value.0] =
                    self.code.new_tmp(self.code.proc.value(value).result_bank());
            }

            tmp = self.value_to_tmp[value.0];
        }

        tmp
    }

    fn tmp_promise(&mut self, value: Option<ValueId>) -> ArgPromise {
        ArgPromise::new(Arg::default(), value)
    }

    fn tmp_for_type(&mut self, typ: Type) -> Tmp {
        self.code.new_tmp(bank_for_type(typ))
    }

    fn can_be_internal(&self, value: ValueId) -> bool {
        // If one of the internal things has already been computed, then we don't want to cause
        // it to be recomputed again.
        if self.value_to_tmp[value.0] != Tmp::empty() {
            return false;
        }

        // We require internals to have only one use - us. It's not clear if this should be numUses() or
        // numUsingInstructions(). Ideally, it would be numUsingInstructions(), except that it's not clear
        // if we'd actually do the right thing when matching over such a DAG pattern. For now, it simply
        // doesn't matter because we don't implement patterns that would trigger this.
        if self.use_counts.num_uses(value) != 1 {
            return false;
        }

        true
    }

    fn commit_internal(&mut self, value: Option<ValueId>) {
        if let Some(value) = value {
            if self.locked.contains(&value) {
                return;
            }

            self.locked.push(value);
        }
    }

    fn crosses_interference(&self, value: ValueId) -> bool {
        // If it's in a foreign block, then be conservative. We could handle this if we were
        // willing to do heavier analysis. For example, if we had liveness, then we could label
        // values as "crossing interference" if they interfere with anything that they are live
        // across. But, it's not clear how useful this would be.
        if self.code.proc.value(value).owner != self.code.proc.value(self.value).owner {
            return true;
        }

        let effects = self.code.proc.value(value).effects();

        for i in (0..self.index).rev() {
            let other_value = self.code.proc.block(self.block)[i];

            if other_value == value {
                return false;
            }

            if effects.interferes(&self.code.proc.value(other_value).effects()) {
                return true;
            }
        }

        unreachable!()
    }


    #[allow(dead_code)]
    fn is_mergeable_value(&self, v: ValueId, opcode: Opcode, check_can_be_internal: bool) -> bool {
        if self.code.proc.value(v).kind.opcode() != opcode {
            return false;
        }

        if check_can_be_internal && self.can_be_internal(v) {
            return false;
        }

        if self.locked.contains(&self.code.proc.value(v).children[0]) {
            return false;
        }

        true
    }

    fn index_arg(&mut self, base: Tmp, index: ValueId, scale: usize, offset: isize) -> Arg {
        let index = self.tmp(index);
        Arg::new_index(base, index, scale, offset, Extend::None)
    }

    fn scale_for_shl(
        &self,
        opcode: AirOpcode,
        shl: ValueId,
        offset: isize,
        width: Option<Width>,
    ) -> Option<usize> {
        if self.code.proc.value(shl).kind.opcode() != Opcode::Shl {
            return None;
        }

        if !self
            .code
            .proc
            .value(self.code.proc.value(shl).children[0])
            .has_int32()
        {
            return None;
        }

        let mut log_scale = self
            .code
            .proc
            .value(self.code.proc.value(shl).children[0])
            .as_int32()
            .unwrap();

        if self.code.proc.value(shl).typ.kind() == TypeKind::Int32 {
            log_scale &= 31;
        } else {
            log_scale &= 63;
        }

        let big_scale = 1u64 << log_scale as u64;

        if big_scale as i32 as u64 != big_scale {
            return None;
        }

        let scale = big_scale as i32 as usize;

        if !Arg::is_valid_index_form(opcode, scale as _, offset as _, width) {
            return None;
        }

        Some(scale)
    }

    pub fn effective_addr(&mut self, address: ValueId, offset: isize, width: Width) -> Arg {
        let fallback = |this: &mut Self| {
            let address = this.tmp(address);
            Arg::new_addr(address, offset as _)
        };

        match self.code.proc.value(address).kind.opcode() {
            Opcode::Add => {
                let left = self.code.proc.value(address).children[0];
                let right = self.code.proc.value(address).children[1];

                let try_index = |this: &mut Self, index, base| {
                    let scale = this.scale_for_shl(AirOpcode::Move, index, offset, Some(width));

                    if let Some(scale) = scale {
                        if this
                            .locked
                            .contains(&this.code.proc.value(index).children[0])
                            || this.locked.contains(&base)
                        {
                            return None;
                        }
                        let base = this.tmp(base);
                        Some(this.index_arg(
                            base,
                            this.code.proc.value(index).children[0],
                            scale,
                            offset,
                        ))
                    } else {
                        None
                    }
                };

                if let Some(result) = try_index(self, left, right) {
                    return result;
                }

                if let Some(result) = try_index(self, right, left) {
                    return result;
                }

                if self.locked.contains(&left)
                    || self.locked.contains(&right)
                    || !Arg::is_valid_index_form(AirOpcode::Move, 1, offset as _, Some(width))
                {
                    return fallback(self);
                }

                let left = self.tmp(left);

                return self.index_arg(left, right, 1, offset);
            }

            Opcode::Shl => {
                let left = self.code.proc.value(address).children[0];

                if self.locked.contains(&left)
                    || self.child(left, 0).as_int32() != Some(1)
                    || !Arg::is_valid_index_form(AirOpcode::Move, 1, offset as _, Some(width))
                {
                    return fallback(self);
                }

                let left_tmp = self.tmp(left);

                return self.index_arg(left_tmp, left, 1, offset);
            }

            Opcode::FramePointer => {
                return Arg::new_addr(
                    Tmp::from_reg(Reg::new_gpr(CALL_FRAME_REGISTER)),
                    offset as _,
                );
            }

            Opcode::SlotBase => {
                Arg::new_stack(self.value(address).slot_base_value().unwrap(), offset as _)
            }

            _ => fallback(self),
        }
    }

    // This gives you the address of the given Load or Store. If it's not a Load or Store, then
    // it returns Arg().
    fn addr(&mut self, memory_value: ValueId, mode: AddrRequestMode) -> Option<Arg> {
        if let ValueData::MemoryValue {
            offset,
            ref range,
            ref fence_range,
        } = self.value(memory_value).data
        {
            let offset = offset;
            let _range = range.clone();
            let _fence_range = fence_range.clone();

            let width = self.value(memory_value).access_width(self.code.proc);

            if mode == AddrRequestMode::PreferSimpleAddr {
                if offset == 0 {
                    return Some(Arg::new_simple_addr(
                        self.tmp(self.value(memory_value).children.last().copied().unwrap()),
                        0,
                    ));
                }
            }

            Some(self.effective_addr(
                self.value(memory_value).children.last().copied().unwrap(),
                offset as _,
                width,
            ))
        } else {
            None
        }
    }

    fn load_promise_any_opcode(
        &mut self,
        load_value: ValueId,
        mode: AddrRequestMode,
    ) -> ArgPromise {
        if !self.can_be_internal(load_value) {
            return ArgPromise::new(Arg::default(), None);
        }

        if self.crosses_interference(load_value) {
            return ArgPromise::new(Arg::default(), None);
        }

        let load_addr = self.addr(load_value, mode);

        let mut result = ArgPromise::new(load_addr.unwrap(), Some(load_value));

        if self.value(load_value).kind.traps() {
            result.set_traps();
        }

        result
    }

    fn load_promise(
        &mut self,
        load_value: ValueId,
        load_opcode: Opcode,
        mode: AddrRequestMode,
    ) -> ArgPromise {
        if self.value(load_value).kind.opcode() != load_opcode {
            return ArgPromise::new(Arg::default(), None);
        }

        self.load_promise_any_opcode(load_value, mode)
    }

    fn load_promise_default(&mut self, load_value: ValueId) -> ArgPromise {
        self.load_promise(load_value, Opcode::Load, AddrRequestMode::NoRestriction)
    }

    fn imm(&self, int_value: i64) -> Option<Arg> {
        if Arg::is_valid_imm_form(int_value) {
            Some(Arg::new_imm(int_value))
        } else {
            None
        }
    }

    fn imm_from_value(&self, value: ValueId) -> Option<Arg> {
        if let Some(int_value) = self.value(value).as_int() {
            self.imm(int_value)
        } else {
            None
        }
    }

    fn bit_imm(&self, value: ValueId) -> Option<Arg> {
        if let Some(int_value) = self.value(value).as_int() {
            if Arg::is_valid_bit_imm_form(int_value) {
                return Some(Arg::new_bitimm(int_value));
            }
        }

        None
    }

    fn bit_imm64(&self, value: ValueId) -> Option<Arg> {
        if let Some(int_value) = self.value(value).as_int() {
            if Arg::is_valid_bit_imm64_form(int_value) {
                return Some(Arg::new_bitimm64(int_value));
            }
        }

        None
    }

    fn zero_reg(&self) -> Arg {
        Arg::zero_reg()
    }

    fn imm_or_tmp_or_zero_reg(&mut self, value: ValueId) -> Arg {
        if let Some(arg) = self.imm_from_value(value) {
            arg
        } else if self.value(value).as_int() == Some(0) && cfg!(target_arch = "aarch64") {
            self.zero_reg()
        } else {
            Arg::new_tmp(self.tmp(value))
        }
    }

    fn imm_or_tmp(&mut self, value: ValueId) -> Arg {
        if let Some(arg) = self.imm_from_value(value) {
            arg
        } else {
            Arg::new_tmp(self.tmp(value))
        }
    }

    fn for_each_imm_or_tmp_or_zero_reg(
        &mut self,
        value: ValueId,
        mut func: impl FnMut(&mut Self, Arg, Type, usize),
    ) {
        let typ = self.value(value).typ();
        let tmp = self.imm_or_tmp_or_zero_reg(value);
        func(self, tmp, typ, 0);
    }

    fn append(&mut self, opcode: impl Into<air::kind::Kind>, args: &[Arg]) {
        self.insts
            .last_mut()
            .unwrap()
            .push(Inst::new(opcode.into(), self.value, args))
    }

    fn append_inst(&mut self, inst: Inst) {
        self.insts.last_mut().unwrap().push(inst)
    }

    fn move_to_tmp(&mut self, opcode: AirOpcode, source: Arg, dest: Tmp) {
        match opcode {
            AirOpcode::Move => {}

            AirOpcode::MoveFloat => {
                if source.is_zero_reg() {
                    self.append(AirOpcode::MoveZeroToDouble, &[Arg::new_tmp(dest)]);
                    return;
                }
            }

            _ => unreachable!(),
        }

        self.append(opcode, &[source, Arg::new_tmp(dest)]);
    }

    fn try_opcode_for_type(
        &self,
        opcode32: AirOpcode,
        opcode64: AirOpcode,
        opcode_double: AirOpcode,
        opcode_float: AirOpcode,
        typ: Type,
    ) -> AirOpcode {
        match typ.kind() {
            TypeKind::Int32 => opcode32,
            TypeKind::Int64 => opcode64,
            TypeKind::Float => opcode_float,
            TypeKind::Double => opcode_double,
            _ => AirOpcode::Oops,
        }
    }

    fn append_un_op<
        const OPCODE32: AirOpcode,
        const OPCODE64: AirOpcode,
        const OPCODE_FLOAT: AirOpcode,
        const OPCODE_DOUBLE: AirOpcode,
    >(
        &mut self,
        value: ValueId,
    ) {
        let opcode = self.try_opcode_for_type(
            OPCODE32,
            OPCODE64,
            OPCODE_DOUBLE,
            OPCODE_FLOAT,
            self.value(value).typ(),
        );

        assert_ne!(opcode, AirOpcode::Oops);

        let result = self.tmp(self.value);

        // Two operand forms like:
        //     Op a, b
        // mean something like:
        //     b = Op a

        let mut addr = self.load_promise_default(value);

        if is_valid_form(opcode, &[addr.kind(), ArgKind::Tmp]) {
            let arg = addr.consume(self);
            let inst = addr.inst(opcode, self.value, &[arg, Arg::new_tmp(result)]);
            self.append_inst(inst);
            return;
        }

        if is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Tmp]) {
            let tmp = self.tmp(value);
            let inst = addr.inst(
                opcode,
                self.value,
                &[Arg::new_tmp(tmp), Arg::new_tmp(result)],
            );
            self.append_inst(inst);
            return;
        }

        let val_tmp = self.tmp(value);
        self.append(
            relaxed_move_for_type(self.value(self.value).typ()),
            &[Arg::new_tmp(val_tmp), Arg::new_tmp(result)],
        );
        self.append(opcode, &[Arg::new_tmp(result)]);
    }

    
    fn opcode_based_on_shift_kind(
        &self,
        opcode: Opcode,
        shl32: AirOpcode,
        shl64: AirOpcode,
        sshr32: AirOpcode,
        sshr64: AirOpcode,
        zshr32: AirOpcode,
        zshr64: AirOpcode,
    ) -> AirOpcode {
        match opcode {
            Opcode::Shl => self.try_opcode_for_type(
                shl32,
                shl64,
                AirOpcode::Oops,
                AirOpcode::Oops,
                self.value(self.value).typ(),
            ),
            Opcode::SShr => self.try_opcode_for_type(
                sshr32,
                sshr64,
                AirOpcode::Oops,
                AirOpcode::Oops,
                self.value(self.value).typ(),
            ),
            Opcode::ZShr => self.try_opcode_for_type(
                zshr32,
                zshr64,
                AirOpcode::Oops,
                AirOpcode::Oops,
                self.value(self.value).typ(),
            ),
            _ => AirOpcode::Oops,
        }
    }

    /// Call this method when doing two-operand lowering of a commutative operation. You have a choice of
    /// which incoming Value is moved into the result. This will select which one is likely to be most
    /// profitable to use as the result. Doing the right thing can have big performance consequences in tight
    /// kernels.
    fn prepare_right_for_result(&mut self, left: ValueId, right: ValueId) -> bool {
        // The default is to move left into result, because that's required for non-commutative instructions.
        // The value that we want to move into result position is the one that dies here. So, if we're
        // compiling a commutative operation and we know that actually right is the one that dies right here,
        // then we can flip things around to help coalescing, which then kills the move instruction.
        //
        // But it's more complicated:
        // - Used-once is a bad estimate of whether the variable dies here.
        // - A child might be a candidate for coalescing with this value.
        //
        // Currently, we have machinery in place to recognize super obvious forms of the latter issue.

        // We recognize when a child is a Phi that has this value as one of its children. We're very
        // conservative about this; for example we don't even consider transitive Phi children.

        let left_is_phi_with_this = self
            .phi_children
            .at(left)
            .transitively_uses(self.value, self.code.proc);
        let right_is_phi_with_this = self
            .phi_children
            .at(right)
            .transitively_uses(self.value, self.code.proc);

        if left_is_phi_with_this != right_is_phi_with_this {
            return right_is_phi_with_this;
        }

        if self.use_counts.num_using_instructions(right) != 1 {
            return false;
        }

        if self.use_counts.num_using_instructions(left) != 1 {
            return true;
        }

        // The use count might be 1 if the variable is live around a loop. We can guarantee that we
        // pick the variable that is least likely to suffer this problem if we pick the one that
        // is closest to us in an idom walk. By convention, we slightly bias this in favor of
        // returning true.

        // We cannot prefer right if right is further away in an idom walk.

        if self.dominators.strictly_dominates(
            self.value(right).owner.unwrap(),
            self.value(left).owner.unwrap(),
        ) {
            return false;
        }

        true
    }

    fn append_bin_op<
        const OPCODE32: AirOpcode,
        const OPCODE64: AirOpcode,
        const OPCODE_FLOAT: AirOpcode,
        const OPCODE_DOUBLE: AirOpcode,
        const NOT_COMMUTATIVE: bool,
    >(
        &mut self,
        left: ValueId,
        right: ValueId,
    ) {
        let opcode = self.try_opcode_for_type(
            OPCODE32,
            OPCODE64,
            OPCODE_DOUBLE,
            OPCODE_FLOAT,
            self.value(left).typ(),
        );
        assert_ne!(opcode, AirOpcode::Oops);
        // Three-operand forms like:
        //     Op a, b, c
        // mean something like:
        //     c = a Op b

        let result = self.tmp(self.value);

        if is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Tmp, ArgKind::Tmp]) {
            if !NOT_COMMUTATIVE {
                if let Some(arg) = self.imm_from_value(right) {
                    let left_tmp = self.tmp(left);
                    self.append(opcode, &[arg, Arg::new_tmp(left_tmp), Arg::new_tmp(result)]);
                    return;
                }
            } else {
                if let Some(arg) = self.imm_from_value(left) {
                    let right_tmp = self.tmp(right);
                    self.append(
                        opcode,
                        &[arg, Arg::new_tmp(right_tmp), Arg::new_tmp(result)],
                    );
                    return;
                }
            }
        }

        if is_valid_form(opcode, &[ArgKind::BitImm, ArgKind::Tmp, ArgKind::Tmp]) {
            if !NOT_COMMUTATIVE {
                if let Some(right_arg) = self.bit_imm(right) {
                    let left_tmp = self.tmp(left);
                    self.append(
                        opcode,
                        &[right_arg, Arg::new_tmp(left_tmp), Arg::new_tmp(result)],
                    );
                    return;
                }
            } else {
                if let Some(left_arg) = self.bit_imm(left) {
                    let right_tmp = self.tmp(right);
                    self.append(
                        opcode,
                        &[left_arg, Arg::new_tmp(right_tmp), Arg::new_tmp(result)],
                    );
                    return;
                }
            }
        }

        if is_valid_form(opcode, &[ArgKind::BitImm64, ArgKind::Tmp, ArgKind::Tmp]) {
            if !NOT_COMMUTATIVE {
                if let Some(right_arg) = self.bit_imm64(right) {
                    let left_tmp = self.tmp(left);
                    self.append(
                        opcode,
                        &[right_arg, Arg::new_tmp(left_tmp), Arg::new_tmp(result)],
                    );
                    return;
                }
            } else {
                if let Some(left_arg) = self.bit_imm64(left) {
                    let right_tmp = self.tmp(right);
                    self.append(
                        opcode,
                        &[left_arg, Arg::new_tmp(right_tmp), Arg::new_tmp(result)],
                    );
                    return;
                }
            }
        }

        if let Some(imm) = self
            .imm_from_value(right)
            .filter(|_| is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Imm, ArgKind::Tmp]))
        {
            let left_tmp = self.tmp(left);
            self.append(opcode, &[Arg::new_tmp(left_tmp), imm, Arg::new_tmp(result)]);
            return;
        }

        // Note that no extant architecture has a three-operand form of binary operations that also
        // load from memory. If such an abomination did exist, we would handle it somewhere around
        // here.

        // Two-operand forms like:
        //     Op a, b
        // mean something like:
        //     b = b Op a

        // At this point, we prefer versions of the operation that have a fused load or an immediate
        // over three operand forms.

        if left != right {
            let mut left_addr = self.load_promise_default(left);

            if is_valid_form(opcode, &[left_addr.kind(), ArgKind::Tmp, ArgKind::Tmp]) {
                let left = left_addr.consume(self);
                let right_tmp = self.tmp(right);

                let inst = left_addr.inst(
                    opcode,
                    self.value,
                    &[left, Arg::new_tmp(right_tmp), Arg::new_tmp(result)],
                );
                self.append_inst(inst);
                return;
            }

            if !NOT_COMMUTATIVE {
                if is_valid_form(opcode, &[left_addr.kind(), ArgKind::Tmp]) {
                    let right_tmp = self.tmp(right);
                    self.append(
                        relaxed_move_for_type(self.value(self.value).typ()),
                        &[Arg::new_tmp(right_tmp), Arg::new_tmp(result)],
                    );
                    let left = left_addr.consume(self);
                    let inst = left_addr.inst(opcode, self.value, &[left, Arg::new_tmp(result)]);
                    self.append_inst(inst);

                    return;
                }
            }

            let mut right_addr = self.load_promise_default(right);

            if is_valid_form(opcode, &[ArgKind::Tmp, right_addr.kind(), ArgKind::Tmp]) {
                let left_tmp = self.tmp(left);
                let right = right_addr.consume(self);
                let inst = right_addr.inst(
                    opcode,
                    self.value,
                    &[Arg::new_tmp(left_tmp), right, Arg::new_tmp(result)],
                );
                self.append_inst(inst);
                return;
            }

            if !NOT_COMMUTATIVE {
                if is_valid_form(opcode, &[right_addr.kind(), ArgKind::Tmp, ArgKind::Tmp]) {
                    let left_tmp = self.tmp(left);
                    let right = right_addr.consume(self);
                    let inst = right_addr.inst(
                        opcode,
                        self.value,
                        &[right, Arg::new_tmp(left_tmp), Arg::new_tmp(result)],
                    );
                    self.append_inst(inst);
                    return;
                }
            }

            if is_valid_form(opcode, &[right_addr.kind(), ArgKind::Tmp]) {
                let left_tmp = self.tmp(left);
                self.append(
                    relaxed_move_for_type(self.value(self.value).typ()),
                    &[Arg::new_tmp(left_tmp), Arg::new_tmp(result)],
                );
                let right = right_addr.consume(self);
                let inst = right_addr.inst(opcode, self.value, &[right, Arg::new_tmp(result)]);
                self.append_inst(inst);
                return;
            }
        }

        if let Some(imm) = self
            .imm_from_value(right)
            .filter(|_| is_valid_form(opcode, &[ArgKind::Imm, ArgKind::Tmp]))
        {
            let left = self.tmp(left);
            self.append(
                relaxed_move_for_type(self.value(self.value).typ()),
                &[Arg::new_tmp(left), Arg::new_tmp(result)],
            );
            self.append(opcode, &[imm, Arg::new_tmp(result)]);
            return;
        }

        if is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Tmp, ArgKind::Tmp]) {
            let left = self.tmp(left);
            let right = self.tmp(right);

            self.append(
                opcode,
                &[
                    Arg::new_tmp(left),
                    Arg::new_tmp(right),
                    Arg::new_tmp(result),
                ],
            );
            return;
        }

        if !NOT_COMMUTATIVE && self.prepare_right_for_result(left, right) {
            let right = self.tmp(right);
            let left = self.tmp(left);

            self.append(
                relaxed_move_for_type(self.value(self.value).typ()),
                &[Arg::new_tmp(right), Arg::new_tmp(result)],
            );
            self.append(opcode, &[Arg::new_tmp(left), Arg::new_tmp(result)]);

            return;
        }

        let right = self.tmp(right);
        let left = self.tmp(left);

        self.append(
            relaxed_move_for_type(self.value(self.value).typ()),
            &[Arg::new_tmp(left), Arg::new_tmp(result)],
        );
        self.append(opcode, &[Arg::new_tmp(right), Arg::new_tmp(result)]);
    }

    fn append_bin_op_int<
        const OPCODE32: AirOpcode,
        const OPCODE64: AirOpcode,
        const NOT_COMMUTATIVE: bool,
    >(
        &mut self,
        left: ValueId,
        right: ValueId,
    ) {
        self.append_bin_op::<OPCODE32, OPCODE64, { AirOpcode::Oops }, { AirOpcode::Oops }, NOT_COMMUTATIVE>(left, right)
    }

    fn append_shift<const OPCODE32: AirOpcode, const OPCODE64: AirOpcode>(
        &mut self,
        value: ValueId,
        amount: ValueId,
    ) {
        let opcode = self.try_opcode_for_type(
            OPCODE32,
            OPCODE64,
            AirOpcode::Oops,
            AirOpcode::Oops,
            self.value(value).typ(),
        );

        assert_ne!(opcode, AirOpcode::Oops);

        if let Some(imm) = self.imm_from_value(amount) {
            if is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Imm, ArgKind::Tmp]) {
                let value = self.tmp(value);
                let result = self.tmp(self.value);
                self.append(opcode, &[Arg::new_tmp(value), imm, Arg::new_tmp(result)]);
                return;
            }

            if is_valid_form(opcode, &[ArgKind::Imm, ArgKind::Tmp]) {
                let value = self.tmp(value);
                let result = self.tmp(self.value);
                self.append(
                    AirOpcode::Move,
                    &[Arg::new_tmp(value), Arg::new_tmp(result)],
                );
                self.append(opcode, &[imm, Arg::new_tmp(result)]);
                return;
            }
        }

        if is_valid_form(opcode, &[ArgKind::Tmp, ArgKind::Tmp, ArgKind::Tmp]) {
            let value = self.tmp(value);
            let amount = self.tmp(amount);
            let result = self.tmp(self.value);
            self.append(
                opcode,
                &[
                    Arg::new_tmp(value),
                    Arg::new_tmp(amount),
                    Arg::new_tmp(result),
                ],
            );
            return;
        }

        let value = self.tmp(value);
        let amount = self.tmp(amount);
        let result = self.tmp(self.value);

        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(value), Arg::new_tmp(result)],
        );
        self.append(opcode, &[Arg::new_tmp(amount), Arg::new_tmp(self.ecx)]);
        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(self.ecx), Arg::new_tmp(result)],
        );
    }

    fn trapping_inst(&mut self, traps: bool, opcode: AirOpcode, args: &[Arg]) -> Inst {
        let mut inst = Inst::new(opcode.into(), self.value, args);
        inst.kind.effects |= traps;
        inst
    }

    fn trapping_inst2(&mut self, traps: bool, mut inst: Inst) -> Inst {
        inst.kind.effects |= traps;
        inst
    }

    fn try_append_store_unop<const OPCODE32: AirOpcode, const OPCODE64: AirOpcode>(
        &mut self,
        value: ValueId,
    ) -> bool {
        let opcode = self.try_opcode_for_type(
            OPCODE32,
            OPCODE64,
            AirOpcode::Oops,
            AirOpcode::Oops,
            self.value(value).typ(),
        );

        if opcode == AirOpcode::Oops {
            return false;
        }

        let store_addr = self
            .addr(self.value, AddrRequestMode::NoRestriction)
            .unwrap_or(Arg::default());

        let mut load_promise = self.load_promise_default(value);

        if load_promise.arg != store_addr {
            return false;
        }

        if !is_valid_form(opcode, &[store_addr.kind()]) {
            return false;
        }

        load_promise.consume(self);

        let traps = self.value(self.value).kind.traps();

        let inst = load_promise.inst(opcode, self.value, &[store_addr]);
        let inst = self.trapping_inst2(traps, inst);
        self.append_inst(inst);
        true
    }

    fn try_append_store_binop<
        const OPCODE32: AirOpcode,
        const OPCODE64: AirOpcode,
        const NOT_COMMUTATIVE: bool,
    >(
        &mut self,
        left: ValueId,
        right: ValueId,
    ) -> bool {
        assert!(self.value(self.value).memory_value().is_some());

        let opcode = self.try_opcode_for_type(
            OPCODE32,
            OPCODE64,
            AirOpcode::Oops,
            AirOpcode::Oops,
            self.value(left).typ(),
        );

        if self.value(self.value).memory_value().unwrap().1.start != 0 {
            return false;
        }

        let store_addr = self
            .addr(self.value, AddrRequestMode::NoRestriction)
            .unwrap();

        let get_load_promise = |this: &mut Self, load: ValueId| {
            match this.value(this.value).kind.opcode() {
                Opcode::Store => {
                    if this.value(load).kind.opcode() != Opcode::Load {
                        return None;
                    }
                }

                Opcode::Store8 => {
                    if this.value(load).kind.opcode() != Opcode::Load8Z
                        && this.value(load).kind.opcode() != Opcode::Load8S
                    {
                        return None;
                    }
                }

                Opcode::Store16 => {
                    if this.value(load).kind.opcode() != Opcode::Load16Z
                        && this.value(load).kind.opcode() != Opcode::Load16S
                    {
                        return None;
                    }
                }

                _ => return None,
            }

            Some(this.load_promise_any_opcode(load, AddrRequestMode::NoRestriction))
        };
        let other_value;
        let mut load_promise;
        match get_load_promise(self, left) {
            Some(load_promises) => {
                if load_promises.arg == store_addr {
                    load_promise = load_promises;
                    other_value = right;
                } else {
                    if !NOT_COMMUTATIVE {
                        let load_promises = get_load_promise(self, right);
                        if let Some(load_promises) = load_promises {
                            load_promise = load_promises;
                            if load_promise.arg == store_addr {
                                other_value = left;
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }
            None => {
                if !NOT_COMMUTATIVE {
                    let load_promises = get_load_promise(self, right);
                    if let Some(load_promises) = load_promises {
                        load_promise = load_promises;
                        if load_promise.arg == store_addr {
                            other_value = left;
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        };

        if is_valid_form(opcode, &[ArgKind::Imm, store_addr.kind()])
            && self.imm_from_value(other_value).is_some()
        {
            load_promise.consume(self);
            let imm = self.imm_from_value(other_value).unwrap();
            let inst = load_promise.inst(opcode, self.value, &[imm, store_addr]);
            self.append_inst(inst);
            return true;
        }

        if !is_valid_form(opcode, &[ArgKind::Tmp, store_addr.kind()]) {
            return false;
        }

        load_promise.consume(self);
        let other_tmp = self.tmp(other_value);
        let inst = load_promise.inst(opcode, self.value, &[Arg::new_tmp(other_tmp), store_addr]);
        self.append_inst(inst);
        true
    }

    fn create_store(
        &mut self,
        mov: crate::air::kind::Kind,
        value: ValueId,
        dest: &Arg,
    ) -> Option<Inst> {
        if let Some(imm_value) = self.imm_from_value(value) {
            if imm_value.value() == 0 {
                match mov.opcode {
                    AirOpcode::Move32 => {
                        if is_valid_form(AirOpcode::Store32, &[ArgKind::ZeroReg, dest.kind()])
                            && dest.is_valid_form(AirOpcode::Move, Some(Width::W32))
                        {
                            return Some(Inst::new(
                                AirOpcode::Store32.into(),
                                self.value,
                                &[self.zero_reg(), dest.clone()],
                            ));
                        }
                    }

                    AirOpcode::Move => {
                        if is_valid_form(AirOpcode::Store64, &[ArgKind::ZeroReg, dest.kind()])
                            && dest.is_valid_form(AirOpcode::Move, Some(Width::W64))
                        {
                            return Some(Inst::new(
                                AirOpcode::Store64.into(),
                                self.value,
                                &[self.zero_reg(), dest.clone()],
                            ));
                        }
                    }

                    _ => (),
                }
            }

            if is_valid_form(mov.opcode, &[ArgKind::Imm, dest.kind()]) {
                return Some(Inst::new(mov, self.value, &[imm_value, dest.clone()]));
            }
        }
        let tmp = self.tmp(value);

        Some(Inst::new(
            mov,
            self.value,
            &[Arg::new_tmp(tmp), dest.clone()],
        ))
    }

    fn store_opcode(&mut self, width: Width, bank: Bank) -> AirOpcode {
        match width {
            Width::W8 => {
                assert_eq!(bank, Bank::GP);

                AirOpcode::Store8
            }

            Width::W16 => {
                assert_eq!(bank, Bank::GP);

                AirOpcode::Store16
            }

            Width::W32 => match bank {
                Bank::GP => AirOpcode::Move32,
                Bank::FP => AirOpcode::MoveFloat,
            },

            Width::W64 => match bank {
                Bank::GP => AirOpcode::Move,
                Bank::FP => AirOpcode::MoveDouble,
            },

            Width::W128 => {
                assert_eq!(bank, Bank::FP);

                todo!()
            }
        }
    }

    fn append_store(&mut self, value: ValueId, dest: &Arg) {
        let (_offset, fence, _fence_range) = self.value(value).memory_value().unwrap();

        let mut kind;

        if fence.start != 0 {
            if is_x86() {
                kind = Kind {
                    opcode: opcode_for_width!(Xchg, self.value(value).access_width(self.code.proc)),
                    effects: true,
                };
                let swap_tmp = self.code.new_tmp(Bank::GP);

                let tmp = self.child_id(value, 0);
                let tmp = self.tmp(tmp);

                self.append(
                    relaxed_move_for_type(self.value(value).typ()),
                    &[Arg::new_tmp(tmp), Arg::new_tmp(swap_tmp)],
                );
                self.append(kind, &[Arg::new_tmp(swap_tmp), dest.clone()]);
                return;
            }

            kind = Kind {
                opcode: opcode_for_width!(StoreRel, self.value(value).access_width(self.code.proc)),
                effects: false,
            };
        } else {
            kind = self
                .store_opcode(
                    self.value(value).access_width(self.code.proc),
                    self.value(value).access_bank(self.code.proc),
                )
                .into();
        }

        kind.effects = self.value(value).kind.traps();

        let inst = self
            .create_store(kind, self.child_id(value, 0), dest)
            .unwrap();
        self.append_inst(inst);
    }

    fn finish_appending_instructions(&mut self, target: BasicBlockId) {
        for i in (0..self.insts.len()).rev() {
            for j in 0..self.insts[i].len() {
                let inst = self.insts[i][j].clone();

                self.code.block_mut(target).insts.push(inst);
            }
        }

        self.insts.truncate(0);
    }


    /// Create an Inst to do the comparison specified by the given value.
    fn create_generic_compare(
        &mut self,
        mut value: ValueId,
        mut compare: impl FnMut(&mut Self, Width, Arg, &mut ArgPromise, &mut ArgPromise) -> Option<Inst>,
        mut test: impl FnMut(&mut Self, Width, Arg, &mut ArgPromise, &mut ArgPromise) -> Option<Inst>,
        mut compare_double: impl FnMut(&mut Self, Arg, &mut ArgPromise, &mut ArgPromise) -> Option<Inst>,
        mut compare_float: impl FnMut(&mut Self, Arg, &mut ArgPromise, &mut ArgPromise) -> Option<Inst>,
        mut inverted: bool,
    ) -> Option<Inst> {
        // NOTE: This is totally happy to match comparisons that have already been computed elsewhere
        // since on most architectures, the cost of branching on a previously computed comparison
        // result is almost always higher than just doing another fused compare/branch. The only time
        // it could be worse is if we have a binary comparison and both operands are variables (not
        // constants), and we encounter register pressure. Even in this case, duplicating the compare
        // so that we can fuse it to the branch will be more efficient most of the time, since
        // register pressure is not *that* common. For this reason, this algorithm will always
        // duplicate the comparison.
        //
        // However, we cannot duplicate loads. The canBeInternal() on a load will assume that we
        // already validated canBeInternal() on all of the values that got us to the load. So, even
        // if we are sharing a value, we still need to call canBeInternal() for the purpose of
        // tracking whether we are still in good shape to fuse loads.
        //
        // We could even have a chain of compare values that we fuse, and any member of the chain
        // could be shared. Once any of them are shared, then the shared one's transitive children
        // cannot be locked (i.e. commitInternal()). But if none of them are shared, then we want to
        // lock all of them because that's a prerequisite to fusing the loads so that the loads don't
        // get duplicated. For example, we might have:
        //
        //     @tmp1 = LessThan(@a, @b)
        //     @tmp2 = Equal(@tmp1, 0)
        //     Branch(@tmp2)
        //
        // If either @a or @b are loads, then we want to have locked @tmp1 and @tmp2 so that they
        // don't emit the loads a second time. But if we had another use of @tmp2, then we cannot
        // lock @tmp1 (or @a or @b) because then we'll get into trouble when the other values that
        // try to share @tmp1 with us try to do their lowering.
        //
        // There's one more wrinkle. If we don't lock an internal value, then this internal value may
        // have already separately locked its children. So, if we're not locking a value then we need
        // to make sure that its children aren't locked. We encapsulate this in two ways:
        //
        // canCommitInternal: This variable tells us if the values that we've fused so far are
        // locked. This means that we're not sharing any of them with anyone. This permits us to fuse
        // loads. If it's false, then we cannot fuse loads and we also need to ensure that the
        // children of any values we try to fuse-by-sharing are not already locked. You don't have to
        // worry about the children locking thing if you use prepareToFuse() before trying to fuse a
        // sharable value. But, you do need to guard any load fusion by checking if canCommitInternal
        // is true.
        //
        // FusionResult prepareToFuse(value): Call this when you think that you would like to fuse
        // some value and that value is not a load. It will automatically handle the shared-or-locked
        // issues and it will clear canCommitInternal if necessary. This will return CannotFuse
        // (which acts like false) if the value cannot be locked and its children are locked. That's
        // rare, but you just need to make sure that you do smart things when this happens (i.e. just
        // use the value rather than trying to fuse it). After you call prepareToFuse(), you can
        // still change your mind about whether you will actually fuse the value. If you do fuse it,
        // you need to call commitFusion(value, fusionResult).
        //
        // commitFusion(value, fusionResult): Handles calling commitInternal(value) if fusionResult
        // is FuseAndCommit.

        let mut can_commit_internal = true;

        #[derive(Copy, Clone, PartialEq, Eq)]
        enum FusionResult {
            CannotFuse,
            FuseAndCommit,
            Fuse,
        }

        let mut prepare_to_fuse = |this: &mut Self, value: ValueId| -> FusionResult {
            if value == this.value {
                // It's not actually internal. It's the root value. We're good to go.
                return FusionResult::Fuse;
            }

            if can_commit_internal && this.can_be_internal(value) {
                // We are the only users of this value. This also means that the value's children
                // could not have been locked, since we have now proved that self.value dominates value
                // in the data flow graph. To only other way to value is from a user of m_value. If
                // value's children are shared with others, then they could not have been locked
                // because their use count is greater than 1. If they are only used from value, then
                // in order for value's children to be locked, value would also have to be locked,
                // and we just proved that it wasn't.

                return FusionResult::FuseAndCommit;
            }

            // We're going to try to share value with others. It's possible that some other basic
            // block had already emitted code for value and then matched over its children and then
            // locked them, in which case we just want to use value instead of duplicating it. So, we
            // validate the children. Note that this only arises in linear chains like:
            //
            //     BB#1:
            //         @1 = Foo(...)
            //         @2 = Bar(@1)
            //         Jump(#2)
            //     BB#2:
            //         @3 = Baz(@2)
            //
            // Notice how we could start by generating code for BB#1 and then decide to lock @1 when
            // generating code for @2, if we have some way of fusing Bar and Foo into a single
            // instruction. This is legal, since indeed @1 only has one user. The fact that @2 now
            // has a tmp (i.e. @2 is pinned), canBeInternal(@2) will return false, which brings us
            // here. In that case, we cannot match over @2 because then we'd hit a hazard if we end
            // up deciding not to fuse Foo into the fused Baz/Bar.
            //
            // Happily, there are only two places where this kind of child validation happens is in
            // rules that admit sharing, like this and effectiveAddress().
            //
            // N.B. We could probably avoid the need to do value locking if we committed to a well
            // chosen code generation order. For example, if we guaranteed that all of the users of
            // a value get generated before that value, then there's no way for the lowering of @3 to
            // see @1 locked. But we don't want to do that, since this is a greedy instruction
            // selector and so we want to be able to play with order.

            for child in this.value(value).children.iter() {
                if this.locked.contains(child) {
                    return FusionResult::CannotFuse;
                }
            }

            can_commit_internal = false;

            FusionResult::Fuse
        };

        let commit_fusion = |this: &mut Self, value: ValueId, result: FusionResult| {
            if result == FusionResult::FuseAndCommit {
                this.commit_internal(Some(value));
            }
        };

        loop {
            let should_invert = (self.value(value).kind.opcode() == Opcode::BitXor
                && self.child(value, 1).as_int() == Some(1)
                && self.child(value, 0).returns_bool(self.code.proc))
                || (self.value(value).kind.opcode() == Opcode::Equal
                    && self.child(value, 1).as_int() == Some(0));
            if !should_invert {
                break;
            }

            let fustion_result = prepare_to_fuse(self, value);

            if fustion_result == FusionResult::CannotFuse {
                break;
            }

            commit_fusion(self, value, fustion_result);

            value = self.child_id(value, 0);
            inverted = !inverted;
        }

        let mut create_rel_cond =
            |this: &mut Self,
             relational_condition: RelationalCondition,
             double_condition: DoubleCondition,
             can_commit_internal: &mut bool| {
                let rel_cond = Arg::new_rel_cond(relational_condition).inverted(inverted);
                let double_cond = Arg::new_double_cond(double_condition).inverted(inverted);

                let left = this.child_id(value, 0);
                let right = this.child_id(value, 1);

                if this.value(left).typ().is_int() {
                    let right_imm = this.imm_from_value(right);

                    let mut try_compare = |this: &mut Self,
                                           width: Width,
                                           left: &mut ArgPromise,
                                           right: &mut ArgPromise|
                     -> Option<Inst> {
                        if let Some(result) = compare(this, width, rel_cond.clone(), left, right) {
                            return Some(result);
                        }

                        if let Some(result) =
                            compare(this, width, rel_cond.flipped(true).unwrap(), right, left)
                        {
                            return Some(result);
                        }

                        None
                    };

                    let mut try_compare_load_imm = |this: &mut Self,
                                                    width: Width,
                                                    load_opcode: Opcode,
                                                    signedness: ArgSignedness|
                     -> Option<Inst> {
                        if let Some(ref right_imm) = right_imm {
                            if is_representable_as(width, signedness, right_imm.value()) {
                                let mut leftp = this.load_promise(
                                    left,
                                    load_opcode,
                                    AddrRequestMode::NoRestriction,
                                );
                                let mut right = ArgPromise::new(right_imm.clone(), None);
                                if let Some(result) =
                                    try_compare(this, width, &mut leftp, &mut right)
                                {
                                    this.commit_internal(Some(left));
                                    return Some(result);
                                }
                            }
                        }

                        None
                    };

                    let width = this.child(value, 0).result_width();
                    if *can_commit_internal {
                        if rel_cond.is_signed_cond() {
                            if let Some(result) = try_compare_load_imm(
                                this,
                                Width::W8,
                                Opcode::Load8S,
                                ArgSignedness::Signed,
                            ) {
                                return Some(result);
                            }
                        }

                        if rel_cond.is_unsigned_cond() {
                            if let Some(result) = try_compare_load_imm(
                                this,
                                Width::W8,
                                Opcode::Load8Z,
                                ArgSignedness::Unsigned,
                            ) {
                                return Some(result);
                            }
                        }

                        if rel_cond.is_signed_cond() {
                            if let Some(result) = try_compare_load_imm(
                                this,
                                Width::W16,
                                Opcode::Load16S,
                                ArgSignedness::Signed,
                            ) {
                                return Some(result);
                            }
                        }

                        if rel_cond.is_unsigned_cond() {
                            if let Some(result) = try_compare_load_imm(
                                this,
                                Width::W16,
                                Opcode::Load16Z,
                                ArgSignedness::Unsigned,
                            ) {
                                return Some(result);
                            }
                        }

                        if let Some(result) =
                            try_compare_load_imm(this, width, Opcode::Load, ArgSignedness::Signed)
                        {
                            return Some(result);
                        }

                        let mut leftp = this.load_promise_default(left);
                        let mut rightp = this.tmp_promise(Some(right));

                        if let Some(result) = try_compare(this, width, &mut leftp, &mut rightp) {
                            this.commit_internal(Some(left));

                            return Some(result);
                        }

                        let mut leftp = this.tmp_promise(Some(left));
                        let mut rightp = this.load_promise_default(right);

                        if let Some(result) = try_compare(this, width, &mut leftp, &mut rightp) {
                            this.commit_internal(Some(right));

                            return Some(result);
                        }
                    }

                    if let Some(ref right_imm) = right_imm {
                        let mut leftp = this.tmp_promise(Some(left));
                        let mut rightp = ArgPromise::new(right_imm.clone(), None);
                        if let Some(result) = try_compare(this, width, &mut leftp, &mut rightp) {
                            return Some(result);
                        }
                    }

                    let mut left_promise = this.tmp_promise(Some(left));
                    let mut right_promise = this.tmp_promise(Some(right));

                    return compare(this, width, rel_cond, &mut left_promise, &mut right_promise);
                }

                let mut left_promise = this.tmp_promise(Some(left));
                let mut right_promise = this.tmp_promise(Some(right));

                if this.child(value, 0).typ().kind() == TypeKind::Float {
                    return compare_float(this, double_cond, &mut left_promise, &mut right_promise);
                }

                return compare_double(this, double_cond, &mut left_promise, &mut right_promise);
            };

        let width = self.value(value).result_width();
        let res_cond = Arg::new_res_cond(ResultCondition::NotZero).inverted(inverted);

        let mut try_test =
            |this: &mut Self, width: Width, left: &mut ArgPromise, right: &mut ArgPromise| {
                if let Some(result) = test(this, width, res_cond.clone(), left, right) {
                    return Some(result);
                }

                if let Some(result) = test(this, width, res_cond.clone(), right, left) {
                    return Some(result);
                }

                None
            };

        let mut attemp_fused = |this: &mut Self, can_commit_internal: &mut bool| -> Option<Inst> {
            match this.value(value).kind.opcode() {
                Opcode::NotEqual => create_rel_cond(
                    this,
                    RelationalCondition::NotEqual,
                    DoubleCondition::NotEqualOrUnordered,
                    can_commit_internal,
                ),
                Opcode::Equal => create_rel_cond(
                    this,
                    RelationalCondition::Equal,
                    DoubleCondition::EqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::LessThan => create_rel_cond(
                    this,
                    RelationalCondition::LessThan,
                    DoubleCondition::LessThanAndOrdered,
                    can_commit_internal,
                ),
                Opcode::LessEqual => create_rel_cond(
                    this,
                    RelationalCondition::LessThanOrEqual,
                    DoubleCondition::LessThanOrEqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::GreaterThan => create_rel_cond(
                    this,
                    RelationalCondition::GreaterThan,
                    DoubleCondition::GreaterThanAndOrdered,
                    can_commit_internal,
                ),
                Opcode::GreaterEqual => create_rel_cond(
                    this,
                    RelationalCondition::GreaterThanOrEqual,
                    DoubleCondition::GreaterThanOrEqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::EqualOrUnordered => create_rel_cond(
                    this,
                    RelationalCondition::Equal,
                    DoubleCondition::EqualOrUnordered,
                    can_commit_internal,
                ),
                Opcode::Above => create_rel_cond(
                    this,
                    RelationalCondition::Above,
                    DoubleCondition::EqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::AboveEqual => create_rel_cond(
                    this,
                    RelationalCondition::AboveOrEqual,
                    DoubleCondition::EqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::Below => create_rel_cond(
                    this,
                    RelationalCondition::Below,
                    DoubleCondition::EqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::BelowEqual => create_rel_cond(
                    this,
                    RelationalCondition::BelowOrEqual,
                    DoubleCondition::EqualAndOrdered,
                    can_commit_internal,
                ),
                Opcode::BitAnd => {
                    let left = this.child_id(value, 0);
                    let right = this.child_id(value, 1);

                    let has_right_const;
                    let right_const;
                    let right_imm;
                    let right_imm64;

                    has_right_const = this.value(right).has_int();

                    if has_right_const {
                        right_const = this.value(right).as_int();
                        right_imm = this.bit_imm(right);
                        right_imm64 = this.bit_imm64(right);
                    } else {
                        right_const = None;
                        right_imm = None;
                        right_imm64 = None;
                    }

                    let mut try_test_load_imm = |this: &mut Self,
                                                 width: Width,
                                                 signedness: ArgSignedness,
                                                 load_opcode: Opcode|
                     -> Option<Inst> {
                        if !has_right_const {
                            return None;
                        }

                        if signedness == ArgSignedness::Signed
                            && !is_representable_as(
                                width,
                                ArgSignedness::Unsigned,
                                right_const.unwrap(),
                            )
                        {
                            return None;
                        }

                        if let Some(right_imm) = right_imm.clone() {
                            let mut leftp = this.load_promise(
                                left,
                                load_opcode,
                                AddrRequestMode::NoRestriction,
                            );

                            if let Some(result) = try_test(
                                this,
                                width,
                                &mut leftp,
                                &mut ArgPromise::new(right_imm, None),
                            ) {
                                this.commit_internal(Some(left));
                                return Some(result);
                            }
                        }

                        if let Some(right_imm64) = right_imm64.clone() {
                            let mut leftp = this.load_promise(
                                left,
                                load_opcode,
                                AddrRequestMode::NoRestriction,
                            );

                            if let Some(result) = try_test(
                                this,
                                width,
                                &mut leftp,
                                &mut ArgPromise::new(right_imm64, None),
                            ) {
                                this.commit_internal(Some(left));
                                return Some(result);
                            }
                        }

                        None
                    };

                    if *can_commit_internal {
                        if let Some(result) = try_test_load_imm(
                            this,
                            Width::W8,
                            ArgSignedness::Unsigned,
                            Opcode::Load8Z,
                        ) {
                            return Some(result);
                        }

                        if let Some(result) = try_test_load_imm(
                            this,
                            Width::W8,
                            ArgSignedness::Signed,
                            Opcode::Load8S,
                        ) {
                            return Some(result);
                        }

                        if let Some(result) = try_test_load_imm(
                            this,
                            Width::W16,
                            ArgSignedness::Unsigned,
                            Opcode::Load16Z,
                        ) {
                            return Some(result);
                        }

                        if let Some(result) = try_test_load_imm(
                            this,
                            Width::W16,
                            ArgSignedness::Signed,
                            Opcode::Load16S,
                        ) {
                            return Some(result);
                        }

                        // This allows us to use a 32-bit test for 64-bit BitAnd if the immediate is
                        // representable as an unsigned 32-bit value. The logic involved is the same
                        // as if we were pondering using a 32-bit test for
                        // BitAnd(SExt(Load(ptr)), const), in the sense that in both cases we have
                        // to worry about high bits. So, we use the "Signed" version of this helper.
                        if let Some(result) =
                            try_test_load_imm(this, Width::W32, ArgSignedness::Signed, Opcode::Load)
                        {
                            return Some(result);
                        }

                        if let Some(result) = try_test_load_imm(
                            this,
                            Width::W64,
                            ArgSignedness::Unsigned,
                            Opcode::Load,
                        ) {
                            return Some(result);
                        }

                        let width = this.value(left).result_width();

                        let mut leftp = this.load_promise_default(left);
                        let mut rightp = this.tmp_promise(Some(right));
                        if let Some(result) = try_test(this, width, &mut leftp, &mut rightp) {
                            this.commit_internal(Some(left));
                            return Some(result);
                        }

                        let mut leftp = this.tmp_promise(Some(left));
                        let mut rightp = this.load_promise_default(right);

                        if let Some(result) = try_test(this, width, &mut leftp, &mut rightp) {
                            this.commit_internal(Some(right));
                            return Some(result);
                        }
                    }

                    if has_right_const {
                        if (width == Width::W32 && right_const.unwrap() == 0xffffffff)
                            || (width == Width::W64 && right_const.unwrap() == -1)
                        {
                            let mut leftp = this.tmp_promise(Some(left));
                            let mut leftp2 = this.tmp_promise(Some(left));
                            if let Some(result) = try_test(this, width, &mut leftp, &mut leftp2) {
                                return Some(result);
                            }
                        }

                        if right_const.unwrap() as u32 as i64 == right_const.unwrap() {
                            let mut leftp = this.tmp_promise(Some(left));

                            if let Some(result) = try_test(
                                this,
                                Width::W32,
                                &mut leftp,
                                &mut ArgPromise::new(right_imm.clone().unwrap(), None),
                            ) {
                                return Some(result);
                            }

                            if let Some(result) = try_test(
                                this,
                                Width::W32,
                                &mut leftp,
                                &mut ArgPromise::new(right_imm64.clone().unwrap(), None),
                            ) {
                                return Some(result);
                            }
                        }

                        let mut leftp = this.tmp_promise(Some(left));

                        if let Some(result) = try_test(
                            this,
                            width,
                            &mut leftp,
                            &mut ArgPromise::new(right_imm.clone().unwrap(), None),
                        ) {
                            return Some(result);
                        }

                        if let Some(result) = try_test(
                            this,
                            width,
                            &mut leftp,
                            &mut ArgPromise::new(right_imm64.clone().unwrap(), None),
                        ) {
                            return Some(result);
                        }
                    }

                    let mut leftp = this.tmp_promise(Some(left));
                    let mut rightp = this.tmp_promise(Some(right));
                    try_test(this, width, &mut leftp, &mut rightp)
                }

                _ => None,
            }
        };

        let fusion_result = prepare_to_fuse(self, value);

        if fusion_result != FusionResult::CannotFuse {
            if let Some(result) = attemp_fused(self, &mut can_commit_internal) {
                commit_fusion(self, value, fusion_result);

                return Some(result);
            }
        }

        let mut left_promise = self.tmp_promise(Some(value));
        let mut right_promise = self.tmp_promise(Some(value));
        test(self, width, res_cond, &mut left_promise, &mut right_promise)
    }

    fn create_branch(&mut self, value: ValueId, inverted: bool) -> Option<Inst> {
        self.create_generic_compare(
            value,
            |this, width, rel_cond, left, right| match width {
                Width::W8 => {
                    if is_valid_form(
                        AirOpcode::Branch8,
                        &[ArgKind::RelCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let mut rinst =
                            right.inst(AirOpcode::Branch8, this.value, &[rel_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W16 => return None,

                Width::W32 => {
                    if is_valid_form(
                        AirOpcode::Branch32,
                        &[ArgKind::RelCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);

                        let mut rinst =
                            right.inst(AirOpcode::Branch32, this.value, &[rel_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W64 => {
                    if is_valid_form(
                        AirOpcode::Branch64,
                        &[ArgKind::RelCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let mut rinst =
                            right.inst(AirOpcode::Branch64, this.value, &[rel_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                _ => unreachable!(),
            },
            |this, width, res_cond, left, right| match width {
                Width::W8 => {
                    if is_valid_form(
                        AirOpcode::BranchTest8,
                        &[ArgKind::ResCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let mut rinst =
                            right.inst(AirOpcode::BranchTest8, this.value, &[res_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W16 => return None,

                Width::W32 => {
                    if is_valid_form(
                        AirOpcode::BranchTest32,
                        &[ArgKind::ResCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let mut rinst =
                            right.inst(AirOpcode::BranchTest32, this.value, &[res_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W64 => {
                    if is_valid_form(
                        AirOpcode::BranchTest64,
                        &[ArgKind::ResCond, left.kind(), right.kind()],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let mut rinst =
                            right.inst(AirOpcode::BranchTest64, this.value, &[res_cond, l, r]);
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                _ => unreachable!(),
            },
            |this, double_cond, left, right| {
                if is_valid_form(
                    AirOpcode::BranchDouble,
                    &[ArgKind::DoubleCond, left.kind(), right.kind()],
                ) {
                    let l = left.consume(this);
                    let r = right.consume(this);
                    let mut rinst =
                        right.inst(AirOpcode::BranchDouble, this.value, &[double_cond, l, r]);
                    rinst.kind.effects |= left.traps;
                    return Some(rinst);
                }

                None
            },
            |this, double_cond, left, right| {
                if is_valid_form(
                    AirOpcode::BranchFloat,
                    &[ArgKind::DoubleCond, left.kind(), right.kind()],
                ) {
                    let l = left.consume(this);
                    let r = right.consume(this);
                    let mut rinst =
                        right.inst(AirOpcode::BranchFloat, this.value, &[double_cond, l, r]);
                    rinst.kind.effects |= left.traps;
                    return Some(rinst);
                }

                None
            },
            inverted,
        )
    }

    fn create_compare(&mut self, value: ValueId, inverted: bool) -> Option<Inst> {
        self.create_generic_compare(
            value,
            |this, width, rel_cond, left, right| match width {
                Width::W8 => return None,

                Width::W16 => return None,

                Width::W32 => {
                    if is_valid_form(
                        AirOpcode::Compare32,
                        &[ArgKind::RelCond, left.kind(), right.kind(), ArgKind::Tmp],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let result = this.tmp(this.value);
                        let mut rinst = right.inst(
                            AirOpcode::Compare32,
                            this.value,
                            &[rel_cond, l, r, Arg::new_tmp(result)],
                        );
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W64 => {
                    if is_valid_form(
                        AirOpcode::Compare64,
                        &[ArgKind::RelCond, left.kind(), right.kind(), ArgKind::Tmp],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let result = this.tmp(this.value);
                        let mut rinst = right.inst(
                            AirOpcode::Compare64,
                            this.value,
                            &[rel_cond, l, r, Arg::new_tmp(result)],
                        );
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                _ => unreachable!(),
            },
            |this, width, res_cond, left, right| match width {
                Width::W8 => {
                    return None;
                }

                Width::W16 => return None,

                Width::W32 => {
                    if is_valid_form(
                        AirOpcode::Test32,
                        &[ArgKind::ResCond, left.kind(), right.kind(), ArgKind::Tmp],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let result = this.tmp(this.value);
                        let mut rinst = right.inst(
                            AirOpcode::Test32,
                            this.value,
                            &[res_cond, l, r, Arg::new_tmp(result)],
                        );
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                Width::W64 => {
                    if is_valid_form(
                        AirOpcode::Test64,
                        &[ArgKind::ResCond, left.kind(), right.kind(), ArgKind::Tmp],
                    ) {
                        let l = left.consume(this);
                        let r = right.consume(this);
                        let result = this.tmp(this.value);
                        let mut rinst = right.inst(
                            AirOpcode::Test64,
                            this.value,
                            &[res_cond, l, r, Arg::new_tmp(result)],
                        );
                        rinst.kind.effects |= left.traps;
                        return Some(rinst);
                    }

                    return None;
                }

                _ => unreachable!(),
            },
            |this, double_cond, left, right| {
                if is_valid_form(
                    AirOpcode::CompareDouble,
                    &[ArgKind::DoubleCond, left.kind(), right.kind(), ArgKind::Tmp],
                ) {
                    let l = left.consume(this);
                    let r = right.consume(this);
                    let result = this.tmp(this.value);
                    let mut rinst = right.inst(
                        AirOpcode::CompareDouble,
                        this.value,
                        &[double_cond, l, r, Arg::new_tmp(result)],
                    );
                    rinst.kind.effects |= left.traps;
                    return Some(rinst);
                }

                None
            },
            |this, double_cond, left, right| {
                if is_valid_form(
                    AirOpcode::CompareFloat,
                    &[ArgKind::DoubleCond, left.kind(), right.kind(), ArgKind::Tmp],
                ) {
                    let l = left.consume(this);
                    let r = right.consume(this);
                    let result = this.tmp(this.value);
                    let mut rinst = right.inst(
                        AirOpcode::CompareFloat,
                        this.value,
                        &[double_cond, l, r, Arg::new_tmp(result)],
                    );
                    rinst.kind.effects |= left.traps;
                    return Some(rinst);
                }

                None
            },
            inverted,
        )
    }

    fn create_select(&mut self, config: &MoveConditionallyConfig) -> Option<Inst> {
        let create_select_instruction =
            |this: &mut Self,
             opcode: AirOpcode,
             condition: Arg,
             left: &mut ArgPromise,
             right: &mut ArgPromise| {
                if is_valid_form(
                    opcode,
                    &[
                        condition.kind(),
                        left.kind(),
                        right.kind(),
                        ArgKind::Tmp,
                        ArgKind::Tmp,
                        ArgKind::Tmp,
                    ],
                ) {
                    let result = this.tmp(this.value);
                    let then_case = this.tmp(this.child_id(this.value, 1));
                    let else_case = this.tmp(this.child_id(this.value, 2));

                    let l = left.consume(this);
                    let r = right.consume(this);

                    let mut rinst = right.inst(
                        opcode,
                        this.value,
                        &[
                            condition,
                            l,
                            r,
                            Arg::new_tmp(then_case),
                            Arg::new_tmp(else_case),
                            Arg::new_tmp(result),
                        ],
                    );

                    rinst.kind.effects |= left.traps;

                    return Some(rinst);
                }

                if is_valid_form(
                    opcode,
                    &[
                        condition.kind(),
                        left.kind(),
                        right.kind(),
                        ArgKind::Tmp,
                        ArgKind::Tmp,
                    ],
                ) {
                    let result = this.tmp(this.value);
                    let source = this.tmp(this.child_id(this.value, 1));

                    let tmp = this.tmp(this.child_id(this.value, 2));
                    this.append(
                        relaxed_move_for_type(this.value(this.value).typ()),
                        &[Arg::new_tmp(tmp), Arg::new_tmp(result)],
                    );

                    let l = left.consume(this);
                    let r = right.consume(this);

                    let mut rinst = right.inst(
                        opcode,
                        this.value,
                        &[condition, l, r, Arg::new_tmp(source), Arg::new_tmp(result)],
                    );

                    rinst.kind.effects |= left.traps;

                    return Some(rinst);
                }

                None
            };

        self.create_generic_compare(
            self.child_id(self.value, 0),
            |this, width, rel_cond, left, right| {
                match width {
                    // FIXME: Add support for 8-bit and 16-bit select
                    Width::W8 => None,
                    Width::W16 => None,
                    Width::W32 => create_select_instruction(
                        this,
                        config.move_conditionally32,
                        rel_cond,
                        left,
                        right,
                    ),

                    Width::W64 => create_select_instruction(
                        this,
                        config.move_conditionally64,
                        rel_cond,
                        left,
                        right,
                    ),

                    _ => None,
                }
            },
            |this, width, res_cond, left, right| match width {
                Width::W8 => None,
                Width::W16 => None,
                Width::W32 => create_select_instruction(
                    this,
                    config.move_conditionally32,
                    res_cond,
                    left,
                    right,
                ),

                Width::W64 => create_select_instruction(
                    this,
                    config.move_conditionally64,
                    res_cond,
                    left,
                    right,
                ),

                _ => None,
            },
            |this, double_cond, left, right| {
                create_select_instruction(
                    this,
                    config.move_conditionally_double,
                    double_cond,
                    left,
                    right,
                )
            },
            |this, double_cond, left, right| {
                create_select_instruction(
                    this,
                    config.move_conditionally_float,
                    double_cond,
                    left,
                    right,
                )
            },
            false,
        )
    }

    fn try_append_lea(&mut self) -> bool {
        let lea_opcode = self.try_opcode_for_type(
            AirOpcode::Lea32,
            AirOpcode::Lea64,
            AirOpcode::Oops,
            AirOpcode::Oops,
            self.value(self.value).typ(),
        );
        if !is_valid_form(lea_opcode, &[ArgKind::Index, ArgKind::Tmp]) {
            return false;
        }
        // This lets us turn things like this:
        //
        //     Add(Add(@x, Shl(@y, $2)), $100)
        //
        // Into this:
        //
        //     lea 100(%rdi,%rsi,4), %rax
        //
        // We have a choice here between committing the internal bits of an index or sharing
        // them. There are solid arguments for both.
        //
        // Sharing: The word on the street is that the cost of a lea is one cycle no matter
        // what it does. Every experiment I've ever seen seems to confirm this. So, sharing
        // helps us in situations where Wasm input did this:
        //
        //     x = a[i].x;
        //     y = a[i].y;
        //
        // With sharing we would do:
        //
        //     leal (%a,%i,4), %tmp
        //     cmp (%size, %tmp)
        //     ja _fail
        //     movl (%base, %tmp), %x
        //     leal 4(%a,%i,4), %tmp
        //     cmp (%size, %tmp)
        //     ja _fail
        //     movl (%base, %tmp), %y
        //
        // In the absence of sharing, we may find ourselves needing separate registers for
        // the innards of the index. That's relatively unlikely to be a thing due to other
        // optimizations that we already have, but it could happen
        //
        // Committing: The worst case is that there is a complicated graph of additions and
        // shifts, where each value has multiple uses. In that case, it's better to compute
        // each one separately from the others since that way, each calculation will use a
        // relatively nearby tmp as its input. That seems uncommon, but in those cases,
        // committing is a clear winner: it would result in a simple interference graph
        // while sharing would result in a complex one. Interference sucks because it means
        // more time in IRC and it means worse code.
        //
        // It's not super clear if any of these corner cases would ever arise. Committing
        // has the benefit that it's easier to reason about, and protects a much darker
        // corner case (more interference).

        // Here are the things we want to match:
        // Add(Add(@x, @y), $c)
        // Add(Shl(@x, $c), @y)
        // Add(@x, Shl(@y, $c))
        // Add(Add(@x, Shl(@y, $c)), $d)
        // Add(Add(Shl(@x, $c), @y), $d)
        //
        // Note that if you do Add(Shl(@x, $c), $d) then we will treat $d as a non-constant and
        // force it to materialize. You'll get something like this:
        //
        // movl $d, %tmp
        // leal (%tmp,%x,1<<c), %result
        //
        // Which is pretty close to optimal and has the nice effect of being able to handle large
        // constants gracefully.

        let mut inner_add = None;
        let mut value = self.value;

        // We're going to consume Add(Add(_), $c). If we succeed at consuming it then we have these
        // patterns left (i.e. in the Add(_)):
        //
        // Add(Add(@x, @y), $c)
        // Add(Add(@x, Shl(@y, $c)), $d)
        // Add(Add(Shl(@x, $c), @y), $d)
        //
        // Otherwise we are looking at these patterns:
        //
        // Add(Shl(@x, $c), @y)
        // Add(@x, Shl(@y, $c))
        //
        // This means that the subsequent code only has to worry about three patterns:
        //
        // Add(Shl(@x, $c), @y)
        // Add(@x, Shl(@y, $c))
        // Add(@x, @y) (only if offset != 0)

        let mut offset = 0;

        if self.child(value, 1).as_int32().is_some()
            && self.can_be_internal(self.child_id(value, 1))
            && self.child(value, 0).kind.opcode() == Opcode::Add
        {
            inner_add = Some(self.child_id(value, 0));
            offset = self.child(inner_add.unwrap(), 1).as_int32().unwrap();
            value = self.child_id(value, 0);
        }

        let try_shl = |this: &mut Self, shl: ValueId, other: ValueId| -> bool {
            let scale = this.scale_for_shl(lea_opcode, shl, offset as _, None);

            if let Some(scale) = scale {
                if !this.can_be_internal(shl) {
                    return false;
                }

                let base = this.tmp(other);
                let index = this.tmp(this.child_id(shl, 0));
                let index_arg = Arg::new_index(base, index, scale as _, offset as _, Extend::None);
                let tmp = this.tmp(this.value);
                this.append(lea_opcode, &[index_arg, Arg::new_tmp(tmp)]);
                this.commit_internal(inner_add);
                this.commit_internal(Some(shl));
                true
            } else {
                false
            }
        };

        let left = self.child_id(value, 0);
        let right = self.child_id(value, 1);

        if try_shl(self, left, right) {
            return true;
        }

        if try_shl(self, right, left) {
            return true;
        }

        // The remaining pattern is just:
        // Add(@x, @y) (only if offset != 0)
        if offset == 0 {
            return false;
        }

        assert!(!self.locked.contains(&self.child_id(value, 0)));
        assert!(!self.locked.contains(&self.child_id(value, 1)));

        let base = self.tmp(self.child_id(value, 0));
        let index = self.tmp(self.child_id(value, 1));
        let index_arg = Arg::new_index(base, index, 1, offset as _, Extend::None);
        let tmp = self.tmp(self.value);

        self.append(lea_opcode, &[index_arg, Arg::new_tmp(tmp)]);
        self.commit_internal(inner_add);
        true
    }

    fn append_x86_div(&mut self, op: Opcode) {
        let convert_to_double_word;
        let div;

        match self.value(self.value).typ.kind() {
            TypeKind::Int32 => {
                convert_to_double_word = AirOpcode::X86ConvertToDoubleWord32;
                div = AirOpcode::X86Div32;
            }

            TypeKind::Int64 => {
                convert_to_double_word = AirOpcode::X86ConvertToQuadWord64;
                div = AirOpcode::X86Div64;
            }

            _ => unreachable!(),
        }

        assert!(op == Opcode::Div || op == Opcode::Mod);

        let result = if op == Opcode::Div {
            self.eax
        } else {
            self.edx
        };

        let tmp = self.tmp(self.child_id(self.value, 0));
        let tmp1 = self.tmp(self.child_id(self.value, 1));

        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(tmp), Arg::new_tmp(self.eax)],
        );
        self.append(
            convert_to_double_word,
            &[Arg::new_tmp(self.eax), Arg::new_tmp(self.edx)],
        );
        self.append(
            div,
            &[
                Arg::new_tmp(self.eax),
                Arg::new_tmp(self.edx),
                Arg::new_tmp(tmp1),
            ],
        );
        let value_tmp = self.tmp(self.value);
        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(result), Arg::new_tmp(value_tmp)],
        );
    }

    fn append_x86_udiv(&mut self, op: Opcode) {
        let div = match self.value(self.value).typ.kind() {
            TypeKind::Int32 => AirOpcode::X86UDiv32,
            TypeKind::Int64 => AirOpcode::X86UDiv64,
            _ => unreachable!(),
        };

        let result = if op == Opcode::UDiv {
            self.eax
        } else {
            self.edx
        };

        let tmp = self.tmp(self.child_id(self.value, 0));
        let tmp1 = self.tmp(self.child_id(self.value, 1));

        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(tmp), Arg::new_tmp(self.eax)],
        );
        self.append(AirOpcode::Move, &[Arg::new_imm(0), Arg::new_tmp(self.edx)]);
        self.append(
            div,
            &[
                Arg::new_tmp(self.eax),
                Arg::new_tmp(self.edx),
                Arg::new_tmp(tmp1),
            ],
        );
        let value_tmp = self.tmp(self.value);
        self.append(
            AirOpcode::Move,
            &[Arg::new_tmp(result), Arg::new_tmp(value_tmp)],
        );
    }

    fn try_append_bin_op_with_shift(
        &mut self,
        left: ValueId,
        right: ValueId,
        opcode: AirOpcode,
    ) -> bool {
        if !is_valid_form(
            opcode,
            &[ArgKind::Tmp, ArgKind::Tmp, ArgKind::Imm, ArgKind::Tmp],
        ) {
            return false;
        }

        if !self.can_be_internal(right)
            || self.imm_from_value(self.child_id(right, 1)).is_none()
            || self.child(right, 1).as_int().unwrap() < 0
        {
            return false;
        }

        let amount = self.child(right, 1).as_int().unwrap() as u64;
        let data_size = if self.value(left).typ().kind() == TypeKind::Int64 {
            64
        } else {
            32
        };

        if amount >= data_size {
            return false;
        }

        let tmp = self.tmp(left);
        let tmp1 = self.tmp(self.child_id(right, 0));
        let tmp2 = self.imm_from_value(self.child_id(right, 1)).unwrap();
        let tmp3 = self.tmp(self.value);

        self.append(
            opcode,
            &[
                Arg::new_tmp(tmp),
                Arg::new_tmp(tmp1),
                tmp2,
                Arg::new_tmp(tmp3),
            ],
        );

        self.commit_internal(Some(left));

        true
    }

    fn lower(&mut self) {
        match self.value(self.value).kind.opcode() {
            Opcode::Nop => (),
            Opcode::Load => {
                let memory = self.value(self.value).memory_value().unwrap();

                let mut kind: Kind = move_for_type(self.value(self.value).typ()).into();
                if memory.1.start != 0 {
                    if is_x86() {
                        kind.effects = true;
                    } else {
                        match self.value(self.value).typ().kind() {
                            TypeKind::Int32 => {
                                kind.opcode = AirOpcode::LoadAcq32;
                            }

                            TypeKind::Int64 => {
                                kind.opcode = AirOpcode::LoadAcq64;
                            }

                            _ => unreachable!(),
                        }
                    }
                }

                let addr = self
                    .addr(self.value, AddrRequestMode::NoRestriction)
                    .unwrap();
                let tmp = self.tmp(self.value);

                let mut inst = Inst::new(kind, self.value, &[addr, Arg::new_tmp(tmp)]);
                inst.kind.effects |= self.value(self.value).kind.traps();
                self.append_inst(inst);
                return;
            }

            Opcode::Load8S => {
                let mut kind: Kind = AirOpcode::Load8SignedExtendTo32.into();

                let memory = self.value(self.value).memory_value().unwrap();

                if memory.1.start != 0 {
                    if is_x86() {
                        kind.effects = true;
                    } else {
                        kind.opcode = AirOpcode::LoadAcq8SignedExtendTo32;
                    }
                }

                let addr = self
                    .addr(self.value, AddrRequestMode::NoRestriction)
                    .unwrap();
                let tmp = self.tmp(self.value);

                let mut inst = Inst::new(kind, self.value, &[addr, Arg::new_tmp(tmp)]);
                inst.kind.effects |= self.value(self.value).kind.traps();
                self.append_inst(inst);
                return;
            }

            Opcode::Load8Z => {
                let mut kind: Kind = AirOpcode::Load8.into();

                let memory = self.value(self.value).memory_value().unwrap();

                if memory.1.start != 0 {
                    if is_x86() {
                        kind.effects = true;
                    } else {
                        kind.opcode = AirOpcode::LoadAcq8;
                    }
                }

                let addr = self
                    .addr(self.value, AddrRequestMode::NoRestriction)
                    .unwrap();
                let tmp = self.tmp(self.value);

                let mut inst = Inst::new(kind, self.value, &[addr, Arg::new_tmp(tmp)]);
                inst.kind.effects |= self.value(self.value).kind.traps();
                self.append_inst(inst);
                return;
            }

            Opcode::Load16S => {
                let mut kind: Kind = AirOpcode::Load16SignedExtendTo32.into();

                let memory = self.value(self.value).memory_value().unwrap();

                if memory.1.start != 0 {
                    if is_x86() {
                        kind.effects = true;
                    } else {
                        kind.opcode = AirOpcode::LoadAcq16SignedExtendTo32;
                    }
                }

                let addr = self
                    .addr(self.value, AddrRequestMode::NoRestriction)
                    .unwrap();
                let tmp = self.tmp(self.value);

                let mut inst = Inst::new(kind, self.value, &[addr, Arg::new_tmp(tmp)]);
                inst.kind.effects |= self.value(self.value).kind.traps();
                self.append_inst(inst);
                return;
            }

            Opcode::Load16Z => {
                let mut kind: Kind = AirOpcode::Load16.into();

                let memory = self.value(self.value).memory_value().unwrap();

                if memory.1.start != 0 {
                    if is_x86() {
                        kind.effects = true;
                    } else {
                        kind.opcode = AirOpcode::LoadAcq16;
                    }
                }

                let addr = self
                    .addr(self.value, AddrRequestMode::NoRestriction)
                    .unwrap();
                let tmp = self.tmp(self.value);

                let mut inst = Inst::new(kind, self.value, &[addr, Arg::new_tmp(tmp)]);
                inst.kind.effects |= self.value(self.value).kind.traps();
                self.append_inst(inst);
                return;
            }

            Opcode::Add => {
                if self.try_append_lea() {
                    return;
                }

                let left = self.value(self.value).children[0];
                let right = self.value(self.value).children[1];
                self.append_bin_op::<{ AirOpcode::Add32 }, { AirOpcode::Add64}, { AirOpcode::AddFloat }, { AirOpcode::AddDouble }, false>(left, right);
            }

            Opcode::Sub => {
                let left = self.value(self.value).children[0];
                let right = self.value(self.value).children[1];
                self.append_bin_op::<{ AirOpcode::Sub32 }, { AirOpcode::Sub64}, { AirOpcode::SubFloat }, { AirOpcode::SubDouble }, false>(left, right);
            }

            Opcode::Neg => {
                let child = self.value(self.value).children[0];

                self.append_un_op::<{ AirOpcode::Neg32 }, { AirOpcode::Neg64}, { AirOpcode::NegateFloat }, { AirOpcode::NegateDouble }>(child);
            }

            Opcode::Mul => {
                let left = self.value(self.value).children[0];
                let right = self.value(self.value).children[1];
                self.append_bin_op::<{ AirOpcode::Mul32 }, { AirOpcode::Mul64}, { AirOpcode::MulFloat }, { AirOpcode::MulDouble }, false>(left, right);
            }

            Opcode::Div => {
                if self.value(self.value).typ().is_int() && is_x86() {
                    self.append_x86_div(Opcode::Div);
                    return;
                }

                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_bin_op::<{ AirOpcode::Div32 }, { AirOpcode::Div64}, { AirOpcode::DivFloat }, { AirOpcode::DivDouble }, false>(left, right);
            }

            Opcode::UDiv => {
                if self.value(self.value).typ().is_int() && is_x86() {
                    self.append_x86_div(Opcode::UDiv);
                    return;
                }

                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_bin_op::<{ AirOpcode::UDiv32 }, { AirOpcode::UDiv64}, { AirOpcode::Oops }, { AirOpcode::Oops }, false>(left, right);
            }

            Opcode::Mod => {
                assert!(is_x86());
                self.append_x86_div(Opcode::Mod);
            }

            Opcode::UMod => {
                assert!(is_x86());
                self.append_x86_udiv(Opcode::UMod);
            }

            Opcode::FMin => {
                let left = self.tmp(self.child_id(self.value, 0));
                let right = self.tmp(self.child_id(self.value, 1));
                let result = self.tmp(self.value);

                self.append(
                    if self.value(self.value).typ().kind() == TypeKind::Float {
                        AirOpcode::FloatMin
                    } else {
                        AirOpcode::DoubleMin
                    },
                    &[
                        Arg::new_tmp(left),
                        Arg::new_tmp(right),
                        Arg::new_tmp(result),
                    ],
                );
            }

            Opcode::FMax => {
                let left = self.tmp(self.child_id(self.value, 0));
                let right = self.tmp(self.child_id(self.value, 1));
                let result = self.tmp(self.value);

                self.append(
                    if self.value(self.value).typ().kind() == TypeKind::Float {
                        AirOpcode::FloatMax
                    } else {
                        AirOpcode::DoubleMax
                    },
                    &[
                        Arg::new_tmp(left),
                        Arg::new_tmp(right),
                        Arg::new_tmp(result),
                    ],
                );
            }

            Opcode::BitAnd => {
                let right = self.child_id(self.value, 0);
                let left = self.child_id(self.value, 1);

                if self.value(right).is_int_of(0xff) {
                    self.append_un_op::<{ AirOpcode::ZeroExtend8To32 }, { AirOpcode::ZeroExtend8To32 }, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
                    return;
                }

                if self.value(right).is_int_of(0xffff) {
                    self.append_un_op::<{ AirOpcode::ZeroExtend16To32 }, { AirOpcode::ZeroExtend16To32 }, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
                    return;
                }

                if self.value(right).is_int_of(0xffffffff) {
                    self.append_un_op::<{ AirOpcode::Move32 }, { AirOpcode::Move32 }, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
                    return;
                }

                self.append_bin_op::<{ AirOpcode::And32 }, { AirOpcode::And64}, { AirOpcode::AndFloat }, { AirOpcode::AndDouble }, false>(left, right);
            }

            Opcode::BitOr => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_bin_op::<{ AirOpcode::Or32 }, { AirOpcode::Or64}, { AirOpcode::OrFloat }, { AirOpcode::OrDouble }, false>(left, right);
            }

            Opcode::BitXor => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                if self.value(right).is_int_of(-1) {
                    self.append_un_op::<{ AirOpcode::Not32 }, { AirOpcode::Not64 }, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
                    return;
                }

                self.append_bin_op::<{ AirOpcode::Xor32 }, { AirOpcode::Xor64}, { AirOpcode::XorFloat }, { AirOpcode::XorDouble }, false>(left, right);
            }

            Opcode::Depend => {
                assert!(is_arm64());
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Depend32 }, { AirOpcode::Depend64 }, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
            }

            Opcode::Shl => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_shift::<{ AirOpcode::Lshift32 }, { AirOpcode::Lshift64 }>(left, right);
            }

            Opcode::SShr => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_shift::<{ AirOpcode::Rshift32 }, { AirOpcode::Rshift64 }>(left, right);
            }

            Opcode::ZShr => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_shift::<{ AirOpcode::Urshift32 }, { AirOpcode::Urshift64 }>(
                    left, right,
                );
            }

            Opcode::RotR => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_shift::<{ AirOpcode::RotateRight32 }, { AirOpcode::RotateRight64 }>(
                    left, right,
                );
            }

            Opcode::RotL => {
                let left = self.child_id(self.value, 0);
                let right = self.child_id(self.value, 1);

                self.append_shift::<{ AirOpcode::RotateLeft32 }, { AirOpcode::RotateLeft64 }>(
                    left, right,
                );
            }

            Opcode::Clz => {
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::CountLeadingZeros32 }, { AirOpcode::CountLeadingZeros64}, { AirOpcode::Oops }, { AirOpcode::Oops }>(left);
            }

            Opcode::Abs => {
                assert!(
                    !is_x86(),
                    "Abs is not supported natively for x86. It must be replaced before generation"
                );
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::AbsFloat }, { AirOpcode::AbsDouble }>(left);
            }

            Opcode::Ceil => {
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::CeilFloat }, { AirOpcode::CeilDouble }>(left);
            }

            Opcode::Floor => {
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::FloorFloat }, { AirOpcode::FloorDouble }>(left);
            }

            Opcode::Sqrt => {
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::SqrtFloat }, { AirOpcode::SqrtDouble }>(left);
            }

            Opcode::BitwiseCast => {
                let left = self.child_id(self.value, 0);

                self.append_un_op::<{ AirOpcode::Move32ToFloat }, { AirOpcode::Move64ToDouble }, { AirOpcode::MoveFloatTo32 }, { AirOpcode::MoveDoubleTo64 }>(left);
            }

            Opcode::Store => {
                let value_to_store = self.child_id(self.value, 0);

                if self.can_be_internal(value_to_store) {
                    let mut matched = false;

                    match self.value(value_to_store).kind.opcode() {
                        Opcode::Add => {
                            let left = self.child_id(value_to_store, 0);
                            let right = self.child_id(value_to_store, 1);
                            matched = self.try_append_store_binop::<{AirOpcode::Add32}, {AirOpcode::Add64}, false>(left, right);
                        }

                        Opcode::Sub => {
                            let left = self.child_id(value_to_store, 0);
                            let right = self.child_id(value_to_store, 1);

                            if self.value(left).is_int_of(0) {
                                matched = self.try_append_store_unop::<{AirOpcode::Neg32}, {AirOpcode::Neg64}>(right);
                            } else {
                                matched = self.try_append_store_binop::<{AirOpcode::Sub32}, {AirOpcode::Sub64}, false>(left, right);
                            }
                        }

                        Opcode::BitAnd => {
                            let left = self.child_id(value_to_store, 0);
                            let right = self.child_id(value_to_store, 1);
                            matched = self.try_append_store_binop::<{AirOpcode::And32}, {AirOpcode::And64}, false>(left, right);
                        }

                        Opcode::BitXor => {
                            let left = self.child_id(value_to_store, 0);
                            let right = self.child_id(value_to_store, 1);

                            if self.value(right).is_int_of(-1) {
                                matched = self.try_append_store_unop::<{AirOpcode::Not32}, {AirOpcode::Not64}>(left);
                            } else {
                                matched = self.try_append_store_binop::<{AirOpcode::Xor32}, {AirOpcode::Xor64}, false>(left, right);
                            }
                        }
                        _ => (),
                    }

                    if matched {
                        self.commit_internal(Some(value_to_store));
                        return;
                    }
                }

                let addr = self.addr(self.value, AddrRequestMode::NoRestriction);

                self.append_store(self.value, &addr.unwrap());
            }

            Opcode::Store8 => {
                let addr = self.addr(self.value, AddrRequestMode::NoRestriction);

                self.append_store(self.value, &addr.unwrap());
            }

            Opcode::Store16 => {
                let addr = self.addr(self.value, AddrRequestMode::NoRestriction);

                self.append_store(self.value, &addr.unwrap());
            }

            Opcode::Trunc => (),
            Opcode::SExt8 => {
                self.append_un_op::<{ AirOpcode::SignExtend8To32 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::SExt16 => {
                self.append_un_op::<{ AirOpcode::SignExtend16To32 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::SExt8To64 => {
                self.append_un_op::<{ AirOpcode::SignExtend8To64 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::SExt16To64 => {
                self.append_un_op::<{ AirOpcode::SignExtend16To64 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::ZExt32 => {
                self.append_un_op::<{ AirOpcode::Move32 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::SExt32 => {
                self.append_un_op::<{ AirOpcode::SignExtend32To64 }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::FloatToDouble => {
                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::ConvertFloatToDouble }, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::DoubleToFloat => {
                self.append_un_op::<{ AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::Oops }, { AirOpcode::ConvertDoubleToFloat }>(self.child_id(self.value, 0));
            }

            Opcode::ArgumentReg => {
                let result = self.tmp(self.value);
                let inst = Inst::new(
                    move_for_type(self.value(self.value).typ()).into(),
                    self.value,
                    &[
                        Arg::new_tmp(Tmp::from_reg(
                            self.value(self.value).argument_reg().unwrap(),
                        )),
                        Arg::new_tmp(result),
                    ],
                );

                self.prologue.push(inst);
            }

            Opcode::Const32 | Opcode::Const64 => {
                let result = self.tmp(self.value);
                if let Some(imm) = self.imm_from_value(self.value) {
                    self.append(AirOpcode::Move, &[imm, Arg::new_tmp(result)]);
                } else {
                    self.append(
                        AirOpcode::Move,
                        &[
                            Arg::new_bigimm(self.value(self.value).as_int().unwrap()),
                            Arg::new_tmp(result),
                        ],
                    );
                }
            }

            Opcode::ConstDouble | Opcode::ConstFloat => {
                unreachable!("move_constants phase should fuse these");
            }

            Opcode::FramePointer => (),
            Opcode::SlotBase => {
                let slot = self.value(self.value).slot_base_value().unwrap();
                let result = self.tmp(self.value);

                self.append(
                    AirOpcode::Lea64,
                    &[Arg::new_stack(slot, 0), Arg::new_tmp(result)],
                );
            }

            Opcode::Equal
            | Opcode::NotEqual
            | Opcode::LessThan
            | Opcode::LessEqual
            | Opcode::GreaterThan
            | Opcode::GreaterEqual
            | Opcode::Above
            | Opcode::AboveEqual
            | Opcode::Below
            | Opcode::BelowEqual
            | Opcode::EqualOrUnordered => {
                let cmp = self.create_compare(self.value, false).unwrap();
                self.insts.last_mut().unwrap().push(cmp);
            }

            Opcode::Select => {
                let config = if self.value(self.value).typ().is_int() {
                    MoveConditionallyConfig {
                        move_conditionally32: AirOpcode::MoveConditionally32,
                        move_conditionally64: AirOpcode::MoveConditionally64,
                        move_conditionally_test32: AirOpcode::MoveConditionallyTest32,
                        move_conditionally_test64: AirOpcode::MoveConditionallyTest64,
                        move_conditionally_double: AirOpcode::MoveConditionallyDouble,
                        move_conditionally_float: AirOpcode::MoveConditionallyFloat,
                    }
                } else {
                    MoveConditionallyConfig {
                        move_conditionally32: AirOpcode::MoveDoubleConditionally32,
                        move_conditionally64: AirOpcode::MoveDoubleConditionally64,
                        move_conditionally_test32: AirOpcode::MoveDoubleConditionallyTest32,
                        move_conditionally_test64: AirOpcode::MoveDoubleConditionallyTest64,
                        move_conditionally_double: AirOpcode::MoveDoubleConditionallyDouble,
                        move_conditionally_float: AirOpcode::MoveDoubleConditionallyFloat,
                    }
                };

                let select = self.create_select(&config).unwrap();
                self.insts.last_mut().unwrap().push(select);
            }

            Opcode::IToD => {
                self.append_un_op::<{ AirOpcode::ConvertInt32ToDouble }, { AirOpcode::ConvertInt64ToDouble }, { AirOpcode::Oops}, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::IToF => {
                self.append_un_op::<{ AirOpcode::ConvertInt32ToFloat }, { AirOpcode::ConvertInt64ToFloat }, { AirOpcode::Oops}, { AirOpcode::Oops }>(self.child_id(self.value, 0));
            }

            Opcode::Upsilon => {
                let value = self.child_id(self.value, 0);
                let phi = self.value(self.value).phi().unwrap();

                let arg = self.imm_or_tmp_or_zero_reg(value);

                self.move_to_tmp(
                    relaxed_move_for_type(self.value(value).typ()),
                    arg,
                    self.phi_to_tmp[phi.0],
                );
            }

            Opcode::Phi => {
                // Snapshot the value of the Phi. It may change under us because you could do:
                // a = Phi()
                // Upsilon(@x, ^a)
                // @a => this should get the value of the Phi before the Upsilon, i.e. not @x.

                let tmp = self.tmp(self.value);
                self.append(
                    relaxed_move_for_type(self.value(self.value).typ()),
                    &[
                        Arg::new_tmp(self.phi_to_tmp[self.value.0]),
                        Arg::new_tmp(tmp),
                    ],
                );
            }

            Opcode::Set => {
                let value = self.child_id(self.value, 0);
                let source = self.imm_or_tmp_or_zero_reg(value);
                let var = self.value(self.value).as_variable().unwrap();
                self.move_to_tmp(
                    relaxed_move_for_type(self.value(value).typ()),
                    source,
                    self.variable_to_tmps[&var][0],
                );
            }

            Opcode::Get => {
                let var = self.value(self.value).as_variable().unwrap();
                let result = self.tmp(self.value);
                self.append(
                    relaxed_move_for_type(self.value(self.value).typ()),
                    &[
                        Arg::new_tmp(self.variable_to_tmps[&var][0]),
                        Arg::new_tmp(result),
                    ],
                );
            }

            Opcode::Branch => {
                let branch = self
                    .create_branch(self.child_id(self.value, 0), false)
                    .unwrap();

                self.insts.last_mut().unwrap().push(branch);
            }

            Opcode::Jump => {
                self.append(AirOpcode::Jump, &[]);
            }

            Opcode::Return => {
                if self.value(self.value).children.is_empty() {
                    self.append(AirOpcode::RetVoid, &[]);
                } else {
                    let return_value_gpr = Tmp::from_reg(Reg::new_gpr(RETURN_VALUE_GPR));
                    let return_value_fpr = Tmp::from_reg(Reg::new_fpr(xmm0));

                    let value = self.child_id(self.value, 0);

                    match self.value(value).typ().kind() {
                        TypeKind::Int32 => {
                            let source = self.imm_or_tmp_or_zero_reg(value);
                            self.append(AirOpcode::Move, &[source, Arg::new_tmp(return_value_gpr)]);

                            self.append(AirOpcode::Ret32, &[Arg::new_tmp(return_value_gpr)]);
                        }

                        TypeKind::Int64 => {
                            let source = self.imm_or_tmp_or_zero_reg(value);
                            self.append(AirOpcode::Move, &[source, Arg::new_tmp(return_value_gpr)]);

                            self.append(AirOpcode::Ret64, &[Arg::new_tmp(return_value_gpr)]);
                        }

                        TypeKind::Float => {
                            let source = self.imm_or_tmp_or_zero_reg(value);
                            self.append(
                                AirOpcode::MoveFloat,
                                &[source, Arg::new_tmp(return_value_fpr)],
                            );

                            self.append(AirOpcode::RetFloat, &[Arg::new_tmp(return_value_fpr)]);
                        }

                        TypeKind::Double => {
                            let source = self.imm_or_tmp_or_zero_reg(value);
                            self.append(
                                AirOpcode::MoveDouble,
                                &[source, Arg::new_tmp(return_value_fpr)],
                            );

                            self.append(AirOpcode::RetDouble, &[Arg::new_tmp(return_value_fpr)]);
                        }

                        _ => unreachable!(),
                    }
                }
            }

            Opcode::Oops => {
                self.append(AirOpcode::Oops, &[]);
            }

            Opcode::EntrySwitch => {
                self.append(AirOpcode::EntrySwitch, &[]);
            }
            opcode => todo!("NYI: Could not lower {:?}", opcode),
        }
    }

    fn value(&self, value: ValueId) -> &Value {
        &self.code.proc.value(value)
    }

    fn value_mut(&mut self, value: ValueId) -> &mut Value {
        self.code.proc.value_mut(value)
    }

    fn child(&self, value: ValueId, index: usize) -> &Value {
        &self.code.proc.value(self.value(value).children[index])
    }

    fn child_mut(&mut self, value: ValueId, index: usize) -> &mut Value {
        let child = self.value(value).children[index];
        self.code.proc.value_mut(child)
    }

    fn child_id(&self, value: ValueId, index: usize) -> ValueId {
        self.value(value).children[index]
    }

    fn should_copy_propagate(&self, value: ValueId) -> bool {
        match self.code.proc.value(value).kind.opcode() {
            Opcode::Trunc | Opcode::Identity | Opcode::Opaque => true,
            _ => false,
        }
    }
}


struct ArgPromise {
    arg: Arg,
    value: Option<ValueId>,
    was_consumed: bool,
    was_wrapped: bool,
    traps: bool,
}
#[allow(dead_code)]
impl ArgPromise {
    pub fn new(arg: Arg, value_to_lock: Option<ValueId>) -> Self {
        Self {
            arg,
            value: value_to_lock,
            was_consumed: false,
            was_wrapped: false,
            traps: false,
        }
    }

    pub fn swap(&mut self, other: &mut Self) {
        std::mem::swap(self, other);
    }

    pub fn set_traps(&mut self) {
        self.traps = true;
    }

    pub fn tmp(&self) -> Self {
        let result = Self::new(Arg::default(), self.value);
        result
    }

    pub fn is_set(&self) -> bool {
        self.arg != Arg::default() || self.value.is_some()
    }

    pub fn kind(&self) -> ArgKind {
        if self.arg == Arg::default() && self.value.is_some() {
            ArgKind::Tmp
        } else {
            self.arg.kind()
        }
    }

    pub fn consume(&mut self, lower: &mut LowerToAir) -> Arg {
        self.was_consumed = true;
        if self.arg == Arg::default() && self.value.is_some() {
            Arg::new_tmp(lower.tmp(self.value.unwrap()))
        } else {
            lower.commit_internal(self.value);

            self.arg.clone()
        }
    }

    pub fn inst(&mut self, op: AirOpcode, origin: ValueId, args: &[Arg]) -> Inst {
        let res = Inst::new(
            crate::air::kind::Kind {
                opcode: op,
                effects: self.traps,
            },
            origin,
            args,
        );

        self.was_wrapped = true;

        res
    }
}
