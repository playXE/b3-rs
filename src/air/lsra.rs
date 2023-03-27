use std::{collections::VecDeque, mem::size_of, ops::Range};

use bitvec::vec::BitVec;

use crate::{
    bank::Bank,
    jit::{
        reg::Reg,
        register_set::{RegisterSet, RegisterSetBuilder, ScalarRegisterSet},
    },
    liveness::Liveness,
    utils::{deque::VecDequeExt, index_set::IndexMap},
};

use super::{
    arg::{Arg, ArgRole, ArgTiming},
    basic_block::BasicBlockId,
    code::Code,
    insertion_set::phased::PhasedInsertionSet,
    inst::Inst,
    liveness_adapter::UnifiedTmpLivenessAdapter,
    opcode::Opcode,
    pad_interference::pad_interference,
    reg_liveness::LocalCalcForUnifiedTmpLiveness,
    stack_slot::{StackSlotId, StackSlotKind},
    tmp::Tmp,
    tmp_set::TmpMap, fix_spills_after_terminals::fix_spills_after_terminals, handle_callee_saves::handle_callee_saves, stack_allocation::{allocate_escaped_stack_slots, update_frame_size_based_on_stack_slots},
};

/// This implements the Poletto and Sarkar register allocator called "linear scan":
/// http://dl.acm.org/citation.cfm?id=330250
///
/// This is not Air's primary register allocator. We use it only when running at optLevel<2.
/// That's not the default level. This register allocator is optimized primarily for running
/// quickly. It's expected that improvements to this register allocator should focus on improving
/// its execution time without much regard for the quality of generated code. If you want good
/// code, use graph coloring.
///
/// For Air's primary register allocator, see AirAllocateRegistersByGraphColoring.h|cpp.
///
/// This also does stack allocation as an afterthought. It does not do any spill coalescing.
pub fn allocate_registers_and_stack_by_linear_scan<'a>(code: &mut Code<'a>) {
    let mut lsra = LinearScan::new(code);
    lsra.run();
}

struct LinearScan<'a, 'b> {
    code: &'a mut Code<'b>,
    // 0: GP
    // 1: FP
    allowed_registers_in_priority_order: [Vec<Reg>; 2],
    allowed_registers: [ScalarRegisterSet; 2],
    all_allowed_registers: ScalarRegisterSet,
    start_index: IndexMap<usize, BasicBlockId>,
    map: TmpMap<TmpData>,
    insertion_sets: IndexMap<PhasedInsertionSet<Inst>, BasicBlockId>,
    clobbers: Vec<Clobber>,
    tmps: Vec<Tmp>,
    active: VecDeque<Tmp>,
    active_regs: RegisterSet,
    used_spillslots: BitVec<usize>,
    did_spill: bool,
}

impl<'a, 'b: 'a> LinearScan<'a, 'b> {
    fn new(code: &'a mut Code<'b>) -> Self {
        let mut this = Self {
            start_index: IndexMap::with_capacity(code.blocks.len()),
            map: TmpMap::with_capacity(code.num_gp_tmps + code.num_fp_tmps),
            insertion_sets: IndexMap::with_capacity(code.blocks.len()),
            code,
            allowed_registers_in_priority_order: [Vec::new(), Vec::new()],
            allowed_registers: [ScalarRegisterSet::empty(), ScalarRegisterSet::empty()],
            all_allowed_registers: ScalarRegisterSet::empty(),
            clobbers: Vec::new(),
            tmps: Vec::new(),
            active: VecDeque::new(),
            active_regs: RegisterSet::default(),
            used_spillslots: BitVec::new(),
            did_spill: false,
        };

        for block_id in (0..this.code.blocks.len()).map(BasicBlockId) {
            this.insertion_sets.insert(block_id, PhasedInsertionSet::new());
        }

        this
    }

    pub fn run(&mut self) {
        pad_interference(self.code);
        self.build_register_set_builder();
        self.build_indices();
        self.build_intervals();

        loop {
            self.prepare_intervals_for_scan_for_registers();
            self.did_spill = false;

            self.attempt_scan_for_registers(Bank::GP);
            self.attempt_scan_for_registers(Bank::FP);

            if !self.did_spill {
                break;
            }

            self.emit_spill_code();
        }

        self.insert_spill_code();
        self.assign_registers();
        fix_spills_after_terminals(self.code);
        handle_callee_saves(self.code);
        allocate_escaped_stack_slots(self.code);
        self.prepare_intervals_for_scan_for_stack();
        self.scan_for_stack();
        update_frame_size_based_on_stack_slots(self.code);
        self.code.stack_is_allocated = true;
    }

    fn build_register_set_builder(&mut self) {
        let mut for_bank = |bank: Bank| {
            self.allowed_registers_in_priority_order[bank as usize] = match bank {
                Bank::GP => self.code.gp_regs_in_priority_order.clone(),
                Bank::FP => self.code.fp_regs_in_priority_order.clone(),
            };

            for r in &self.allowed_registers_in_priority_order[bank as usize] {
                self.allowed_registers[bank as usize].add(*r);
            }

            let mut set = self.all_allowed_registers.to_register_set();
            set.merge(&self.allowed_registers[bank as usize].to_register_set());

            self.all_allowed_registers = set.build_scalar_register_set();
        };

        for_bank(Bank::GP);
        for_bank(Bank::FP);
    }

    fn build_indices(&mut self) {
        let mut index = 0;

        for block in 0..self.code.blocks.len() {
            self.start_index.insert(BasicBlockId(block), index);
            index += self.code.blocks[block].insts.len() * 2;
        }
    }

    fn index_of_head(&self, block: BasicBlockId) -> usize {
        self.start_index[block]
    }

    fn index_of_tail(&self, block: BasicBlockId) -> usize {
        self.index_of_head(block) + self.code.block(block).insts.len() * 2
    }

    fn early_interval(index_of_early: usize) -> Range<usize> {
        index_of_early..index_of_early
    }

    fn late_interval(index_of_late: usize) -> Range<usize> {
        index_of_late..index_of_late + 1
    }

    fn early_and_late_interval(index_of_early: usize) -> Range<usize> {
        index_of_early..index_of_early + 1
    }

    #[allow(dead_code)]
    fn interval(timing: ArgTiming) -> Range<usize> {
        match timing {
            ArgTiming::OnlyEarly => Self::early_interval(0),
            ArgTiming::OnlyLate => Self::late_interval(0),
            ArgTiming::EarlyAndLate => Self::early_and_late_interval(0),
        }
    }

    fn interval_for_spill(index_of_early: usize, role: ArgRole) -> Range<usize> {
        let timing = role.timing();

        match timing {
            ArgTiming::OnlyEarly => {
                if role.is_any_def() {
                    return Self::early_and_late_interval(index_of_early);
                }

                Self::early_interval(index_of_early)
            }

            ArgTiming::OnlyLate => {
                if role.is_any_use() {
                    return Self::early_and_late_interval(index_of_early);
                }

                Self::late_interval(index_of_early)
            }

            ArgTiming::EarlyAndLate => Self::early_and_late_interval(index_of_early),
        }
    }

    fn build_intervals(&mut self) {
        let code2 = unsafe { &*(self.code as *const Code) };

        let mut adapter = UnifiedTmpLivenessAdapter::new(code2, false);
        let mut liveness = Liveness::new(&mut adapter);
        liveness.compute();

        for block_id in 0..self.code.blocks.len() {
            let block_id = BasicBlockId(block_id);
            let index_of_head = self.index_of_head(block_id);
            let index_of_tail = self.index_of_tail(block_id);

            for tmp in liveness.live_at_head(block_id) {
                if !tmp.is_reg() {
                    add_interval_mut(
                        &mut self.map.entry(tmp).or_insert(TmpData::default()).interval,
                        &(index_of_head..index_of_head),
                    );
                }
            }

            for tmp in liveness.live_at_tail(block_id) {
                if !tmp.is_reg() {
                    add_interval_mut(
                        &mut self.map.entry(tmp).or_insert(TmpData::default()).interval,
                        &(index_of_tail..index_of_tail),
                    );
                }
            }

            for inst_index in 0..self.code.block(block_id).insts.len() {
                let inst = &self.code.block(block_id).insts[inst_index];
                let index_of_early = index_of_head + inst_index * 2;

                inst.for_each_tmp(self.code, |tmp, role, _, _| {
                    if tmp.is_reg() {
                        return;
                    }

                    /*self.map[tmp].interval = add_interval(
                        &self.map.entry(tmp).or_insert(TmpData::default()),
                        &Self::interval_for_spill(index_of_early, role),
                    );*/

                    add_interval_mut(
                        &mut self.map.entry(tmp).or_insert(TmpData::default()).interval,
                        &Self::interval_for_spill(index_of_early, role),
                    );
                });
            }

            let mut local_calc = LocalCalcForUnifiedTmpLiveness::new(&liveness, block_id);

            let record = |this: &mut Self,
                          local_calc: &LocalCalcForUnifiedTmpLiveness,
                          inst_index: usize| {
                let regs = local_calc.live();

                if let Some(prev) = this
                    .code
                    .block(block_id)
                    .insts
                    .get(inst_index.wrapping_sub(1))
                {
                    let mut prev_regs = RegisterSetBuilder::from_regs(regs);

                    prev.for_each_reg(this.code, |reg, role, _, width| {
                        if role.is_late_def() {
                            prev_regs.add(reg, width);
                        }
                    });

                    if prev.kind.opcode == Opcode::Patch {
                        todo!()
                    }

                    prev_regs.filter_regs(
                        &this
                            .all_allowed_registers
                            .to_register_set()
                            .include_whole_register_width(),
                    );

                    if !prev_regs.is_empty() {
                        this.clobbers.push(Clobber {
                            index: index_of_head + inst_index * 2 - 1,
                            regs: prev_regs.build_and_validate(),
                        })
                    }
                }

                if let Some(next) = this.code.block(block_id).insts.get(inst_index) {
                    let mut next_regs = RegisterSetBuilder::from_regs(regs);

                    next.for_each_reg(this.code, |reg, role, _, width| {
                        if role.is_early_def() {
                            next_regs.add(reg, width);
                        }
                    });

                    if next.kind.opcode == Opcode::Patch {
                        todo!()
                    }

                    if !next_regs.is_empty() {
                        this.clobbers.push(Clobber {
                            index: index_of_head + inst_index * 2,
                            regs: next_regs.build_and_validate(),
                        });
                    }
                }
            };

            let len = self.code.block(block_id).len();
            record(self, &local_calc, len);

            for inst_index in (0..len).rev() {
                local_calc.execute(inst_index);
                record(self, &local_calc, inst_index);
            }
        }

        self.clobbers.sort_by_key(|clobber| clobber.index);
    }

    fn prepare_intervals_for_scan_for_registers(&mut self) {
        self.prepare_intervals(|data| {
            if data.spilled.is_some() {
                return false;
            }

            data.assigned = Reg::default();

            true
        });
    }

    fn prepare_intervals_for_scan_for_stack(&mut self) {
        self.prepare_intervals(|data| data.spilled.is_some());
    }

    fn prepare_intervals(&mut self, select_func: impl Fn(&mut TmpData) -> bool) {
        self.tmps.clear();

        self.code.for_each_tmp(|tmp| {
            let data = self.map.entry(tmp).or_insert(TmpData::default());
            if !select_func(data) {
                return;
            }

            self.tmps.push(tmp);
        });

        self.tmps.sort_by_key(|tmp| self.map[*tmp].interval.start);
    }

    fn add_spill_tmp_with_interval(&mut self, bank: Bank, interval: Range<usize>) -> Tmp {
        let mut data = TmpData::default();
        data.interval = interval;
        data.is_unspillable = true;

        let tmp = self.code.new_tmp(bank);
        self.map.insert(tmp, data);
        tmp
    }

    fn attempt_scan_for_registers(&mut self, bank: Bank) {
        // This is modeled after LinearScanRegisterAllocation in Fig. 1 in
        // http://dl.acm.org/citation.cfm?id=330250.

        self.active.clear();
        self.active_regs = Default::default();

        let mut clobber_index = 0;

        for tmp in 0..self.tmps.len() {
            let tmp = self.tmps[tmp];
            if tmp.bank() != bank {
                continue;
            }

            let entry = &self.map[tmp];
            let index = entry.interval.start;

            // This is ExpireOldIntervals in Fig. 1.
            while !self.active.is_empty() {
                let tmp = self.active.front().copied().unwrap();
                let entry = &self.map[tmp];

                let expired = entry.interval.end <= index;

                if !expired {
                    break;
                }

                self.active.pop_front();
                self.active_regs.remove(entry.assigned);
            }

            // If necessary, compute the set of registers that this tmp could even use. This is not
            // part of Fig. 1, but it's a technique that the authors claim to have implemented in one of
            // their two implementations. There may be other more efficient ways to do this, but this
            // implementation gets some perf wins from piggy-backing this calculation in the scan.
            //
            // Note that the didBuild flag sticks through spilling. Spilling doesn't change the
            // interference situation.
            //
            // Note that we could short-circuit this if we're dealing with a spillable tmp, there are no
            // free registers, and this register's interval ends after the one on the top of the active
            // stack.

            if !entry.did_build_possible_regs {
                while clobber_index < self.clobbers.len()
                    && self.clobbers[clobber_index].index < index
                {
                    clobber_index += 1;
                }

                let mut possible_regs = self.allowed_registers[bank as usize].to_register_set();

                let mut i = clobber_index;

                while i < self.clobbers.len() && self.clobbers[i].index < entry.interval.end {
                    possible_regs.exclude(&self.clobbers[i].regs.include_whole_register_width());

                    i += 1;
                }

                self.map[tmp].possible_regs = possible_regs.build_scalar_register_set();
                self.map[tmp].did_build_possible_regs = true;
            }

            if self.active.len() != self.allowed_registers_in_priority_order[bank as usize].len() {
                let mut did_assign = false;

                for reg in self.allowed_registers_in_priority_order[bank as usize]
                    .iter()
                    .copied()
                {
                    if !self
                        .active_regs
                        .contains(reg, reg.conservative_width_without_vectors())
                        && self.map[tmp].possible_regs.contains(reg)
                    {
                        self.assign(tmp, reg);
                        did_assign = true;
                        break;
                    }
                }

                if did_assign {
                    continue;
                }
            }

            let spill_tmp = self.active.take_last(|spill_candidate| {
                self.map[tmp]
                    .possible_regs
                    .contains(self.map[*spill_candidate].assigned)
            });

            if let Some(spill_tmp) = spill_tmp {
                let spill_entry = &self.map[spill_tmp];

                if spill_entry.is_unspillable
                    || (!self.map[tmp].is_unspillable
                        && spill_entry.interval.end <= self.map[tmp].interval.end)
                {
                    self.spill(tmp);
                    self.add_to_active(spill_tmp);
                    continue;
                }

                self.assign(tmp, spill_entry.assigned);
                self.spill(spill_tmp);
            } else {
                self.spill(tmp);
                continue;
            }
        }
    }

    fn add_to_active(&mut self, tmp: Tmp) {
        if self.map[tmp].is_unspillable {
            self.active.push_front(tmp);
            return;
        }

        self.active.append_and_bubble(tmp, |other_tmp| {
            let other_entry = &self.map[*other_tmp];

            if other_entry.is_unspillable {
                return false;
            }

            self.map[*other_tmp].interval.end > self.map[tmp].interval.end
        });
    }

    fn assign(&mut self, tmp: Tmp, reg: Reg) {
        let entry = &mut self.map[tmp];
        entry.assigned = reg;
        println!("assign {} to {}", tmp, reg);
        self.active_regs
            .add(reg, reg.conservative_width_without_vectors());
    }

    fn spill(&mut self, tmp: Tmp) {
        let slot = self
            .code
            .add_stack_slot(size_of::<usize>(), StackSlotKind::Spill);

        let entry = &mut self.map[tmp];
        entry.spilled = Some(slot);
        entry.assigned = Reg::default();
        self.did_spill = true;
    }

    fn emit_spill_code(&mut self) {
        for block_id in (0..self.code.blocks.len()).map(BasicBlockId) {
            let index_of_head = self.index_of_head(block_id);

            for inst_index in 0..self.code.block(block_id).insts.len() {
                let index_of_early = index_of_head + inst_index * 2;

                // first try to spill directly
                for i in 0..self.code.block(block_id).insts[inst_index].args.len() {
                    let arg = &self.code.block(block_id).insts[inst_index].args[i];

                    if !arg.is_tmp() {
                        continue;
                    }

                    if arg.is_reg() {
                        continue;
                    }

                    let spilled = self.map[arg.tmp()].spilled;

                    if spilled.is_none() {
                        continue;
                    }

                    if !self.code.block(block_id).insts[inst_index].admits_stack(i, self.code) {
                        continue;
                    }

                    self.code.block_mut(block_id).insts[inst_index].args[i] =
                        Arg::new_stack(spilled.unwrap(), 0);
                }

                // TODO: How do I make this safe?
                let code2 = unsafe { &mut *(self.code as *const Code as *mut Code) };
                let code3 = unsafe { &mut *(code2 as *const Code as *mut Code) };
                code2.block_mut(block_id).insts[inst_index].for_each_tmp_mut(
                    code3, |tmp, role, bank, _| {
                        if tmp.is_reg() {
                            return;
                        }

                        let spilled = self.map[*tmp].spilled;
                        if let Some(spilled) = spilled {
                            let mov = if bank == Bank::GP {
                                Opcode::Move
                            } else {
                                Opcode::MoveDouble
                            };

                            *tmp = self.add_spill_tmp_with_interval(
                                bank,
                                Self::interval_for_spill(index_of_early, role),
                            );

                            if role == ArgRole::Scratch {
                                return;
                            }

                            if role.is_any_use() {
                                self.insertion_sets[block_id].insert_inst(
                                    inst_index,
                                    1,
                                    Inst::new(
                                        mov.into(),
                                        self.code.block(block_id).insts[inst_index].origin,
                                        &[Arg::new_stack(spilled, 0), Arg::new_tmp(*tmp)],
                                    ),
                                );
                            }

                            if role.is_any_def() {
                                self.insertion_sets[block_id].insert_inst(
                                    inst_index + 1,
                                    0,
                                    Inst::new(
                                        mov.into(),
                                        self.code.block(block_id).insts[inst_index].origin,
                                        &[Arg::new_tmp(*tmp), Arg::new_stack(spilled, 0)],
                                    ),
                                );
                            }
                        }
                    },
                );
            }
        }
    }

    fn scan_for_stack(&mut self) {
        // This is loosely modeled after LinearScanRegisterAllocation in Fig. 1 in
        // http://dl.acm.org/citation.cfm?id=330250.

        self.active.clear();
        self.used_spillslots.clear();

        for i in 0..self.tmps.len() {
            let tmp = self.tmps[i];

            if let Some(spilled) = self.map[tmp].spilled {
                let index = self.map[tmp].interval.start;

                while !self.active.is_empty() {
                    let tmp = self.active.front().unwrap();

                    let expired = self.map[*tmp].interval.end <= index;

                    if !expired {
                        break;
                    }

                    let spill_index = self.map[*tmp].spill_index;
                    self.active.pop_front();
                    self.used_spillslots.remove(spill_index);
                }

                self.map[tmp].spill_index = self
                    .used_spillslots
                    .iter()
                    .enumerate()
                    .find(|i| i.1 == false)
                    .map(|x| x.0)
                    .unwrap_or_else(|| self.used_spillslots.len());

                let slot_size = 64;

                let offset = -(self.code.frame_size as isize)
                    - (self.map[tmp].spill_index as isize) * (slot_size as isize)
                    - (slot_size as isize);

                self.code.proc.stack_slots[spilled.0].offset_from_fp = offset;
                if self.map[tmp].spill_index >= self.used_spillslots.len() {
                    self.used_spillslots
                        .resize(self.map[tmp].spill_index + 1, false);
                }
                self.used_spillslots.set(self.map[tmp].spill_index, true);
                self.active.push_back(tmp);
            }
        }
    }

    fn insert_spill_code(&mut self) {
        for block_id in (0..self.code.blocks.len()).map(BasicBlockId) {
            self.insertion_sets[block_id].execute(&mut self.code.block_mut(block_id).insts);
        }
    }

    fn assign_registers(&mut self) {
        for block_id in 0..self.code.blocks.len() {
            let block_id = BasicBlockId(block_id);
            let code2 = unsafe { &mut *(self.code as *const Code as *mut Code) };
            self.code
                .block_mut(block_id)
                .insts
                .iter_mut()
                .for_each(|inst| {
                    inst.for_each_tmp_fast_mut(code2, |tmp| {
                        if tmp.is_reg() {
                            return;
                        }

                        let reg = self.map[*tmp].assigned;
                        if reg == Reg::default() {
                            panic!("Failed to allocate reg for: {}", tmp);
                        }

                        *tmp = Tmp::from_reg(reg);
                    });
                })
        }
    }
}

#[derive(Default)]
struct TmpData {
    interval: Range<usize>,
    spilled: Option<StackSlotId>,
    possible_regs: ScalarRegisterSet,
    assigned: Reg,
    is_unspillable: bool,
    did_build_possible_regs: bool,
    spill_index: usize,
}
#[derive(Default)]
struct Clobber {
    index: usize,
    regs: RegisterSet,
}

#[allow(dead_code)]
fn add_interval(this: &Range<usize>, other: &Range<usize>) -> Range<usize> {
    if this.start == this.end {
        return other.clone();
    }

    if other.start == other.end {
        return this.clone();
    }

    this.start.min(other.start)..this.end.max(other.end)
}

fn add_interval_mut(this: &mut Range<usize>, other: &Range<usize>) {
    if this.start == this.end {
        *this = other.clone();
        return;
    }

    if other.start == other.end {
        return;
    }

    this.start = this.start.min(other.start);
    this.end = this.end.max(other.end);
}
