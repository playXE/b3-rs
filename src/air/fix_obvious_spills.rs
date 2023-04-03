
use crate::{jit::reg::Reg, utils::{index_set::IndexMap, bitvector::BitVector}, width::Width};

use super::{
    arg::{Arg, ArgKind, ArgRole},
    basic_block::BasicBlockId,
    code::*,
    form_table::is_valid_form,
    inst::Inst,
    opcode::Opcode,
    stack_slot::StackSlotId,
    tmp::Tmp,
};

/// This is a forward flow phase that tracks equivalence between spills slots and registers. It
/// removes loads from spill slots in cases when the contents of the spill slot can be found in (or
/// computed from) a register.
pub fn fix_obvious_spills(code: &mut Code) {
    let mut fixer = FixObviousSpills::new(code);
    fixer.run();
}

struct FixObviousSpills<'a, 'b> {
    code: &'a mut Code<'b>,
    at_head: IndexMap<State, BasicBlockId>,
    state: State,
    not_bottom:BitVector,
    should_visit: BitVector,
    block: Option<BasicBlockId>,
    inst_index: usize,
}

impl<'a, 'b> FixObviousSpills<'a, 'b> {
    fn new(code: &'a mut Code<'b>) -> Self {
        let not_bottom = BitVector::new();
        let should_visit = BitVector::new();
        let mut at_head = IndexMap::with_capacity(code.blocks.len());
      

        for block in code.indices() {
            at_head.insert(block, State::new());
        }

        Self {
            code,
            at_head,
            state: State::new(),
            not_bottom,
            should_visit,
            block: None,
            inst_index: 0,
        }
    }

    fn run(&mut self) {
        self.compute_aliases();
        self.fix_code();

        self.code.blocks.iter_mut().for_each(|block| {
            block.retain(|inst| inst != &Inst::default());
        })
    }

    fn compute_aliases(&mut self) {
        self.not_bottom.set(0, true);
        self.should_visit.set(0, true);

        let mut changed = true;

        while changed {
            changed = false;

            for block_index in 0..self.should_visit.len() {
                if self.should_visit.get(block_index) {
                    
                    self.should_visit.set(block_index, false);
                    assert!(self.not_bottom.get(block_index));
                    self.block = Some(BasicBlockId(block_index));
                    self.inst_index = 0;
                    self.state = self.at_head[self.block.unwrap()].clone();
                    for inst_index in 0..self.code.block(self.block.unwrap()).insts.len() {
                        self.inst_index = inst_index;
                        self.execute_inst();
                    }

                    self.state.sort();

                    for successor in self.code.block(self.block.unwrap()).successors.iter() {
                        let successor_index = successor.0 .0;
                        let to_state = &mut self.at_head[successor.0];

                        if self.not_bottom.get(successor_index) {
                            let changed_at_successor_head = to_state.merge(&self.state);
                            if changed_at_successor_head {
                                
                                changed = true;
                                self.should_visit.set(successor_index, true);
                            }
                        } else {
                            *to_state = self.state.clone();
                            changed = true;
                            self.not_bottom.set(successor_index, true);
                            self.should_visit.set(successor_index, true);
                        }
                    }
                }
            }
        }
    }

    fn fix_code(&mut self) {
        for block in self.code.indices() {
            self.block = Some(block);
            self.state = self.at_head[block].clone();
            for inst_index in 0..self.code.block(block).insts.len() {
                self.inst_index = inst_index;
                self.fix_inst();
                self.execute_inst();
            }
        }
    }

    fn fix_inst(&mut self) {
        let mut inst = self.code.block(self.block.unwrap()).insts[self.inst_index].clone();
        //println!("{}", inst);
        let mut should_live = true;

        self.for_all_aliases(|this, alias| {
            should_live &= !this.state.contains(&alias);
        });

        if !should_live {
           
            self.code.block_mut(self.block.unwrap()).insts[self.inst_index] = Inst::default();
            return;
        }

        match inst.kind.opcode {
            Opcode::Move => {
                if false && inst.args[0].is_big_imm()
                    && inst.args[1].is_reg()
                    && is_valid_form(Opcode::Add64, &[ArgKind::Tmp, ArgKind::Tmp, ArgKind::Tmp])
                {
                    // BigImm materializations are super expensive on both x86 and ARM. Let's try to
                    // materialize this bad boy using math instead. Note that we use unsigned math here
                    // since it's more deterministic.
                    let my_value = inst.args[0].value();
                    let my_dest = inst.args[1].reg();

                    for reg_const in self.state.reg_const.iter() {
                        let other_value = reg_const.constant;
                        // Let's try add. That's the only thing that works on all platforms, since it's
                        // the only cheap arithmetic op that x86 does in three operands. Long term, we
                        // should add fancier materializations here for ARM if the BigImm is yuge.
                        let delta = my_value.wrapping_sub(other_value);

                        if Arg::is_valid_imm_form(delta) {
                            inst.kind = Opcode::Add64.into();
                            inst.args.clear();
                            inst.args.push(Arg::new_imm(delta as _));
                            inst.args.push(Arg::new_tmp(Tmp::from_reg(reg_const.reg)));
                            inst.args.push(Arg::new_tmp(Tmp::from_reg(my_dest)));

                            self.code.block_mut(self.block.unwrap()).insts[self.inst_index] = inst;
                            return;
                        }
                    }
                }

                
            }

            _ => (),
        }

        // Create a copy in case we invalidate the instruction. That doesn't happen often.
        let inst_copy = inst.clone();

        // The goal is to replace references to stack slots. We only care about early uses. We can't
        // handle UseDefs. We could teach this to handle UseDefs if we inserted a store instruction
        // after and we proved that the register aliased to the stack slot dies here. We can get that
        // information from the liveness analysis. We also can't handle late uses, because we don't
        // look at late clobbers when doing this.

        let mut did_things = false;

        let handle_arg =
            |this: &Self, arg: &mut Arg, role: ArgRole, _, width: Width, did_things: &mut bool| {
              
                if !this.is_spill_slot(arg) {
                    return;
                }

                if !role.is_early_use() {
                    return;
                }

                if role.is_any_def() {
                    return;
                }

                if let Some(alias) = this.state.get_reg_slot_for_slot(arg.stack_slot()) {
                    match width {
                        Width::W64 => {
                            if alias.mode != RegMode::AllBits {
                                return;
                            }

                            //println!("   Replacing {} with {} (fullwidth case)", arg, alias.reg);

                            *arg = Arg::new_tmp(Tmp::from_reg(alias.reg));
                            *did_things = true;
                            return;
                        }

                        Width::W32 => {
                            //println!("   Replacing {} with {} (subwidth case)", arg, alias.reg);
                            *arg = Arg::new_tmp(Tmp::from_reg(alias.reg));
                            *did_things = true;
                            return;
                        }

                        _ => return,
                    }
                } else {
                    
                }

                if let Some(alias) = this.state.get_slot_const(arg.stack_slot()) {
                    //println!("  Replacing {} with constant {}", arg, alias.constant);
                    if Arg::is_valid_imm_form(alias.constant) {
                        *arg = Arg::new_imm(alias.constant as _);
                    } else {
                        *arg = Arg::new_bigimm(alias.constant);
                    }

                    *did_things = true;
                    return;
                } else {
                    
                }
            };

        let this = unsafe { &mut *(self as *mut Self) };

        inst.for_each_arg_mut(this.code, |_, arg, role, bank, width| {
            handle_arg(self, arg, role, bank, width, &mut did_things)
        });

        if !did_things || inst.is_valid_form(self.code) {
            self.code.block_mut(self.block.unwrap()).insts[self.inst_index] = inst;
            return;
        }

        inst = inst_copy;
        assert!(inst.is_valid_form(self.code), "invalid form for {}", inst);
        let inst2 = unsafe { &*(&inst as *const Inst) };
        inst.for_each_arg_mut(this.code, |_, arg, role, bank, width| {
            let arg_copy = *arg;
            handle_arg(self, arg, role, bank, width, &mut did_things);
            if !inst2.is_valid_form(self.code) {
                *arg = arg_copy;
            }
        });
        
        self.code.block_mut(self.block.unwrap()).insts[self.inst_index] = inst;
    }

    fn execute_inst(&mut self) {
        let inst = &self.code.block(self.block.unwrap()).insts[self.inst_index];

        Inst::for_each_def_with_extra_clobbered_regs(
            Some(inst),
            Some(inst),
            self.code,
            |tmp, _, _, _, _| {
                if tmp.is_reg() {
                    self.state.clobber_reg(tmp.reg());
                }
            },
        );

        Inst::for_each_def_stack(Some(inst), Some(&inst), self.code, |arg, _, _, _| {
            self.state.clobber_stack(arg);
        });

        self.for_all_aliases(|this, alias| {
            this.state.add_alias(alias);
        });
    }

    fn for_all_aliases(&mut self, mut func: impl FnMut(&mut Self, Alias)) {
        let inst = &self.code.block(self.block.unwrap()).insts[self.inst_index];

        match inst.kind.opcode {
            Opcode::Move => {
                if inst.args[0].is_some_imm() {
                    if inst.args[1].is_reg() {
                        let alias = Alias::RegConst(RegConst {
                            reg: inst.args[1].reg(),
                            constant: inst.args[0].value(),
                        });

                        func(self, alias);
                    } else if self.is_spill_slot(&inst.args[1]) {
                        let alias = Alias::SlotConst(SlotConst {
                            slot: Some(inst.args[1].stack_slot()),
                            constant: inst.args[0].value(),
                        });

                        func(self, alias);
                    }
                } else if self.is_spill_slot(&inst.args[0]) && inst.args[1].is_reg() {
                    if let Some(constant) = self.state.constant_for(&inst.args[0]) {
                        let alias = Alias::RegConst(RegConst {
                            reg: inst.args[1].reg(),
                            constant,
                        });

                        func(self, alias);
                    } else {
                        let alias = Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[0].stack_slot()),
                            reg: inst.args[1].reg(),
                            mode: RegMode::AllBits,
                        });

                        func(self, alias);
                    }
                } else if inst.args[0].is_reg() && self.is_spill_slot(&inst.args[1]) {
                    if let Some(constant) = self.state.constant_for(&inst.args[1]) {
                        let alias = Alias::SlotConst(SlotConst {
                            slot: Some(inst.args[1].stack_slot()),
                            constant,
                        });
                        func(self, alias);
                    } else {
                        let alias = Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[1].stack_slot()),
                            reg: inst.args[0].reg(),
                            mode: RegMode::AllBits,
                        });

                        func(self, alias);
                    }
                }
            }

            Opcode::Move32 => {
                if inst.args[0].is_some_imm() {
                    if inst.args[1].is_reg() {
                        let alias = Alias::RegConst(RegConst {
                            reg: inst.args[1].reg(),
                            constant: inst.args[0].value(),
                        });

                        func(self, alias);
                    } else if self.is_spill_slot(&inst.args[1]) {
                        let alias = Alias::SlotConst(SlotConst {
                            slot: Some(inst.args[1].stack_slot()),
                            constant: inst.args[0].value(),
                        });

                        func(self, alias);
                    }
                } else if self.is_spill_slot(&inst.args[0]) && inst.args[1].is_reg() {
                    if let Some(constant) = self.state.constant_for(&inst.args[0]) {
                        let alias = Alias::RegConst(RegConst {
                            reg: inst.args[1].reg(),
                            constant,
                        });

                        func(self, alias);
                    } else {
                        let alias = Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[0].stack_slot()),
                            reg: inst.args[1].reg(),
                            mode: RegMode::ZExt32,
                        });

                        func(self, alias);
                    }
                } else if inst.args[0].is_reg() && self.is_spill_slot(&inst.args[1]) {
                    if let Some(constant) = self.state.constant_for(&inst.args[1]) {
                        let alias = Alias::SlotConst(SlotConst {
                            slot: Some(inst.args[1].stack_slot()),
                            constant,
                        });
                        func(self, alias);
                    } else {
                        let alias = Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[1].stack_slot()),
                            reg: inst.args[0].reg(),
                            mode: RegMode::Match32,
                        });

                        func(self, alias);
                    }
                }
            }

            Opcode::MoveFloat => {
                if self.is_spill_slot(&inst.args[0]) && inst.args[1].is_reg() {
                    func(
                        self,
                        Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[0].stack_slot()),
                            reg: inst.args[1].reg(),
                            mode: RegMode::AllBits,
                        }),
                    );
                } else if inst.args[0].is_reg() && self.is_spill_slot(&inst.args[1]) {
                    func(
                        self,
                        Alias::RegSlot(RegSlot {
                            slot: Some(inst.args[1].stack_slot()),
                            reg: inst.args[0].reg(),
                            mode: RegMode::AllBits,
                        }),
                    );
                }
            }

            _ => (),
        }
    }

    fn is_spill_slot(&self, arg: &Arg) -> bool {
        arg.is_stack() && self.code.stack_slot(arg.stack_slot()).is_spill()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Alias {
    RegConst(RegConst),
    RegSlot(RegSlot),
    SlotConst(SlotConst),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct RegConst {
    reg: Reg,
    constant: i64,
}

impl Default for RegConst {
    fn default() -> Self {
        Self {
            reg: Reg::default(),
            constant: 0,
        }
    }
}

impl PartialOrd for RegConst {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.reg < other.reg {
            Some(std::cmp::Ordering::Less)
        } else if self.reg > other.reg {
            Some(std::cmp::Ordering::Greater)
        } else {
            self.constant.partial_cmp(&other.constant)
        }
    }
}

impl Ord for RegConst {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.reg < other.reg {
            std::cmp::Ordering::Less
        } else if self.reg > other.reg {
            std::cmp::Ordering::Greater
        } else {
            self.constant.cmp(&other.constant)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum RegMode {
    AllBits,
    /// Register contains zero-extended contents of stack slot.
    ZExt32,
    /// Low 32 bits of register match low 32 bits of stack slot.
    Match32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct RegSlot {
    slot: Option<StackSlotId>,
    reg: Reg,
    mode: RegMode,
}

impl Default for RegSlot {
    fn default() -> Self {
        Self {
            slot: None,
            reg: Reg::default(),
            mode: RegMode::AllBits,
        }
    }
}
impl PartialOrd for RegSlot {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(if self.slot < other.slot {
            std::cmp::Ordering::Less
        } else if self.slot > other.slot {
            std::cmp::Ordering::Greater
        } else {
            self.reg.cmp(&other.reg)
        })
    }
}

impl Ord for RegSlot {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.slot < other.slot {
            std::cmp::Ordering::Less
        } else if self.slot > other.slot {
            std::cmp::Ordering::Greater
        } else {
            self.reg.cmp(&other.reg)
        }
    }
}

impl std::fmt::Display for RegSlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.slot {
            Some(id) => write!(f, "{:?}", id)?,
            None => write!(f, "<none>")?,
        }

        write!(f, "->{}({:?})", self.reg, self.mode)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct SlotConst {
    slot: Option<StackSlotId>,
    constant: i64,
}

impl Default for SlotConst {
    fn default() -> Self {
        Self {
            slot: None,
            constant: 0,
        }
    }
}

impl PartialOrd for SlotConst {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.slot < other.slot {
            Some(std::cmp::Ordering::Less)
        } else if self.slot > other.slot {
            Some(std::cmp::Ordering::Greater)
        } else {
            self.constant.partial_cmp(&other.constant)
        }
    }
}

impl Ord for SlotConst {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.slot < other.slot {
            std::cmp::Ordering::Less
        } else if self.slot > other.slot {
            std::cmp::Ordering::Greater
        } else {
            self.constant.cmp(&other.constant)
        }
    }
}

impl std::fmt::Display for SlotConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.slot {
            Some(id) => write!(f, "{:?}", id)?,
            None => write!(f, "<none>")?,
        }

        write!(f, "->{}", self.constant)
    }
}

#[derive(Clone, Debug, Default)]
struct State {
    reg_const: Vec<RegConst>,
    slot_const: Vec<SlotConst>,
    reg_slot: Vec<RegSlot>,
}

impl State {
    fn new() -> Self {
        Self {
            reg_const: Vec::new(),
            slot_const: Vec::new(),
            reg_slot: Vec::new(),
        }
    }

    fn contains(&self, alias: &Alias) -> bool {
        match alias {
            Alias::RegConst(a) => self.contains_reg_const(a),
            Alias::RegSlot(a) => self.contains_reg_slot(a),
            Alias::SlotConst(a) => self.contains_slot_const(a),
        }
    }

    fn add_alias(&mut self, alias: Alias) {
        match alias {
            Alias::RegConst(a) => self.add_reg_const_alias(a),
            Alias::RegSlot(a) => self.add_reg_slot_alias(a),
            Alias::SlotConst(a) => self.add_slot_const_alias(a),
        }
    }

    fn add_reg_const_alias(&mut self, alias: RegConst) {
        self.reg_const.push(alias);
    }

    fn add_reg_slot_alias(&mut self, alias: RegSlot) {
        self.reg_slot.push(alias);
    }

    fn add_slot_const_alias(&mut self, alias: SlotConst) {
        self.slot_const.push(alias);
    }

    fn contains_reg_const(&self, alias: &RegConst) -> bool {
        self.reg_const.contains(alias)
    }

    fn contains_reg_slot(&self, alias: &RegSlot) -> bool {
        self.reg_slot.contains(alias)
    }

    fn contains_slot_const(&self, alias: &SlotConst) -> bool {
        self.slot_const.contains(alias)
    }

    fn get_reg_const(&self, reg: Reg) -> Option<&RegConst> {
        self.reg_const.iter().find(|x| x.reg == reg)
    }

    fn get_slot_const(&self, slot: StackSlotId) -> Option<&SlotConst> {
        self.slot_const.iter().find(|x| x.slot == Some(slot))
    }
    #[allow(dead_code)]
    fn get_reg_slot(&self, reg: Reg) -> Option<&RegSlot> {
        self.reg_slot.iter().find(|x| x.reg == reg)
    }

    fn get_reg_slot_for_slot(&self, slot: StackSlotId) -> Option<&RegSlot> {
        self.reg_slot.iter().find(|x| x.slot == Some(slot))
    }
    #[allow(dead_code)]
    fn get_reg_slot_stack(&self, reg: Reg, slot: StackSlotId) -> Option<&RegSlot> {
        self.reg_slot
            .iter()
            .find(|x| x.reg == reg && x.slot == Some(slot))
    }

    fn constant_for(&self, arg: &Arg) -> Option<i64> {
        if arg.is_reg() {
            if let Some(alias) = self.get_reg_const(arg.reg()) {
                return Some(alias.constant);
            }

            return None;
        }

        if arg.is_stack() {
            if let Some(alias) = self.get_slot_const(arg.stack_slot()) {
                return Some(alias.constant);
            }

            return None;
        }

        None
    }

    fn clobber_reg(&mut self, reg: Reg) {
        self.reg_const.retain(|x| x.reg != reg);
        self.reg_slot.retain(|x| x.reg != reg);
    }

    fn clobber_stack(&mut self, slot: StackSlotId) {
        self.slot_const.retain(|x| x.slot != Some(slot));
        self.reg_slot.retain(|x| x.slot != Some(slot));
    }

    fn sort(&mut self) {
        self.reg_const.sort();
        self.slot_const.sort();
        self.reg_slot.sort();
    }

    fn merge(&mut self, other: &Self) -> bool {
        let mut changed = false;

        changed |=
            Self::filter_vector_against(&mut self.reg_const, &other.reg_const, |a, b| &*a == b);

        changed |=
            Self::filter_vector_against(&mut self.slot_const, &other.slot_const, |a, b| &*a == b);

        changed |= Self::filter_vector_against(&mut self.reg_slot, &other.reg_slot, |a, b| {
            if a.reg != b.reg {
                return false;
            }

            if a.slot != b.slot {
                return false;
            }

            if a.mode != RegMode::Match32 && b.mode != a.mode {
                a.mode = RegMode::Match32;
                changed = true;
            }

            true
        });

        changed
    }

    /// Takes two sorted vectors, for each element in the first, it looks for the first element in the second which is not smaller.
    /// If such an element exist, call f on both the element from the first vector and this element.
    /// Remove the element from the first vector unless f returned true (so f says whether to keep the element)
    /// Returns true if any element has been removed.
    fn filter_vector_against<T: PartialOrd + Clone>(
        own: &mut Vec<T>,
        other: &[T],
        mut func: impl FnMut(&mut T, &T) -> bool,
    ) -> bool {
        
        let old_size = own.len();
        own.retain_mut(|x| {
            let it = other.iter().find(|y| !(y < &&*x));

            if let Some(y) = it {
                func(x, y)
            } else {
                false
            }
        });
        own.len() != old_size
    }
}


