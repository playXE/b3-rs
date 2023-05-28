use crate::utils::interference_graph::Iterable;

use crate::{
    analysis::liveness::{IndexSparseSetEntry, LocalCalc},
    utils::interference_graph::{
        InterferenceBitVector, InterferenceGraph, InterferenceVector, LargeInterferenceGraph,
        SmallInterferenceGraph, MAX_SIZE_FOR_SMALL_INTERFERENCE_GRAPH,
    },
    width::{bytes_for_width, Width},
};

use super::handle_callee_saves::handle_callee_saves;
use super::stack_allocation::update_frame_size_based_on_stack_slots;
use super::{
    arg::Arg,
    code::Code,
    inst::Inst,
    liveness_adapter::{StackSlotLiveness, StackSlotLivenessAdapter},
    opcode::Opcode,
    stack_allocation::allocate_and_get_escaped_slots_without_changing_frame_size,
    stack_slot::{StackSlotId, StackSlotKind},
};

/// This allocates StackSlots to places on the stack. It first allocates the pinned ones in index
/// order and then it allocates the rest using first fit. Takes the opportunity to kill dead
/// assignments to stack slots, since it knows which ones are live. Also coalesces spill slots
/// whenever possible.
///
/// This is meant to be an optimal stack allocator, focused on generating great code. It's not
/// particularly fast, though.
pub fn allocate_stack_by_graph_coloring(code: &mut Code) {
    handle_callee_saves(code);
    let assigned_escaped_stack_slots =
        allocate_and_get_escaped_slots_without_changing_frame_size(code);
    if code.proc.stack_slots.len() < MAX_SIZE_FOR_SMALL_INTERFERENCE_GRAPH {
        let interference = SmallInterferenceGraph::new(InterferenceBitVector::new());
        let mut allocator = GraphColoringStackAllocator::new(code, interference);
        allocator.run(&assigned_escaped_stack_slots);
    } else {
        let interference = LargeInterferenceGraph::new(InterferenceVector::new());
        let mut allocator = GraphColoringStackAllocator::new(code, interference);
        allocator.run(&assigned_escaped_stack_slots);
    }

    update_frame_size_based_on_stack_slots(code);
}

struct GraphColoringStackAllocator<'a, 'b, G: InterferenceGraph> {
    code: &'a mut Code<'b>,
    remapped_stack_indices: Vec<usize>,
    interference: G,
    coalescable_moves: Vec<CoalescableMove>,
}

impl<'a, 'b, G: InterferenceGraph> GraphColoringStackAllocator<'a, 'b, G> {
    fn new(code: &'a mut Code<'b>, interference: G) -> Self {
        let mut this = GraphColoringStackAllocator {
            code,
            remapped_stack_indices: Vec::new(),
            interference,
            coalescable_moves: Vec::new(),
        };

        for i in 0..this.code.proc.stack_slots.len() {
            this.remapped_stack_indices.push(i);
        }
        this.interference
            .set_max_index(this.code.proc.stack_slots.len() as _);

        this
    }
    /// We will perform some spill coalescing. To make that effective, we need to be able to identify
    /// coalescable moves and handle them specially in interference analysis.
    fn is_coalescable_move(&self, inst: &Inst) -> bool {
        let width = match inst.kind.opcode {
            Opcode::Move | Opcode::MoveDouble => Width::W64,
            Opcode::Move32 | Opcode::MoveFloat => Width::W32,
            _ => return false,
        };

        if inst.args.len() != 2 {
            return false;
        }

        for i in 0..2 {
            let arg = &inst.args[i];

            if !arg.is_stack() {
                return false;
            }

            let slot = self.code.stack_slot(arg.stack_slot());

            if slot.kind() != StackSlotKind::Spill {
                return false;
            }

            if slot.byte_size() != bytes_for_width(width) as u32 {
                return false;
            }
        }

        true
    }

    fn is_useless_move(&self, inst: &Inst) -> bool {
        self.is_coalescable_move(inst) && inst.args[0] == inst.args[1]
    }

    fn remap(&self, mut slot_index: usize) -> usize {
        loop {
            let remapped_slot_index = self.remapped_stack_indices[slot_index];

            if remapped_slot_index == slot_index {
                return remapped_slot_index;
            }

            slot_index = remapped_slot_index;
        }
    }

    /*fn remap_stack_slot(&self, slot: StackSlotId) -> &StackSlot {
        self.code.stack_slot(StackSlotId(self.remap(slot.0)))
    }

    fn remap_stack_slot_mut(&mut self, slot: StackSlotId) -> &mut StackSlot {
        let slot_index = self.remap(slot.0);
        self.code.stack_slot_mut(StackSlotId(slot_index))
    }*/

    fn remap_stack_slot_id(&mut self, slot: StackSlotId) -> StackSlotId {
        StackSlotId(self.remap(slot.0))
    }

    fn is_remapped_stack_slot(&self, slot: StackSlotId) -> bool {
        self.remapped_stack_indices[slot.0] != slot.0
    }

    fn add_edge(&mut self, u: StackSlotId, v: StackSlotId) {
        if u == v {
            return;
        }

        self.interference.add(u.0 as _, v.0 as _);
    }

    fn run(&mut self, assigned_escaped_stack_slots: &Vec<StackSlotId>) {
        let this = unsafe { &mut *(self as *mut Self) };

        let mut stack_liveness_adapter = StackSlotLivenessAdapter::new(this.code);
        let mut stack_liveness = StackSlotLiveness::new(&mut stack_liveness_adapter);

        stack_liveness.compute();

        for block in self.code.indices() {
            let mut local_calc =
                LocalCalc::<StackSlotLivenessAdapter>::new(&mut stack_liveness, block);

            let interfere = |this: &mut Self, local_calc: &LocalCalc<_>, inst_index: usize| {
                let this2 = unsafe { &mut *(this as *mut Self) };
                let prev_inst = this.code.block(block).get(inst_index);
                let next_inst = this.code.block(block).get(inst_index.wrapping_add(1));

                if let Some(prev_inst) = prev_inst.filter(|x| this.is_coalescable_move(x)) {
                    let mov = CoalescableMove {
                        src: prev_inst.args[0].stack_slot().0 as _,
                        dst: prev_inst.args[1].stack_slot().0 as _,
                        frequency: this.code.block(block).frequency as _,
                    };

                    this.coalescable_moves.push(mov);
                    for other_slot in local_calc.live().iter() {
                        let other_slot = StackSlotId(other_slot.key());
                        if other_slot.0 as u32 != mov.src {
                            this2.add_edge(StackSlotId(mov.src as _), other_slot);
                        }
                    }

                    this2
                        .code
                        .block_mut(block)
                        .get_mut(inst_index)
                        .replace(&mut Inst::default());
                }

                let prev_inst = this.code.block(block).get(inst_index);

                Inst::for_each_def_arg(prev_inst, next_inst, this.code, |arg, _, _, _| {
                    if !arg.is_stack() {
                        return;
                    }

                    let slot = arg.stack_slot();

                    if this2.code.stack_slot(slot).kind != StackSlotKind::Spill {
                        return;
                    }

                    for other_slot in local_calc.live().iter() {
                        let other_slot = StackSlotId(other_slot.key());

                        this2.add_edge(slot, other_slot);
                    }
                });
            };

            for inst_index in (0..self.code.block(block).len()).rev() {
                let inst = &self.code.block(block)[inst_index];

                // Kill dead stores. For simplicity we say that a store is killable if it has only late
                // defs and those late defs are to things that are dead right now. We only do that
                // because that's the only kind of dead stack store we will see here.
                if !inst.has_non_arg_effects(self.code) {
                    let mut ok = true;

                    inst.for_each_arg(self.code, |_, arg, role, _, _| {
                        if role.is_early_def() {
                            ok = false;
                            return;
                        }

                        if !role.is_late_def() {
                            return;
                        }

                        if !arg.is_stack() {
                            ok = false;
                            return;
                        }

                        let slot = arg.stack_slot();

                        if self.code.stack_slot(slot).kind != StackSlotKind::Spill {
                            ok = false;
                            return;
                        }

                        if local_calc.is_live(slot) {
                            ok = false;
                            return;
                        }
                    });

                    if ok {
                        
                        self.code
                            .block_mut(block)
                            .get_mut(inst_index)
                            .replace(&mut Inst::default());
                    }
                }

                interfere(self, &local_calc, inst_index);

                local_calc.execute(inst_index);
            }

            interfere(self, &local_calc, usize::MAX);

            self.code.block_mut(block).retain(|x| x != &Inst::default());
        }

       

        self.coalescable_moves
            .sort_by(|a, b| b.frequency.partial_cmp(&a.frequency).unwrap());

        for i in 0..self.coalescable_moves.len() {
            let slot_to_kill = self.remap(self.coalescable_moves[i].src as _);
            let slot_to_keep = self.remap(self.coalescable_moves[i].dst as _);

            if slot_to_kill == slot_to_keep {
                continue;
            }

            if self
                .interference
                .contains(slot_to_kill as _, slot_to_keep as _)
            {
                continue;
            }

            self.remapped_stack_indices[slot_to_kill as usize] = slot_to_keep as _;

            let interferring_slots = self
                .interference
                .iter(slot_to_kill as _)
                .iter()
                .collect::<Vec<_>>();
            for interfering_slot in interferring_slots {
                self.add_edge(
                    StackSlotId(interfering_slot as _),
                    StackSlotId(slot_to_keep as _),
                );
            }
            
            self.interference.may_clear(slot_to_kill as _);
        }

        for block in self.code.indices() {
            for inst_index in 0..self.code.block(block).len() {
                for i in 0..self.code.block(block)[inst_index].args.len() {
                    /*if arg.is_stack() {
                        let slot = arg.stack_slot();
                        let slot = self.remap_stack_slot_id(slot);
                        *arg = Arg::new_stack(slot, arg.offset() as _);
                    }*/

                    if self.code.block(block)[inst_index].args[i].is_stack() {
                        let slot = self.code.block(block)[inst_index].args[i].stack_slot();
                        let slot = self.remap_stack_slot_id(slot);
                        
                        self.code.block_mut(block)[inst_index].args[i] = Arg::new_stack(
                            slot,
                            self.code.block(block)[inst_index].args[i].offset() as _,
                        );
                    }
                }

                if self.is_useless_move(&self.code.block(block)[inst_index]) {
                    
                    self.code.block_mut(block)[inst_index] = Inst::default();
                }
            }
        }

        // Now we assign stack locations. At its heart this algorithm is just first-fit. For each
        // StackSlot we just want to find the offsetFromFP that is closest to zero while ensuring no
        // overlap with other StackSlots that this overlaps with.
        let mut other_slots = assigned_escaped_stack_slots.clone();

        for slot in (0..self.code.proc.stack_slots.len()).map(StackSlotId) {
            if self.is_remapped_stack_slot(slot) {
                continue;
            }

            if self.code.stack_slot(slot).offset_from_fp != 0 {
                continue; // already assigned offset
            }

            other_slots.resize(assigned_escaped_stack_slots.len(), StackSlotId(usize::MAX));

            for other_slot_index in self.interference.iter(slot.0 as _).iter() {
                if self.is_remapped_stack_slot(StackSlotId(other_slot_index as _)) {
                    continue;
                }

                let other_slot = StackSlotId(other_slot_index as _);
                other_slots.push(other_slot);
            }

            super::stack_allocation::assign(self.code, slot, &other_slots);
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CoalescableMove {
    src: u32,
    dst: u32,
    frequency: f32,
}

impl Default for CoalescableMove {
    fn default() -> Self {
        CoalescableMove {
            src: u32::MAX,
            dst: u32::MAX,
            frequency: 0.0,
        }
    }
}
