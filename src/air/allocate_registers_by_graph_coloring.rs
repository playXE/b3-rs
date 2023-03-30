#![allow(dead_code, unused_mut, unused_variables)]
use std::collections::{HashMap, HashSet};

use bitvec::vec::BitVec;
use tinyvec::TinyVec;

use super::{
    arg::{Arg, ArgRole},
    basic_block::BasicBlockId,
    code::Code,
    fix_spills_after_terminals::fix_spills_after_terminals,
    form_table::is_x86,
    insertion_set::InsertionSet,
    inst::Inst,
    liveness_adapter::{TmpLiveness, TmpLivenessAdapter},
    opcode::Opcode,
    pad_interference::pad_interference,
    stack_slot::StackSlotKind,
    tmp::{AbsoluteIndexed, Tmp},
    tmp_width::TmpWidth,
    use_counts::UseCounts,
};
use crate::{
    bank::Bank,
    jit::{reg::Reg, register_set::RegisterSet},
    liveness::{IndexSparseSetEntry, Iterable, LocalCalc},
    utils::interference_graph::*,
    width::{bytes_for_width, Width},
};

/// We have two register allocators, both fundamentally derived from Chaitin's Yorktown
/// allocator:
/// http://cs.gmu.edu/~white/CS640/p98-chaitin.pdf
///
/// We have an implementation of Briggs's optimistic allocator which is derivative of Chaitin's allocator:
/// http://www.cs.utexas.edu/users/mckinley/380C/lecs/briggs-thesis-1992.pdf
///
/// And an implementation of Andrew Appel's Iterated Register Coalescing which is derivative of Briggs's allocator.
/// http://www.cs.cmu.edu/afs/cs/academic/class/15745-s07/www/papers/george.pdf
pub fn allocate_registers_by_graph_coloring(code: &mut Code) {
    let use_counts = UseCounts::new(code);
    let mut allocator = GraphColoringRegisterAllocation {
        code: unsafe { std::mem::transmute(code) },
        use_counts,
    };

    allocator.run();
}

struct MoveOperands {
    src_index: u32,
    dst_index: u32,
}

/// The AbstractColoringAllocator defines all the code that is independant
/// from the bank or register and can be shared when allocating registers.
struct AbstractColoringAllocator<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    regs_in_priority_order: Vec<Reg>,
    last_precolored_register_index: u32,
    interference_edges: InterferenceSet,
    adjacency_list: Vec<TinyVec<[u32; 4]>>,
    degrees: Vec<u32>,

    biases: HashMap<u32, HashSet<u32>>,

    coalescing_candidates: Vec<MoveOperands>,
    move_list: Vec<HashSet<usize>>,

    colored_tmp: Vec<Reg>,
    spilled_tmps: Vec<u32>,

    coalesced_tmps: Vec<u32>,

    is_on_select_stack: BitVec<u32, bitvec::order::Lsb0>,
    select_stack: Vec<u32>,
    simplify_worklist: Vec<u32>,
    spill_worklist: BitVec<u32, bitvec::order::Lsb0>,

    has_selected_spill: bool,
    has_coalesced_non_trivial_move: bool,

    coalesced_tmps_at_spill: Vec<u32>,

    unspillable_temps: &'a BitVec<u32, bitvec::order::Lsb0>,
    use_counts: &'a UseCounts,
    code: &'a mut Code<'a>,
    pinned_regs: TinyVec<[Reg; 4]>,
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank>
    AbstractColoringAllocator<'a, InterferenceSet, BANK>
{
    fn new(
        code: &'a mut Code<'a>,
        regs_in_priority_order: Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_temps: &'a BitVec<u32, bitvec::order::Lsb0>,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self {
        let mut allocator = Self {
            regs_in_priority_order,
            last_precolored_register_index,
            interference_edges,
            adjacency_list: vec![TinyVec::new(); tmp_array_size],
            degrees: vec![0; tmp_array_size],

            biases: HashMap::new(),

            coalescing_candidates: Vec::new(),
            move_list: vec![HashSet::new(); tmp_array_size],

            colored_tmp: vec![Reg::default(); tmp_array_size],
            spilled_tmps: Vec::new(),

            coalesced_tmps: vec![0; tmp_array_size],

            is_on_select_stack: BitVec::new(),
            select_stack: Vec::new(),
            simplify_worklist: Vec::new(),
            spill_worklist: BitVec::new(),

            has_selected_spill: false,
            has_coalesced_non_trivial_move: false,

            coalesced_tmps_at_spill: Vec::new(),

            unspillable_temps,
            use_counts,
            code,
            pinned_regs: Default::default(),
        };

        allocator.initialize_degrees(tmp_array_size);
        allocator
    }

    fn tmp_to_index(&self, tmp: Tmp) -> u32 {
        match BANK {
            Bank::GP => AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp) as _,
            Bank::FP => AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp) as _,
        }
    }

    fn register_count(&self) -> u32 {
        self.regs_in_priority_order.len() as u32
    }

    fn get_alias(&self, tmp_index: u32) -> u32 {
        let mut alias = tmp_index;
        while let Some(next_alias) = self.coalesced_tmps.get(alias as usize) {
            alias = *next_alias;
        }
        alias
    }

    fn add_edge(&mut self, a: u32, b: u32) {
        if a == b {
            return;
        }

        self.add_edge_distinct(a, b);
    }

    fn add_to_spill(&mut self, to_spill: usize) {
        if self.unspillable_temps.len() > to_spill && !self.unspillable_temps[to_spill] {
            if self.spill_worklist.len() <= to_spill {
                self.spill_worklist.resize(to_spill + 1, false);
            }

            self.spill_worklist.set(to_spill, true);
        }
    }

    fn is_precolored(&self, tmp_index: u32) -> bool {
        tmp_index <= self.last_precolored_register_index
    }

    fn initialize_degrees(&mut self, tmp_array_size: usize) {
        self.degrees.resize(tmp_array_size, 0);

        let first_non_reg_index = self.last_precolored_register_index + 1;
        for i in 0..first_non_reg_index {
            self.degrees[i as usize] = u32::MAX;
        }
        println!("first_non_reg_index: {}", first_non_reg_index);
        println!("tmp_array_size: {}", tmp_array_size);
        if first_non_reg_index < (tmp_array_size as u32 - first_non_reg_index) {
            self.degrees[first_non_reg_index as usize..(tmp_array_size - first_non_reg_index as usize)]
                .fill(0);
        }
        
    }

    fn add_edge_distinct(&mut self, a: u32, b: u32) {
        let is_new_edge = self.add_interference_edge(a, b);

        if is_new_edge {
            if !self.is_precolored(a) {
                self.adjacency_list[a as usize].push(b);
                self.degrees[a as usize] += 1;
            }

            if !self.is_precolored(b) {
                self.adjacency_list[b as usize].push(a);
                self.degrees[b as usize] += 1;
            }
        }
    }

    fn add_edge_distinct_without_degree_change(&mut self, a: u32, b: u32) -> bool {
        let is_new_edge = self.add_interference_edge(a, b);

        if is_new_edge {
            if !self.is_precolored(a) {
                self.adjacency_list[a as usize].push(b);
            }

            if !self.is_precolored(b) {
                self.adjacency_list[b as usize].push(a);
            }

            return true;
        }

        false
    }

    fn has_been_simplified(&self, index: u32) -> bool {
        self.is_on_select_stack
            .get(index as usize)
            .map(|x| *x)
            .unwrap_or(false)
            || self
                .coalesced_tmps
                .get(index as usize)
                .map(|&alias| alias != 0)
                .unwrap_or(false)
    }

    fn for_each_adjacent(&self, tmp_index: u32, mut f: impl FnMut(u32)) {
        for adjacent_tmp_index in self.adjacency_list[tmp_index as usize].iter().copied() {
            if !self.has_been_simplified(adjacent_tmp_index) {
                f(adjacent_tmp_index);
            }
        }
    }

    fn can_be_safely_coalesced(&self, u: u32, v: u32) -> bool {
        if self.is_precolored(u) {
            return self.precolored_coalescing_heuristic(u, v);
        }

        self.conservative_heuristic(u, v)
    }

    fn conservative_heuristic(&self, u: u32, v: u32) -> bool {
        // This is using the Briggs' conservative coalescing rule:
        // If the number of combined adjacent node with a degree >= K is less than K,
        // it is safe to combine the two nodes. The reason is that we know that if the graph
        // is colorable, we have fewer than K adjacents with high order and there is a color
        // for the current node.

        if self.unspillable_temps.get(u as usize) != self.unspillable_temps.get(v as usize) {
            return false;
        }

        let adjacency_of_u = &self.adjacency_list[u as usize];
        let adjacency_of_v = &self.adjacency_list[v as usize];

        let mut high_order_adjacents = vec![];

        let mut num_candidates = adjacency_of_u.len() + adjacency_of_v.len();

        if num_candidates < self.register_count() as usize {
            // Shortcut: if the total number of adjacents is less than the number of register, the condition is always met.
            return true;
        }

        for adjacent_tmp_index in adjacency_of_u.iter().copied() {
            num_candidates -= 1;

            if !self.has_been_simplified(adjacent_tmp_index)
                && self.degrees[adjacent_tmp_index as usize] >= self.register_count()
            {
                high_order_adjacents.push(adjacent_tmp_index);

                if high_order_adjacents.len() >= self.register_count() as usize {
                    return false;
                }
            } else if high_order_adjacents.len() + num_candidates < self.register_count() as usize {
                return true;
            }
        }

        for adjacent_tmp_index in adjacency_of_v.iter().copied() {
            num_candidates -= 1;

            if !self.has_been_simplified(adjacent_tmp_index)
                && self.degrees[adjacent_tmp_index as usize] >= self.register_count()
                && high_order_adjacents
                    .iter()
                    .find(|x| x == &&adjacent_tmp_index)
                    .is_none()
            {
                high_order_adjacents.push(adjacent_tmp_index);
                if high_order_adjacents.len() >= self.register_count() as usize {
                    return false;
                }
            } else if high_order_adjacents.len() + num_candidates < self.register_count() as usize {
                return true;
            }
        }

        true
    }

    fn precolored_coalescing_heuristic(&self, u: u32, v: u32) -> bool {
        assert!(self.is_precolored(u) && !self.is_precolored(v));

        // If any adjacent of the non-colored node is not an adjacent of the colored node AND has a degree >= K
        // there is a risk that this node needs to have the same color as our precolored node. If we coalesce such
        // move, we may create an uncolorable graph.
        let adjacents_of_v = &self.adjacency_list[v as usize];

        for adjacency_tmp_index in adjacents_of_v.iter().copied() {
            if !self.is_precolored(adjacency_tmp_index)
                && !self.has_been_simplified(adjacency_tmp_index)
                && self.degrees[adjacency_tmp_index as usize] >= self.register_count()
                && !self.has_inteferece_edge(u, adjacency_tmp_index)
            {
                return false;
            }
        }

        true
    }

    fn add_bias(&mut self, u: u32, v: u32) {
        // We implement biased coloring as proposed by Briggs. See section
        // 5.3.3 of his thesis for more information: http://www.cs.utexas.edu/users/mckinley/380C/lecs/briggs-thesis-1992.pdf
        // The main idea of biased coloring is this:
        // We try to coalesce a move between u and v, but the conservative heuristic
        // fails. We don't want coalesce the move because we don't want to risk
        // creating an uncolorable graph. However, if the conservative heuristic fails,
        // it's not proof that the graph is uncolorable if the move were indeed coalesced.
        // So, when we go to color the tmps, we'll remember that we really want the
        // same register for u and v, and if legal, we will assign them to the same register.
        if !self.is_precolored(u) {
            let mut set = self.biases.entry(u).or_insert_with(|| HashSet::new());
            set.insert(u);
        }

        if !self.is_precolored(v) {
            let mut set = self.biases.entry(v).or_insert_with(|| HashSet::new());
            set.insert(v);
        }
    }

    fn select_spill(&mut self) -> u32 {
        if !self.has_selected_spill {
            self.has_selected_spill = true;

            if self.has_coalesced_non_trivial_move {
                self.coalesced_tmps_at_spill = self.coalesced_tmps.clone();
            }
        }

        let mut spill_worklist = self.spill_worklist.iter_ones();

        let mut victim_index = spill_worklist.next().unwrap();

        let mut max_score = 0f32;

        for index in spill_worklist {
            let uses = self.use_counts.num_warm_uses_and_defs::<BANK>(index);

            if uses == 0.0 || uses.is_nan() {
                victim_index = index;
                break;
            }

            // Higher score means more desirable to spill. Lower scores maximize the likelihood that a tmp
            // gets a register.
            let mut score = self.degrees[index as usize] as f32 / uses;

            if self.use_counts.is_const_def::<BANK>(index) {
                score *= 2.0;
            }

            if score > max_score {
                max_score = score;
                victim_index = index;
            }
        }

        victim_index as _
    }

    fn assign_colors(&mut self) {
        assert!(self.simplify_worklist.is_empty());
        assert!(self.spill_worklist.is_empty());

        self.clear_interference_edges();

        self.degrees.clear();
        self.move_list.clear();
        self.simplify_worklist.clear();
        self.spill_worklist.clear();

        self.colored_tmp
            .resize(self.adjacency_list.len(), Reg::default());

        {
            let mut now_aliased_biases = TinyVec::<[u32; 4]>::new();

            for key in self.biases.keys().copied() {
                if key != self.get_alias(key) {
                    now_aliased_biases.push(key);
                }
            }

            for key in now_aliased_biases {
                let mut keys_biases = self.biases.remove(&key).unwrap();
                let is_new_entry = self
                    .biases
                    .insert(self.get_alias(key), Default::default())
                    .is_none();

                if is_new_entry {
                    self.biases
                        .get_mut(&self.get_alias(key))
                        .unwrap()
                        .extend(keys_biases);
                } else {
                    self.biases
                        .get_mut(&self.get_alias(key))
                        .unwrap()
                        .extend(keys_biases.drain());
                }
            }
        }

        while let Some(tmp_index) = self.select_stack.pop() {
            let mut colored_registers = RegisterSet::default();

            for adjacent_tmp_idnex in self.adjacency_list[tmp_index as usize].iter().copied() {
                let alias_tmp_index = self.get_alias(adjacent_tmp_idnex);
                let reg = self.colored_tmp[alias_tmp_index as usize];

                if reg != Reg::default() {
                    colored_registers.add(reg, Width::W64)
                }
            }

            let mut color_assigned = false;

            let biases = self.biases.get(&tmp_index);

            if let Some(bias) = biases {
                for desired_bias in bias.iter().copied() {
                    let desired_color = self.colored_tmp[desired_bias as usize];

                    if desired_color != Reg::default() {
                        if !colored_registers.contains(desired_color, Width::W64) {
                            self.colored_tmp[tmp_index as usize] = desired_color;
                            color_assigned = true;
                            break;
                        }
                    }
                }
            }

            if !color_assigned {
                for reg in self.regs_in_priority_order.iter().copied() {
                    if !colored_registers.contains(reg, Width::W64) {
                        self.colored_tmp[tmp_index as usize] = reg;
                        color_assigned = true;
                        break;
                    }
                }
            }

            if !color_assigned {
                self.spilled_tmps.push(tmp_index);
            }
        }

        self.select_stack.clear();

        if self.spilled_tmps.is_empty() {
            self.coalesced_tmps_at_spill.clear();
        } else {
            self.colored_tmp.clear();
        }
    }

    fn add_interference_edge(&mut self, u: u32, v: u32) -> bool {
        self.interference_edges.add_and_return_is_new_entry(u, v)
    }

    fn has_inteferece_edge(&self, u: u32, v: u32) -> bool {
        self.interference_edges.contains(u, v)
    }

    fn clear_interference_edges(&mut self) {
        self.interference_edges.clear();
    }

    fn dump_interference_graph_in_dot<W>(&mut self, fmt: &mut W) -> std::fmt::Result
    where
        W: std::fmt::Write,
    {
        writeln!(fmt, "graph InterferenceGraph {{").unwrap();
        let mut tmps_with_interference = HashSet::new();

        let tmp_from_index = |x: u32| {
            if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(x as _)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(x as _)
            }
        };

        let absolute_index = |tmp: &Tmp| {
            if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::absolute_index(tmp)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::absolute_index(tmp)
            }
        };

        self.interference_edges.for_each(|u, v| {
            tmps_with_interference.insert(tmp_from_index(u));
            tmps_with_interference.insert(tmp_from_index(v));
        });

        for tmp in tmps_with_interference {
            let tmp_index = absolute_index(&tmp);
            if tmp_index < self.degrees.len() {
                writeln!(
                    fmt,
                    "    {} [label=\"{}({})\"]",
                    tmp.internal_value(),
                    tmp,
                    self.degrees[tmp_index]
                )?;
            } else {
                writeln!(fmt, "    {} [label=\"{}\"]", tmp.internal_value(), tmp)?;
            }
        }

        self.interference_edges.for_each(|u, v| {
            writeln!(fmt, "    {} -- {}", tmp_from_index(u), tmp_from_index(v)).unwrap();
        });

        writeln!(fmt, "}}")
    }
}

struct MoveSet {
    position_in_move_list: usize,
    move_list: Vec<usize>,
    low_priority_move_list: Vec<usize>,
}

impl MoveSet {
    fn new() -> Self {
        Self {
            position_in_move_list: 0,
            move_list: Vec::new(),
            low_priority_move_list: Vec::new(),
        }
    }

    fn add_move(&mut self) -> usize {
        let next_index = self.position_in_move_list;
        self.position_in_move_list += 1;
        self.move_list.push(next_index);
        next_index
    }

    fn add_low_priority_move(&mut self) -> usize {
        let next_index = self.position_in_move_list;
        self.position_in_move_list += 1;
        self.low_priority_move_list.push(next_index);
        next_index
    }

    fn for_each_move(&self, mut f: impl FnMut(usize)) {
        for index in self.move_list.iter().copied() {
            if index != usize::MAX {
                f(index);
            }
        }
    }

    fn for_each_move_mut(&mut self, mut f: impl FnMut(&mut usize)) {
        for index in self.move_list.iter_mut() {
            if *index != usize::MAX {
                f(index);
            }
        }
    }

    fn for_each_low_priority_move(&self, mut f: impl FnMut(usize)) {
        for index in self.low_priority_move_list.iter().copied() {
            if index != usize::MAX {
                f(index);
            }
        }
    }

    fn for_each_low_priority_move_mut(&mut self, mut f: impl FnMut(&mut usize)) {
        for index in self.low_priority_move_list.iter_mut() {
            if *index != usize::MAX {
                f(index);
            }
        }
    }

    fn clear(&mut self) {
        self.position_in_move_list = 0;
        self.move_list.clear();
        self.low_priority_move_list.clear();
    }
}

struct Briggs<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    base: AbstractColoringAllocator<'a, InterferenceSet, BANK>,
    worklist_moves: MoveSet,
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> Briggs<'a, InterferenceSet, BANK> {
    fn new(
        code: &'a mut Code<'a>,
        regs_in_priority_order: Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_temps: &'a BitVec<u32, bitvec::order::Lsb0>,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self {
        Self {
            base: AbstractColoringAllocator::new(
                code,
                regs_in_priority_order,
                last_precolored_register_index,
                tmp_array_size,
                unspillable_temps,
                use_counts,
                interference_edges,
            ),
            worklist_moves: MoveSet::new(),
        }
    }

    fn allocate(&mut self) {
        let mut changed;

        let this2 = unsafe { &mut *(self as *mut Self) };

        let mut coalesce_move = |mov: &mut usize, changed: &mut bool| {
            let coalesced = this2.coalesce(mov);

            if coalesced {
                *changed = true;
                // We won't need to handle this move in the future since it's already coalesced.
                *mov = usize::MAX;
            }
        };

        // We first coalesce until we can't coalesce any more.
        loop {
            changed = false;
            self.worklist_moves.for_each_move_mut(|mov| {
                coalesce_move(mov, &mut changed);
            });

            if !changed {
                break;
            }
        }

        loop {
            changed = false;
            self.worklist_moves.for_each_low_priority_move_mut(|mov| {
                coalesce_move(mov, &mut changed);
            });

            if !changed {
                break;
            }
        }

        // Then we create our select stack. The invariant we start with here is that nodes in
        // the interference graph with degree >= k are on the spill list. Nodes with degree < k
        // are on the simplify worklist. A node can move from the spill list to the simplify
        // list (but not the other way around, note that this is different than IRC because IRC
        // runs this while coalescing, but we do all our coalescing before this). Once a node is
        // added to the select stack, it's not on either list, but only on the select stack.
        // Once on the select stack, logically, it's no longer in the interference graph.

        self.make_initial_worklist();

        loop {
            changed = false;

            while self.simplify_worklist.len() != 0 {
                self.simplify();
            }

            if !self.spill_worklist.is_empty() {
                self.select_spill();
                changed = true;
            }

            if !changed {
                break;
            }
        }

        self.assign_colors();
    }

    fn coalesce(&mut self, move_index: &mut usize) -> bool {
        let move_operands = &self.coalescing_candidates[*move_index];
        let mut u = self.get_alias(move_operands.src_index);
        let mut v = self.get_alias(move_operands.dst_index);

        if self.is_precolored(v) {
            std::mem::swap(&mut u, &mut v);
        }

        if u == v {
            return false;
        }

        if self.is_precolored(v) || self.has_inteferece_edge(u, v) {
            // No need to ever consider this move again if it interferes.
            // No coalescing will remove the interference.

            *move_index = usize::MAX;

            return false;
        }

        if self.can_be_safely_coalesced(u, v) {
            self.combine(u, v);
            self.has_coalesced_non_trivial_move = true;

            return true;
        }

        self.add_bias(u, v);

        false
    }

    fn combine(&mut self, u: u32, v: u32) {
        self.coalesced_tmps[v as usize] = u;

        let this2 = unsafe { &mut *(self as *mut Self) };

        for mov in self.move_list[v as usize].iter().copied() {
            this2.move_list[u as usize].insert(mov);
        }

        self.for_each_adjacent(v, |adjacent_tmp_index| {
            if this2.add_edge_distinct_without_degree_change(adjacent_tmp_index, u) {
                // If we added a new edge between the adjacentTmp and u, it replaces the edge
                // that existed with v.
                // The degree of adjacentTmp remains the same since the edge just changed from u to v.
                // All we need to do is update the degree of u.
                if !this2.is_precolored(u) {
                    this2.degrees[u as usize] += 1;
                }
            } else {
                this2.decrement_degree(adjacent_tmp_index);
            }
        });
    }

    fn decrement_degree(&mut self, tmp_index: u32) {
        self.degrees[tmp_index as usize] -= 1;
    }

    fn decrement_degree_in_simplification(&mut self, tmp_index: u32) {
        let old_degree = self.degrees[tmp_index as usize];
        self.degrees[tmp_index as usize] = old_degree - 1;

        if old_degree == self.register_count() {
            self.spill_worklist.set(tmp_index as usize, false);
            self.simplify_worklist.push(tmp_index);
        }
    }

    fn make_initial_worklist(&mut self) {
        self.simplify_worklist.clear();
        self.spill_worklist.clear();

        let first_non_reg_index = self.last_precolored_register_index + 1;
        let register_count = self.register_count();

        for i in first_non_reg_index as usize..self.degrees.len() {
            if self.has_been_simplified(i as _) {
                continue;
            }

            let degree = self.degrees[i];

            if degree < register_count {
                self.simplify_worklist.push(i as u32);
            } else {
                self.add_to_spill(i);
            }
        }
    }

    /// Low-degree vertex can always be colored: just pick any of the color taken by any
    /// other adjacent verices.
    /// The "Simplify" phase takes a low-degree out of the interference graph to simplify it.
    fn simplify(&mut self) {
        let last_index = self.simplify_worklist.pop().unwrap();

        self.select_stack.push(last_index);
        self.is_on_select_stack.set(last_index as usize, true);
        let this2 = unsafe { &mut *(self as *mut Self) };
        self.for_each_adjacent(last_index, |adjacent_tmp_index| {
            this2.decrement_degree_in_simplification(adjacent_tmp_index);
        });
    }

    fn select_spill(&mut self) {
        let victim_index = self.base.select_spill();
        self.spill_worklist.set(victim_index as usize, false);
        self.simplify_worklist.push(victim_index);
    }

    fn assign_colors(&mut self) {
        self.worklist_moves.clear();
        self.base.assign_colors();
    }
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::Deref
    for Briggs<'a, InterferenceSet, BANK>
{
    type Target = AbstractColoringAllocator<'a, InterferenceSet, BANK>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::DerefMut
    for Briggs<'a, InterferenceSet, BANK>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

struct ColoringAllocator<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    allocator: Briggs<'a, InterferenceSet, BANK>,
    tmp_width: &'a TmpWidth,
}

fn tmp_array_size<const BANK: Bank>(code: &Code) -> usize {
    let num_tmps = code.num_tmps(BANK);

    if BANK == Bank::GP {
        AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(num_tmps))
    } else {
        AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(num_tmps))
    }
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank>
    ColoringAllocator<'a, InterferenceSet, BANK>
{
    pub fn new(
        code: &'a mut Code<'a>,
        use_counts: &'a UseCounts,
        unspillable_tmps: &'a BitVec<u32, bitvec::order::LocalBits>,
        interference_set: InterferenceSet,
        tmp_width: &'a TmpWidth,
    ) -> Self {
        let regs_in_priority_order = code.regs_in_priority_order(BANK).to_vec();
        let last_reg = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::last_machine_register_index()
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::last_machine_register_index()
        };
        let tmp_array_size = tmp_array_size::<BANK>(code);

        let mut this = Self {
            allocator: Briggs::new(
                code,
                regs_in_priority_order,
                last_reg as _,
                tmp_array_size,
                unspillable_tmps,
                use_counts,
                interference_set,
            ),
            tmp_width,
        };

        this.allocator
            .code
            .pinned_regs
            .to_register_set()
            .for_each(|reg| {
                if (BANK == Bank::GP && reg.is_gpr()) || (BANK == Bank::FP && reg.is_fpr()) {
                    this.allocator.pinned_regs.push(reg);
                    this.allocator.regs_in_priority_order.push(reg);
                }
            });

        let index = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(
                this.code.num_tmps(BANK),
            ))
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(
                this.code.num_tmps(BANK),
            ))
        };

        this.interference_edges.set_max_index(index as _);

        this.initialize_precolored_tmp();
        this.build();

        this
    }

    fn tmp_from_absolute_index(index: usize) -> Tmp {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(index)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(index)
        }
    }

    fn tmp_to_index(tmp: Tmp) -> usize {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
        }
    }

    fn is_useless_move(&self, inst: &Inst) -> bool {
        Self::may_be_coalescable_impl(inst, None) && inst.args[0].tmp() == inst.args[1].tmp()
    }

    fn get_alias(&self, tmp: Tmp) -> Tmp {
        Self::tmp_from_absolute_index(self.allocator.get_alias(Self::tmp_to_index(tmp) as _) as _)
    }

    fn get_alias_when_spilling(&mut self, tmp: Tmp) -> Tmp {
        if self.coalesced_tmps_at_spill.is_empty() {
            return tmp;
        }

        let mut alias_index = Self::tmp_to_index(tmp);

        while let Some(next_alias_index) = self.coalesced_tmps_at_spill.get(alias_index) {
            if next_alias_index == &0 {
                break;
            }

            alias_index = *next_alias_index as usize;
        }

        Self::tmp_from_absolute_index(alias_index as _)
    }

    fn requires_spilling(&self) -> bool {
        !self.spilled_tmps.is_empty()
    }

    fn allocate_reg(&mut self, tmp: Tmp) -> Reg {
        let reg = self.colored_tmp[Self::tmp_to_index(tmp) as usize];

        if reg == Reg::default() {
            panic!("FATAL: No color for {}\nCode:\n{}", tmp, self.code);
        }

        reg
    }

    fn initialize_precolored_tmp(&mut self) {
        let ix = self.last_precolored_register_index;
        self.colored_tmp.resize(ix as usize + 1, Reg::default());
        for i in 1..self.last_precolored_register_index {
            let tmp = Self::tmp_from_absolute_index(i as _);
            self.colored_tmp[i as usize] = tmp.reg();
        }
    }
    fn build(&mut self) {
        self.coalescing_candidates.clear();
        self.worklist_moves.clear();

        let this2 = unsafe { &mut *(self as *mut Self) };
        let this3 = unsafe { &mut *(self as *mut Self as *mut Self) };
        let mut tmp_liveness =
            TmpLivenessAdapter::<BANK>::new(unsafe { std::mem::transmute(&mut this2.code) });
        {
            let mut liveness = TmpLiveness::new(&mut tmp_liveness);

            for block in (0..self.code.blocks.len()).map(BasicBlockId) {
                let mut local_calc = LocalCalc::new(&mut liveness, block);

                for inst_index in (0..self.code.block(block).len()).rev() {
                    let inst = &this3.code.block(block)[inst_index];
                    let next_inst = if inst_index + 1 < self.code.block(block).len() {
                        Some(&this3.code.block(block)[inst_index + 1])
                    } else {
                        None
                    };

                    self.build_for_inst(Some(inst), next_inst, &local_calc);
                    local_calc.execute(inst_index);
                }

                self.build_for_inst(None, Some(&this3.code.block(block)[0]), &local_calc);
            }
        }

        self.build_low_priority_move_list();
    }
    fn build_for_inst<'b, 'c>(
        &mut self,
        prev_inst: Option<&Inst>,
        next_inst: Option<&Inst>,
        local_calc: &LocalCalc<'b, 'c, TmpLivenessAdapter<BANK>>,
    ) {
        let this2 = unsafe { &mut *(self as *mut Self as *mut Self) };
        let this3 = unsafe { &mut *(this2 as *mut Self as *mut Self) };
        Inst::for_each_def_with_extra_clobbered_regs(
            prev_inst,
            next_inst,
            this2.code,
            |arg, _, arg_bank, _, preserve64| {
                if arg_bank != BANK {
                    return;
                }

                // All the Def()s interfere with each other and with all the extra clobbered Tmps.
                // We should not use forEachDefWithExtraClobberedRegs() here since colored Tmps
                // do not need interference edges in our implementation.

                Inst::for_each_def(
                    prev_inst,
                    next_inst,
                    this3.code,
                    |other_arg, _, arg_bank, def_width| {
                        if arg_bank != BANK {
                            return;
                        }

                        if def_width <= Width::W64 && preserve64 {
                            return;
                        }

                        self.add_edge(arg, other_arg);
                    },
                );
            },
        );

        if let Some(prev_inst) = prev_inst.filter(|x| self.may_be_coalescable(x)) {
            // We do not want the Use() of this move to interfere with the Def(), even if it is live
            // after the Move. If we were to add the interference edge, it would be impossible to
            // coalesce the Move even if the two Tmp never interfere anywhere.
            let mut def_tmp = Tmp::empty();
            let mut use_tmp = Tmp::empty();

            prev_inst.for_each_tmp(this2.code, |arg_tmp, role, _, _| {
                if role.is_late_def() {
                    def_tmp = arg_tmp;
                } else {
                    assert!(role.is_early_use());
                    use_tmp = arg_tmp;
                }
            });

            let next_move_index = self.coalescing_candidates.len();

            self.coalescing_candidates.push(MoveOperands {
                src_index: Self::tmp_to_index(use_tmp) as _,
                dst_index: Self::tmp_to_index(def_tmp) as _,
            });

            let new_index_in_worklist = self.worklist_moves.add_move();
            assert_eq!(new_index_in_worklist, next_move_index);

            for arg in prev_inst.args.iter() {
                let list = &mut self.move_list[Self::tmp_to_index(arg.tmp())];
                list.insert(next_move_index);
            }

            let mut consider_edge = |live_tmp: Tmp| {
                if live_tmp != use_tmp {
                    this2.add_edge(def_tmp, live_tmp);
                }
            };

            for live_tmp in local_calc.live().iter() {
                consider_edge(Self::tmp_from_absolute_index(live_tmp.key()));
            }

            for i in 0..self.pinned_regs.len() {
                consider_edge(Self::tmp_from_absolute_index(self.pinned_regs[i].index()));
            }

            self.add_edges(None, next_inst, local_calc.live());
        } else {
            self.add_edges(prev_inst, next_inst, local_calc.live());
        }
    }

    fn may_be_coalescable(&self, inst: &Inst) -> bool {
        Self::may_be_coalescable_impl(inst, None)
    }

    fn may_be_coalesced(&self, left: Arg, right: Arg) -> bool {
        if !left.is_tmp() || !right.is_tmp() {
            return false;
        }

        let left_tmp = left.tmp();
        let right_tmp = right.tmp();

        if left_tmp == right_tmp {
            return false;
        }

        if left_tmp.is_gp() != (BANK == Bank::GP) || right_tmp.is_gp() != (BANK == Bank::GP) {
            return false;
        }

        let left_index = Self::tmp_to_index(left_tmp);
        let right_index = Self::tmp_to_index(right_tmp);

        !self.has_inteferece_edge(left_index as _, right_index as _)
    }

    fn add_low_priority_coalescing_candidates(&mut self, left: Arg, right: Arg) {
        let left_tmp = left.tmp();
        let right_tmp = right.tmp();

        let left_index = Self::tmp_to_index(left_tmp);
        let right_index = Self::tmp_to_index(right_tmp);

        let next_move_index = self.coalescing_candidates.len();
        self.coalescing_candidates.push(MoveOperands {
            src_index: left_index as _,
            dst_index: right_index as _,
        });

        let new_index_in_worklsit = self.worklist_moves.add_low_priority_move();
        assert_eq!(new_index_in_worklsit as u32, next_move_index as u32);
        self.move_list[left_index].insert(next_move_index as _);
        self.move_list[right_index].insert(next_move_index as _);
    }

    fn build_low_priority_move_list(&mut self) {
        if !is_x86() {
            return;
        }
    }

    fn add_edges<'b>(
        &mut self,
        prev_inst: Option<&Inst>,
        next_inst: Option<&Inst>,
        live_tmps: Iterable<'b, TmpLivenessAdapter<BANK>>,
    ) {
        let this2 = unsafe { &mut *(self as *mut Self as *mut Self) };

        Inst::for_each_def_with_extra_clobbered_regs(
            prev_inst,
            next_inst,
            this2.code,
            |arg, _, arg_bank, _, preserve64| {
                if arg_bank != BANK {
                    return;
                }

                for live_tmp in live_tmps.iter() {
                    /*let live_tmp_width = self.tmp_width[live_tmp.key()];

                    if live_tmp_width <= Width::W64 && preserve64 {
                        continue;
                    }*/
                    if preserve64 {
                        continue;
                    }
                    let tmp = Self::tmp_from_absolute_index(live_tmp.key());
                    self.add_edge(arg, tmp);
                }

                for i in 0..self.pinned_regs.len() {
                    let tmp = Tmp::from_reg(self.pinned_regs[i]);
                    self.add_edge(arg, tmp);
                }
            },
        );
    }

    fn add_edge(&mut self, a: Tmp, b: Tmp) {
        self.allocator
            .add_edge(Self::tmp_to_index(a) as _, Self::tmp_to_index(b) as _);
    }

    fn may_be_coalescable_impl(inst: &Inst, tmp_width: Option<&TmpWidth>) -> bool {
        match BANK {
            Bank::GP => match inst.kind.opcode {
                Opcode::Move | Opcode::Move32 => {}
                _ => return false,
            },

            Bank::FP => match inst.kind.opcode {
                Opcode::MoveFloat | Opcode::MoveDouble => {}
                _ => return false,
            },
        }
        // Avoid the three-argument coalescable spill moves.
        if inst.args.len() != 2 {
            return false;
        }

        if !inst.args[0].is_tmp() || !inst.args[1].is_tmp() {
            return false;
        }

        // We can coalesce a Move32 so long as either of the following holds:
        // - The input is already zero-filled.
        // - The output only cares about the low 32 bits.
        //
        // Note that the input property requires an analysis over ZDef's, so it's only valid so long
        // as the input gets a register. We don't know if the input gets a register, but we do know
        // that if it doesn't get a register then we will still emit this Move32.
        if inst.kind.opcode == Opcode::Move32 {
            return false; // TODO: check if the input is zero-filled.
        }

        true
    }
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::Deref
    for ColoringAllocator<'a, InterferenceSet, BANK>
{
    type Target = Briggs<'a, InterferenceSet, BANK>;

    fn deref(&self) -> &Self::Target {
        &self.allocator
    }
}

impl<'a, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::DerefMut
    for ColoringAllocator<'a, InterferenceSet, BANK>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.allocator
    }
}

struct GraphColoringRegisterAllocation<'a, 'b> {
    code: &'a mut Code<'b>,
    use_counts: UseCounts,
}

impl<'a: 'b, 'b> GraphColoringRegisterAllocation<'a, 'b> {
    fn run(&mut self) {
        pad_interference(self.code);

        self.allocate_on_bank::<{ Bank::GP }>();
        self.allocate_on_bank::<{ Bank::FP }>();

        fix_spills_after_terminals(self.code);
    }

    fn allocate_on_bank<const BANK: Bank>(&mut self) {
        // FIXME: If a Tmp is used only from a Scratch role and that argument is !admitsStack, then
        // we should add the Tmp to unspillableTmps. That will help avoid relooping only to turn the
        // Tmp into an unspillable Tmp.
        // https://bugs.webkit.org/show_bug.cgi?id=152699

        let mut unspillable_tmps = self.compute_unspillable_tmps::<BANK>();

        let mut num_iterations = 0;
        let mut done = false;

        while !done {
            num_iterations += 1;

            if self.code.num_tmps(BANK) < 400 {
                let graph = SmallInterferenceGraph::new(InterferenceBitVector::new());

                let this = unsafe { &mut *(self as *mut Self as *mut Self) };
                let unspillable = unsafe {
                    &*(&mut unspillable_tmps as *mut BitVec<u32, bitvec::order::LocalBits>)
                };
                let mut allocator = ColoringAllocator::<_, BANK>::new(
                    this.code,
                    &this.use_counts,
                    &unspillable,
                    graph,
                    &TmpWidth {},
                );

                allocator.allocate();

                if !allocator.requires_spilling() {
                    self.assign_registers_to_tmp(&mut allocator);

                    done = true;
                } else {
                    self.add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                    done = false;
                }

                drop(unspillable);
            } else {
                let graph = LargeInterferenceGraph::new(InterferenceVector::new());

                let this = unsafe { &mut *(self as *mut Self as *mut Self) };
                let unspillable = unsafe {
                    &*(&mut unspillable_tmps as *mut BitVec<u32, bitvec::order::LocalBits>)
                };
                let mut allocator = ColoringAllocator::<_, BANK>::new(
                    this.code,
                    &this.use_counts,
                    &unspillable,
                    graph,
                    &TmpWidth {},
                );

                allocator.allocate();

                if !allocator.requires_spilling() {
                    self.assign_registers_to_tmp(&mut allocator);

                    done = true;
                } else {
                    self.add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                    done = false;
                }
            }
        }
    }

    fn absolute_index<const BANK: Bank>(tmp: &Tmp) -> usize {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(tmp)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(tmp)
        }
    }

    fn compute_unspillable_tmps<const BANK: Bank>(&self) -> BitVec<u32, bitvec::order::LocalBits> {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        struct Range {
            first: usize,
            last: usize,
            count: usize,
            admit_stack_count: usize,
        }

        impl Default for Range {
            fn default() -> Self {
                Self {
                    first: usize::MAX,
                    last: 0,
                    count: 0,
                    admit_stack_count: 0,
                }
            }
        }

        let num_tmps = self.code.num_tmps(BANK);
        let array_size = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(num_tmps))
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(num_tmps))
        };

        let mut ranges = vec![Range::default(); array_size];

        let mut global_index = 0;

        for block in self.code.blocks.iter() {
            for inst in block.insts.iter() {
                inst.for_each_arg(self.code, |arg, _, arg_bank, _| {
                    let arg_index = arg as *const Arg as usize - inst.args.as_ptr() as usize;
                    let arg_index = arg_index / std::mem::size_of::<Arg>();
                    if arg.is_tmp() && inst.admits_stack(arg_index, self.code) {
                        if arg_bank != BANK {
                            return;
                        }

                        let tmp = arg.tmp();
                        let range = &mut ranges[Self::absolute_index::<BANK>(&tmp)];
                        range.count += 1;
                        range.admit_stack_count += 1;

                        if global_index < range.first {
                            range.first = global_index;
                            range.last = global_index;
                        } else {
                            range.last = global_index;
                        }

                        return;
                    }

                    arg.for_each_tmp_fast(|tmp| {
                        if tmp.is_gp() != (BANK == Bank::GP) {
                            return;
                        }

                        let range = &mut ranges[Self::absolute_index::<BANK>(&tmp)];
                        range.count += 1;

                        if global_index < range.first {
                            range.first = global_index;
                            range.last = global_index;
                        } else {
                            range.last = global_index;
                        }
                    });
                });

                global_index += 1;
            }

            global_index += 1;
        }

        let mut unspillable_tmps = BitVec::new();
        unspillable_tmps.resize(array_size, false);
        let last_i = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::last_machine_register_index()
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::last_machine_register_index()
        };

        println!("{} {}", last_i, ranges.len());

        for i in last_i + 1..ranges.len() {
            let range = ranges[i];

            if range.last.wrapping_sub(range.first) <= 1 && range.count > range.admit_stack_count {
                unspillable_tmps.set(i, true);
            }
        }
        unspillable_tmps
    }

    fn assign_registers_to_tmp<'c, Set: InterferenceGraph, const BANK: Bank>(
        &mut self,
        allocator: &mut ColoringAllocator<'c, Set, BANK>,
    ) {
        for block in (0..self.code.blocks.len()).map(BasicBlockId) {
            // Give Tmp a valid register.
            for inst_index in 0..self.code.block(block).len() {
                let this = unsafe { &mut *(self as *mut Self as *mut Self) };
                let inst = &mut self.code.block_mut(block)[inst_index];
                let may_be_coalescable = allocator.may_be_coalescable(inst);

                // Move32 is cheaper if we know that it's equivalent to a Move. It's
                // equivalent if the destination's high bits are not observable or if the source's high
                // bits are all zero. Note that we don't have the opposite optimization for other
                // architectures, which may prefer Move over Move32, because Move is canonical already.
                if BANK == Bank::GP
                    && inst.kind.opcode == Opcode::Move
                    && inst.args[0].is_tmp()
                    && inst.args[1].is_tmp()
                {
                    // TODO
                }

                inst.for_each_tmp_fast_mut(this.code, |tmp| {
                    if tmp.is_reg() || tmp.bank() != BANK {
                        return;
                    }

                    let alias_tmp = allocator.get_alias(*tmp);
                    let assigned_tmp = if alias_tmp.is_reg() {
                        Tmp::from_reg(alias_tmp.reg())
                    } else {
                        let reg = allocator.allocate_reg(alias_tmp);
                        Tmp::from_reg(reg)
                    };

                    *tmp = assigned_tmp;
                });

                if may_be_coalescable
                    && inst.args[0].is_tmp()
                    && inst.args[1].is_tmp()
                    && inst.args[0].tmp() == inst.args[1].tmp()
                {
                    *inst = Inst::default();
                }
            }

            self.code.block_mut(block).retain(|x| x != &Inst::default());
        }
    }

    fn stack_slot_minimum_width(width: Width) -> usize {
        if width <= Width::W32 {
            4
        } else if width <= Width::W64 {
            8
        } else {
            16
        }
    }

    fn add_spill_and_fill<'c, Set: InterferenceGraph, const BANK: Bank>(
        &mut self,
        allocator: &mut ColoringAllocator<'c, Set, BANK>,
        unspillable_tmps: &mut BitVec<u32, bitvec::order::LocalBits>,
    ) {
        let mut stack_slots = HashMap::new();

        for tmp in allocator
            .spilled_tmps
            .iter()
            .map(|x| ColoringAllocator::<Set, BANK>::tmp_from_absolute_index(*x as usize))
        {
            let index = ColoringAllocator::<Set, BANK>::tmp_to_index(tmp);
            if unspillable_tmps.len() <= index {
                unspillable_tmps.resize(index + 1, false);
            }
            unspillable_tmps.set(index, true);

            let stack_slot = self.code.add_stack_slot(
                Self::stack_slot_minimum_width(Width::W64),
                super::stack_slot::StackSlotKind::Spill,
            );

            stack_slots.insert(tmp, stack_slot);
        }

        let mut insertion_set = InsertionSet::new();

        for block in (0..self.code.blocks.len()).map(BasicBlockId) {
            let mut has_aliased_tmps = false;

            for inst_index in 0..self.code.block(block).len() {
                let mut did_spill = false;
                let mut need_scratch = false;

                let this2 = unsafe { &mut *(self as *mut Self as *mut Self) };
                let this3 = unsafe { &mut *(self as *mut Self as *mut Self) };
                let mut inst = &mut this3.code.block_mut(block)[inst_index];
                let inst2 = unsafe { &*(inst as *mut Inst as *mut Inst) };
                inst.for_each_arg_mut(self.code, |arg, role, arg_bank, width| {
                    if !arg.is_tmp() {
                        return;
                    }

                    if arg.bank() != BANK {
                        return;
                    }

                    if arg.is_reg() {
                        return;
                    }

                    let stack_slot_entry = stack_slots.get(&arg.tmp());

                    if let Some(stack_slot_entry) = stack_slot_entry.copied() {
                        let mut needs_scratch_in_spilled_in_place = false;
                        let index =
                            arg as *mut Arg as usize - inst2.args.as_ptr() as *const Inst as usize;
                        let arg_index = index / std::mem::size_of::<Arg>();
                        if !inst2.admits_stack(arg_index, this2.code) {
                            match inst2.kind.opcode {
                                Opcode::Move
                                | Opcode::MoveDouble
                                | Opcode::MoveFloat
                                | Opcode::Move32 => {
                                    let otehr_arg_index = index ^ 1;
                                    let other_arg = &inst2.args[otehr_arg_index];

                                    if inst2.args.len() == 2
                                        && other_arg.is_stack()
                                        && this2.code.stack_slot(other_arg.stack_slot()).kind
                                            == StackSlotKind::Spill
                                    {
                                        needs_scratch_in_spilled_in_place = true;
                                    }
                                }

                                _ => (),
                            }
                        }
                        // If the Tmp holds a constant then we want to rematerialize its
                        // value rather than loading it from the stack. In order for that
                        // optimization to kick in, we need to avoid placing the Tmp's stack
                        // address into the instruction.
                        if !role.is_cold_use()
                            && this2.use_counts.is_const_def::<BANK>(
                                ColoringAllocator::<Set, BANK>::tmp_to_index(arg.tmp()),
                            )
                        {
                            return;
                        }

                        let spill_width = Width::W64;

                        if role.is_any_def() && width < spill_width {
                            // Either there are users of this tmp who will use more than width,
                            // or there are producers who will produce more than width non-zero
                            // bits.
                            // FIXME: It's not clear why we should have to return here. We have
                            // a ZDef fixup in allocateStack. And if this isn't a ZDef, then it
                            // doesn't seem like it matters what happens to the high bits. Note
                            // that this isn't the case where we're storing more than what the
                            // spill slot can hold - we already got that covered because we
                            // stretch the spill slot on demand. One possibility is that it's ZDefs of
                            // smaller width than 32-bit.
                            // https://bugs.webkit.org/show_bug.cgi?id=169823
                            return;
                        }

                        this2.code.stack_slot_mut(stack_slot_entry).byte_size =
                            bytes_for_width(width) as _;

                        *arg = Arg::new_stack(stack_slot_entry, 0);
                        did_spill = true;

                        if needs_scratch_in_spilled_in_place {
                            need_scratch = true;
                        }
                    }
                });

                if need_scratch {
                    let inst_bank = match inst.kind.opcode {
                        Opcode::Move | Opcode::Move32 => Bank::GP,
                        Opcode::MoveFloat | Opcode::MoveDouble => Bank::FP,
                        _ => unreachable!(),
                    };

                    let tmp = self.code.new_tmp(inst_bank);
                    let index = ColoringAllocator::<Set, BANK>::tmp_to_index(tmp);

                    if index >= unspillable_tmps.len() {
                        unspillable_tmps.resize(index + 1, false);
                    }

                    unspillable_tmps.set(index, true);

                    inst.args.push(Arg::new_tmp(tmp));

                    // Without this, a chain of spill moves would need two registers, not one, because
                    // the scratch registers of successive moves would appear to interfere with each
                    // other. As well, we need this if the previous instruction had any late effects,
                    // since otherwise the scratch would appear to interfere with those. On the other
                    // hand, the late use added at the end of this spill move (previously it was just a
                    // late def) doesn't change the padding situation.: the late def would have already
                    // caused it to report hasLateUseOrDef in Inst::needsPadding.
                    insertion_set
                        .insert_inst(inst_index, Inst::new(Opcode::Nop.into(), inst.origin, &[]));
                    continue;
                }

                inst.for_each_tmp_mut(self.code, |tmp, role, arg_bank, _| {
                    if tmp.is_reg() || tmp.bank() != BANK {
                        return;
                    }

                    let stack_slot_entry = stack_slots.get(&tmp);

                    if let Some(stack_slot_entry) = stack_slot_entry {
                        let spill_width = Width::W64;

                        let mov = match BANK {
                            Bank::GP => Opcode::Move,
                            Bank::FP => Opcode::MoveDouble,
                        };

                        let new_tmp = this2.code.new_tmp(BANK);

                        *tmp = new_tmp;
                        let index = ColoringAllocator::<Set, BANK>::tmp_to_index(new_tmp);

                        if index >= unspillable_tmps.len() {
                            unspillable_tmps.resize(index + 1, false);
                        }

                        unspillable_tmps.set(index, true);

                        if role == ArgRole::Scratch {
                            return;
                        }

                        let arg = Arg::new_stack(*stack_slot_entry, 0);

                        if role.is_any_use() {
                            insertion_set.insert_inst(
                                inst_index,
                                Inst::new(mov.into(), inst2.origin, &[arg, Arg::new_tmp(*tmp)]),
                            );
                        }

                        if role.is_any_def() {
                            insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(mov.into(), inst2.origin, &[Arg::new_tmp(*tmp), arg]),
                            );
                        }
                    } else {
                        let alias = allocator.get_alias_when_spilling(*tmp);
                        if alias != *tmp {
                            *tmp = alias;
                            has_aliased_tmps = true;
                        }

                        return;
                    }
                });
            }

            insertion_set.execute(self.code, block);

            if has_aliased_tmps {
                self.code
                    .block_mut(block)
                    .retain(|inst| !allocator.is_useless_move(inst));
            }
        }
    }
}
