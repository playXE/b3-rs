#![allow(dead_code)]
use indexmap::IndexSet;
use macroassembler::assembler::TargetAssembler;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use tinyvec::TinyVec;

use crate::bank::Bank;
use crate::jit::reg::Reg;
use crate::jit::register_set::RegisterSet;
use crate::liveness::IndexSparseSetEntry;
use crate::liveness::Iterable;
use crate::liveness::Liveness;
use crate::liveness::LivenessAdapter;
use crate::liveness::LocalCalc;
use crate::utils::bitvector::*;
use crate::utils::interference_graph::*;
use crate::width::bytes_for_width;
use crate::width::Width;

use super::arg::Arg;
use super::arg::ArgRole;
use super::basic_block::BasicBlockId;
use super::code::Code;
use super::fix_spills_after_terminals::fix_spills_after_terminals;
use super::form_table::is_x86;
use super::insertion_set::InsertionSet;
use super::inst::Inst;
use super::liveness_adapter::TmpLivenessAdapter;
use super::opcode::Opcode;
use super::pad_interference::pad_interference;
use super::stack_slot::StackSlotKind;
use super::tmp::AbsoluteIndexed;
use super::tmp::Tmp;
use super::use_counts::UseCounts;

trait MoveSetTrait {
    fn clear(&mut self);
    fn add_move(&mut self) -> usize;
    fn add_low_priority_move(&mut self) -> usize;
    fn start_adding_low_priority_moves(&mut self);
}

/// Instead of keeping track of the move instructions, we just keep their operands around and use the index
/// in the vector as the "identifier" for the move.
#[derive(Clone, Copy)]
struct MoveOperands {
    src_index: u32,
    dst_index: u32,
}

/// The AbstractColoringAllocator defines all the code that is independant
/// from the bank or register and can be shared when allocating registers.
struct AbstractColoringAllocator<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    regs_in_priority_order: Vec<Reg>,
    last_precolored_register_index: u32,
    interference_edges: InterferenceSet,
    adjacency_list: Vec<TinyVec<[u32; 4]>>,
    degrees: Vec<u32>,
    biases: HashMap<u32, HashSet<u32>>,
    coalescing_candidates: Vec<MoveOperands>,
    move_list: Vec<IndexSet<usize>>,
    colored_tmp: Vec<Reg>,
    spilled_tmps: Vec<u32>,
    coalesced_tmps: Vec<u32>,
    is_on_select_stack: BitVector,
    select_stack: Vec<u32>,
    simplify_worklist: Vec<u32>,
    spill_worklist: BitVector,
    has_selected_spill: bool,
    has_coalesced_non_trivial_move: bool,
    coalesced_tmps_at_spill: Vec<u32>,
    unspillable_tmps: &'a BitVector,
    use_counts: &'a UseCounts,
    code: &'a mut Code<'b>,

    pinned_regs: TinyVec<[Tmp; 4]>,
}

const TRACE_DEBUG: bool = false;

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank>
    AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>
{
    fn dump_interference_graph_in_dot(&self) {
        let mut out = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .append(true)
            .open("interference_graph.dot")
            .unwrap();

        let mut tmps_with_interferences = HashSet::new();

        out.write_all(format!("graph InterferenceGraph{} {{ \n", BANK).as_bytes())
            .unwrap();

        self.interference_edges.for_each(|u, v| {
            tmps_with_interferences.insert(Self::tmp_for_absolute_index(u as _));
            tmps_with_interferences.insert(Self::tmp_for_absolute_index(v as _));
        });

        for &tmp in tmps_with_interferences.iter() {
            let tmp_index = Self::tmp_to_index(tmp);
            if tmp_index < self.degrees.len() {
                out.write_all(
                    format!(
                        "    {} [label=\"{} ({})\"];\n",
                        tmp.internal_value(),
                        tmp,
                        self.degrees[tmp_index]
                    )
                    .as_bytes(),
                )
                .unwrap();
            } else {
                out.write_all(
                    format!("    {} [label=\"{}\"];\n", tmp.internal_value(), tmp).as_bytes(),
                )
                .unwrap();
            }
        }

        self.interference_edges.for_each(|u, v| {
            out.write_all(
                format!(
                    "   \"{}\" -- \"{}\";\n",
                    Self::tmp_for_absolute_index(u as _),
                    Self::tmp_for_absolute_index(v as _)
                )
                .as_bytes(),
            )
            .unwrap();
        });

        out.write_all("}".as_bytes()).unwrap();
    }

    fn tmp_for_absolute_index(index: usize) -> Tmp {
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

    fn new(
        code: &'a mut Code<'b>,
        regs_in_priority_order: &Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_tmps: &'a BitVector,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self {
        let mut this = Self {
            regs_in_priority_order: regs_in_priority_order.clone(),
            last_precolored_register_index,
            interference_edges,
            adjacency_list: vec![TinyVec::new(); tmp_array_size],
            degrees: vec![0; tmp_array_size],
            biases: HashMap::new(),
            coalescing_candidates: Vec::new(),
            move_list: vec![IndexSet::new(); tmp_array_size],
            colored_tmp: vec![],
            spilled_tmps: Vec::new(),
            coalesced_tmps: vec![0; tmp_array_size],
            is_on_select_stack: BitVector::with_capacity(tmp_array_size),
            select_stack: Vec::new(),
            simplify_worklist: Vec::new(),
            spill_worklist: BitVector::with_capacity(tmp_array_size),
            has_selected_spill: false,
            has_coalesced_non_trivial_move: false,
            coalesced_tmps_at_spill: Vec::new(),
            unspillable_tmps,
            use_counts,
            code,
            pinned_regs: TinyVec::new(),
        };

        this.initialize_degrees(tmp_array_size);

        if TRACE_DEBUG {
            print!("Unspillable tmps: [");
            for i in 0..this.unspillable_tmps.len() {
                if this.unspillable_tmps.quick_get(i) {
                    print!("{}, ", Self::tmp_for_absolute_index(i));
                }
            }
            println!("]");
        }

        this.adjacency_list.resize(tmp_array_size, TinyVec::new());
        this.move_list.resize(tmp_array_size, IndexSet::new());
        this.is_on_select_stack.ensure_size(tmp_array_size);
        this.spill_worklist.ensure_size(tmp_array_size);

        this
    }

    fn initialize_degrees(&mut self, tmp_array_size: usize) {
        self.degrees.resize(tmp_array_size, 0);

        let first_non_reg_index = self.last_precolored_register_index + 1;

        for i in 0..first_non_reg_index {
            self.degrees[i as usize] = u32::MAX;
        }
    }

    fn register_count(&self) -> usize {
        self.regs_in_priority_order.len()
    }

    fn get_alias(&self, tmp_index: u32) -> u32 {
        let mut alias = tmp_index;

        while self.coalesced_tmps[alias as usize] != 0 {
            alias = self.coalesced_tmps[alias as usize];
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
        if self.unspillable_tmps.quick_get(to_spill) {
            return;
        }

        self.spill_worklist.set(to_spill, true);
    }

    fn is_precolored(&self, tmp_index: u32) -> bool {
        tmp_index <= self.last_precolored_register_index
    }

    fn index_to_tmp(index: u32) -> Tmp {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(index as usize)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(index as usize)
        }
    }

    fn add_edge_distinct(&mut self, a: u32, b: u32) {
        let is_new_edge = self.interference_edges.add_and_return_is_new_entry(a, b);

        if is_new_edge {
            if !self.is_precolored(a) {
                debug_assert!(!self.adjacency_list[a as usize].contains(&b));
                self.adjacency_list[a as usize].push(b);
                self.degrees[a as usize] += 1;
            }

            if !self.is_precolored(b) {
                debug_assert!(
                    !self.adjacency_list[b as usize].contains(&a),
                    "edge {} {} already exists in adjlist: {:?}",
                    Self::index_to_tmp(a),
                    Self::index_to_tmp(b),
                    {
                        self.dump_interference_graph_in_dot();
                        &self.adjacency_list[b as usize]
                    }
                );
                self.adjacency_list[b as usize].push(a);
                self.degrees[b as usize] += 1;
            }
        }
    }

    fn add_edge_distinct_without_degree_change(&mut self, a: u32, b: u32) -> bool {
        let is_new_edge = self.interference_edges.add_and_return_is_new_entry(a, b);

        if is_new_edge {
            if !self.is_precolored(a) {
                debug_assert!(!self.adjacency_list[a as usize].contains(&b));
                self.adjacency_list[a as usize].push(b);
            }

            if !self.is_precolored(b) {
                debug_assert!(!self.adjacency_list[b as usize].contains(&a));
                self.adjacency_list[b as usize].push(a);
            }

            return true;
        }

        false
    }

    fn has_been_simplified(&self, tmp_index: u32) -> bool {
        if cfg!(debug_assertions) {
            if self.coalesced_tmps[tmp_index as usize] != 0 {
                debug_assert!(self.get_alias(tmp_index) != tmp_index);
            }
        }

        self.is_on_select_stack.quick_get(tmp_index as _)
            || self.coalesced_tmps[tmp_index as usize] != 0
    }

    fn can_be_safely_coalesced(&self, u: u32, v: u32) -> bool {
        debug_assert!(!self.is_precolored(v));
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
        debug_assert!(u != v);
        debug_assert!(!self.is_precolored(u));
        debug_assert!(!self.is_precolored(v));

        if self.unspillable_tmps.get(u as _) != self.unspillable_tmps.get(v as _) {
            return false;
        }

        let adjacnts_of_u = &self.adjacency_list[u as usize];
        let adjacnts_of_v = &self.adjacency_list[v as usize];

        let mut high_order_adjacents = TinyVec::<
            [u32; TargetAssembler::number_of_registers()
                + TargetAssembler::number_of_fp_registers()],
        >::new();
        debug_assert!(
            self.register_count()
                <= TargetAssembler::number_of_registers()
                    + TargetAssembler::number_of_fp_registers()
        );
        let mut num_candidates = adjacnts_of_u.len() + adjacnts_of_v.len();

        if num_candidates < self.register_count() {
            // Shortcut: if the total number of adjacents is less than the number of register, the condition is always met.
            return true;
        }

        for &adjacent_tmp_index in adjacnts_of_u.iter() {
            debug_assert!(adjacent_tmp_index != u);
            debug_assert!(adjacent_tmp_index != v);

            num_candidates -= 1;

            if !self.has_been_simplified(adjacent_tmp_index)
                && self.degrees[adjacent_tmp_index as usize] >= self.register_count() as u32
            {
                debug_assert!(high_order_adjacents
                    .iter()
                    .find(|&&x| x == adjacent_tmp_index)
                    .is_none());
                high_order_adjacents.push(adjacent_tmp_index);
                if high_order_adjacents.len() >= self.register_count() {
                    return false;
                }
            } else if high_order_adjacents.len() + num_candidates < self.register_count() {
                return true;
            }
        }

        debug_assert!(num_candidates == adjacnts_of_v.len());
        let last_high_order_adjacents_len = high_order_adjacents.len();
        for &adjacent_tmp_index in adjacnts_of_v.iter() {
            debug_assert!(adjacent_tmp_index != u);
            debug_assert!(adjacent_tmp_index != v);
            num_candidates -= 1;
            if !self.has_been_simplified(adjacent_tmp_index)
                && self.degrees[adjacent_tmp_index as usize] >= self.register_count() as u32
                && high_order_adjacents[..last_high_order_adjacents_len]
                    .iter()
                    .find(|&&x| x == adjacent_tmp_index)
                    .is_none()
            {
                debug_assert!(high_order_adjacents[last_high_order_adjacents_len..]
                    .iter()
                    .find(|&&x| x == adjacent_tmp_index)
                    .is_none());
                high_order_adjacents.push(adjacent_tmp_index);
                if high_order_adjacents.len() >= self.register_count() {
                    return false;
                }
            } else if high_order_adjacents.len() + num_candidates < self.register_count() {
                return true;
            }
        }

        debug_assert!(num_candidates == 0);
        debug_assert!(high_order_adjacents.len() < self.register_count());
        true
    }

    fn precolored_coalescing_heuristic(&self, u: u32, v: u32) -> bool {
        if TRACE_DEBUG {
            println!("precolored_coalescing_heuristic: u = {}, v = {}", u, v);
        }

        debug_assert!(self.is_precolored(u));
        debug_assert!(!self.is_precolored(v));
        // If any adjacent of the non-colored node is not an adjacent of the colored node AND has a degree >= K
        // there is a risk that this node needs to have the same color as our precolored node. If we coalesce such
        // move, we may create an uncolorable graph.
        let adjacnts_of_v = &self.adjacency_list[v as usize];
        for &adjacent_tmp_index in adjacnts_of_v.iter() {
            if !self.is_precolored(adjacent_tmp_index)
                && !self.has_been_simplified(adjacent_tmp_index)
                && self.degrees[adjacent_tmp_index as usize] >= self.register_count() as u32
                && !self.has_interference_edge(u, adjacent_tmp_index)
            {
                return false;
            }
        }

        true
    }

    fn add_bias(&mut self, u: u32, v: u32) {
        // We implement biased coloring as proposed by Briggs. See section
        // 5.3.3 of his thesis for more information: <http://www.cs.utexas.edu/users/mckinley/380C/lecs/briggs-thesis-1992.pdf>
        // The main idea of biased coloring is this:
        // We try to coalesce a move between u and v, but the conservative heuristic
        // fails. We don't want coalesce the move because we don't want to risk
        // creating an uncolorable graph. However, if the conservative heuristic fails,
        // it's not proof that the graph is uncolorable if the move were indeed coalesced.
        // So, when we go to color the tmps, we'll remember that we really want the
        // same register for u and v, and if legal, we will assign them to the same register.
        if !self.is_precolored(u) {
            self.biases
                .entry(u)
                .or_insert_with(|| HashSet::new())
                .insert(v);
        }

        if !self.is_precolored(v) {
            self.biases
                .entry(v)
                .or_insert_with(|| HashSet::new())
                .insert(u);
        }
    }

    fn select_spill(&mut self) -> u32 {
        if !self.has_selected_spill {
            self.has_selected_spill = true;

            if self.has_coalesced_non_trivial_move {
                self.coalesced_tmps_at_spill = self.coalesced_tmps.clone();
            }
        }

        debug_assert!(
            !self.spill_worklist.is_empty(),
            "select_spill() called but spill_worklist is empty"
        );

        let mut iterator = self.spill_worklist.iter();

        let mut victim_index = iterator
            .next()
            .expect("select_spill() called but spill_worklist is empty");

        let mut max_score = 0.0f32;

        for tmp_index in iterator {
            let uses = self.use_counts.num_warm_uses_and_defs::<BANK>(tmp_index);

            if uses == 0.0 {
                victim_index = tmp_index;
                break;
            }

            // Higher score means more desirable to spill. Lower scores maximize the likelihood that a tmp
            // gets a register.
            let degree = self.degrees[tmp_index as usize] as f32;
            let mut tmp_score = degree / uses;

            // If it's a constant, then it's not as bad to spill. We can rematerialize it in many
            // cases.
            if self.use_counts.is_const_def::<BANK>(tmp_index) {
                tmp_score *= 2.0;
            }

            if tmp_score > max_score {
                max_score = tmp_score;
                victim_index = tmp_index;
            }
        }

        debug_assert!(!self.unspillable_tmps.get(victim_index as _));
        debug_assert!(!self.is_precolored(victim_index as _));

        if TRACE_DEBUG {
            println!(
                "Selecting spill {}({})",
                victim_index,
                Self::tmp_for_absolute_index(victim_index)
            );
        }

        victim_index as u32
    }

    fn assign_colors(&mut self) {
        debug_assert!(self.simplify_worklist.is_empty());
        debug_assert!(self.spill_worklist.is_empty());
        self.interference_edges.clear();

        self.degrees.clear();
        self.move_list.clear();
        self.simplify_worklist.clear();
        self.spill_worklist.clear_all();

        // Try to color the Tmp on the stack.
        self.colored_tmp
            .resize(self.adjacency_list.len(), Reg::default());

        {
            let mut now_aliased_biases = TinyVec::<[u32; 4]>::new();

            for &key in self.biases.keys() {
                if key != self.get_alias(key) {
                    now_aliased_biases.push(key);
                }
            }

            for key in now_aliased_biases {
                let keys_biases = self.biases.remove(&key).unwrap();
                let mut is_new_entry = false;
                let add_result = self.biases.entry(self.get_alias(key)).or_insert_with(|| {
                    is_new_entry = true;
                    HashSet::new()
                });

                if is_new_entry {
                    debug_assert!(add_result.is_empty());
                    let _ = std::mem::replace(add_result, keys_biases);
                } else {
                    for &tmp in keys_biases.iter() {
                        add_result.insert(tmp);
                    }
                }
            }
        }

        while !self.select_stack.is_empty() {
            let tmp_index = self.select_stack.pop().unwrap();
            debug_assert!(!self.is_precolored(tmp_index));
            debug_assert!(self.colored_tmp[tmp_index as usize] == Reg::default());
            debug_assert!(self.get_alias(tmp_index) == tmp_index);

            let mut colored_registers = RegisterSet::default();

            for adjacent_tmp_index in self.adjacency_list[tmp_index as usize].iter() {
                let alias_tmp_index = self.get_alias(*adjacent_tmp_index);
                let reg = self.colored_tmp[alias_tmp_index as usize];

                debug_assert!(
                    !self.is_precolored(alias_tmp_index)
                        || (self.is_precolored(alias_tmp_index) && reg != Reg::default())
                );
                if reg != Reg::default() {
                    colored_registers.add(reg, Width::W64)
                }
            }

            let mut color_assigned = false;

            let item = self.biases.get(&tmp_index);
            if let Some(set) = item {
                for &desired_bias in set.iter() {
                    let desired_color = self.colored_tmp[self.get_alias(desired_bias) as usize];

                    if desired_color != Reg::default()
                        && !colored_registers.contains(desired_color, Width::W64)
                    {
                        self.colored_tmp[tmp_index as usize] = desired_color;
                        color_assigned = true;
                        break;
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
            self.coalesced_tmps.clear();
        }
    }

    fn has_interference_edge(&self, u: u32, v: u32) -> bool {
        self.interference_edges.contains(u, v)
    }
}

struct MoveSet {
    position_in_move_list: usize,
    move_list: Vec<usize>,
    low_priority_move_list: Vec<usize>,
}

impl MoveSetTrait for MoveSet {
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

    fn clear(&mut self) {
        self.position_in_move_list = 0;
        self.move_list.clear();
        self.low_priority_move_list.clear();
    }

    fn start_adding_low_priority_moves(&mut self) {
        debug_assert!(self.low_priority_move_list.is_empty());
    }
}

struct Briggs<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    base: AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>,
    worklist_moves: MoveSet,
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank>
    Allocator<'a, 'b, InterferenceSet, BANK> for Briggs<'a, 'b, InterferenceSet, BANK>
{
    type MoveList = MoveSet;
    fn worklist_moves(&self) -> &Self::MoveList {
        &self.worklist_moves
    }

    fn worklist_moves_mut(&mut self) -> &mut Self::MoveList {
        &mut self.worklist_moves
    }
    fn new(
        code: &'a mut Code<'b>,
        regs_in_priority_order: &Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_tmps: &'a BitVector,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self {
        Self {
            base: AbstractColoringAllocator::new(
                code,
                regs_in_priority_order,
                last_precolored_register_index,
                tmp_array_size,
                unspillable_tmps,
                use_counts,
                interference_edges,
            ),
            worklist_moves: MoveSet {
                position_in_move_list: 0,
                move_list: vec![],
                low_priority_move_list: vec![],
            },
        }
    }

    fn before_build(&mut self) {
        self.worklist_moves.clear();
    }

    fn allocate(&mut self) {
        let mut changed;

        let coalesce_move = |this: &mut Self, mov: &mut usize, changed: &mut bool| {
            if this.coalesce(mov) {
                *changed = true;
                *mov = usize::MAX;
            }
        };

        loop {
            changed = false;
            //for &index in self.move_list.iter().filter(|x| **x != usize::MAX) {
            //  f(index);
            //}
            for i in 0..self.worklist_moves.move_list.len() {
                let mut mov = self.worklist_moves.move_list[i];
                if mov != usize::MAX {
                    coalesce_move(self, &mut mov, &mut changed);

                    self.worklist_moves.move_list[i] = mov;
                }
            }

            if !changed {
                break;
            }
        }

        loop {
            changed = false;

            for i in 0..self.worklist_moves.low_priority_move_list.len() {
                let mut mov = self.worklist_moves.low_priority_move_list[i];
                if mov != usize::MAX {
                    coalesce_move(self, &mut mov, &mut changed);

                    self.worklist_moves.low_priority_move_list[i] = mov;
                }
            }

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

        let assert_invariants = |this: &Self| {
            if false {
                let first_non_reg_index = this.last_precolored_register_index + 1;
                let register_count = this.register_count() as u32;

                for i in first_non_reg_index..this.degrees.len() as u32 {
                    if this.get_alias(i) != i {
                        continue;
                    }

                    if this.is_on_select_stack.contains(i as _) {
                        debug_assert!(
                            !this.simplify_worklist.contains(&i)
                                && !this.spill_worklist.contains(i as _)
                        );
                        continue;
                    }

                    let degree = this.degrees[i as usize];

                    if degree >= register_count {
                        debug_assert!(
                            this.unspillable_tmps.get(i as _)
                                || this.spill_worklist.contains(i as _)
                        );
                        debug_assert!(!this.simplify_worklist.contains(&i));
                        continue;
                    }

                    debug_assert!(this.simplify_worklist.contains(&i));
                }
            }
        };

        self.make_initial_worklist();
        assert_invariants(self);
        loop {
            changed = false;

            while self.simplify_worklist.len() != 0 {
                self.simplify();
                assert_invariants(self);
            }

            if !self.spill_worklist.is_empty() {
                self.select_spill();
                changed = true;
                debug_assert!(self.simplify_worklist.len() == 1);
            }

            if !changed {
                break;
            }
        }

        debug_assert!(self.simplify_worklist.len() == 0);
        debug_assert!(self.spill_worklist.is_empty());

        let first_non_reg_index = self.last_precolored_register_index + 1;
        for i in first_non_reg_index..self.degrees.len() as u32 {
            debug_assert!(self.has_been_simplified(i));
        }

        self.assign_colors();
    }
}
impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank>
    Briggs<'a, 'b, InterferenceSet, BANK>
{
    fn for_each_adjacent(&mut self, tmp_index: u32, mut function: impl FnMut(&mut Self, u32)) {
        for i in 0..self.adjacency_list[tmp_index as usize].len() {
            if !self.has_been_simplified(self.adjacency_list[tmp_index as usize][i]) {
                function(self, self.adjacency_list[tmp_index as usize][i]);
            }
        }
    }
    fn select_spill(&mut self) {
        let victim_index = self.base.select_spill();
        self.spill_worklist.quick_clear(victim_index as _);
        self.simplify_worklist.push(victim_index);
    }

    fn coalesce(&mut self, move_index: &mut usize) -> bool {
        let move_operands = &self.coalescing_candidates[*move_index];
        let mut u = self.get_alias(move_operands.src_index);
        let mut v = self.get_alias(move_operands.dst_index);

        if self.is_precolored(v) {
            std::mem::swap(&mut u, &mut v);
        }

        if TRACE_DEBUG {
            let utmp = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(u as _)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(u as _)
            };

            let vtmp = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(v as _)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(v as _)
            };

            print!(
                "Coalescing move at index {} u = {}, v = {}     ",
                move_index, utmp, vtmp
            );
        }

        if u == v {
            if TRACE_DEBUG {
                println!("Already Coalesced. They're equal.");
            }

            return false;
        }

        if self.is_precolored(v) || self.interference_edges.contains(u, v) {
            // No need to ever consider this move again if it interferes.
            // No coalescing will remove the interference.
            *move_index = usize::MAX;

            if self.is_precolored(v) {
                debug_assert!(self.is_precolored(u));
            }

            if TRACE_DEBUG {
                println!("Constrained");
            }

            return false;
        }

        if self.can_be_safely_coalesced(u, v) {
            self.combine(u, v);
            self.has_coalesced_non_trivial_move = true;

            if TRACE_DEBUG {
                println!("Safe Coalescing");
            }

            return true;
        }

        self.add_bias(u, v);

        if TRACE_DEBUG {
            println!("Failed Coalescing.");
        }

        false
    }

    fn combine(&mut self, u: u32, v: u32) {
        debug_assert!(self.coalesced_tmps[v as usize] == 0);
        self.coalesced_tmps[v as usize] = u;

        for i in 0..self.move_list[v as usize].len() {
            let mov = self.move_list[v as usize][i];
            self.move_list[u as usize].insert(mov);
        }

        self.for_each_adjacent(v, |this, adjacent_tmp_index| {
            if this.add_edge_distinct_without_degree_change(adjacent_tmp_index, u) {
                // If we added a new edge between the adjacentTmp and u, it replaces the edge
                // that existed with v.
                // The degree of adjacentTmp remains the same since the edge just changed from u to v.
                // All we need to do is update the degree of u.
                if !this.is_precolored(u) {
                    this.degrees[u as usize] += 1;
                }
            } else {
                // If we already had an edge between the adjacentTmp and u, the degree of u
                // is already correct. The degree of the adjacentTmp decreases since the edge
                // with v is no longer relevant (we can think of it as merged with the edge with u).
                this.decrement_degree(adjacent_tmp_index);
            }
        });
    }

    fn make_initial_worklist(&mut self) {
        self.simplify_worklist.clear();
        self.spill_worklist.clear_all();

        if TRACE_DEBUG {
            println!("------------------\nMaking initial worklist");
        }

        let first_non_reg_index = self.last_precolored_register_index + 1;
        let register_count = self.register_count();

        for i in first_non_reg_index as usize..self.degrees.len() {
            debug_assert!(!self.is_precolored(i as _));

            if self.has_been_simplified(i as _) {
                continue;
            }

            let degree = self.degrees[i];
            let tmp = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(i)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(i)
            };

            if degree < register_count as u32 {
                if TRACE_DEBUG {
                    println!(
                        "Adding {}({}) with degree {} to simplify worklist",
                        i, tmp, degree
                    );
                }
                self.simplify_worklist.push(i as _);
            } else {
                if TRACE_DEBUG {
                    println!(
                        "Adding {}({}) with degree {} to spill worklist",
                        i, tmp, degree
                    );
                }
                self.add_to_spill(i);
            }
        }
    }

    // Low-degree vertex can always be colored: just pick any of the color taken by any
    // other adjacent verices.
    // The "Simplify" phase takes a low-degree out of the interference graph to simplify it.
    fn simplify(&mut self) {
        let last_index = self.simplify_worklist.pop().unwrap();

        debug_assert!(!self.is_on_select_stack.get(last_index as usize));
        debug_assert!(!self.select_stack.contains(&last_index));
        debug_assert!(!self.spill_worklist.contains(last_index as _));
        self.select_stack.push(last_index);
        self.is_on_select_stack.set(last_index as usize, true);

        if TRACE_DEBUG {
            let ix = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(last_index as _)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(last_index as _)
            };

            println!(
                "Simplifying {}({}) by adding it to select stack",
                last_index, ix
            );
        }

        self.for_each_adjacent(last_index, |this, adjacent_tmp_index| {
            this.decrement_degree_in_simplification(adjacent_tmp_index);
        });
    }

    fn decrement_degree(&mut self, tmp_index: u32) {
        debug_assert!(self.degrees[tmp_index as usize] != 0);
        let old_degree = self.degrees[tmp_index as usize];
        self.degrees[tmp_index as usize] = old_degree - 1;
    }

    fn decrement_degree_in_simplification(&mut self, tmp_index: u32) {
        debug_assert!(self.degrees[tmp_index as usize] != 0);
        let old_degree = self.degrees[tmp_index as usize];
        self.degrees[tmp_index as usize] = old_degree - 1;

        if old_degree == self.register_count() as u32 {
            debug_assert!(self.degrees[tmp_index as usize] < self.register_count() as u32);

            if TRACE_DEBUG {
                println!("Moving tmp {} from spill list to simplify list because it's degree is now less than K", tmp_index);
            }

            self.spill_worklist.quick_clear(tmp_index as _);
            debug_assert!(!self.simplify_worklist.contains(&tmp_index));
            self.simplify_worklist.push(tmp_index);
        }
    }

    fn assign_colors(&mut self) {
        self.worklist_moves.clear();
        self.base.assign_colors();
    }
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> Deref
    for Briggs<'a, 'b, InterferenceSet, BANK>
{
    type Target = AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> DerefMut
    for Briggs<'a, 'b, InterferenceSet, BANK>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}
#[allow(dead_code)]
struct OrderedMoveSet {
    position_in_move_list: Vec<usize>,
    move_list: Vec<usize>,
    low_priority_move_list: Vec<usize>,
    first_low_priority_move_index: usize,
}

impl MoveSetTrait for OrderedMoveSet {
    fn add_move(&mut self) -> usize {
        debug_assert!(self.low_priority_move_list.is_empty());
        debug_assert!(self.first_low_priority_move_index == 0);

        let next_index = self.position_in_move_list.len();
        let position = self.move_list.len();
        self.move_list.push(next_index);
        self.position_in_move_list.push(position);
        next_index
    }

    fn clear(&mut self) {
        self.position_in_move_list.clear();
        self.move_list.clear();
        self.low_priority_move_list.clear();
        self.first_low_priority_move_index = 0;
    }

    fn add_low_priority_move(&mut self) -> usize {
        debug_assert!(self.first_low_priority_move_index == self.move_list.len());

        let next_index = self.position_in_move_list.len();
        let position = self.low_priority_move_list.len();
        self.low_priority_move_list.push(next_index);
        self.position_in_move_list.push(position);

        debug_assert!(next_index >= self.first_low_priority_move_index);

        next_index
    }

    fn start_adding_low_priority_moves(&mut self) {
        debug_assert!(self.low_priority_move_list.is_empty());
        debug_assert!(self.first_low_priority_move_index == 0);

        self.first_low_priority_move_index = self.move_list.len();
    }
}

#[allow(dead_code)]
impl OrderedMoveSet {
    fn new() -> Self {
        Self {
            position_in_move_list: Vec::new(),
            move_list: Vec::new(),
            low_priority_move_list: Vec::new(),
            first_low_priority_move_index: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.move_list.is_empty() && self.low_priority_move_list.is_empty()
    }

    fn contains(&self, index: usize) -> bool {
        self.position_in_move_list[index] != usize::MAX
    }

    fn take_move(&mut self, move_index: usize) {
        let position_in_move_list = self.position_in_move_list[move_index];

        if position_in_move_list == usize::MAX {
            return;
        }

        if move_index < self.first_low_priority_move_index {
            debug_assert!(self.move_list[position_in_move_list] == move_index);
            let last_index = self.move_list.last().copied().unwrap();
            self.position_in_move_list[last_index] = position_in_move_list;
            self.move_list[position_in_move_list] = last_index;
            self.move_list.pop();
        } else {
            debug_assert!(self.low_priority_move_list[position_in_move_list] == move_index);
            let last_index = self.low_priority_move_list.last().copied().unwrap();
            self.position_in_move_list[last_index] = position_in_move_list;
            self.low_priority_move_list[position_in_move_list] = last_index;
            self.low_priority_move_list.pop();
        }

        self.position_in_move_list[move_index] = usize::MAX;

        debug_assert!(!self.contains(move_index));
    }

    fn take_last_move(&mut self) -> usize {
        let last_index = if !self.move_list.is_empty() {
            self.move_list.pop().unwrap()
        } else {
            self.low_priority_move_list.pop().unwrap()
        };

        self.position_in_move_list[last_index] = usize::MAX;

        debug_assert!(!self.contains(last_index));

        last_index
    }

    fn return_move(&mut self, index: usize) {
        // This assertion is a bit strict but that is how the move list should be used. The only kind of moves that can
        // return to the list are the ones that we previously failed to coalesce with the conservative heuristics.
        // Values should not be added back if they were never taken out when attempting coalescing.
        debug_assert!(!self.contains(index));

        if index < self.first_low_priority_move_index {
            let position = self.move_list.len();
            self.move_list.push(index);
            self.position_in_move_list[index] = position;
        } else {
            let position = self.low_priority_move_list.len();
            self.low_priority_move_list.push(index);
            self.position_in_move_list[index] = position;
        }

        debug_assert!(self.contains(index));
    }

    fn total_number_of_moves(&self) -> usize {
        self.move_list.len() + self.low_priority_move_list.len()
    }
}

#[allow(dead_code)]
struct IRC<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> {
    base: AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>,
    // Work lists.
    /// Low-degree, Move related.
    freeze_worklist: IndexSet<u32>,
    /// Set of "move" enabled for possible coalescing.
    worklist_moves: OrderedMoveSet,
    /// Set of "move" not yet ready for coalescing.
    active_moves: BitVector,
    /// Set of Tmps whose moves are now ready for possible coalescing.
    moves_to_enable: BitVector,
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::Deref
    for IRC<'a, 'b, InterferenceSet, BANK>
{
    type Target = AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank> std::ops::DerefMut
    for IRC<'a, 'b, InterferenceSet, BANK>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank>
    Allocator<'a, 'b, InterferenceSet, BANK> for IRC<'a, 'b, InterferenceSet, BANK>
{
    type MoveList = OrderedMoveSet;

    fn worklist_moves(&self) -> &Self::MoveList {
        &self.worklist_moves
    }

    fn worklist_moves_mut(&mut self) -> &mut Self::MoveList {
        &mut self.worklist_moves
    }

    fn before_build(&mut self) {
        self.worklist_moves.clear();
    }
    fn new(
        code: &'a mut Code<'b>,
        regs_in_priority_order: &Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_tmps: &'a BitVector,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self {
        Self {
            base: AbstractColoringAllocator::new(
                code,
                regs_in_priority_order,
                last_precolored_register_index,
                tmp_array_size,
                unspillable_tmps,
                use_counts,
                interference_edges,
            ),
            freeze_worklist: IndexSet::new(),
            worklist_moves: OrderedMoveSet::new(),
            active_moves: BitVector::new(),
            moves_to_enable: BitVector::with_capacity(tmp_array_size),
        }
    }

    fn allocate(&mut self) {
        self.active_moves
            .ensure_size(self.worklist_moves.total_number_of_moves());
        debug_assert!(
            self.active_moves.len() >= self.coalescing_candidates.len(),
            "The activeMove set should be big enough for the quick operations of BitVector."
        );

        //self.dump_interference_graph_in_dot();
        self.make_worklist();

        loop {
            if !self.simplify_worklist.is_empty() {
                self.simplify();
            } else if !self.moves_to_enable.is_empty() {
                self.enable_moves();
            } else if !self.worklist_moves.is_empty() {
                self.coalesce();
            } else if !self.freeze_worklist.is_empty() {
                self.freeze();
            } else if !self.spill_worklist.is_empty() {
                self.select_spill();
            }

            if self.simplify_worklist.is_empty()
                && self.worklist_moves.is_empty()
                && self.freeze_worklist.is_empty()
                && self.spill_worklist.is_empty()
            {
                break;
            }
        }

        self.assign_colors();
    }
}

#[allow(dead_code)]
impl<'a, 'b, InterferenceSet: InterferenceGraph, const BANK: Bank>
    IRC<'a, 'b, InterferenceSet, BANK>
{
    fn make_worklist(&mut self) {
        let first_non_reg_index = self.last_precolored_register_index + 1;

        for i in first_non_reg_index as usize..self.degrees.len() {
            let degree = self.degrees[i];
            if degree >= self.register_count() as u32 {
                self.add_to_spill(i as _);
            } else if !self.move_list[i].is_empty() {
                self.freeze_worklist.insert(i as _);
            } else {
                self.simplify_worklist.push(i as _);
            }
        }
    }
    /// Low-degree vertex can always be colored: just pick any of the color taken by any
    /// other adjacent vertices.
    /// The "Simplify" phase takes a low-degree out of the interference graph to simplify it.
    fn simplify(&mut self) {
        let last_index = self.simplify_worklist.pop().unwrap();

        debug_assert!(!self.select_stack.contains(&last_index));
        debug_assert!(!self.is_on_select_stack.get(last_index as _));

        self.select_stack.push(last_index);
        self.is_on_select_stack.set(last_index as _, true);

        self.for_each_adjacent(last_index, |this, adjacent_tmp_index| {
            this.decrement_degree(adjacent_tmp_index);
        });
    }

    fn coalesce(&mut self) {
        let move_index = self.worklist_moves.take_last_move();
        let move_operands = self.coalescing_candidates[move_index];

        let mut u = self.get_alias(move_operands.src_index);
        let mut v = self.get_alias(move_operands.dst_index);

        if self.is_precolored(v) {
            std::mem::swap(&mut u, &mut v);
        }

        if TRACE_DEBUG {
            println!(
                "Coalescing move at index {} u = {}, v = {}",
                move_index, u, v
            );
        }

        if u == v {
            self.add_worklist(u);

            if TRACE_DEBUG {
                println!("    Coalesced");
            }
        } else if self.is_precolored(v) || self.interference_edges.contains(u, v) {
            self.add_worklist(u);
            self.add_worklist(v);

            if TRACE_DEBUG {
                println!("    Constrained");
            }
        } else if self.can_be_safely_coalesced(u, v) {
            self.combine(u, v);
            self.add_worklist(u);
            self.has_coalesced_non_trivial_move = true;

            if TRACE_DEBUG {
                println!("    Safe Coalescing");
            }
        } else {
            self.active_moves.quick_set(move_index as _, true);

            if TRACE_DEBUG {
                println!("    Failed coalescing, added to active moves.");
            }

            self.add_bias(u, v);
        }
    }

    fn add_worklist(&mut self, tmp_index: u32) {
        if !self.is_precolored(tmp_index)
            && self.degrees[tmp_index as usize] < self.register_count() as u32
            && !self.is_move_related(tmp_index)
        {
            self.freeze_worklist.remove(&tmp_index);
            self.simplify_worklist.push(tmp_index);
        }
    }

    fn combine(&mut self, u: u32, v: u32) {
        if !self.freeze_worklist.remove(&v) {
            self.spill_worklist.quick_clear(v as _);
        }

        debug_assert!(self.coalesced_tmps[v as usize] == 0);
        self.coalesced_tmps[v as usize] = u;

        for i in 0..self.move_list[v as usize].len() {
            let v_mov = self.move_list[v as usize][i];
            self.move_list[u as usize].insert(v_mov);
        }

        self.for_each_adjacent(v, |this, adjacent_tmp_index| {
            if this.add_edge_distinct_without_degree_change(adjacent_tmp_index, u) {
                // If we added a new edge between the adjacentTmp and u, it replaces the edge
                // that existed with v.
                // The degree of adjacentTmp remains the same since the edge just changed from u to v.
                // All we need to do is update the degree of u.
                if !this.is_precolored(u) {
                    this.degrees[u as usize] += 1;
                }
            } else {
                // If we already had an edge between the adjacentTmp and u, the degree of u
                // is already correct. The degree of the adjacentTmp decreases since the edge
                // with v is no longer relevant (we can think of it as merged with the edge with u).
                this.decrement_degree(adjacent_tmp_index);
            }
        });

        if self.degrees[u as usize] >= self.register_count() as u32
            && self.freeze_worklist.remove(&u)
        {
            self.add_to_spill(u as _);
        }
    }

    fn freeze(&mut self) {
        let victim_index = self.freeze_worklist.pop().unwrap();
        debug_assert!(
            self.get_alias(victim_index) == victim_index,
            "coalesce() should not leave aliased Tmp in the worklist"
        );
        self.simplify_worklist.push(victim_index);
        self.freeze_moves(victim_index);
    }

    fn freeze_moves(&mut self, tmp_index: u32) {
        self.for_each_node_moves(tmp_index, |this, move_index| {
            if !this.active_moves.quick_clear(move_index) {
                this.worklist_moves.take_move(move_index);
            }

            let move_operands = &this.coalescing_candidates[move_index];

            let src_tmp_index = move_operands.src_index;
            let dst_tmp_index = move_operands.dst_index;

            let original_other_tmp = if src_tmp_index != tmp_index {
                src_tmp_index
            } else {
                dst_tmp_index
            };
            let other_tmp_index = this.get_alias(original_other_tmp);

            if this.degrees[other_tmp_index as usize] < this.register_count() as u32
                && !this.is_move_related(other_tmp_index)
            {
                if this.freeze_worklist.remove(&other_tmp_index) {
                    this.simplify_worklist.push(other_tmp_index);
                }
            }
        });
    }

    fn decrement_degree(&mut self, tmp_index: u32) {
        debug_assert!(self.degrees[tmp_index as usize] != 0);
        let old_degree = self.degrees[tmp_index as usize];
        self.degrees[tmp_index as usize] = old_degree - 1;

        if old_degree == self.register_count() as u32 {
            self.lazy_enable_moves_on_value_and_adjacents(tmp_index);
            self.spill_worklist.quick_clear(tmp_index as _);

            if self.is_move_related(tmp_index) {
                self.freeze_worklist.insert(tmp_index);
            } else {
                self.simplify_worklist.push(tmp_index);
            }
        }
    }

    fn select_spill(&mut self) {
        let victim_index = self.base.select_spill();
        self.spill_worklist.quick_clear(victim_index as _);
        self.simplify_worklist.push(victim_index);
        self.freeze_moves(victim_index);
    }

    fn assign_colors(&mut self) {
        debug_assert!(self.freeze_worklist.is_empty());
        self.worklist_moves.clear();
        self.base.assign_colors();
    }

    fn for_each_adjacent(&mut self, tmp_index: u32, mut function: impl FnMut(&mut Self, u32)) {
        for i in 0..self.adjacency_list[tmp_index as usize].len() {
            if !self.has_been_simplified(self.adjacency_list[tmp_index as usize][i]) {
                function(self, self.adjacency_list[tmp_index as usize][i]);
            }
        }
    }

    fn is_move_related(&self, tmp_index: u32) -> bool {
        for &move_index in self.move_list[tmp_index as usize].iter() {
            if self.active_moves.quick_get(move_index as _)
                || self.worklist_moves.contains(move_index)
            {
                return true;
            }
        }

        false
    }

    fn for_each_node_moves(&mut self, tmp_index: u32, mut function: impl FnMut(&mut Self, usize)) {
        for i in 0..self.move_list[tmp_index as usize].len() {
            let move_index = self.move_list[tmp_index as usize][i];

            if self.active_moves.quick_get(move_index as _)
                || self.worklist_moves.contains(move_index)
            {
                function(self, move_index);
            }
        }
    }

    fn lazy_enable_moves_on_value_and_adjacents(&mut self, tmp_index: u32) {
        self.moves_to_enable.quick_set(tmp_index as usize, true);
        self.for_each_adjacent(tmp_index, |this, adjacent_tmp_index| {
            this.moves_to_enable
                .quick_set(adjacent_tmp_index as usize, true);
        });
    }

    fn enable_moves_on_value(&mut self, tmp_index: u32) {
        for i in 0..self.move_list[tmp_index as usize].len() {
            let move_index = self.move_list[tmp_index as usize][i];

            if self.active_moves.quick_clear(move_index) {
                self.worklist_moves.return_move(move_index);
            }
        }
    }

    fn enable_moves(&mut self) {
        for i in 0..self.moves_to_enable.len() {
            if self.moves_to_enable.quick_get(i) {
                self.enable_moves_on_value(i as u32);
            }
        }

        self.moves_to_enable.clear_all();
    }
}

trait Allocator<'a, 'b: 'a, InterferenceSet: InterferenceGraph, const BANK: Bank>:
    Deref<Target = AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>> + DerefMut
{
    type MoveList: MoveSetTrait;
    fn allocate(&mut self);
    fn new(
        code: &'a mut Code<'b>,
        regs_in_priority_order: &Vec<Reg>,
        last_precolored_register_index: u32,
        tmp_array_size: usize,
        unspillable_tmps: &'a BitVector,
        use_counts: &'a UseCounts,
        interference_edges: InterferenceSet,
    ) -> Self;

    fn before_build(&mut self);

    fn worklist_moves(&self) -> &Self::MoveList;
    fn worklist_moves_mut(&mut self) -> &mut Self::MoveList;
}

struct ColoringAllocator<
    'a,
    'b: 'a,
    InterferenceSet: InterferenceGraph,
    Alloc: Allocator<'a, 'b, InterferenceSet, BANK>,
    const BANK: Bank,
> {
    allocator: Alloc,
    marker: PhantomData<AbstractColoringAllocator<'a, 'b, InterferenceSet, BANK>>,
}

impl<
        'a,
        'b,
        InterferenceSet: InterferenceGraph,
        Alloc: Allocator<'a, 'b, InterferenceSet, BANK>,
        const BANK: Bank,
    > ColoringAllocator<'a, 'b, InterferenceSet, Alloc, BANK>
{
    fn new(
        code: &'a mut Code<'b>,
        use_counts: &'a UseCounts,
        unspillable_tmps: &'a BitVector,
        graph: InterferenceSet,
    ) -> Self {
        let regs_in_priority_order = code.regs_in_priority_order(BANK).to_vec();
        let tmp_array_size = code.num_tmps(BANK);
        let tmp_array_size = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(tmp_array_size))
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(tmp_array_size))
        };
        let last_precolored_register_index = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::last_machine_register_index()
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::last_machine_register_index()
        };

        let mut this = Self {
            allocator: Alloc::new(
                code,
                &regs_in_priority_order,
                last_precolored_register_index as _,
                tmp_array_size,
                unspillable_tmps,
                use_counts,
                graph,
            ),
            marker: Default::default(),
        };

        let pinned_set = this.allocator.code.pinned_regs.to_register_set();

        pinned_set.for_each(|reg| {
            if (BANK == Bank::GP && reg.is_gpr()) || (BANK == Bank::FP && reg.is_fpr()) {
                this.allocator.pinned_regs.push(Tmp::from_reg(reg));
                this.allocator.regs_in_priority_order.push(reg);
            }
        });

        let absolute_index = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(
                this.allocator.code.num_tmps(BANK),
            ))
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(
                this.allocator.code.num_tmps(BANK),
            ))
        };

        this.allocator
            .interference_edges
            .set_max_index(absolute_index as _);

        this.initialize_precolored_tmp();
        this.build();

        this
    }

    fn tmp_for_absolute_index(absolute_index: usize) -> Tmp {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(absolute_index)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(absolute_index)
        }
    }

    fn initialize_precolored_tmp(&mut self) {
        let n = self.allocator.last_precolored_register_index + 1;
        self.allocator.colored_tmp.resize(n as _, Reg::default());

        for i in 1..=self.allocator.last_precolored_register_index {
            let tmp = Self::tmp_for_absolute_index(i as _);
            debug_assert!(tmp.is_reg());
            self.allocator.colored_tmp[i as usize] = tmp.reg();
        }
    }

    fn get_alias(&self, tmp: Tmp) -> Tmp {
        let ix = Self::tmp_to_index(tmp);
        let alias = self.allocator.get_alias(ix as _);
        Self::tmp_for_absolute_index(alias as _)
    }

    fn get_alias_when_spilling(&self, tmp: Tmp) -> Tmp {
        if self.allocator.coalesced_tmps_at_spill.is_empty() {
            return tmp;
        }

        let mut alias_index = Self::tmp_to_index(tmp) as u32;

        while self.allocator.coalesced_tmps_at_spill[alias_index as usize] != 0 {
            alias_index = self.allocator.coalesced_tmps_at_spill[alias_index as usize];
        }

        Self::tmp_for_absolute_index(alias_index as _)
    }

    fn requires_spilling(&self) -> bool {
        !self.allocator.spilled_tmps.is_empty()
    }

    fn allocated_reg(&self, tmp: Tmp) -> Reg {
        let reg = self.allocator.colored_tmp[Self::tmp_to_index(tmp) as usize];
        debug_assert!(reg.is_set());
        reg
    }

    fn is_useless_move(inst: &Inst) -> bool {
        may_be_coalescable_impl::<BANK>(inst) && inst.args[0].tmp() == inst.args[1].tmp()
    }

    fn tmp_to_index(tmp: Tmp) -> usize {
        if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
        }
    }

    fn build(&mut self) {
        self.allocator.coalescing_candidates.clear();
        self.allocator.before_build();

        // FIXME: It seems like we don't need to recompute liveness. We just need to update its data
        // structures so that it knows that the newly introduced temporaries are not live at any basic
        // block boundary. This should trivially be true for now.

        let code_ptr = (&mut *self.allocator).code as *mut Code<'b>;

        let mut adapter = TmpLivenessAdapter::<BANK>::new(unsafe { &mut *code_ptr });
        let mut liveness = Liveness::new(&mut adapter);
        liveness.compute();

        for i in 0..self.allocator.code.blocks.len() {
            let block = BasicBlockId(i);

            let mut local_calc = LocalCalc::new(&mut liveness, block);

            for inst_index in (0..local_calc.liveness.adapter.cfg().block(block).len()).rev() {
                let inst = local_calc.liveness.adapter.cfg().block(block)[inst_index].clone();
                let next_inst = local_calc
                    .liveness
                    .adapter
                    .cfg()
                    .block(block)
                    .get(inst_index + 1)
                    .cloned();

                self.build_for(Some(&inst), next_inst.as_ref(), &local_calc);
                local_calc.execute(inst_index);
            }

            self.build_for(
                None,
                local_calc.liveness.adapter.cfg().block(block).first(),
                &local_calc,
            );
        }

        self.build_low_priority_move_list();
    }

    fn build_for(
        &mut self,
        prev_inst: Option<&Inst>,
        next_inst: Option<&Inst>,
        local_calc: &LocalCalc<TmpLivenessAdapter<BANK>>,
    ) {
        if TRACE_DEBUG {
            println!("Building between {:?} and {:?}:", prev_inst, next_inst);
            print!("live values: [");
            for live_tmp in local_calc.live().iter() {
                let tmp = if BANK == Bank::GP {
                    AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(live_tmp.key() as _)
                } else {
                    AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(live_tmp.key() as _)
                };
                print!("{},", tmp);
            }
            println!("]");
        }

        let mut edges = TinyVec::<[(Tmp, Tmp); 8]>::new();

        Inst::for_each_def_with_extra_clobbered_regs(
            prev_inst,
            next_inst,
            self.allocator.code,
            |arg, role, arg_bank, _, preserve64| {
                if arg_bank != BANK {
                    return;
                }
                assert!(role.is_any_def());

                // All the Def()s interfere with each other and with all the extra clobbered Tmps.
                // We should not use forEachDefWithExtraClobberedRegs() here since colored Tmps
                // do not need interference edges in our implementation.
                Inst::for_each_def(
                    prev_inst,
                    next_inst,
                    self.allocator.code,
                    |other_arg, other_role, arg_bank, def_width| {
                        if arg_bank != BANK {
                            return;
                        }
                        assert!(other_role.is_any_def());

                        if def_width <= Width::W64 && preserve64 {
                            if TRACE_DEBUG {
                                println!("skipping def-live edge: {}, {} since {} preserves enough lower bits", arg, other_arg, arg);
                            }
                            return;
                        }

                        if TRACE_DEBUG {
                            println!(
                                "    Adding def-def edge: {}, {} (prev={:?}, next={:?})",
                                arg, other_arg, prev_inst, next_inst
                            );
                        }
                        edges.push((arg, other_arg));
                    },
                );
            },
        );

        for &(a, b) in edges.iter() {
            self.add_edge(a, b);
        }

        if let Some(prev_inst) = prev_inst.filter(|inst| self.may_be_coalescable(inst)) {
            // We do not want the Use() of this move to interfere with the Def(), even if it is live
            // after the Move. If we were to add the interference edge, it would be impossible to
            // coalesce the Move even if the two Tmp never interfere anywhere.
            let mut def_tmp = Tmp::empty();
            let mut use_tmp = Tmp::empty();

            prev_inst.for_each_arg(self.allocator.code, |_, arg_tmp, role, _, _| {
                if !arg_tmp.is_tmp() {
                    return;
                }
                if role.is_late_def() {
                    def_tmp = arg_tmp.tmp();
                } else {
                    debug_assert!(role.is_early_use());
                    use_tmp = arg_tmp.tmp();
                }
            });

            let next_move_index = self.allocator.coalescing_candidates.len();
            self.allocator.coalescing_candidates.push(MoveOperands {
                src_index: Self::tmp_to_index(use_tmp) as _,
                dst_index: Self::tmp_to_index(def_tmp) as _,
            });

            let new_index_in_worklist = self.allocator.worklist_moves_mut().add_move();
            debug_assert!(new_index_in_worklist == next_move_index);

            for arg in prev_inst.args.iter() {
                let list = &mut self.allocator.move_list[Self::tmp_to_index(arg.tmp())];
                list.insert(next_move_index);
            }

            let consider_edge = |this: &mut Self, live_tmp: Tmp| {
                if live_tmp != use_tmp {
                    if TRACE_DEBUG {
                        println!(
                            "    Adding def-live for coalescable: {}, {}",
                            def_tmp, live_tmp
                        );
                    }
                    this.add_edge(def_tmp, live_tmp);
                }
            };

            for live_tmp in local_calc
                .live()
                .iter()
                .map(|ix| Self::tmp_for_absolute_index(ix.key()))
            {
                consider_edge(self, live_tmp);
            }

            for i in 0..self.allocator.pinned_regs.len() {
                let pinned_reg = self.allocator.pinned_regs[i];

                consider_edge(self, pinned_reg);
            }

            self.add_edges(None, next_inst, local_calc.live())
        } else {
            self.add_edges(prev_inst, next_inst, local_calc.live())
        }
    }

    fn build_low_priority_move_list(&mut self) {
        if !is_x86() {
            return;
        }

        self.allocator
            .worklist_moves_mut()
            .start_adding_low_priority_moves();

        for i in 0..self.allocator.code.blocks.len() {
            let block = BasicBlockId(i);

            for inst_index in 0..self.allocator.code.block(block).len() {
                let inst = &self.allocator.code.block(block)[inst_index];

                if let Some(def_arg_index) = inst.should_try_aliasing_def(self.allocator.code) {
                    let op1 = inst.args[def_arg_index - 2];
                    let op2 = inst.args[def_arg_index - 1];
                    let dest = inst.args[def_arg_index];

                    if op1 == dest || op2 == dest {
                        continue;
                    }

                    if self.may_be_coalesced(op1, dest) {
                        self.add_to_low_priority_coalescing_candidates(op1, dest);
                    } else if op1 != op2 && self.may_be_coalesced(op2, dest) {
                        self.add_to_low_priority_coalescing_candidates(op2, dest);
                    }
                }
            }
        }
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

        !self
            .allocator
            .interference_edges
            .contains(left_index as _, right_index as _)
    }

    fn add_to_low_priority_coalescing_candidates(&mut self, left: Arg, right: Arg) {
        debug_assert!(self.may_be_coalesced(left, right));

        let left_tmp = left.tmp();
        let right_tmp = right.tmp();

        let left_index = Self::tmp_to_index(left_tmp);
        let right_index = Self::tmp_to_index(right_tmp);

        let next_move_index = self.allocator.coalescing_candidates.len();
        self.allocator.coalescing_candidates.push(MoveOperands {
            src_index: left_index as _,
            dst_index: right_index as _,
        });

        let new_index_in_worklist = self.allocator.worklist_moves_mut().add_low_priority_move();
        debug_assert!(new_index_in_worklist == next_move_index);
        self.allocator.move_list[left_index].insert(next_move_index);
        self.allocator.move_list[right_index].insert(next_move_index);
    }

    fn add_edges(
        &mut self,
        prev_inst: Option<&Inst>,
        next_inst: Option<&Inst>,
        live_tmps: Iterable<TmpLivenessAdapter<BANK>>,
    ) {
        let mut edges = TinyVec::<[(Tmp, Tmp); 8]>::new();
        let pinned_regs = &self.allocator.pinned_regs;
        Inst::for_each_def_with_extra_clobbered_regs(
            prev_inst,
            next_inst,
            self.allocator.code,
            |tmp, role, arg_bank, _, _preserve64| {
                if arg_bank != BANK {
                    return;
                }
                assert!(role.is_any_def());
                for live_tmp in live_tmps.iter() {
                    let live_tmp = Self::tmp_for_absolute_index(live_tmp.key());

                    debug_assert!(live_tmp.is_gp() == (BANK == Bank::GP));
                    if TRACE_DEBUG {
                        println!(
                            "      Adding def-live edge {}, {} in {:?} {:?}",
                            tmp, live_tmp, prev_inst, next_inst
                        );
                    }
                    edges.push((tmp, live_tmp));
                }

                for pinned_reg_tmp in pinned_regs {
                    edges.push((tmp, *pinned_reg_tmp));
                }
            },
        );

        for (a, b) in edges {
            self.add_edge(a, b);
        }
    }

    fn add_edge(&mut self, a: Tmp, b: Tmp) {
        self.allocator
            .add_edge(Self::tmp_to_index(a) as u32, Self::tmp_to_index(b) as u32);
    }

    fn may_be_coalescable(&self, inst: &Inst) -> bool {
        may_be_coalescable_impl::<BANK>(inst)
    }
}

fn may_be_coalescable_impl<const BANK: Bank>(inst: &Inst) -> bool {
    match BANK {
        Bank::GP => match inst.kind.opcode {
            Opcode::Move32 | Opcode::Move => (),
            _ => return false,
        },

        Bank::FP => match inst.kind.opcode {
            Opcode::MoveFloat | Opcode::MoveDouble => (),
            _ => return false,
        },
    }

    if inst.args.len() != 2 {
        return false;
    }

    if !inst.args[0].is_tmp() || !inst.args[1].is_tmp() {
        return false;
    }

    if inst.kind.opcode == Opcode::Move32 {
        return false;
    }

    true
}
struct GraphColoringRegisterAllocation<'a> {
    use_counts: &'a UseCounts,
}

impl<'a> GraphColoringRegisterAllocation<'a> {
    fn new(use_counts: &'a UseCounts) -> Self {
        Self { use_counts }
    }

    fn run<'b>(&mut self, code: &'a mut Code<'b>) {
        pad_interference(code);

        self.allocate_on_bank::<{ Bank::GP }>(code);
        self.allocate_on_bank::<{ Bank::FP }>(code);

        fix_spills_after_terminals(code);
    }

    fn allocate_on_bank<'c, const BANK: Bank>(&mut self, code: &'c mut Code) {
        let mut unspillable_tmps = self.compute_unspillable_tmps::<{ BANK }>(code);
        let use_irc = !code.proc.options.air_force_briggs_allocator;
        let mut num_iterations = 0;
        let mut done = false;

        while !done {
            num_iterations += 1;

            if code.num_tmps(BANK) < MAX_SIZE_FOR_SMALL_INTERFERENCE_GRAPH {
                let graph = SmallInterferenceGraph::new(InterferenceBitVector::new());
                let cloned = unspillable_tmps.clone();

                if !use_irc {
                    let mut allocator = ColoringAllocator::<_, Briggs<_, BANK>, BANK>::new(
                        code,
                        self.use_counts,
                        &cloned,
                        graph,
                    );
                    allocator.allocator.allocate();

                    if !allocator.requires_spilling() {
                        Self::assign_registers_to_tmp(&mut allocator);
                        done = true;
                    } else {
                        Self::add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                        done = false
                    }
                } else {
                    let mut allocator = ColoringAllocator::<_, IRC<_, BANK>, BANK>::new(
                        code,
                        self.use_counts,
                        &cloned,
                        graph,
                    );
                    allocator.allocator.allocate();

                    if !allocator.requires_spilling() {
                        Self::assign_registers_to_tmp(&mut allocator);
                        done = true;
                    } else {
                        Self::add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                        done = false
                    }
                }
            } else {
                let graph = LargeInterferenceGraph::new(InterferenceVector::new());

                let cloned = unspillable_tmps.clone();
                if !use_irc {
                    let mut allocator = ColoringAllocator::<_, Briggs<_, BANK>, BANK>::new(
                        code,
                        self.use_counts,
                        &cloned,
                        graph,
                    );
                    allocator.allocator.allocate();

                    if !allocator.requires_spilling() {
                        Self::assign_registers_to_tmp(&mut allocator);
                        done = true;
                    } else {
                        Self::add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                        done = false
                    }
                } else {
                    let mut allocator = ColoringAllocator::<_, IRC<_, BANK>, BANK>::new(
                        code,
                        self.use_counts,
                        &cloned,
                        graph,
                    );
                    allocator.allocator.allocate();

                    if !allocator.requires_spilling() {
                        Self::assign_registers_to_tmp(&mut allocator);
                        done = true;
                    } else {
                        Self::add_spill_and_fill(&mut allocator, &mut unspillable_tmps);
                        done = false
                    }
                }
            }

            if num_iterations > 50 {
                break;
            }
        }
    }

    fn compute_unspillable_tmps<const BANK: Bank>(&self, code: &Code) -> BitVector {
        #[derive(Clone, Copy)]
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

        let num_tmps = code.num_tmps(BANK);
        let array_size = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&Tmp::gp_tmp_for_index(num_tmps))
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&Tmp::fp_tmp_for_index(num_tmps))
        };

        let mut ranges = vec![Range::default(); array_size];

        let mut global_index = 0;

        for block in code.blocks.iter() {
            for inst in block.iter() {
                inst.for_each_arg(code, |arg_index, arg, _, arg_bank, _| {
                    if arg.is_tmp() && inst.admits_stack(arg_index, code) {
                        if arg_bank != BANK {
                            return;
                        }

                        let tmp = arg.tmp();
                        let index = if BANK == Bank::GP {
                            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
                        } else {
                            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
                        };
                        let range = &mut ranges[index];
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
                        let index = if BANK == Bank::GP {
                            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
                        } else {
                            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
                        };
                        let range = &mut ranges[index];
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

        let mut unspillable_tmps = BitVector::with_capacity(array_size);

        let start = if BANK == Bank::GP {
            AbsoluteIndexed::<{ Bank::GP }>::last_machine_register_index()
        } else {
            AbsoluteIndexed::<{ Bank::FP }>::last_machine_register_index()
        };

        for i in start + 1..ranges.len() {
            let range = &ranges[i];

            if range.last.wrapping_sub(range.first) <= 1 && range.count > range.admit_stack_count {
                unspillable_tmps.quick_set(i, true);
            }
        }

        unspillable_tmps
    }

    fn assign_registers_to_tmp<
        'c,
        'b: 'c,
        InterferenceSet: InterferenceGraph,
        Alloc: Allocator<'c, 'b, InterferenceSet, BANK>,
        const BANK: Bank,
    >(
        allocator: &mut ColoringAllocator<'c, 'b, InterferenceSet, Alloc, BANK>,
    ) {
        let code_ptr = (&mut *allocator.allocator).code as *mut Code;
        for i in 0..allocator.allocator.code.blocks.len() {
            let block = BasicBlockId(i);

            for inst_index in 0..allocator.allocator.code.block(block).len() {
                let mut inst = allocator.allocator.code.block_mut(block)[inst_index].clone();

                // The mayBeCoalescable() method will change its mind for some operations after we
                // complete register allocation. So, we record this before starting.
                let may_be_coalescable = allocator.may_be_coalescable(&inst);

                inst.for_each_tmp_fast_mut(unsafe { &mut *code_ptr }, |tmp| {
                    if tmp.is_reg() || tmp.bank() != BANK {
                        return;
                    }

                    let alias_tmp = allocator.get_alias(*tmp);
                    let assigned_tmp = if alias_tmp.is_reg() {
                        Tmp::from_reg(alias_tmp.reg())
                    } else {
                        let reg = allocator.allocated_reg(alias_tmp);

                        Tmp::from_reg(reg)
                    };

                    *tmp = assigned_tmp;
                });

                if may_be_coalescable
                    && inst.args[0].is_tmp()
                    && inst.args[1].is_tmp()
                    && inst.args[0] == inst.args[1]
                {
                    inst = Inst::default();
                }

                allocator.allocator.code.block_mut(block)[inst_index] = inst;
            }

            allocator
                .allocator
                .code
                .block_mut(block)
                .retain(|inst| inst != &Inst::default());
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

    fn add_spill_and_fill<
        'c,
        'b: 'c,
        InterferenceSet: InterferenceGraph,
        Alloc: Allocator<'c, 'b, InterferenceSet, BANK>,
        const BANK: Bank,
    >(
        allocator: &mut ColoringAllocator<'c, 'b, InterferenceSet, Alloc, BANK>,
        unspillable_tmps: &mut BitVector,
    ) {
        let mut stackslots = HashMap::new();
        let code_ptr = (&mut *allocator.allocator).code as *mut Code;
        for &tmp in allocator.allocator.spilled_tmps.iter() {
            let tmp = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::tmp_for_absolute_index(tmp as usize)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::tmp_for_absolute_index(tmp as usize)
            };
           

            let index = if BANK == Bank::GP {
                AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
            } else {
                AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
            };

            unspillable_tmps.set(index, true);

            let stackslot = unsafe { &mut *code_ptr }.add_stack_slot(
                Self::stack_slot_minimum_width(Width::W64),
                StackSlotKind::Spill,
            );
            let is_new_tmp = stackslots.insert(tmp, stackslot).is_none();

            debug_assert!(is_new_tmp);
        }

        let mut insertion_set = InsertionSet::new();

        let code_ptr = (&mut *allocator.allocator).code as *mut Code;

        for i in 0..allocator.allocator.code.blocks.len() {
            let block = BasicBlockId(i);
            let mut has_aliased_tmps = false;

            for inst_index in 0..allocator.allocator.code.block(block).len() {
                let mut inst = allocator.allocator.code.block_mut(block)[inst_index].clone();

                let mut did_spill = false;
                let mut needs_scratch = false;
                let args = inst.args.as_ptr() as usize;
                inst.for_each_arg_mut(
                    unsafe { &mut *code_ptr },
                    |arg_index, arg, role, arg_bank, _width| {
                        if !arg.is_tmp() {
                            return;
                        }

                        if arg_bank != BANK {
                            return;
                        }

                        if arg.is_reg() {
                            return;
                        }

                        if let Some(stack_slot) = stackslots.get(&arg.tmp()).copied() {
                            let mut needs_scratch_if_spilled_in_place = false;

                            if !allocator.allocator.code.block(block)[inst_index]
                                .admits_stack(arg_index, allocator.allocator.code)
                            {
                                match allocator.allocator.code.block(block)[inst_index]
                                    .kind
                                    .opcode
                                {
                                    Opcode::Move
                                    | Opcode::MoveDouble
                                    | Opcode::MoveFloat
                                    | Opcode::Move32 => {
                                        let other_arg_index = arg as *mut Arg as usize - args;
                                        let other_arg_index = other_arg_index / std::mem::size_of::<Arg>();
                                        let other_arg_index = other_arg_index ^ 1;
                                        if other_arg_index >= allocator.allocator.code.block(block)[inst_index].args.len() {
                                            return;
                                        }
                                        let other_arg = allocator.allocator.code.block(block)
                                            [inst_index]
                                            .args[other_arg_index];

                                        if allocator.allocator.code.block(block)[inst_index]
                                            .args
                                            .len()
                                            == 2
                                            && other_arg.is_stack()
                                            && allocator
                                                .allocator
                                                .code
                                                .stack_slot(other_arg.stack_slot())
                                                .is_spill()
                                        {
                                            needs_scratch_if_spilled_in_place = true;
                                        } else {
                                            return;
                                        }
                                    }

                                    _ => return,
                                }
                            }

                            // If the Tmp holds a constant then we want to rematerialize its
                            // value rather than loading it from the stack. In order for that
                            // optimization to kick in, we need to avoid placing the Tmp's stack
                            // address into the instruction.

                            let index = if BANK == Bank::GP {
                                AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&arg.tmp())
                            } else {
                                AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&arg.tmp())
                            };

                            if !role.is_cold_use()
                                && allocator.allocator.use_counts.is_const_def::<BANK>(index)
                            {
                                return;
                            }

                            let spill_width = Width::W64;

                            allocator
                                .allocator
                                .code
                                .stack_slot_mut(stack_slot)
                                .byte_size = bytes_for_width(spill_width) as _;

                            *arg = Arg::new_stack(stack_slot, 0);

                            did_spill = true;

                            if needs_scratch_if_spilled_in_place {
                                needs_scratch = true;
                            }
                        }
                    },
                );

                if needs_scratch {
                    let inst_bank = match inst.kind.opcode {
                        Opcode::Move32 | Opcode::Move => Bank::GP,
                        Opcode::MoveFloat | Opcode::MoveDouble => Bank::FP,
                        _ => unreachable!(),
                    };

                    let tmp = allocator.allocator.code.new_tmp(inst_bank);

                    let index = if BANK == Bank::GP {
                        AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&tmp)
                    } else {
                        AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&tmp)
                    };

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
                    allocator.allocator.code.block_mut(block)[inst_index] = inst;
                    continue;
                }
                let origin = inst.origin;
                // For every other case, add Load/Store as needed.
                inst.for_each_tmp_mut(unsafe { &mut *code_ptr }, |tmp, role, arg_bank, _| {
                    if tmp.is_reg() || arg_bank != BANK {
                        return;
                    }

                    if let Some(stack_slot) = stackslots.get(tmp).copied() {
                        let _spill_width = Width::W64;

                        let mov = if BANK == Bank::GP {
                            Opcode::Move
                        } else {
                            Opcode::MoveDouble
                        };

                        let new_tmp = allocator.allocator.code.new_tmp(BANK);
                        *tmp = new_tmp;

                        let index = if BANK == Bank::GP {
                            AbsoluteIndexed::<{ Bank::GP }>::absolute_index(&new_tmp)
                        } else {
                            AbsoluteIndexed::<{ Bank::FP }>::absolute_index(&new_tmp)
                        };

                        unspillable_tmps.set(index, true);

                        if role == ArgRole::Scratch {
                            return;
                        }

                        let arg = Arg::new_stack(stack_slot, 0);
                        if role.is_any_use() {
                            insertion_set.insert_inst(
                                inst_index,
                                Inst::new(mov.into(), origin, &[arg, Arg::new_tmp(*tmp)]),
                            );
                        }

                        if role.is_any_def() {
                            insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(mov.into(), origin, &[Arg::new_tmp(*tmp), arg]),
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

                allocator.allocator.code.block_mut(block)[inst_index] = inst;
            }

            insertion_set.execute(allocator.allocator.code, block);

            if has_aliased_tmps {
                allocator.allocator.code.block_mut(block).retain(|inst| {
                    !ColoringAllocator::<InterferenceSet, Alloc, BANK>::is_useless_move(inst)
                });
            }
        }
    }
}

pub fn allocate_registers_by_graph_coloring(code: &mut Code) {
    let use_counts = UseCounts::new(code);
    let mut allocator = GraphColoringRegisterAllocation::new(&use_counts);
    allocator.run(code);
}
