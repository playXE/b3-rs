use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use tinyvec::TinyVec;

use crate::{air::arg::ArgKind, bank::Bank, value::ValueId, width::Width};

use super::{
    arg::Arg, code::Code, form_table::is_valid_form, inst::Inst, opcode::Opcode, tmp::Tmp,
};

pub fn move_for(bank: Bank, width: Width) -> Opcode {
    match width {
        Width::W32 => match bank {
            Bank::FP => Opcode::MoveFloat,
            Bank::GP => Opcode::Move32,
        },
        Width::W64 => match bank {
            Bank::FP => Opcode::MoveDouble,
            Bank::GP => Opcode::Move,
        },

        _ => unreachable!(),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ShufflePair {
    pub src: Arg,
    pub dst: Arg,
    pub width: Width,
}

impl std::fmt::Display for ShufflePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.src, self.dst)
    }
}

fn find_possible_scratch(code: &Code, bank: Bank, mut f: impl FnMut(Tmp) -> bool) -> Option<Tmp> {
    for reg in code.regs_in_priority_order(bank) {
        let tmp = Tmp::from_reg(*reg);

        if f(tmp) {
            return Some(tmp);
        }
    }

    None
}

fn find_possible_scratch_for_args(code: &Code, bank: Bank, arg1: &Arg, arg2: &Arg) -> Option<Tmp> {
    find_possible_scratch(code, bank, |tmp| !arg1.uses_tmp(tmp) && !arg2.uses_tmp(tmp))
}

/// Example: (a => b, b => a, a => c, b => d)
struct Rotate {
    /// in the example, this is the loop: (a => b, b => a)
    loop_: Vec<ShufflePair>,
    /// in the example, these are the associated shifts: (a => c, b => d)
    fringe: Vec<ShufflePair>,
}

impl Rotate {
    fn new() -> Self {
        Self {
            loop_: Vec::new(),
            fringe: Vec::new(),
        }
    }
}

impl ShufflePair {
    pub const fn new(src: Arg, dst: Arg, width: Width) -> Self {
        Self { src, dst, width }
    }

    pub fn bank(&self) -> Bank {
        if self.src.is_memory() && self.dst.is_memory() && self.width > Width::W64 {
            // 8-byte memory-to-memory moves on a 32-bit platform are best handled as float moves.
            Bank::FP
        } else if self.src.is_gp() && self.dst.is_gp() {
            // This means that gpPairs gets memory-to-memory shuffles. The assumption is that we
            // can do that more efficiently using GPRs, except in the special case above.
            Bank::GP
        } else {
            Bank::FP
        }
    }

    pub fn insts(&self, code: &mut Code, origin: ValueId) -> TinyVec<[Inst; 2]> {
        if self.src.is_memory() && self.dst.is_memory() {
            let new_tmp = code.new_tmp(self.bank());

            let inst = Inst::new(
                move_for(self.bank(), self.width).into(),
                origin,
                &[self.src.clone(), self.dst.clone(), Arg::new_tmp(new_tmp)],
            );

            let mut insts = TinyVec::new();
            insts.push(inst);
            return insts;
        }

        if is_valid_form(
            move_for(self.bank(), self.width),
            &[self.src.kind(), self.dst.kind()],
        ) {
            let inst = Inst::new(
                move_for(self.bank(), self.width).into(),
                origin,
                &[self.src.clone(), self.dst.clone()],
            );

            let mut insts = TinyVec::new();
            insts.push(inst);
            return insts;
        }

        assert!(is_valid_form(
            move_for(self.bank(), self.width),
            &[ArgKind::Addr, ArgKind::Tmp]
        ));
        assert!(is_valid_form(
            move_for(self.bank(), self.width),
            &[ArgKind::Tmp, ArgKind::Addr]
        ));
        assert!(is_valid_form(
            move_for(self.bank(), self.width),
            &[ArgKind::Tmp, ArgKind::Tmp]
        ));
        assert!(self.src.is_some_imm());

        let tmp = code.new_tmp(self.bank());
        assert!(is_valid_form(
            Opcode::Move,
            &[ArgKind::BigImm, ArgKind::Tmp]
        ));
        assert!(is_valid_form(
            move_for(self.bank(), self.width),
            &[ArgKind::Tmp, self.dst.kind()]
        ));

        let mut insts = TinyVec::new();

        insts.push(Inst::new(
            Opcode::Move.into(),
            origin,
            &[self.src.clone(), Arg::new_tmp(tmp)],
        ));

        insts.push(Inst::new(
            move_for(self.bank(), self.width).into(),
            origin,
            &[Arg::new_tmp(tmp), self.dst.clone()],
        ));

        insts
    }
}

pub fn create_shuffle(origin: ValueId, pairs: &[ShufflePair]) -> Inst {
    let mut result = Inst::new(Opcode::Shuffle.into(), origin, &[]);

    for pair in pairs {
        result.args.extend_from_slice(&[
            pair.src.clone(),
            pair.dst.clone(),
            Arg::new_width_arg(pair.width),
        ]);
    }

    result
}

pub fn emit_shuffle_for_bank(
    code: &mut Code,
    mut pairs: Vec<ShufflePair>,
    mut scratches: [Arg; 2],
    bank: Bank,
    origin: ValueId,
) -> Vec<Inst> {
    pairs.retain(|pair| pair.src != pair.dst);

    // There are two possible kinds of operations that we will do:
    //
    // - Shift. Example: (a => b, b => c). We emit this as "Move b, c; Move a, b". This only requires
    //   scratch registers if there are memory->memory moves. We want to find as many of these as
    //   possible because they are cheaper. Note that shifts can involve the same source mentioned
    //   multiple times. Example: (a => b, a => c, b => d, b => e).
    //
    // - Rotate. Example: (a => b, b => a). We want to emit this as "Swap a, b", but that instruction
    //   may not be available, in which case we may need a scratch register or a scratch memory
    //   location. A gnarlier example is (a => b, b => c, c => a). We can emit this as "Swap b, c;
    //   Swap a, b". Note that swapping has to be careful about differing widths.
    //
    // Note that a rotate can have "fringe". For example, we might have (a => b, b => a, a =>c,
    // b => d). This has a rotate loop (a => b, b => a) and some fringe (a => c, b => d). We treat
    // the whole thing as a single rotate.
    //
    // We will find multiple disjoint such operations. We can execute them in any order.

    // We interpret these as Moves that should be executed backwards. All shifts are keyed by their
    // starting source.

    let mut shifts = HashMap::<Arg, Vec<ShufflePair>>::new();

    // We interpret these as Swaps over src()'s that should be executed backwards, i.e. for a list
    // of size 3 we would do "Swap list[1].src(), list[2].src(); Swap list[0].src(), list[1].src()".
    // Note that we actually can't do that if the widths don't match or other bad things happen.
    // But, prior to executing all of that, we need to execute the fringe: the shifts comming off the
    // rotate.
    let mut rotates = Vec::new();

    {
        let mut mapping = HashMap::new();

        for pair in &pairs {
            mapping.insert(pair.src, vec![pair.clone()]);
        }

        let mut current_pairs = Vec::new();

        while !mapping.is_empty() {
            let original_src = mapping.keys().next().unwrap().clone();

            let mut worklist = GraphNodeWorklist::<Arg>::new();

            worklist.push(original_src);

            while let Some(src) = worklist.pop() {
                if let Some(pairs) = mapping.remove(&src) {
                    for pair in pairs {
                        worklist.push(pair.dst);
                        current_pairs.push(pair);
                    }
                } else {
                    if let Some(pairs) = shifts.remove(&src) {
                        current_pairs.extend_from_slice(&pairs);
                        continue;
                    }
                }
            }

            let mut is_rotate = false;

            for pair in current_pairs.iter() {
                if pair.dst == original_src {
                    is_rotate = true;
                    break;
                }
            }

            if is_rotate {
                let mut rotate = Rotate::new();

                let ok = if current_pairs.last().unwrap().dst == original_src {
                    let mut ok = true;
                    for i in (0..current_pairs.len() - 1).rev() {
                        ok &= current_pairs[i].dst == current_pairs[i + 1].src;
                    }
                    ok
                } else {
                    false
                };

                if ok {
                    rotate.loop_ = std::mem::take(&mut current_pairs);
                } else {
                    // This is the slow path. The rotate has fringe.

                    let mut dst_mapping = HashMap::new();

                    for pair in current_pairs.iter() {
                        dst_mapping.insert(pair.dst, *pair);
                    }

                    let mut pair = dst_mapping.remove(&original_src).unwrap();

                    loop {
                        rotate.loop_.push(pair);
                        if let Some(value) = dst_mapping.remove(&pair.src) {
                            pair = value;
                        } else {
                            break;
                        }
                    }

                    rotate.loop_.reverse();

                    // Make sure that the fringe appears in the same order as how it appeared in the
                    // current_pairs, since that's the DFS order.
                    for pair in current_pairs.iter() {
                        if dst_mapping.contains_key(&pair.dst) {
                            rotate.fringe.push(*pair);
                        }
                    }
                }

                // If the graph search terminates because we returned to the first source, then the
                // pair list has to have a very particular shape.
                for i in (0..rotate.loop_.len() - 1).rev() {
                    assert_eq!(rotate.loop_[i].dst, rotate.loop_[i + 1].src);
                }

                rotates.push(rotate);
                current_pairs.truncate(0);
            } else {
                shifts.insert(original_src, std::mem::take(&mut current_pairs));
            }
        }
    }

    // In the worst case, we need two scratch registers. The way we do this is that the client passes
    // us what scratch registers he happens to have laying around. We will need scratch registers in
    // the following cases:
    //
    // - Shuffle pairs where both src and dst refer to memory.
    // - Rotate when no Swap instruction is available.
    //
    // Lucky for us, we are guaranteed to have extra scratch registers anytime we have a Shift that
    // ends with a register. We search for such a register right now.

    let move_for_width = |width| -> Opcode { move_for(bank, width) };

    let conservative_move = move_for_width(Width::W64);

    // We will emit things in reverse. We maintain a list of packs of instructions, and then we emit
    // append them together in reverse (for example the thing at the end of result_packs is placed
    // first). This is useful because the last thing we emit frees up its destination registers, so
    // it affects how we emit things before it.
    let mut result_packs = Vec::new();
    let mut result = Vec::new();

    let commit_result = |result_packs: &mut Vec<_>, result: &mut Vec<_>| {
        result_packs.push(std::mem::take(result));
    };

    let get_scratch = |result: &mut Vec<_>,
                       scratches: &[Arg; 2],
                       index: usize,
                       possible_scratch|
     -> Option<Tmp> {
        if scratches[index].is_tmp() {
            return Some(scratches[index].tmp());
        }

        if let Some(possible_scratch) = possible_scratch {
            result.push(Inst::new(
                conservative_move.into(),
                origin,
                &[Arg::new_tmp(possible_scratch), scratches[index]],
            ));
            Some(possible_scratch)
        } else {
            None
        }
    };

    let return_scratch = |result: &mut Vec<_>, scratches: &[Arg; 2], index: usize, tmp| {
        if Arg::new_tmp(tmp) != scratches[index] {
            result.push(Inst::new(
                conservative_move.into(),
                origin,
                &[scratches[index], Arg::new_tmp(tmp)],
            ));
        }
    };

    let handle_shift_pair = |result: &mut Vec<Inst>,
                             scratches: &[Arg; 2],
                             pair: &ShufflePair,
                             scratch_index: usize| {
        let mov = move_for_width(pair.width);

        if !is_valid_form(mov, &[pair.src.kind(), pair.dst.kind()]) {
            let possible_scratch = find_possible_scratch_for_args(code, bank, &pair.src, &pair.dst);
            let scratch = get_scratch(result, scratches, scratch_index, possible_scratch).unwrap();

            if is_valid_form(mov, &[pair.src.kind(), ArgKind::Tmp]) {
                result.push(Inst::new(
                    move_for_width(pair.width).into(),
                    origin,
                    &[pair.src, Arg::new_tmp(scratch)],
                ));
            } else {
                result.push(Inst::new(
                    Opcode::Move.into(),
                    origin,
                    &[Arg::new_bigimm(pair.src.value()), Arg::new_tmp(scratch)],
                ));
            }
        }
    };

    let handle_shift =
        |result: &mut Vec<Inst>, scratches: &mut [Arg; 2], shift: &mut Vec<ShufflePair>| {
            // FIXME: We could optimize the spill behavior of the shifter by checking if any of the
            // shifts need spills. If they do, then we could try to get a register out here. Note that
            // this may fail where the current strategy succeeds: out here we need a register that does
            // not interfere with any of the shifts, while the current strategy only needs to find a
            // scratch register that does not interfer with a particular shift. So, this optimization
            // will be opportunistic: if it succeeds, then the individual shifts can use that scratch,
            // otherwise they will do what they do now.

            for i in (0..shift.len()).rev() {
                handle_shift_pair(result, &scratches, &shift[i], 0);
            }

            let last_dst = shift.last().unwrap().dst;

            if last_dst.is_tmp() {
                for scratch in scratches.iter_mut() {
                    if !scratch.is_tmp() {
                        *scratch = Arg::new_tmp(last_dst.tmp());
                        break;
                    }
                }
            }
        };

    // First handle shifts whose last destination is a tmp because these free up scratch registers.
    // These end up last in the final sequence, so the final destination of these shifts will be
    // available as a scratch location for anything emitted prior (so, after, since we're emitting in
    // reverse).
    for shift in shifts.values_mut() {
        if shift.last().unwrap().dst.is_tmp() {
            handle_shift(&mut result, &mut scratches, shift);
        }

        commit_result(&mut result_packs, &mut result);
    }

    // Now handle the rest of the shifts.
    for shift in shifts.values_mut() {
        if !shift.last().unwrap().dst.is_tmp() {
            handle_shift(&mut result, &mut scratches, shift);
        }

        commit_result(&mut result_packs, &mut result);
    }

    for rotate in rotates.iter_mut() {
        if !rotate.fringe.is_empty() {
            // Make sure we do the fringe first! This won't clobber any of the registers that are
            // part of the rotation.
            handle_shift(&mut result, &mut scratches, &mut rotate.fringe);
        }

        let mut can_swap = false;
        let mut swap = Opcode::Oops;
        let mut swap_width = Width::W8; // bogus value

        // Currently, the swap instruction is not available for floating point on any architecture we
        // support.

        if bank == Bank::GP {
            swap = Opcode::Swap32;
            swap_width = Width::W32;

            let mut has_memory = false;
            let mut has_index = false;

            for pair in rotate.loop_.iter() {
                match pair.width {
                    Width::W32 => (),
                    Width::W64 => {
                        swap = Opcode::Swap64;
                        swap_width = Width::W64;
                    }
                    _ => unreachable!(),
                }

                has_memory |= pair.src.is_memory() || pair.dst.is_memory();
                has_index |= pair.src.is_index() || pair.dst.is_index();
            }

            can_swap = is_valid_form(swap, &[ArgKind::Tmp, ArgKind::Tmp]);

            if has_memory {
                can_swap &= is_valid_form(swap, &[ArgKind::Tmp, ArgKind::Addr]);

                // We don't take the swapping path if there is a mix of widths and some of the
                // shuffles involve memory. That gets too confusing. We might be able to relax this
                // to only bail if there are subwidth pairs involving memory, but I haven't thought
                // about it very hard. Anyway, this case is not common: rotates involving memory
                // don't arise for function calls, and they will only happen for rotates in user code
                // if some of the variables get spilled. It's hard to imagine a program that rotates
                // data around in variables while also doing a combination of uint32->uint64 and
                // int64->int32 casts.
                for pair in rotate.loop_.iter() {
                    can_swap &= pair.width == swap_width;
                }
            }

            if has_index {
                can_swap &= is_valid_form(swap, &[ArgKind::Tmp, ArgKind::Index]);
            }
        }

        if can_swap {
            for i in (0..rotate.loop_.len() - 1).rev() {
                let mut left = &rotate.loop_[i].src;
                let mut right = &rotate.loop_[i + 1].src;

                if left.is_memory() && right.is_memory() {
                    // Note that this is a super rare outcome. Rotates are rare. Spills are rare.
                    // Moving data between two spills is rare. To get here a lot of rare stuff has to
                    // all happen at once.
                    let scratch = get_scratch(
                        &mut result,
                        &scratches,
                        0,
                        find_possible_scratch_for_args(code, bank, left, right),
                    )
                    .expect("no scratch register available");

                    result.push(Inst::new(
                        move_for_width(swap_width).into(),
                        origin,
                        &[*left, Arg::new_tmp(scratch)],
                    ));
                    result.push(Inst::new(
                        swap.into(),
                        origin,
                        &[Arg::new_tmp(scratch), *right],
                    ));
                    result.push(Inst::new(
                        move_for_width(swap_width).into(),
                        origin,
                        &[Arg::new_tmp(scratch), *left],
                    ));
                    return_scratch(&mut result, &scratches, 0, scratch);
                    continue;
                }

                if left.is_memory() {
                    std::mem::swap(&mut left, &mut right);
                }

                result.push(Inst::new(swap.into(), origin, &[*left, *right]));
            }

            for pair in rotate.loop_.iter_mut() {
                if pair.width == swap_width {
                    continue;
                }

                // Need to do an extra zero extension.
                result.push(Inst::new(
                    Opcode::Move32.into(),
                    origin,
                    &[pair.dst, pair.dst],
                ));
            }
        } else {
            // We can treat this as a shift so long as we take the last destination (i.e. first
            // source) and save it first. Then we handle the first entry in the pair in the rotate
            // specially, after we restore the last destination. This requires some special care to
            // find a scratch register. It's possible that we have a rotate that uses the entire
            // available register file.

            let scratch = find_possible_scratch(code, bank, |tmp| {
                for pair in rotate.loop_.iter() {
                    if pair.src.uses_tmp(tmp) || pair.dst.uses_tmp(tmp) {
                        return false;
                    }
                }

                true
            });

            let scratch = get_scratch(&mut result, &scratches, 0, scratch);

            let rotate_save = scratch
                .map(|scratch| Arg::new_tmp(scratch))
                .unwrap_or_else(|| scratches[0]);

            handle_shift_pair(
                &mut result,
                &mut scratches,
                &ShufflePair::new(
                    rotate.loop_.last().unwrap().dst,
                    rotate_save,
                    rotate.loop_[0].width,
                ),
                1,
            );

            for i in (1..rotate.loop_.len()).rev() {
                handle_shift_pair(&mut result, &mut scratches, &rotate.loop_[i], 0);
            }

            if let Some(scratch) = scratch {
                return_scratch(&mut result, &scratches, 0, scratch);
            }
        }

        commit_result(&mut result_packs, &mut result);
    }

    for item in result_packs.drain(..).rev() {
        result.extend(item.into_iter());
    }

    result
}

pub fn emit_shuffle(
    code: &mut Code,
    pairs: &[ShufflePair],
    gp_scratch: [Arg; 2],
    fp_scratch: [Arg; 2],
    origin: ValueId,
) -> Vec<Inst> {
    let mut gp_pairs = vec![];
    let mut fp_pairs = vec![];

    for pair in pairs {
        match pair.bank() {
            Bank::GP => gp_pairs.push(*pair),
            Bank::FP => fp_pairs.push(*pair),
        }
    }

    let mut result = vec![];

    result.extend(emit_shuffle_for_bank(code, gp_pairs, gp_scratch, Bank::GP, origin).into_iter());
    result.extend(emit_shuffle_for_bank(code, fp_pairs, fp_scratch, Bank::FP, origin).into_iter());

    result
}

pub struct GraphNodeWorklist<Node: Copy + Clone + PartialEq + Eq + Hash> {
    seen: HashSet<Node>,
    stack: Vec<Node>,
}

impl<Node: Copy + Clone + PartialEq + Eq + Hash> GraphNodeWorklist<Node> {
    pub fn new() -> Self {
        Self {
            seen: HashSet::new(),
            stack: Vec::new(),
        }
    }

    pub fn push(&mut self, node: Node) -> bool {
        if self.seen.contains(&node) {
            false
        } else {
            self.seen.insert(node);
            self.stack.push(node);
            true
        }
    }

    pub fn push_all<I>(&mut self, iter: I)
    where
        I: Iterator<Item = Node>,
    {
        for node in iter {
            self.push(node);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn not_empty(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn saw(&self, node: Node) -> bool {
        self.seen.contains(&node)
    }

    pub fn seen(&self) -> &HashSet<Node> {
        &self.seen
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.stack.pop()
    }
}
