#![allow(unused_comparisons, unused_imports, dead_code)]
use num::{
    traits::{WrappingAdd, WrappingShl, WrappingShr, WrappingSub},
    Integer,
};

use crate::{
    blocks_in_pre_order, compute_division_magic::compute_division_magic, dominators::Dominators,
    eliminate_dead_code::eliminate_dead_code, insertion_set::InsertionSet, kind::Kind,
    phi_children::PhiChildren, procedure::Procedure, pure_cse::PureCSE, size_of_type, BlockId,
    Frequency, NumChildren, Opcode, OptLevel, TriState, Type, TypeKind, Value, ValueData, ValueId,
};

/// Does strength reduction, constant folding, canonicalization, CFG simplification, DCE, and very
/// simple CSE. This phase runs those optimizations to fixpoint. The goal of the phase is to
/// dramatically reduce the complexity of the code. In the future, it's preferable to add optimizations
/// to this phase rather than creating new optimizations because then the optimizations can participate
/// in the fixpoint. However, because of the many interlocking optimizations, it can be difficult to
/// add sophisticated optimizations to it. For that reason we have full CSE in a different phase, for
/// example.
pub fn reduce_strength(proc: &mut Procedure) -> bool {
    let mut reduce_strength = ReduceStrength::new(proc);

    reduce_strength.run()
}

// The goal of this phase is to:
//
// - Replace operations with less expensive variants. This includes constant folding and classic
//   strength reductions like turning Mul(x, 1 << k) into Shl(x, k).
//
// - Reassociate constant operations. For example, Load(Add(x, c)) is turned into Load(x, offset = c)
//   and Add(Add(x, c), d) is turned into Add(x, c + d).
//
// - Canonicalize operations. There are some cases where it's not at all obvious which kind of
//   operation is less expensive, but it's useful for subsequent phases - particularly LowerToAir -
//   to have only one way of representing things.
//
// This phase runs to fixpoint. Therefore, the canonicalizations must be designed to be monotonic.
// For example, if we had a canonicalization that said that Add(x, -c) should be Sub(x, c) and
// another canonicalization that said that Sub(x, d) should be Add(x, -d), then this phase would end
// up running forever. We don't want that.
//
// Therefore, we need to prioritize certain canonical forms over others. Naively, we want strength
// reduction to reduce the number of values, and so a form involving fewer total values is more
// canonical. But we might break this, for example when reducing strength of Mul(x, 9). This could be
// better written as Add(Shl(x, 3), x), which also happens to be representable using a single
// instruction on x86.
//
// Here are some of the rules we have:
//
// Canonical form of logical not: BitXor(value, 1). We may have to avoid using this form if we don't
// know for sure that 'value' is 0-or-1 (i.e. returnsBool). In that case we fall back on
// Equal(value, 0).
//
// Canonical form of commutative operations: if the operation involves a constant, the constant must
// come second. Add(x, constant) is canonical, while Add(constant, x) is not. If there are no
// constants then the canonical form involves the lower-indexed value first. Given Add(x, y), it's
// canonical if x->index() <= y->index().
struct ReduceStrength<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
    root: BlockId,
    block: BlockId,
    index: usize,
    value: ValueId,
    dominators: Dominators<Procedure>,
    pure_cse: PureCSE,
    changed: bool,
    changed_cfg: bool,
}

impl<'a> ReduceStrength<'a> {
    pub fn new(proc: &'a mut Procedure) -> Self {
        let doms = proc.dominators_or_compute().clone();
        let pure_cse = PureCSE::new();
        let iset = InsertionSet::new();

        Self {
            proc,
            insertion_set: iset,
            root: BlockId(0),
            block: BlockId(usize::MAX),
            index: 0,
            value: ValueId(usize::MAX),
            dominators: doms,
            pure_cse,
            changed: false,
            changed_cfg: false,
        }
    }

    pub fn run(&mut self) -> bool {
        let mut result = false;
        let mut first = true;

        let mut index = 0;

        loop {
            self.changed = false;
            self.changed_cfg = false;
            index += 1;

            if first {
                first = false;
            } else if self.proc.options.dump_b3_reduce_strength {
                println!("B3 after iteration #{} of reduce_strength", index - 1);
                print!("{}", self.proc.display_());
            }

            self.simplify_cfg();

            if self.changed_cfg {
                self.proc.reset_reachability();
                self.proc.invalidate_cfg();
                self.changed = true;
            }

            // We definitely want to do DCE before we do CSE so that we don't hoist things. For
            // example:
            //
            // @dead = Mul(@a, @b)
            // ... lots of control flow and stuff
            // @thing = Mul(@a, @b)
            //
            // If we do CSE before DCE, we will remove @thing and keep @dead. Effectively, we will
            // "hoist" @thing. On the other hand, if we run DCE before CSE, we will kill @dead and
            // keep @thing. That's better, since we usually want things to stay wherever the client
            // put them. We're not actually smart enough to move things around at random.
            //self.changed |= eliminate_dead_code(self.proc);

            self.simplify_ssa();

            if self.proc.options.opt_level >= OptLevel::O2 {
                self.proc.reset_value_owners();
                self.dominators = self.proc.dominators_or_compute().clone();
                self.pure_cse.clear();
            }

            for block in blocks_in_pre_order(BlockId(0), self.proc) {
                self.block = block;

                self.index = 0;
                while self.index < self.block.size(self.proc) {
                    self.value = self.block.value(self.proc, self.index);
                    Value::perform_substitution(self.value, self.proc);
                    self.reduce_value_strength();

                    self.index += 1;
                }

                self.insertion_set.execute(self.proc, block);
            }

            self.handle_changed_cfg_if_necessary();
            result |= self.changed;

            if self.changed && self.proc.options.opt_level >= OptLevel::O2 {
                continue;
            } else {
                break;
            }
        }

        if self.proc.options.opt_level < OptLevel::O2 {
            self.changed_cfg = false;
            self.simplify_cfg();
            self.handle_changed_cfg_if_necessary();
        }

        result
    }

    fn handle_changed_cfg_if_necessary(&mut self) {
        if self.changed_cfg {
            self.proc.reset_reachability();
            self.proc.invalidate_cfg();
            self.dominators = self.proc.dominators_or_compute().clone();
            self.changed = true;
        }
    }

    fn reduce_value_strength(&mut self) {
        let opcode = self.proc.value(self.value).kind.opcode();

        match opcode {
            Opcode::Opaque => {
                // Turn this: Opaque(Opaque(value))
                // Into this: Opaque(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Opaque {
                    let child = self.value.child(self.proc, 0);
                    self.proc.value_mut(self.value).replace_with_identity(child);
                }
            }

            Opcode::Add => {
                
                self.handle_commutativity();

                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Add
                    && self.proc.value(self.value).is_integer()
                {
                    // Turn this: Add(Add(value, constant1), constant2)
                    // Into this: Add(value, constant1 + constant2)
                    /*
                    let new_sum = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .add_constant(
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1)),
                        );

                    if let Some(new_sum) = new_sum {
                        //println!("Reducing strength of Add(Add(value, constant1), constant2) to Add(value, constant1 + constant2): {:?} = {:?} + {:?} => {:?}", self.value, self.value.child(self.proc, 1),self.value.child(self.proc, 0).child(self.proc, 1), new_sum.as_int());
                        
                        let new_sum = self.proc.add(new_sum);
                        self.insertion_set.insert_value(self.index, new_sum);
                        self.proc.value_mut(self.value).children[0] =
                            self.value.child(self.proc, 0).child(self.proc, 0);
                        self.proc.value_mut(self.value).children[1] = new_sum;
                        self.changed = true;
                        return;
                    }*/
                    {
                        // Turn this: Add(Add(value, constant), otherValue)
                        // Into this: Add(Add(value, otherValue), constant)

                        if !self.proc.value(self.value.child(self.proc, 1)).has_int()
                            && self
                                .proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1))
                                .has_int()
                        {
                            let value = self.value.child(self.proc, 0).child(self.proc, 0);
                            let constant = self.value.child(self.proc, 0).child(self.proc, 1);
                            let other_value = self.value.child(self.proc, 1);
                            // This could create duplicate code if Add(value, constant) is used elsewhere.
                            // However, we already model adding a constant as if it was free in other places
                            // so let's just roll with it. The alternative would mean having to do good use
                            // counts, which reduce_strength() currently doesn't have.
                            let new_add = Value::new(
                                Opcode::Add,
                                self.proc.value(self.value).typ(),
                                NumChildren::Two,
                                &[value, other_value],
                                ValueData::None,
                            );
                            let new_add = self.proc.add(new_add);
                            self.insertion_set.insert_value(self.index, new_add);
                            self.proc.value_mut(self.value).children[0] = new_add;
                            self.proc.value_mut(self.value).children[1] = constant;
                            self.changed = true;
                            return;
                        }
                    }
                }
                {
                    // Turn this: Add(otherValue, Add(value, constant))
                    // Into this: Add(Add(value, otherValue), constant)
                    if self.proc.value(self.value).is_integer()
                        && !self.proc.value(self.value.child(self.proc, 0)).has_int()
                        && self.value.child(self.proc, 1).opcode(self.proc) == Opcode::Add
                        && self
                            .proc
                            .value(self.value.child(self.proc, 1).child(self.proc, 1))
                            .has_int()
                    {
                        let value = self.value.child(self.proc, 1).child(self.proc, 0);
                        let constant = self.value.child(self.proc, 1).child(self.proc, 1);
                        let other_value = self.value.child(self.proc, 0);

                        let new_add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[value, other_value],
                            ValueData::None,
                        );

                        let new_add = self.proc.add(new_add);

                        self.insertion_set.insert_value(self.index, new_add);

                        self.proc.value_mut(self.value).children[0] = new_add;
                        self.proc.value_mut(self.value).children[1] = constant;
                        self.changed = true;
                        return;

                        // Turn this: Add(constant1, constant2)
                        // Into this: constant1 + constant2
                    }
                    if let Some(constant_add) = self
                        .proc
                        .value(self.value.child(self.proc, 0))
                        .add_constant(&self.proc.value(self.value.child(self.proc, 1)))
                    {
                        let constant_add = self.proc.add(constant_add);
                        self.replace_with_new_value(Some(constant_add));
                        return;
                        // Turn this: Integer Add(value, value)
                        // Into this: Shl(value, 1)
                        // This is a useful canonicalization. It's not meant to be a strength reduction.
                    }
                    if self.proc.value(self.value).is_integer()
                        && self.value.child(self.proc, 0) == self.value.child(self.proc, 1)
                    {
                        let value = self.value.child(self.proc, 0);
                        let constant = self
                            .proc
                            .add_int_constant(self.proc.value(self.value).typ, 1);
                        self.insertion_set.insert_value(self.index, constant);
                        let new_shl = Value::new(
                            Opcode::Shl,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[value, constant],
                            ValueData::None,
                        );
                        let new_shl = self.proc.add(new_shl);
                        self.replace_with_new_value(Some(new_shl));
                        return;
                    }
                    {
                        // Turn this: Add(value, zero)
                        // Into an Identity.
                        //
                        // Addition is subtle with doubles. Zero is not the neutral value, negative zero is:
                        //    0 + 0 = 0
                        //    0 + -0 = 0
                        //    -0 + 0 = 0
                        //    -0 + -0 = -0
                        if !self.proc.value(self.value).is_sensitive_to_nan() {
                            if self.proc.value(self.value.child(self.proc, 1)).is_int_of(0)
                                || self
                                    .proc
                                    .value(self.value.child(self.proc, 1))
                                    .is_negative_zero()
                            {
                                self.replace_with_identity(self.value.child(self.proc, 0));
                                return;
                            }
                        }

                        if self.proc.value(self.value).is_integer() {
                            // Turn this: Integer Add(value, Neg(otherValue))
                            // Into this: Sub(value, otherValue)
                            if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::Neg {
                                let value = self.value.child(self.proc, 0);
                                let other_value =
                                    self.value.child(self.proc, 1).child(self.proc, 0);
                                let new_sub = Value::new(
                                    Opcode::Sub,
                                    self.proc.value(self.value).typ(),
                                    NumChildren::Two,
                                    &[value, other_value],
                                    ValueData::None,
                                );
                                let new_sub = self.proc.add(new_sub);
                                self.replace_with_new_value(Some(new_sub));
                                return;
                            }

                            // Turn this: Integer Add(Neg(value), otherValue)
                            // Into this: Sub(otherValue, value)
                            if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Neg {
                                let value = self.value.child(self.proc, 1);
                                let other_value =
                                    self.value.child(self.proc, 0).child(self.proc, 0);
                                let new_sub = Value::new(
                                    Opcode::Sub,
                                    self.proc.value(self.value).typ(),
                                    NumChildren::Two,
                                    &[value, other_value],
                                    ValueData::None,
                                );
                                let new_sub = self.proc.add(new_sub);
                                self.replace_with_new_value(Some(new_sub));
                                return;
                            }
                            // Turn this: Integer Add(Sub(0, value), -1)
                            // Into this: BitXor(value, -1)
                            if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Sub
                                && self
                                    .proc
                                    .value(self.value.child(self.proc, 1))
                                    .is_int_of(-1)
                                && self
                                    .proc
                                    .value(self.value.child(self.proc, 0).child(self.proc, 0))
                                    .is_int_of(0)
                            {
                                let value = self.value.child(self.proc, 0).child(self.proc, 1);
                                let constant = self.value.child(self.proc, 1);
                                let new_bitxor = Value::new(
                                    Opcode::BitXor,
                                    self.proc.value(self.value).typ(),
                                    NumChildren::Two,
                                    &[value, constant],
                                    ValueData::None,
                                );
                                let new_bitxor = self.proc.add(new_bitxor);
                                self.replace_with_new_value(Some(new_bitxor));
                                return;
                            }

                            if self.handle_mul_distributivity() {
                                return;
                            }
                        }
                    }
                }
            }
            Opcode::Sub => {
                
                // Turn this: Sub(BitXor(BitAnd(value, mask1), mask2), mask2)
                // Into this: SShr(Shl(value, amount), amount)
                // Conditions:
                // 1. mask1 = (1 << width) - 1
                // 2. mask2 = 1 << (width - 1)
                // 3. amount = datasize - width
                // 4. 0 < width < datasize

                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitXor
                    && self
                        .value
                        .child(self.proc, 0)
                        .child(self.proc, 0)
                        .opcode(self.proc)
                        == Opcode::BitAnd
                    && self
                        .proc
                        .value(
                            self.value
                                .child(self.proc, 0)
                                .child(self.proc, 0)
                                .child(self.proc, 1),
                        )
                        .has_int()
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .has_int()
                    && self.proc.value(self.value.child(self.proc, 1)).has_int()
                {
                   
                    let mask1 = self
                        .proc
                        .value(
                            self.value
                                .child(self.proc, 0)
                                .child(self.proc, 0)
                                .child(self.proc, 1),
                        )
                        .as_int()
                        .unwrap();
                    let mask2 = self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .unwrap();
                    let mask3 = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .unwrap();
                    let width = mask1.count_ones();
                    let datasize = if self.proc.value(self.value).typ() == Type::Int64 {
                        64
                    } else {
                        32
                    };
                    let is_valid_mask1 =
                        mask1 != 0 && (mask1 & (mask1 + 1)) == 0 && width < datasize;
                    let is_valid_mask2 = mask2 == mask3 && ((mask2 << 1) - 1) == mask1;

                    if is_valid_mask1 && is_valid_mask2 {
                        let amount = self
                            .proc
                            .add_int_constant(Type::Int32, (datasize - width) as i64);
                        let value = self
                            .value
                            .child(self.proc, 0)
                            .child(self.proc, 0)
                            .child(self.proc, 0);
                        let shl_value = Value::new(
                            Opcode::Shl,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[value, amount],
                            ValueData::None,
                        );
                        let shl_value = self.proc.add(shl_value);
                        let shr_value = Value::new(
                            Opcode::SShr,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[shl_value, amount],
                            ValueData::None,
                        );
                        let shr_value = self.proc.add(shr_value);
                        self.replace_with_new_value(Some(shr_value));
                        return;
                    }
                }

                // Turn this: Sub(constant1, constant2)
                // Into this: constant1 - constant2
                if let Some(constant_sub) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .sub_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant_sub = self.proc.add(constant_sub);
                    self.replace_with_new_value(Some(constant_sub));
                    return;
                }

                if self.proc.value(self.value).is_integer() {
                   
                    // Turn this: Sub(Neg(value), 1)
                    // Into this: BitXor(value, -1)
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Neg
                        && self.proc.value(self.value.child(self.proc, 1)).is_int_of(1)
                    {
                        let minus_one = if self
                            .proc
                            .value(self.value.child(self.proc, 0).child(self.proc, 0))
                            .typ
                            .kind()
                            == TypeKind::Int32
                        {
                            self.proc.add_int_constant(Type::Int32, -1)
                        } else {
                            self.proc.add_int_constant(Type::Int64, -1)
                        };

                        let bitxor = Value::new(
                            Opcode::BitXor,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 0),
                                minus_one,
                            ],
                            ValueData::None,
                        );

                        self.insertion_set.insert_value(self.index, minus_one);

                        let bitxor = self.proc.add(bitxor);
                        self.replace_with_new_value(Some(bitxor));

                        return;
                    }

                    // Turn this: Sub(value, constant)
                    // Into this: Add(value, -constant)
                    if let Some(negated_constant) = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .neg_constant()
                    {
                        let negated_constant = self.proc.add(negated_constant);
                        self.insertion_set
                            .insert_value(self.index, negated_constant);
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[self.value.child(self.proc, 0), negated_constant],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.replace_with_new_value(Some(add));
                        return;
                    }

                    // Turn this: Sub(0, value)
                    // Into this: Neg(value)
                    if self.proc.value(self.value.child(self.proc, 0)).is_int_of(0) {
                        let neg = Value::new(
                            Opcode::Neg,
                            self.proc.value(self.value).typ(),
                            NumChildren::One,
                            &[self.value.child(self.proc, 1)],
                            ValueData::None,
                        );
                        let neg = self.proc.add(neg);
                        self.replace_with_new_value(Some(neg));
                        return;
                    }

                    // Turn this: Sub(value, value)
                    // Into this: 0
                    if self.value.child(self.proc, 0) == self.value.child(self.proc, 1) {
                        let zero = self
                            .proc
                            .add_int_constant(self.proc.value(self.value).typ(), 0);
                        self.replace_with_new_value(Some(zero));
                        return;
                    }
                    // Turn this: Sub(value, Neg(otherValue))
                    // Into this: Add(value, otherValue)
                    if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::Neg {
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0),
                                self.value.child(self.proc, 1).child(self.proc, 0),
                            ],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.replace_with_new_value(Some(add));
                        return;
                    }
                    // Turn this: Sub(Neg(value), value2)
                    // Into this: Neg(Add(value, value2))
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Neg {
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 0),
                                self.value.child(self.proc, 1),
                            ],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.insertion_set.insert_value(self.index, add);
                        let neg = Value::new(
                            Opcode::Neg,
                            self.proc.value(self.value).typ(),
                            NumChildren::One,
                            &[add],
                            ValueData::None,
                        );
                        let neg = self.proc.add(neg);
                        self.replace_with_new_value(Some(neg));
                        return;
                    }
                    // Turn this: Sub(Sub(a, b), c)
                    // Into this: Sub(a, Add(b, c))
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Sub {
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 1),
                                self.value.child(self.proc, 1),
                            ],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.insertion_set.insert_value(self.index, add);
                        let sub = Value::new(
                            Opcode::Sub,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[self.value.child(self.proc, 0).child(self.proc, 0), add],
                            ValueData::None,
                        );
                        let sub = self.proc.add(sub);
                        self.replace_with_new_value(Some(sub));
                        return;
                    }

                    // Turn this: Sub(a, Sub(b, c))
                    // Into this: Add(Sub(a, b), c)
                    if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::Sub {
                        let sub = Value::new(
                            Opcode::Sub,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0),
                                self.value.child(self.proc, 1).child(self.proc, 0),
                            ],
                            ValueData::None,
                        );
                        let sub = self.proc.add(sub);
                        self.insertion_set.insert_value(self.index, sub);
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[sub, self.value.child(self.proc, 1).child(self.proc, 1)],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.replace_with_new_value(Some(add));
                        return;
                    }

                    // Turn this: Sub(Add(a, b), c)
                    // Into this: Add(a, Sub(b, c))
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Add {
                        let sub = Value::new(
                            Opcode::Sub,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 1),
                                self.value.child(self.proc, 1),
                            ],
                            ValueData::None,
                        );
                        let sub = self.proc.add(sub);
                        self.insertion_set.insert_value(self.index, sub);
                        let add = Value::new(
                            Opcode::Add,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[self.value.child(self.proc, 0).child(self.proc, 0), sub],
                            ValueData::None,
                        );
                        let add = self.proc.add(add);
                        self.replace_with_new_value(Some(add));
                        return;
                    }

                    if self.handle_mul_distributivity() {
                        return;
                    }
                }
            }

            Opcode::Neg => {
                // Turn this: Neg(constant)
                // Into this: -constant
                if let Some(negated_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .neg_constant()
                {
                    let negated_constant = self.proc.add(negated_constant);
                    self.replace_with_new_value(Some(negated_constant));
                    return;
                }

                // Turn this: Neg(Neg(value))
                // Into this: value
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Neg {
                    let value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.replace_with_identity(value);
                    return;
                }

                if self.proc.value(self.value).is_integer() {
                    // Turn this: Integer Neg(Sub(value, otherValue))
                    // Into this: Sub(otherValue, value)
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Sub {
                        let sub = Value::new(
                            Opcode::Sub,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 1),
                                self.value.child(self.proc, 0).child(self.proc, 0),
                            ],
                            ValueData::None,
                        );
                        let sub = self.proc.add(sub);
                        self.replace_with_new_value(Some(sub));
                        return;
                    }
                    // Turn this: Integer Neg(Mul(value, c))
                    // Into this: Mul(value, -c), as long as -c does not overflow
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Mul
                        && self
                            .proc
                            .value(self.value.child(self.proc, 0).child(self.proc, 1))
                            .has_int()
                    {
                        let factor = self
                            .proc
                            .value(self.value.child(self.proc, 0).child(self.proc, 1))
                            .as_int()
                            .unwrap();

                        if self.proc.value(self.value).typ() == Type::Int32
                            && factor != i32::MIN as i64
                        {
                            let new_factor = self.proc.add_int_constant(Type::Int32, -factor);
                            self.insertion_set.insert_value(self.index, new_factor);
                            let mul = Value::new(
                                Opcode::Mul,
                                self.proc.value(self.value).typ(),
                                NumChildren::Two,
                                &[
                                    self.value.child(self.proc, 0).child(self.proc, 0),
                                    new_factor,
                                ],
                                ValueData::None,
                            );
                            let mul = self.proc.add(mul);
                            self.replace_with_new_value(Some(mul));
                        } else if self.proc.value(self.value).typ() == Type::Int64
                            && factor != i64::MIN
                        {
                            let new_factor = self.proc.add_int_constant(Type::Int64, -factor);
                            self.insertion_set.insert_value(self.index, new_factor);
                            let mul = Value::new(
                                Opcode::Mul,
                                self.proc.value(self.value).typ(),
                                NumChildren::Two,
                                &[
                                    self.value.child(self.proc, 0).child(self.proc, 0),
                                    new_factor,
                                ],
                                ValueData::None,
                            );
                            let mul = self.proc.add(mul);
                            self.replace_with_new_value(Some(mul));
                        }
                    }
                }
            }

            Opcode::Mul => {
                self.handle_commutativity();

                // Turn this: Mul(constant1, constant2)
                // Into this: constant1 * constant2
                if let Some(mul_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .mul_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let mul_constant = self.proc.add(mul_constant);
                    self.replace_with_new_value(Some(mul_constant));
                    return;
                }

                if let Some(factor) = self.proc.value(self.value.child(self.proc, 1)).as_int() {
                    // Turn this: Mul(value, 0)
                    // Into this: 0
                    // Note that we don't do this for doubles because that's wrong. For example, -1 * 0
                    // and 1 * 0 yield different results.
                    if factor == 0 {
                        self.replace_with_identity(self.value.child(self.proc, 1));
                        return;
                    }

                    // Turn this: Mul(value, 1)
                    // Into this: value
                    if factor == 1 {
                        self.replace_with_identity(self.value.child(self.proc, 0));
                        return;
                    }
                    // Turn this: Mul(value, -1)
                    // Into this: Neg(value)
                    if factor == -1 {
                        let neg = Value::new(
                            Opcode::Neg,
                            self.proc.value(self.value).typ(),
                            NumChildren::One,
                            &[self.value.child(self.proc, 0)],
                            ValueData::None,
                        );
                        let neg = self.proc.add(neg);
                        self.replace_with_new_value(Some(neg));
                        return;
                    }
                    // Turn this: Mul(value, constant)
                    // Into this: Shl(value, log2(constant))
                    if has_one_bit_set(factor) {
                        let log2 = factor.trailing_zeros();
                        let log2 = self.proc.add_int_constant(Type::Int32, log2 as i64);
                        self.insertion_set.insert_value(self.index, log2);
                        let shl = Value::new(
                            Opcode::Shl,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[self.value.child(self.proc, 0), log2],
                            ValueData::None,
                        );
                        let shl = self.proc.add(shl);
                        self.replace_with_new_value(Some(shl));
                        return;
                    }
                } else if let Some(dbl) =
                    self.proc.value(self.value.child(self.proc, 1)).as_double()
                {
                    // Turn this: Mul(value, 1.0)
                    // Into this: value
                    if dbl == 1.0 {
                        self.replace_with_identity(self.value.child(self.proc, 0));
                        return;
                    }
                }

                if self.proc.value(self.value).is_integer() {
                    // Turn this: Integer Mul(value, Neg(otherValue))
                    // Into this: Neg(Mul(value, otherValue))
                    if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::Neg {
                        let mul = Value::new(
                            Opcode::Mul,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0),
                                self.value.child(self.proc, 1).child(self.proc, 0),
                            ],
                            ValueData::None,
                        );
                        let mul = self.proc.add(mul);
                        self.insertion_set.insert_value(self.index, mul);
                        let neg = Value::new(
                            Opcode::Neg,
                            self.proc.value(self.value).typ(),
                            NumChildren::One,
                            &[mul],
                            ValueData::None,
                        );
                        let neg = self.proc.add(neg);
                        self.replace_with_new_value(Some(neg));
                        return;
                    }

                    // Turn this: Integer Mul(Neg(value), otherValue)
                    // Into this: Neg(Mul(value, value2))
                    if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Neg {
                        let mul = Value::new(
                            Opcode::Mul,
                            self.proc.value(self.value).typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 0),
                                self.value.child(self.proc, 1),
                            ],
                            ValueData::None,
                        );
                        let mul = self.proc.add(mul);
                        self.insertion_set.insert_value(self.index, mul);
                        let neg = Value::new(
                            Opcode::Neg,
                            self.proc.value(self.value).typ(),
                            NumChildren::One,
                            &[mul],
                            ValueData::None,
                        );
                        let neg = self.proc.add(neg);
                        self.replace_with_new_value(Some(neg));
                        return;
                    }
                }
            }

            Opcode::Div => {
                // Turn this: Div(constant1, constant2)
                // Into this: constant1 / constant2
                // Note that this uses Div<Chill> semantics. That's fine, because the rules for Div
                // are strictly weaker: it has corner cases where it's allowed to do anything it
                // likes.
                if let Some(div_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .div_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let div_constant = self.proc.add(div_constant);
                    self.replace_with_new_value(Some(div_constant));
                    return;
                }

                if let Some(divisor) = self.proc.value(self.value.child(self.proc, 1)).as_int() {
                    match divisor {
                        -1 => {
                            // Turn this: Div(value, -1)
                            // Into this: Neg(value)
                            let neg = Value::new(
                                Opcode::Neg,
                                self.proc.value(self.value).typ(),
                                NumChildren::One,
                                &[self.value.child(self.proc, 0)],
                                ValueData::None,
                            );
                            let neg = self.proc.add(neg);
                            self.replace_with_new_value(Some(neg));
                            return;
                        }
                        0 => {
                            // Turn this: Div(value, 0)
                            // Into this: 0
                            // We can do this because it's precisely correct for ChillDiv and for Div we
                            // are allowed to do whatever we want.
                            self.replace_with_identity(self.value.child(self.proc, 1));
                        }
                        1 => {
                            // Turn this: Div(value, 1)
                            // Into this: value
                            self.replace_with_identity(self.value.child(self.proc, 0));
                            return;
                        }

                        divisor => {
                            // Perform super comprehensive strength reduction of division. Currently we
                            // only do this for 32-bit divisions, since we need a high multiply
                            // operation. We emulate it using 64-bit multiply. We can't emulate 64-bit
                            // high multiply with a 128-bit multiply because we don't have a 128-bit
                            // multiply. We could do it with a patchpoint if we cared badly enough.
                            if self.proc.value(self.value).typ().kind() != TypeKind::Int32 {
                                return;
                            }

                            if self.proc.options.opt_level < OptLevel::O2 {
                                return;
                            }

                            let divisor = divisor as i32;
                            let magic = compute_division_magic(divisor);

                            // Perform the "high" multiplication. We do it just to get the high bits.
                            // This is sort of like multiplying by the reciprocal, just more gnarly. It's
                            // from Hacker's Delight and I don't claim to understand it.

                            let left = self.value.child(self.proc, 0);
                            let sext32 = Value::new(
                                Opcode::SExt32,
                                Type::Int64,
                                NumChildren::One,
                                &[left],
                                ValueData::None,
                            );
                            let sext32 = self.proc.add(sext32);
                            self.insertion_set.insert_value(self.index, sext32);

                            let const64 = self
                                .proc
                                .add_int_constant(Type::Int64, magic.magic_multiplier as i64);
                            self.insertion_set.insert_value(self.index, const64);

                            let mul = Value::new(
                                Opcode::Mul,
                                Type::Int64,
                                NumChildren::Two,
                                &[sext32, const64],
                                ValueData::None,
                            );
                            let mul = self.proc.add(mul);
                            self.insertion_set.insert_value(self.index, mul);
                            let const32 = self.proc.add_int_constant(Type::Int32, 32);
                            self.insertion_set.insert_value(self.index, const32);

                            let zshr = Value::new(
                                Opcode::ZShr,
                                Type::Int64,
                                NumChildren::Two,
                                &[mul, const32],
                                ValueData::None,
                            );
                            let zshr = self.proc.add(zshr);
                            self.insertion_set.insert_value(self.index, zshr);

                            let magic_quotient = Value::new(
                                Opcode::Trunc,
                                Type::Int32,
                                NumChildren::One,
                                &[zshr],
                                ValueData::None,
                            );
                            let mut magic_quotient = self.proc.add(magic_quotient);
                            self.insertion_set.insert_value(self.index, magic_quotient);

                            if divisor > 0 && magic.magic_multiplier < 0 {
                                let sub = Value::new(
                                    Opcode::Add,
                                    Type::Int32,
                                    NumChildren::Two,
                                    &[magic_quotient, left],
                                    ValueData::None,
                                );
                                let sub = self.proc.add(sub);
                                self.insertion_set.insert_value(self.index, sub);
                                magic_quotient = sub;
                            }

                            if divisor < 0 && magic.magic_multiplier > 0 {
                                let add = Value::new(
                                    Opcode::Sub,
                                    Type::Int32,
                                    NumChildren::Two,
                                    &[magic_quotient, left],
                                    ValueData::None,
                                );
                                let add = self.proc.add(add);
                                self.insertion_set.insert_value(self.index, add);
                                magic_quotient = add;
                            }

                            if magic.shift > 0 {
                                let const32 =
                                    self.proc.add_int_constant(Type::Int32, magic.shift as i32);
                                self.insertion_set.insert_value(self.index, const32);
                                let sshr = Value::new(
                                    Opcode::SShr,
                                    Type::Int32,
                                    NumChildren::Two,
                                    &[magic_quotient, const32],
                                    ValueData::None,
                                );

                                let sshr = self.proc.add(sshr);
                                self.insertion_set.insert_value(self.index, sshr);
                                magic_quotient = sshr;
                            }

                            let const31 = self.proc.add_int_constant(Type::Int32, 31);
                            self.insertion_set.insert_value(self.index, const31);
                            let zshr = Value::new(
                                Opcode::ZShr,
                                Type::Int32,
                                NumChildren::Two,
                                &[magic_quotient, const31],
                                ValueData::None,
                            );

                            let zshr = self.proc.add(zshr);
                            self.insertion_set.insert_value(self.index, zshr);

                            let add = Value::new(
                                Opcode::Add,
                                Type::Int32,
                                NumChildren::Two,
                                &[zshr, magic_quotient],
                                ValueData::None,
                            );

                            let add = self.proc.add(add);
                            self.insertion_set.insert_value(self.index, add);

                            self.replace_with_identity(add);
                        }
                    }
                }
            }

            Opcode::UDiv => {
                // Turn this: UDiv(constant1, constant2)
                // Into this: constant1 / constant2
                if let Some(udiv_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .udiv_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(udiv_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                if let Some(divisor) = self.proc.value(self.value.child(self.proc, 1)).as_int() {
                    match divisor {
                        0 => {
                            // Turn this: UDiv(x, 0)
                            // Into this: 0
                            // We can do whatever we want here so we might as well do the chill thing,
                            // in case we add chill versions of UDiv in the future.
                            self.replace_with_identity(self.value.child(self.proc, 1));
                        }

                        1 => {
                            // Turn this: UDiv(x, 1)
                            // Into this: x
                            self.replace_with_identity(self.value.child(self.proc, 0));
                        }

                        _ => {
                            // FIXME: We should do comprehensive strength reduction for unsigned numbers
                        }
                    }
                }
            }

            Opcode::Mod => {
                // Turn this: Mod(constant1, constant2)
                // Into this: constant1 % constant2
                // Note that this uses Mod<Chill> semantics.
                if let Some(mod_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .mod_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(mod_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Modulo by constant is more efficient if we turn it into Div, and then let Div get
                // optimized.
                if let Some(divisor) = self.proc.value(self.value.child(self.proc, 1)).as_int() {
                    match divisor {
                        0 => {
                            // Turn this: Mod(value, 0)
                            // Into this: 0
                            // This is correct according to ChillMod semantics.
                            self.replace_with_identity(self.value.child(self.proc, 1));
                        }

                        _ => {
                            // Turn this: Mod(N, D)
                            // Into this: Sub(N, Mul(Div(N, D), D))
                            //
                            // This is a speed-up because we use our existing Div optimizations.
                            //
                            // Here's an easier way to look at it:
                            //     N % D = N - N / D * D
                            //
                            // Note that this does not work for D = 0 and ChillMod. The expected result is 0.
                            // That's why we have a special-case above.
                            //     X % 0 = X - X / 0 * 0 = X     (should be 0)
                            //
                            // This does work for the D = -1 special case.
                            //     -2^31 % -1 = -2^31 - -2^31 / -1 * -1
                            //                = -2^31 - -2^31 * -1
                            //                = -2^31 - -2^31
                            //                = 0

                            let mut div_kind: Kind = Opcode::Div.into();
                            div_kind.set_is_chill(true);

                            let div = Value::new(
                                div_kind,
                                self.proc.value(self.value.child(self.proc, 0)).typ(),
                                NumChildren::Two,
                                &[
                                    self.value.child(self.proc, 0),
                                    self.value.child(self.proc, 1),
                                ],
                                ValueData::None,
                            );

                            let div = self.proc.add(div);
                            self.insertion_set.insert_value(self.index, div);

                            let mul = Value::new(
                                Opcode::Mul,
                                self.proc.value(self.value.child(self.proc, 0)).typ(),
                                NumChildren::Two,
                                &[div, self.value.child(self.proc, 1)],
                                ValueData::None,
                            );

                            let mul = self.proc.add(mul);
                            self.insertion_set.insert_value(self.index, mul);

                            let sub = Value::new(
                                Opcode::Sub,
                                self.proc.value(self.value.child(self.proc, 0)).typ(),
                                NumChildren::Two,
                                &[self.value.child(self.proc, 0), mul],
                                ValueData::None,
                            );

                            let sub = self.proc.add(sub);
                            self.insertion_set.insert_value(self.index, sub);
                            self.replace_with_new_value(Some(sub));
                        }
                    }
                }
            }

            Opcode::UMod => {
                // Turn this: UMod(constant1, constant2)
                // Into this: constant1 % constant2

                if let Some(umod_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .umod_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(umod_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }
            }

            Opcode::FMax => {
                // Turn this: FMax(constant1, constant2)
                // Into this: constant1.max(constant2)
                if let Some(fmax_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .fmax_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(fmax_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }
            }

            Opcode::FMin => {
                // Turn this: FMin(constant1, constant2)
                // Into this: constant1.min(constant2)
                if let Some(fmin_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .fmin_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(fmin_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }
            }

            Opcode::BitAnd => {
                self.handle_commutativity();
                // Turn this: BitAnd(constant1, constant2)
                // Into this: constant1 & constant2
                if let Some(bitand_constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .bit_and_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(bitand_constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: BitAnd(BitAnd(value, constant1), constant2)
                // Into this: BitAnd(value, constant1 & constant2).
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitAnd {
                    if let Some(bitand_constant) = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .bit_and_constant(
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1)),
                        )
                    {
                        let constant = self.proc.add(bitand_constant);
                        self.insertion_set.insert_value(self.index, constant);

                        self.proc.value_mut(self.value).children[0] =
                            self.value.child(self.proc, 0).child(self.proc, 0);
                        self.proc.value_mut(self.value).children[1] = constant;
                        self.changed = true;
                        return;
                    }
                }

                // Turn this: BitAnd(valueX, valueX)
                // Into this: valueX.
                if self.value.child(self.proc, 0) == self.value.child(self.proc, 1) {
                    self.replace_with_identity(self.value.child(self.proc, 0));
                    return;
                }

                // Turn this: BitAnd(value, zero-constant)
                // Into this: zero-constant.
                if self.proc.value(self.value.child(self.proc, 1)).is_int_of(0) {
                    self.replace_with_identity(self.value.child(self.proc, 1));
                    return;
                }

                // Turn this: BitAnd(ZShr(value, shiftAmount), mask)
                // Conditions:
                // 1. mask = (1 << width) - 1
                // 2. 0 <= shiftAmount < datasize
                // 3. 0 < width < datasize
                // 4. shiftAmount + width >= datasize
                // Into this: ZShr(value, shiftAmount)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::ZShr
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .filter(|x| *x >= 0)
                        .is_some()
                    && self.proc.value(self.value.child(self.proc, 1)).has_int()
                {
                    let shift_amount = self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;
                    let mask = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;

                    let is_valid_mask = mask != 0 && (mask & (mask + 1)) == 0;
                    let datasize = if self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 0))
                        .typ()
                        .kind()
                        == TypeKind::Int32
                    {
                        32
                    } else {
                        64
                    };
                    let width = mask.count_ones() as u64;

                    if shift_amount < datasize
                        && is_valid_mask
                        && shift_amount.wrapping_add(width) >= datasize
                    {
                        self.replace_with_identity(self.value.child(self.proc, 0));
                        return;
                    }
                }

                // Turn this: BitAnd(Shl(value, shiftAmount), maskShift)
                // Into this: Shl(BitAnd(value, mask), shiftAmount)
                // Conditions:
                // 1. maskShift = mask << shiftAmount
                // 2. mask = (1 << width) - 1
                // 3. 0 <= shiftAmount < datasize
                // 4. 0 < width < datasize
                // 5. shiftAmount + width <= datasize
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Shl
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .filter(|x| *x >= 0)
                        .is_some()
                    && self.proc.value(self.value.child(self.proc, 1)).has_int()
                {
                    let shift_amount = self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;
                    let mask_shift = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;
                    let mask_shift_amount = mask_shift.trailing_zeros() as u64;
                    let mask = mask_shift >> mask_shift_amount;
                    let width = mask.count_ones() as u64;
                    let datasize = if self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 0))
                        .typ()
                        .kind()
                        == TypeKind::Int32
                    {
                        32
                    } else {
                        64
                    };

                    let is_valid_shift_amount =
                        shift_amount == mask_shift_amount && shift_amount < datasize;
                    let is_valid_mask = mask != 0 && (mask & (mask + 1)) == 0 && width < datasize;

                    if is_valid_shift_amount && is_valid_mask && shift_amount + width <= datasize {
                        let mask_value = if datasize == 32 {
                            self.proc.add_int_constant(Type::Int32, mask as i64)
                        } else {
                            self.proc.add_int_constant(Type::Int64, mask as i64)
                        };

                        self.insertion_set.insert_value(self.index, mask_value);

                        let bitand = Value::new(
                            Opcode::BitAnd,
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 0))
                                .typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 0),
                                mask_value,
                            ],
                            ValueData::None,
                        );

                        let bitand = self.proc.add(bitand);
                        self.insertion_set.insert_value(self.index, bitand);
                        let shl = Value::new(
                            Opcode::Shl,
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 0))
                                .typ(),
                            NumChildren::Two,
                            &[bitand, self.value.child(self.proc, 0).child(self.proc, 1)],
                            ValueData::None,
                        );

                        let shl = self.proc.add(shl);
                        self.replace_with_new_value(Some(shl));
                        return;
                    }
                }

                // Turn this: BitAnd(value, all-ones)
                // Into this: value.
                if (self.proc.value(self.value).typ() == Type::Int64
                    && self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .is_int_of(u64::MAX as i64))
                    || (self.proc.value(self.value).typ() == Type::Int32
                        && self
                            .proc
                            .value(self.value.child(self.proc, 1))
                            .is_int_of(u32::MAX as i64))
                {
                    self.replace_with_identity(self.value.child(self.proc, 0));
                }

                // Turn this: BitAnd(64-bit value, 32 ones)
                // Into this: ZExt32(Trunc(64-bit value))
                if self.proc.value(self.value).is_int64_of(0xffffffff) {
                    let trunc = Value::new(
                        Opcode::Trunc,
                        Type::Int32,
                        NumChildren::One,
                        &[self.value.child(self.proc, 0)],
                        ValueData::None,
                    );

                    let trunc = self.proc.add(trunc);
                    self.insertion_set.insert_value(self.index, trunc);

                    let zext = Value::new(
                        Opcode::ZExt32,
                        Type::Int64,
                        NumChildren::One,
                        &[trunc],
                        ValueData::None,
                    );

                    let zext = self.proc.add(zext);
                    self.replace_with_identity(zext);
                    return;
                }

                // Turn this: BitAnd(SExt8(value), mask) where (mask & 0xffffff00) == 0
                // Into this: BitAnd(value, mask)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt8
                    && self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .filter(|x| *x & 0xffffff00 == 0)
                        .is_some()
                {
                    self.proc.value_mut(self.value).children[0] =
                        self.value.child(self.proc, 0).child(self.proc, 0);
                    self.changed = true;
                    return;
                }

                // Turn this: BitAnd(SExt16(value), mask) where (mask & 0xffff0000) == 0
                // Into this: BitAnd(value, mask)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt16
                    && self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .filter(|x| *x & 0xffff0000 == 0)
                        .is_some()
                {
                    self.proc.value_mut(self.value).children[0] =
                        self.value.child(self.proc, 0).child(self.proc, 0);
                    self.changed = true;
                    return;
                }

                // Turn this: BitAnd(SExt32(value), mask) where (mask & 0xffffffff00000000) == 0
                // Into this: BitAnd(ZExt32(value), mask)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt32
                    && self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .filter(|x| *x as u64 & 0xffffffff00000000u64 == 0)
                        .is_some()
                {
                    let zext = Value::new(
                        Opcode::ZExt32,
                        Type::Int64,
                        NumChildren::One,
                        &[self.value.child(self.proc, 0).child(self.proc, 0)],
                        ValueData::None,
                    );

                    let zext = self.proc.add(zext);
                    self.insertion_set.insert_value(self.index, zext);

                    self.proc.value_mut(self.value).children[0] = zext;
                    self.changed = true;
                    return;
                }

                // Turn this: BitAnd(Op(value, constant1), constant2)
                //     where !(constant1 & constant2)
                //       and Op is BitOr or BitXor
                // into this: BitAnd(value, constant2)
                if let Some(constant2) = self.proc.value(self.value.child(self.proc, 1)).as_int() {
                    let mut replaced = false;
                    match self.value.child(self.proc, 0).opcode(self.proc) {
                        Opcode::BitOr | Opcode::BitXor => {
                            if let Some(constant1) = self
                                .proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1))
                                .as_int()
                            {
                                if (constant1 & constant2) == 0 {
                                    self.proc.value_mut(self.value).children[0] =
                                        self.value.child(self.proc, 0).child(self.proc, 0);
                                    self.changed = true;
                                    replaced = true;
                                }
                            }
                        }
                        _ => (),
                    }

                    if replaced {
                        return;
                    }
                }
            }

            Opcode::BitOr => {
                self.handle_commutativity();

                // Turn this: BitOr(constant1, constant2)
                // Into this: constant1 | constant2
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .bit_or_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: BitOr(BitOr(value, constant1), constant2)
                // Into this: BitOr(value, constant1 | constant2).
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitOr {
                    if let Some(constant) = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .bit_or_constant(
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1)),
                        )
                    {
                        let constant = self.proc.add(constant);
                        self.insertion_set.insert_value(self.index, constant);
                        self.proc.value_mut(self.value).children[1] = constant;
                        self.proc.value_mut(self.value).children[0] =
                            self.value.child(self.proc, 0).child(self.proc, 0);
                        self.changed = true;
                        return;
                    }
                }

                // Turn this: BitOr(valueX, valueX)
                // Into this: valueX.
                if self.value.child(self.proc, 0) == self.value.child(self.proc, 1) {
                    self.replace_with_new_value(Some(self.value.child(self.proc, 0)));
                    return;
                }

                // Turn this: BitOr(value, zero-constant)
                // Into this: value.
                if self
                    .proc
                    .value(self.value.child(self.proc, 1))
                    .as_int()
                    .filter(|x| *x == 0)
                    .is_some()
                {
                    self.replace_with_new_value(Some(self.value.child(self.proc, 0)));
                    return;
                }

                // Turn this: BitOr(value, all-ones)
                // Into this: all-ones.
                if (self.proc.value(self.value).typ() == Type::Int32
                    && self.proc.value(self.value.child(self.proc, 1)).as_int() == Some(0xffffffff))
                    || (self.proc.value(self.value).typ() == Type::Int64
                        && self
                            .proc
                            .value(self.value.child(self.proc, 1))
                            .as_int()
                            .map(|x| x as u64)
                            == Some(0xffffffffffffffff))
                {
                    self.replace_with_new_value(Some(self.value.child(self.proc, 1)));
                    return;
                }

                if self.handle_bit_and_diversity() {
                    return;
                }
            }

            Opcode::BitXor => {
                // Turn this: BitXor(constant1, constant2)
                // Into this: constant1 ^ constant2
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .bit_xor_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: BitXor(BitXor(value, constant1), constant2)
                // Into this: BitXor(value, constant1 ^ constant2).
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitXor {
                    if let Some(constant) = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .bit_xor_constant(
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 1)),
                        )
                    {
                        let constant = self.proc.add(constant);
                        self.insertion_set.insert_value(self.index, constant);
                        self.proc.value_mut(self.value).children[1] = constant;
                        self.proc.value_mut(self.value).children[0] =
                            self.value.child(self.proc, 0).child(self.proc, 0);
                        self.changed = true;
                        return;
                    }
                }

                // Turn this: BitXor(valueX, valueX)
                // Into this: zero-constant.
                if self.value.child(self.proc, 0) == self.value.child(self.proc, 1) {
                    let zero = self
                        .proc
                        .add_int_constant(self.proc.value(self.value).typ(), 0);
                    self.replace_with_new_value(Some(zero));
                    return;
                }

                // Turn this: BitXor(value, zero-constant)
                // Into this: value.
                if self
                    .proc
                    .value(self.value.child(self.proc, 1))
                    .as_int()
                    .filter(|x| *x == 0)
                    .is_some()
                {
                    self.replace_with_identity(self.value.child(self.proc, 0));
                    return;
                }
            }

            Opcode::Shl => {
                // Turn this: Shl(constant1, constant2)
                // Into this: constant1 << constant2
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .shl_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: Shl(<S|Z>Shr(@x, @const), @const)
                // Into this: BitAnd(@x, -(1<<@const))
                if (self.value.child(self.proc, 0).opcode(self.proc) == Opcode::ZShr
                    || self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SShr)
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .has_int()
                    && self.proc.value(self.value.child(self.proc, 1)).has_int()
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        == self.proc.value(self.value.child(self.proc, 1)).as_int()
                {
                    let shift_amount = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .unwrap()
                        & if self.proc.value(self.value).typ() == Type::Int32 {
                            31
                        } else {
                            63
                        };
                    let new_consst = self
                        .proc
                        .add_int_constant(self.proc.value(self.value).typ(), -(1 << shift_amount));
                    self.insertion_set.insert_value(self.index, new_consst);

                    let bitand = Value::new(
                        Opcode::BitAnd,
                        self.proc.value(self.value).typ(),
                        NumChildren::Two,
                        &[
                            self.value.child(self.proc, 0).child(self.proc, 0),
                            new_consst,
                        ],
                        ValueData::None,
                    );
                    let bitand = self.proc.add(bitand);
                    self.replace_with_new_value(Some(bitand));
                    return;
                }

                self.handle_shift_amount();
            }

            Opcode::SShr => {
                // Turn this: SShr(constant1, constant2)
                // Into this: constant1 >> constant2
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .sshr_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                if self.proc.value(self.value.child(self.proc, 1)).has_int32()
                    && self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Shl
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .has_int32()
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int32()
                        == self.proc.value(self.value.child(self.proc, 1)).as_int32()
                {
                    match self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int32()
                        .unwrap()
                    {
                        16 => {
                            // Turn this: SShr(Shl(value, 16), 16)
                            // Into this: SExt16(value)
                            let sext16 = Value::new(
                                Opcode::SExt16,
                                Type::Int32,
                                NumChildren::One,
                                &[self.value.child(self.proc, 0).child(self.proc, 0)],
                                ValueData::None,
                            );

                            let sext16 = self.proc.add(sext16);
                            self.replace_with_new_value(Some(sext16));
                        }

                        24 => {
                            // Turn this: SShr(Shl(value, 24), 24)
                            // Into this: SExt8(value)
                            let sext8 = Value::new(
                                Opcode::SExt8,
                                Type::Int32,
                                NumChildren::One,
                                &[self.value.child(self.proc, 0).child(self.proc, 0)],
                                ValueData::None,
                            );

                            let sext8 = self.proc.add(sext8);
                            self.replace_with_new_value(Some(sext8));
                        }

                        32 => {
                            // Turn this: SShr(Shl(value, 32), 32)
                            // Into this: SExt32(Trunc(value))

                            let trunc = Value::new(
                                Opcode::Trunc,
                                Type::Int32,
                                NumChildren::One,
                                &[self.value.child(self.proc, 0).child(self.proc, 0)],
                                ValueData::None,
                            );

                            let trunc = self.proc.add(trunc);
                            self.insertion_set.insert_value(self.index, trunc);

                            let sext32 = Value::new(
                                Opcode::SExt32,
                                Type::Int64,
                                NumChildren::One,
                                &[trunc],
                                ValueData::None,
                            );

                            let sext32 = self.proc.add(sext32);
                            self.replace_with_new_value(Some(sext32));
                        }

                        48 => {
                            if self.proc.value(self.value).typ() == Type::Int64 {
                                // Turn this: SShr(Shl(value, 48), 48)
                                // Into this: SExt16To64(Trunc(value))

                                let trunc = Value::new(
                                    Opcode::Trunc,
                                    Type::Int32,
                                    NumChildren::One,
                                    &[self.value.child(self.proc, 0).child(self.proc, 0)],
                                    ValueData::None,
                                );

                                let trunc = self.proc.add(trunc);
                                self.insertion_set.insert_value(self.index, trunc);

                                let sext16 = Value::new(
                                    Opcode::SExt16To64,
                                    Type::Int64,
                                    NumChildren::One,
                                    &[trunc],
                                    ValueData::None,
                                );

                                let sext16 = self.proc.add(sext16);
                                self.replace_with_new_value(Some(sext16));
                            }
                        }

                        56 => {
                            if self.proc.value(self.value).typ() == Type::Int64 {
                                // Turn this: SShr(Shl(value, 56), 56)
                                // Into this: SExt8To64(Trunc(value))

                                let trunc = Value::new(
                                    Opcode::Trunc,
                                    Type::Int32,
                                    NumChildren::One,
                                    &[self.value.child(self.proc, 0).child(self.proc, 0)],
                                    ValueData::None,
                                );

                                let trunc = self.proc.add(trunc);
                                self.insertion_set.insert_value(self.index, trunc);

                                let sext8 = Value::new(
                                    Opcode::SExt8To64,
                                    Type::Int64,
                                    NumChildren::One,
                                    &[trunc],
                                    ValueData::None,
                                );

                                let sext8 = self.proc.add(sext8);
                                self.replace_with_new_value(Some(sext8));
                            }
                        }

                        _ => (),
                    }

                    if self.value.opcode(self.proc) != Opcode::SShr {
                        return;
                    }
                }

                self.handle_shift_amount();
            }

            Opcode::ZShr => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .zshr_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: ZShr(Shl(value, amount)), amount)
                // Into this: BitAnd(value, mask)
                // Conditions:
                // 1. 0 <= amount < datasize
                // 2. width = datasize - amount
                // 3. mask is !(mask & (mask + 1)) where bitCount(mask) == width
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Shl
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .filter(|&x| x >= 0)
                        .is_some()
                    && self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .filter(|&x| x >= 0)
                        .is_some()
                {
                    let amount1 = self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;
                    let amount2 = self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .as_int()
                        .unwrap() as u64;
                    let datasize = if self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 0))
                        .typ()
                        == Type::Int64
                    {
                        64
                    } else {
                        32
                    };

                    if amount1 == amount2 && amount1 < datasize {
                        let width = datasize - amount1;
                        let mask = (1 << width) - 1;

                        let mask_value = if datasize == 32 {
                            let c = self.proc.add_int_constant(Type::Int32, mask as i64);
                            self.insertion_set.insert_value(self.index, c);
                            c
                        } else {
                            let c = self.proc.add_int_constant(Type::Int64, mask as i64);
                            self.insertion_set.insert_value(self.index, c);
                            c
                        };

                        let bitand = Value::new(
                            Opcode::BitAnd,
                            self.proc
                                .value(self.value.child(self.proc, 0).child(self.proc, 0))
                                .typ(),
                            NumChildren::Two,
                            &[
                                self.value.child(self.proc, 0).child(self.proc, 0),
                                mask_value,
                            ],
                            ValueData::None,
                        );

                        let bitand = self.proc.add(bitand);
                        self.replace_with_new_value(Some(bitand));
                        return;
                    }
                }
            }

            Opcode::RotR => {
                // Turn this: RotR(constant1, constant2)
                // Into this: (constant1 >> constant2) | (constant1 << sizeof(constant1) * 8 - constant2)
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .rotr_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                self.handle_shift_amount();
            }

            Opcode::RotL => {
                // Turn this: RotL(constant1, constant2)
                // Into this: (constant1 << constant2) | (constant1 >> sizeof(constant1) * 8 - constant2)
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .rotl_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                self.handle_shift_amount();
            }

            Opcode::BitwiseCast => {
                // Turn this: BitwiseCast(constant)
                // Into this: transmute::<self.value.typ()>(constant)
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .bitwise_cast_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                    return;
                }

                // Turn this: BitwiseCast(BitwiseCast(value))
                // Into this: value
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitwiseCast {
                    let value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.replace_with_identity(value);
                    return;
                }
            }

            Opcode::SExt8 => {
                // Turn this: SExt8(constant)
                // Into this: static_cast<int8_t>(constant)
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int8 = self.proc.add_int_constant(
                        self.proc.value(self.value).typ(),
                        int32 as i8 as i32 as i64,
                    );
                    self.replace_with_new_value(Some(int8));
                    return;
                }

                // Turn this: SExt8(SExt8(value))
                //   or this: SExt8(SExt16(value))
                // Into this: SExt8(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt8
                    || self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt16
                {
                    let value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.replace_with_identity(value);
                    return;
                }
            }

            Opcode::SExt16 => {
                // Turn this: SExt16(constant)
                // Into this: static_cast<int16_t>(constant)
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int16 = self.proc.add_int_constant(
                        self.proc.value(self.value).typ(),
                        int32 as i16 as i32 as i64,
                    );
                    self.replace_with_new_value(Some(int16));
                    return;
                }

                // Turn this: SExt16(SExt16(value))
                // Into this: SExt16(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt16 {
                    self.proc.value_mut(self.value).children[0] =
                        self.value.child(self.proc, 0).child(self.proc, 0);
                    self.changed = true;
                }

                // Turn this: SExt16(SExt8(value))
                // Into this: SExt8(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt8 {
                    let value = self.value.child(self.proc, 0);
                    self.replace_with_identity(value);
                    return;
                }
            }

            Opcode::SExt8To64 => {
                // Turn this: SExt8To64(constant)
                // Into this: static_cast<int64_t>(static_cast<int8_t>(constant))
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int64 = self.proc.add_int_constant(
                        self.proc.value(self.value).typ(),
                        int32 as i8 as i32 as i64,
                    );
                    self.replace_with_new_value(Some(int64));
                    return;
                }

                // Turn this: SExt8To64(SExt8(value))
                //   or this: SExt8To64(SExt16(value))
                // Into this: SExt8To64(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt8
                    || self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt16
                {
                    let value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.proc.value_mut(self.value).children[0] = value;
                    self.changed = true;
                    return;
                }
            }

            Opcode::SExt16To64 => {
                // Turn this: SExt16To64(constant)
                // Into this: static_cast<int64_t>(static_cast<int16_t>(constant))
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int64 = self.proc.add_int_constant(
                        self.proc.value(self.value).typ(),
                        int32 as i16 as i32 as i64,
                    );
                    self.replace_with_new_value(Some(int64));
                    return;
                }

                // Turn this: SExt16To64(SExt16(value))
                // Into this: SExt16To64(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt16 {
                    let value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.proc.value_mut(self.value).children[0] = value;
                    self.changed = true;
                    return;
                }

                // Turn this: SExt16To64(SExt8(value))
                // Into this: SExt8To64(value)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::SExt8 {
                    let value = self.value.child(self.proc, 0);
                    let new_value = Value::new(
                        Opcode::SExt8To64,
                        self.proc.value(self.value).typ(),
                        NumChildren::One,
                        &[value],
                        ValueData::None,
                    );
                    let new_value = self.proc.add(new_value);
                    self.replace_with_new_value(Some(new_value));
                    return;
                }
            }

            Opcode::SExt32 => {
                // Turn this: SExt32(constant)
                // Into this: static_cast<int32_t>(constant)
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int32 = self
                        .proc
                        .add_int_constant(self.proc.value(self.value).typ(), int32 as i32 as i64);
                    self.replace_with_new_value(Some(int32));
                    return;
                }
            }

            Opcode::ZExt32 => {
                // Turn this: ZExt32(constant)
                // Into this: static_cast<uint32_t>(constant)
                if let Some(int32) = self.proc.value(self.value.child(self.proc, 0)).as_int32() {
                    let int32 = self
                        .proc
                        .add_int_constant(self.proc.value(self.value).typ(), int32 as u32 as i64);
                    self.replace_with_new_value(Some(int32));
                    return;
                }
            }

            Opcode::Trunc => {
                // Turn this: Trunc(constant)
                // Into this: static_cast<int32_t>(constant)
                if let Some(int64) = self.proc.value(self.value.child(self.proc, 0)).as_int64() {
                    let int32 = self
                        .proc
                        .add_int_constant(self.proc.value(self.value).typ(), int64 as i32 as i64);
                    self.replace_with_new_value(Some(int32));
                    return;
                }

                if let Some(float64) = self.proc.value(self.value.child(self.proc, 0)).as_double() {
                    let float32 = self.proc.add_bits_constant(
                        self.proc.value(self.value).typ(),
                        (float64 as f32).to_bits() as u64,
                    );

                    self.replace_with_new_value(Some(float32));
                    return;
                }
            }

            Opcode::IToD => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .i2d_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::IToF => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .i2f_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::DToI => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .d2i_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::FToI => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .f2i_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::FloatToDouble => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .f2d_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::DoubleToFloat => {
                if let Some(constant) = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .d2f_constant()
                {
                    let constant = self.proc.add(constant);
                    self.replace_with_new_value(Some(constant));
                }
            }

            Opcode::Load8Z
            | Opcode::Load8S
            | Opcode::Load16Z
            | Opcode::Load16S
            | Opcode::Load
            | Opcode::Store16
            | Opcode::Store8
            | Opcode::Store => {
                let mut address = self
                    .proc
                    .value(self.value)
                    .children
                    .last()
                    .copied()
                    .unwrap();

                // Turn this: Load(Add(address, offset1), offset = offset2)
                // Into this: Load(address, offset = offset1 + offset2)
                //
                // Also turns this: Store(value, Add(address, offset1), offset = offset2)
                // Into this: Store(value, address, offset = offset1 + offset2)

                let memory = self.proc.value(self.value).memory_value().unwrap();
                if address.opcode(self.proc) == Opcode::Add
                    && self.proc.value(address.child(self.proc, 0)).has_int64()
                {
                    let mut offset = self
                        .proc
                        .value(address.child(self.proc, 1))
                        .as_int64()
                        .unwrap();

                    if !sum_overflows::<i64>(offset, memory.0 as i64) {
                        offset += memory.0 as i64;
                        let small_offset = offset as i32;

                        if small_offset as i64 == offset {
                            address = address.child(self.proc, 0);
                            *self.proc.value_mut(self.value).children.last_mut().unwrap() = address;
                            *self
                                .proc
                                .value_mut(self.value)
                                .memory_value_mut()
                                .unwrap()
                                .0 = small_offset;
                            self.changed = true;
                        }
                    }
                }

                // Turn this: Load(constant1, offset = constant2)
                // Into this: Load(constant1 + constant2)
                //
                // This is a fun canonicalization. It purely regresses naively generated code. We rely
                // on constant materialization to be smart enough to materialize this constant the smart
                // way. We want this canonicalization because we want to know if two memory accesses see
                // the same address.
                let memory = self.proc.value(self.value).memory_value().unwrap();

                if memory.0 != 0 {
                    if let Some(new_address) = self.proc.value(address).add_constant_i32(memory.0) {
                        let new_address = self.proc.add(new_address);
                        self.insertion_set.insert_value(self.index, new_address);
                        *self.proc.value_mut(self.value).children.last_mut().unwrap() = new_address;
                        *self
                            .proc
                            .value_mut(self.value)
                            .memory_value_mut()
                            .unwrap()
                            .0 = 0;
                        self.changed = true;
                    }
                }
            }

            Opcode::Equal => {
                self.handle_commutativity();

                // Turn this: Equal(bool, 0)
                // Into this: BitXor(bool, 1)
                if self
                    .proc
                    .value(self.value.child(self.proc, 1))
                    .is_int32_of(0)
                {
                    let bool = self.value.child(self.proc, 0);
                    let one = self.proc.add_int_constant(Type::Int32, 1);
                    self.insertion_set.insert_value(self.index, one);
                    let xor = Value::new(
                        Opcode::BitXor,
                        Type::Int32,
                        NumChildren::Two,
                        &[bool, one],
                        ValueData::None,
                    );
                    let xor = self.proc.add(xor);
                    self.replace_with_new_value(Some(xor));
                    return;
                }

                // Turn this Equal(bool, 1)
                // Into this: bool
                if self
                    .proc
                    .value(self.value.child(self.proc, 1))
                    .is_int32_of(1)
                {
                    let bool = self.value.child(self.proc, 0);
                    self.replace_with_identity(bool);
                    return;
                }

                // Turn this: Equal(const1, const2)
                // Into this: const1 == const2
                match self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .equal_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    TriState::True => {
                        let constant = self.proc.add_int_constant(Type::Int32, 1);
                        self.replace_with_new_value(Some(constant));
                    }
                    TriState::False => {
                        let constant = self.proc.add_int_constant(Type::Int32, 0);
                        self.replace_with_new_value(Some(constant));
                    }
                    TriState::Undeterminate => (),
                }
            }

            Opcode::NotEqual => {
                self.handle_commutativity();

                if self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .returns_bool(self.proc)
                {
                    // Turn this: NotEqual(bool, 0)
                    // Into this: bool
                    if self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .is_int32_of(0)
                    {
                        let bool = self.value.child(self.proc, 0);
                        self.replace_with_identity(bool);
                        return;
                    }

                    // Turn this: NotEqual(bool, 1)
                    // Into this: Equal(bool, 0)
                    if self
                        .proc
                        .value(self.value.child(self.proc, 1))
                        .is_int32_of(1)
                    {
                        let bool = self.value.child(self.proc, 0);
                        let zero = self.proc.add_int_constant(Type::Int32, 0);
                        self.insertion_set.insert_value(self.index, zero);
                        let equal = Value::new(
                            Opcode::Equal,
                            Type::Int32,
                            NumChildren::Two,
                            &[bool, zero],
                            ValueData::None,
                        );
                        let equal = self.proc.add(equal);
                        self.replace_with_new_value(Some(equal));
                        return;
                    }
                }

                // Turn this: NotEqual(const1, const2)
                // Into this: const1 != const2
                match self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .equal_constant(self.proc.value(self.value.child(self.proc, 1)))
                {
                    TriState::True => {
                        let constant = self.proc.add_int_constant(Type::Int32, 0);
                        self.replace_with_new_value(Some(constant));
                    }
                    TriState::False => {
                        let constant = self.proc.add_int_constant(Type::Int32, 1);
                        self.replace_with_new_value(Some(constant));
                    }
                    TriState::Undeterminate => (),
                }
            }

            Opcode::LessThan
            | Opcode::GreaterThan
            | Opcode::LessEqual
            | Opcode::GreaterEqual
            | Opcode::Below
            | Opcode::Above
            | Opcode::BelowEqual
            | Opcode::AboveEqual => {
                let comparison = canonicalize_comparison(self.proc, self.value);

                let result;

                match comparison.opcode {
                    Opcode::LessThan => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .greater_than_constant(self.proc.value(comparison.operands[0]));
                    }
                    Opcode::GreaterThan => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .less_than_constant(self.proc.value(comparison.operands[0]));
                    }
                    Opcode::LessEqual => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .greater_than_equal_constant(self.proc.value(comparison.operands[0]));
                    }

                    Opcode::GreaterEqual => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .less_than_equal_constant(self.proc.value(comparison.operands[0]));
                    }

                    Opcode::Below => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .above_constant(self.proc.value(comparison.operands[0]));
                    }

                    Opcode::Above => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .below_constant(self.proc.value(comparison.operands[0]));
                    }

                    Opcode::BelowEqual => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .above_equal_constant(self.proc.value(comparison.operands[0]));
                    }

                    Opcode::AboveEqual => {
                        result = self
                            .proc
                            .value(comparison.operands[1])
                            .below_equal_constant(self.proc.value(comparison.operands[0]));
                    }

                    _ => unreachable!(),
                };

                match result {
                    TriState::True => {
                        let constant = self.proc.add_int_constant(Type::Int32, 1);
                        self.replace_with_new_value(Some(constant));
                        return;
                    }
                    TriState::False => {
                        let constant = self.proc.add_int_constant(Type::Int32, 0);
                        self.replace_with_new_value(Some(constant));
                        return;
                    }
                    TriState::Undeterminate => (),
                }

                if comparison.opcode != self.value.opcode(self.proc) {
                    let new_value = Value::new(
                        comparison.opcode,
                        Type::Int32,
                        NumChildren::Two,
                        &comparison.operands,
                        ValueData::None,
                    );
                    let new_value = self.proc.add(new_value);
                    self.replace_with_new_value(Some(new_value));
                }
            }

            Opcode::Branch => {
                // Turn this: Branch(NotEqual(x, 0))
                // Into this: Branch(x)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::NotEqual
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .is_int_of(0)
                {
                    let new_value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.proc.value_mut(self.value).children[0] = new_value;
                    self.changed = true;
                }

                // Turn this: Branch(Equal(x, 0), then, else)
                // Into this: Branch(x, else, then)
                if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::Equal
                    && self
                        .proc
                        .value(self.value.child(self.proc, 0).child(self.proc, 1))
                        .is_int_of(0)
                {
                    let new_value = self.value.child(self.proc, 0).child(self.proc, 0);
                    self.proc.value_mut(self.value).children[0] = new_value;
                    let taken = self.proc.block(self.block).taken();
                    let not_taken = self.proc.block(self.block).not_taken();
                    self.proc.block_mut(self.block).successor_list[0] = not_taken;
                    self.proc.block_mut(self.block).successor_list[1] = taken;
                    self.changed = true;
                }

                let tri_state = self
                    .proc
                    .value(self.value.child(self.proc, 0))
                    .as_tri_state();

                match tri_state {
                    TriState::True => {
                        let taken = self.proc.block(self.block).taken();
                        let not_taken = self.proc.block(self.block).not_taken();
                        self.proc
                            .block_mut(not_taken.0)
                            .remove_predecessor(self.block);
                        self.proc
                            .value_mut(self.value)
                            .replace_with_jump(self.block, taken);
                        self.changed_cfg = true;
                    }
                    TriState::False => {
                        let taken = self.proc.block(self.block).taken();
                        let not_taken = self.proc.block(self.block).not_taken();
                        self.proc.block_mut(taken.0).remove_predecessor(self.block);
                        self.proc
                            .value_mut(self.value)
                            .replace_with_jump(self.block, not_taken);
                        self.changed_cfg = true;
                    }
                    TriState::Undeterminate => (),
                }
            }
            _ => (),
        }
    }

    fn find_recent_node_matching(
        &self,
        _start: ValueId,
        bound: usize,
        _functor: impl Fn(&Self, ValueId) -> bool,
    ) -> Option<ValueId> {
        let _start_index = if bound < self.index {
            self.index - bound
        } else {
            0
        };

        todo!()
    }

    /// For Op==Add or Sub, turn any of these:
    ///      Op(Mul(x1, x2), Mul(x1, x3))
    ///      Op(Mul(x2, x1), Mul(x1, x3))
    ///      Op(Mul(x1, x2), Mul(x3, x1))
    ///      Op(Mul(x2, x1), Mul(x3, x1))
    /// Into this: Mul(x1, Op(x2, x3))
    fn handle_mul_distributivity(&mut self) -> bool {
        let mut x1 = None::<ValueId>;
        let mut x2 = None::<ValueId>;
        let mut x3 = None::<ValueId>;

        if self.value.child(self.proc, 0).opcode(&self.proc) == Opcode::Mul
            && self.value.child(self.proc, 1).opcode(&self.proc) == Opcode::Mul
        {
            if self.value.child(self.proc, 0).child(self.proc, 0)
                == self.value.child(self.proc, 1).child(self.proc, 0)
            {
                // Op(Mul(x1, x2), Mul(x1, x3))
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 1));
            } else if self.value.child(self.proc, 0).child(self.proc, 1)
                == self.value.child(self.proc, 1).child(self.proc, 0)
            {
                // Op(Mul(x2, x1), Mul(x1, x3))
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 1));
            } else if self.value.child(self.proc, 0).child(self.proc, 0)
                == self.value.child(self.proc, 1).child(self.proc, 1)
            {
                // Op(Mul(x1, x2), Mul(x3, x1))
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 0));
            } else if self.value.child(self.proc, 0).child(self.proc, 1)
                == self.value.child(self.proc, 1).child(self.proc, 1)
            {
                // Op(Mul(x2, x1), Mul(x3, x1))
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 0));
            }
        }

        if let (Some(x1), Some(x2), Some(x3)) = (x1, x2, x3) {
            let opcode = self.proc.value(self.value).kind.opcode();
            let new_op = Value::new(
                opcode,
                self.proc.value(self.value).typ(),
                NumChildren::Two,
                &[x2, x3],
                ValueData::None,
            );
            let new_op = self.proc.add(new_op);
            let replace = Value::new(
                Opcode::Mul,
                self.proc.value(self.value).typ(),
                NumChildren::Two,
                &[x1, new_op],
                ValueData::None,
            );

            let new_value = self.proc.add(replace);
            self.insertion_set.insert_value(self.index, new_value);
            self.replace_with_new_value(Some(new_value));
            return true;
        }

        false
    }

    /// For Op==BitOr or BitXor, turn any of these:
    ///      Op(BitAnd(x1, x2), BitAnd(x1, x3))
    ///      Op(BitAnd(x2, x1), BitAnd(x1, x3))
    ///      Op(BitAnd(x1, x2), BitAnd(x3, x1))
    ///      Op(BitAnd(x2, x1), BitAnd(x3, x1))
    /// Into this: BitAnd(Op(x2, x3), x1)
    /// And any of these:
    ///      Op(BitAnd(x1, x2), x1)
    ///      Op(BitAnd(x2, x1), x1)
    ///      Op(x1, BitAnd(x1, x2))
    ///      Op(x1, BitAnd(x2, x1))
    /// Into this: BitAnd(Op(x2, x1), x1)
    /// This second set is equivalent to doing x1 => BitAnd(x1, x1), and then applying the first set.
    /// It does not reduce the number of operations executed, but provides some useful normalization: we prefer to have BitAnd at the outermost, then BitXor, and finally BitOr at the innermost
    fn handle_bit_and_diversity(&mut self) -> bool {
        let mut x1 = None::<ValueId>;
        let mut x2 = None::<ValueId>;
        let mut x3 = None::<ValueId>;

        if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitAnd
            && self.value.child(self.proc, 1).opcode(self.proc) == Opcode::BitAnd
        {
            if self.value.child(self.proc, 1) == self.value.child(self.proc, 0) {
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 1));
            } else if self.value.child(self.proc, 0).child(self.proc, 1)
                == self.value.child(self.proc, 1).child(self.proc, 0)
            {
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 1));
            } else if self.value.child(self.proc, 0).child(self.proc, 0)
                == self.value.child(self.proc, 1).child(self.proc, 1)
            {
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 0));
            } else if self.value.child(self.proc, 0).child(self.proc, 1)
                == self.value.child(self.proc, 1).child(self.proc, 1)
            {
                x1 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
                x3 = Some(self.value.child(self.proc, 1).child(self.proc, 0));
            }
        } else if self.value.child(self.proc, 0).opcode(self.proc) == Opcode::BitAnd {
            if self.value.child(self.proc, 0) == self.value.child(self.proc, 1) {
                x1 = Some(self.value.child(self.proc, 1));
                x3 = x1;
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 1));
            } else if self.value.child(self.proc, 0).child(self.proc, 1)
                == self.value.child(self.proc, 1)
            {
                x1 = Some(self.value.child(self.proc, 1));
                x3 = x1;
                x2 = Some(self.value.child(self.proc, 0).child(self.proc, 0));
            }
        } else if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::BitAnd {
            if self.value.child(self.proc, 1).child(self.proc, 0) == self.value.child(self.proc, 0)
            {
                x1 = Some(self.value.child(self.proc, 1));
                x3 = x1;
                x2 = Some(self.value.child(self.proc, 1).child(self.proc, 1));
            } else if self.value.child(self.proc, 1).child(self.proc, 1)
                == self.value.child(self.proc, 0)
            {
                x1 = Some(self.value.child(self.proc, 0));
                x3 = x1;
                x2 = Some(self.value.child(self.proc, 1).child(self.proc, 0));
            }
        }

        if let (Some(x1), Some(x2), Some(x3)) = (x1, x2, x3) {
            let bitop = Value::new(
                self.value.opcode(self.proc),
                self.proc.value(self.value).typ(),
                crate::NumChildren::Two,
                &[x2, x3],
                ValueData::None,
            );
            let bitop = self.proc.add(bitop);
            self.insertion_set.insert_value(self.index, bitop);
            let bitand = Value::new(
                Opcode::BitAnd,
                self.proc.value(self.value).typ(),
                crate::NumChildren::Two,
                &[x1, bitop],
                ValueData::None,
            );
            let bitand = self.proc.add(bitand);
            self.replace_with_new_value(Some(bitand));
            return true;
        }

        false
    }

    /// FIXME: This should really be a forward analysis. Instead, we uses a bounded-search backwards
    /// analysis.
    fn range_for(&self, value: ValueId, time_to_live: usize) -> IntRange {
        if time_to_live == 0 {
            return IntRange::top_ty(self.proc.value(value).typ());
        }

        match value.opcode(self.proc) {
            Opcode::Const32 | Opcode::Const64 => {
                let int_value = self.proc.value(value).as_int().unwrap();

                return IntRange {
                    min: int_value,
                    max: int_value,
                };
            }

            Opcode::BitAnd => {
                if let Some(int) = self.proc.value(value.child(self.proc, 1)).as_int() {
                    return IntRange::range_for_mask_ty(int, self.proc.value(value).typ());
                }
            }

            Opcode::SShr => {
                if let Some(int32) = self.proc.value(value.child(self.proc, 1)).as_int32() {
                    let range = self
                        .range_for(value.child(self.proc, 0), time_to_live - 1)
                        .sshr_typ(int32, self.proc.value(value).typ());

                    return range;
                }
            }

            Opcode::ZShr => {
                if let Some(int32) = self.proc.value(value.child(self.proc, 1)).as_int32() {
                    let range = self
                        .range_for(value.child(self.proc, 0), time_to_live - 1)
                        .zshr_typ(int32, self.proc.value(value).typ());

                    return range;
                }
            }

            Opcode::Shl => {
                if let Some(int32) = self.proc.value(value.child(self.proc, 1)).as_int32() {
                    let range = self
                        .range_for(value.child(self.proc, 0), time_to_live - 1)
                        .shl_ty(int32, self.proc.value(value).typ());

                    return range;
                }
            }

            Opcode::Add => {
                let left = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                let right = self.range_for(value.child(self.proc, 1), time_to_live - 1);

                return left.add_typ(&right, self.proc.value(value).typ());
            }

            Opcode::Sub => {
                let left = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                let right = self.range_for(value.child(self.proc, 1), time_to_live - 1);

                return left.sub_typ(&right, self.proc.value(value).typ());
            }

            Opcode::Mul => {
                let left = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                let right = self.range_for(value.child(self.proc, 1), time_to_live - 1);

                return left.mul_typ(&right, self.proc.value(value).typ());
            }

            Opcode::SExt8 | Opcode::SExt8To64 => {
                let range = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                return range.sext::<i8>();
            }

            Opcode::SExt16 | Opcode::SExt16To64 => {
                let range = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                return range.sext::<i16>();
            }

            Opcode::SExt32 => {
                let range = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                return range.sext::<i32>();
            }

            Opcode::ZExt32 => {
                let range = self.range_for(value.child(self.proc, 0), time_to_live - 1);
                return range.zext32();
            }

            _ => (),
        }

        IntRange::top_ty(self.proc.value(value).typ())
    }

    fn replace_with_new_value(&mut self, value: Option<ValueId>) -> bool {
        if let Some(new_value) = value {
            self.insertion_set.insert_value(self.index, new_value);
            self.proc
                .value_mut(self.value)
                .replace_with_identity(new_value);
            self.changed = true;
            true
        } else {
            false
        }
    }

    fn replace_with_identity(&mut self, new_value: ValueId) {
        self.changed = true;
        self.proc
            .value_mut(self.value)
            .replace_with_identity(new_value);
    }

    fn handle_shift_amount(&mut self) {
        // Shift anything by zero is identity.
        if let Some(0) = self.proc.value(self.value.child(self.proc, 1)).as_int32() {
            self.replace_with_identity(self.value.child(self.proc, 0));
            return;
        }

        // The shift already masks its shift amount. If the shift amount is being masked by a
        // redundant amount, then remove the mask. For example,
        // Turn this: Shl(@x, BitAnd(@y, 63))
        // Into this: Shl(@x, @y)
        let mask = size_of_type(self.proc.value(self.value).typ()) * 8 - 1;

        if self.value.child(self.proc, 1).opcode(self.proc) == Opcode::BitAnd
            && self
                .proc
                .value(self.value.child(self.proc, 1).child(self.proc, 1))
                .has_int32()
            && (self
                .proc
                .value(self.value.child(self.proc, 1).child(self.proc, 1))
                .as_int32()
                .unwrap() as u32
                & mask)
                == mask
        {
            self.proc.value_mut(self.value).children[1] =
                self.value.child(self.proc, 1).child(self.proc, 0);
            self.changed = true;
        }
    }

    fn replace_if_redundant(&mut self) {
        let dominators = self.dominators.clone();
        self.changed |= self.pure_cse.process(self.proc, self.value, &dominators);
    }

    fn handle_commutativity(&mut self) {
        if should_swap_binary_operands(self.proc, self.value) {
            self.proc.value_mut(self.value).children.swap(0, 1);
            self.changed = true;
        }
    }

    /// We have three easy simplification rules:
    ///
    /// 1) If a successor is a block that just jumps to another block, then jump directly to
    ///    that block.
    ///
    /// 2) If all successors are the same and the operation has no effects, then use a jump
    ///    instead.
    ///
    /// 3) If you jump to a block that is not you and has one predecessor, then merge.
    ///
    /// Note that because of the first rule, this phase may introduce critical edges. That's fine.
    /// If you need broken critical edges, then you have to break them yourself.
    ///
    /// Note that this relies on predecessors being at least conservatively correct. It's fine for
    /// predecessors to mention a block that isn't actually a predecessor. It's *not* fine for a
    /// predecessor to be omitted. We assert as much in the loop. In practice, we precisely preserve
    /// predecessors during strength reduction since that minimizes the total number of fixpoint
    /// iterations needed to kill a lot of code.

    fn simplify_cfg(&mut self) {
        for block in blocks_in_pre_order(BlockId(0), self.proc) {
            // We don't care about blocks that don't have successors.
            if self.proc.block(block).successor_list.len() == 0 {
                continue;
            }

            for i in 0..block.successor_list(self.proc).len() {
                let successor = block.successor_list(self.proc)[i];

                if successor.0 != block
                    && successor.0.size(self.proc) == 1
                    && self
                        .proc
                        .value(successor.0.value(self.proc, 0))
                        .kind
                        .opcode()
                        == Opcode::Jump
                {
                    let new_successor = successor.0.successor_list(self.proc)[0].0;
                    if new_successor != successor.0 {
                        if self.proc.options.dump_b3_reduce_strength {
                            println!(
                                "Replacing BB{} -> BB{} with BB{} -> BB{}",
                                block.0, successor.0 .0, block.0, new_successor.0
                            );
                        }

                        self.proc.block_mut(block).successor_list[i].0 = new_successor;
                        self.proc
                            .block_mut(new_successor)
                            .predecessor_list
                            .push(block);
                        self.changed_cfg = true;
                    }
                }
            }

            // Now check if the block's terminal can be replaced with a jump.

            if block.successor_list(self.proc).len() > 1 {
                // The terminal must not have weird effects.
                let mut effects = self
                    .proc
                    .value(self.proc.block(block).last().copied().unwrap())
                    .effects();
                effects.terminal = false;

                if !effects.must_execute() {
                    // All of the successors must be the same.
                    let mut all_same = true;
                    let first = block.successor_list(self.proc)[0].0;
                    for i in 1..block.successor_list(self.proc).len() {
                        if block.successor_list(self.proc)[i].0
                            != block.successor_list(self.proc)[0].0
                        {
                            all_same = false;
                            break;
                        }
                    }

                    if all_same {
                        if self.proc.options.dump_b3_reduce_strength {
                            println!("Changin BB{}'s terminal to a Jump", block.0);
                        }

                        let last = self
                            .proc
                            .value_mut(self.proc.block(block).last().copied().unwrap());
                        last.replace_with_jump(block, (first, Frequency::Normal));
                        self.changed_cfg = true;
                        self.proc.block_mut(block).successor_list =
                            vec![(first, Frequency::Normal)];
                    }
                }
            }

            // Finally handle jumps to a block with one predecessor.
            if block.successor_list(self.proc).len() == 1 {
                let successor = block.successor_list(self.proc)[0].0;

                if successor != block && successor.predecessor_list(self.proc).len() == 1 {
                    // We can merge the two blocks, because the predecessor only jumps to the successor
                    // and the successor is only reachable from the predecessor.

                    // Remove the terminal.
                    let _ = self.proc.block_mut(block).pop().unwrap();
                    let new_values = self.proc.block(successor).values.clone();
                    self.proc.block_mut(block).values.extend(new_values);
                    self.proc.block_mut(block).successor_list =
                        self.proc.block(successor).successor_list.clone();

                    self.proc.block_mut(successor).truncate(0);
                    let value = Value::new(
                        Opcode::Oops,
                        Type::Void,
                        crate::NumChildren::Zero,
                        &[],
                        ValueData::None,
                    );
                    let value = self.proc.add(value);
                    self.proc.block_mut(successor).values.push(value);
                    self.proc.block_mut(successor).successor_list = vec![];

                    for i in 0..block.successor_list(self.proc).len() {
                        let new_successor = block.successor_list(self.proc)[i].0;
                        self.proc
                            .block_mut(new_successor)
                            .replace_predecessor(successor, block);
                    }

                    if self.proc.options.dump_b3_reduce_strength {
                        println!("Merged BB{} -> BB{}", successor.0, block.0);
                    }

                    self.changed_cfg = true;
                }
            }
        }

        if self.proc.options.dump_b3_reduce_strength {
            println!("B3 after Simplify CFG:");
            print!("{}", self.proc.display_());
        }
    }

    /// This runs Aycock and Horspool's algorithm on our Phi functions [1]. For most CFG patterns,
    /// this can take a suboptimal arrangement of Phi functions and make it optimal, as if you had
    /// run Cytron, Ferrante, Rosen, Wegman, and Zadeck. It's only suboptimal for irreducible
    /// CFGs. In practice, that doesn't matter, since we expect clients of B3 to run their own SSA
    /// conversion before lowering to B3, and in the case of the DFG, that conversion uses Cytron
    /// et al. In that context, this algorithm is intended to simplify Phi functions that were
    /// made redundant by prior CFG simplification. But according to Aycock and Horspool's paper,
    /// this algorithm is good enough that a B3 client could just give us maximal Phi's (i.e. Phi
    /// for each variable at each basic block) and we will make them optimal.
    /// [1] http://pages.cpsc.ucalgary.ca/~aycock/papers/ssa.ps
    ///
    /// Aycock and Horspool prescribe two rules that are to be run to fixpoint:
    ///
    /// 1) If all of the Phi's children are the same (i.e. it's one child referenced from one or
    ///    more Upsilons), then replace all uses of the Phi with the one child.
    ///
    /// 2) If all of the Phi's children are either the Phi itself or exactly one other child, then
    ///    replace all uses of the Phi with the one other child.
    ///
    /// Rule (2) subsumes rule (1), so we can just run (2). We only run one fixpoint iteration
    /// here. This premise is that in common cases, this will only find optimization opportunities
    /// as a result of CFG simplification and usually CFG simplification will only do one round
    /// of block merging per ReduceStrength fixpoint iteration, so it's OK for this to only do one
    /// round of Phi merging - since Phis are the value analogue of blocks.
    fn simplify_ssa(&mut self) {
        let phi_children = PhiChildren::new(self.proc);

        for phi in phi_children.phis().iter().copied() {
            let mut other_child = None;

            let mut ok = true;

            for child in phi_children.at(phi).values().iter() {
                if child == phi {
                    continue;
                }

                if Some(child) == other_child {
                    continue;
                }

                if other_child.is_none() {
                    other_child = Some(child);
                    continue;
                }

                ok = false;
                break;
            }

            if !ok {
                continue;
            }

            if let Some(other_child) = other_child {
                self.changed = true;

                for upsilon in phi_children.at(phi).iter() {
                    self.proc.value_mut(upsilon).replace_with_nop();
                }

                self.proc.value_mut(phi).replace_with_identity(other_child);
            } else {
                // Wow, this would be super weird. It probably won't happen, except that things could
                // get weird as a consequence of stepwise simplifications in the strength reduction
                // fixpoint.
                continue;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct IntRange {
    min: i64,
    max: i64,
}

macro_rules! range_for_mask {
    ($mask: expr) => {{
        if $mask.wrapping_add(1) == 0 {
            top_of(&$mask)
        } else if $mask < 0 {
            IntRange {
                min: (i32::min_value() as i64) & $mask as i64,
                max: $mask as i64 & (i32::max_value() as i64),
            }
        } else {
            IntRange {
                min: 0,
                max: $mask as i64,
            }
        }
    }};
}

macro_rules! range_for_mask_num {
    ($t: ty, $mask: expr) => {{
        if $mask.wrapping_add(&T::one()) == T::zero() {
            top_of(&$mask)
        } else if $mask < T::zero() {
            let mask_i64 = $mask.to_i64().unwrap();

            IntRange {
                min: (i32::min_value() as i64) & mask_i64,
                max: mask_i64 as i64 & (i32::max_value() as i64),
            }
        } else {
            let mask_i64 = $mask.to_i64().unwrap();
            IntRange {
                min: 0,
                max: mask_i64 as i64,
            }
        }
    }};
}

fn top_of<T: num::PrimInt>(_: &T) -> IntRange {
    IntRange::top::<T>()
}

impl IntRange {
    pub fn top<T: num::PrimInt>() -> Self {
        Self {
            min: T::min_value().to_i64().unwrap(),
            max: T::max_value().to_i64().unwrap(),
        }
    }

    pub fn top_ty(ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => Self::top::<i32>(),
            TypeKind::Int64 => Self::top::<i64>(),
            _ => panic!("Unexpected type: {:?}", ty),
        }
    }

    fn rnage_for_zshr32(shift_amount: i32) -> Self {
        let mut mask = 0u32;
        mask = mask.wrapping_sub(1);
        mask = mask.wrapping_shr(shift_amount as u32);
        range_for_mask!(mask)
    }

    fn range_for_zshr64(shift_amount: i32) -> Self {
        let mut mask = 0u64;
        mask = mask.wrapping_sub(1);
        mask = mask.wrapping_shr(shift_amount as u32);
        range_for_mask!(mask)
    }

    fn range_for_zshrt<T: num::PrimInt + WrappingShr + WrappingSub + Integer + WrappingAdd>(
        shift_amount: i32,
    ) -> Self {
        let mut mask = T::zero();
        mask = mask.wrapping_sub(&T::one());
        mask = mask.wrapping_shr(shift_amount as u32);
        range_for_mask_num!(T, mask)
    }

    fn range_for_zshr(ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => Self::rnage_for_zshr32(32),
            TypeKind::Int64 => Self::range_for_zshr64(64),
            _ => panic!("Unexpected type: {:?}", ty),
        }
    }

    fn range_for_mask<T: num::PrimInt + WrappingShr + WrappingSub + Integer + WrappingAdd>(
        mask: T,
    ) -> Self {
        if mask.wrapping_add(&T::one()) == T::zero() {
            top_of(&mask)
        } else if mask < T::zero() {
            let mask_i64 = mask.to_i64().unwrap();

            IntRange {
                min: (i32::min_value() as i64) & mask_i64,
                max: mask_i64 as i64 & (i32::max_value() as i64),
            }
        } else {
            let mask_i64 = mask.to_i64().unwrap();
            IntRange {
                min: 0,
                max: mask_i64 as i64,
            }
        }
    }

    fn range_for_mask_ty(mask: i64, typ: Type) -> Self {
        match typ.kind() {
            TypeKind::Int32 => Self::range_for_mask::<i32>(mask as i32),
            TypeKind::Int64 => Self::range_for_mask::<i64>(mask),
            _ => panic!("Unexpected type: {:?}", typ),
        }
    }

    fn could_overflow_add<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> bool {
        match (
            T::try_from(self.min),
            T::try_from(self.max),
            T::try_from(other.min),
            T::try_from(other.max),
        ) {
            (Ok(amin), Ok(amax), Ok(bmin), Ok(bmax)) => {
                sum_overflows(amin, bmin)
                    || sum_overflows(amin, bmax)
                    || sum_overflows(amax, bmin)
                    || sum_overflows(amax, bmax)
            }

            _ => true,
        }
    }

    fn could_overflow_add_typ(&self, other: &Self, ty: Type) -> bool {
        match ty.kind() {
            TypeKind::Int32 => self.could_overflow_add::<i32>(other),
            TypeKind::Int64 => self.could_overflow_add::<i64>(other),
            _ => true,
        }
    }

    fn could_overflow_sub<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> bool {
        match (
            T::try_from(self.min),
            T::try_from(self.max),
            T::try_from(other.min),
            T::try_from(other.max),
        ) {
            (Ok(amin), Ok(amax), Ok(bmin), Ok(bmax)) => {
                difference_overflows(amin, bmin)
                    || difference_overflows(amin, bmax)
                    || difference_overflows(amax, bmin)
                    || difference_overflows(amax, bmax)
            }

            _ => true,
        }
    }

    fn could_overflow_sub_typ(&self, other: &Self, ty: Type) -> bool {
        match ty.kind() {
            TypeKind::Int32 => self.could_overflow_sub::<i32>(other),
            TypeKind::Int64 => self.could_overflow_sub::<i64>(other),
            _ => true,
        }
    }

    fn could_overflow_mul<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> bool {
        match (
            T::try_from(self.min),
            T::try_from(self.max),
            T::try_from(other.min),
            T::try_from(other.max),
        ) {
            (Ok(amin), Ok(amax), Ok(bmin), Ok(bmax)) => {
                product_overflows(amin, bmin)
                    || product_overflows(amin, bmax)
                    || product_overflows(amax, bmin)
                    || product_overflows(amax, bmax)
            }

            _ => true,
        }
    }

    fn could_overflow_mul_typ(&self, other: &Self, ty: Type) -> bool {
        match ty.kind() {
            TypeKind::Int32 => self.could_overflow_mul::<i32>(other),
            TypeKind::Int64 => self.could_overflow_mul::<i64>(other),
            _ => true,
        }
    }

    fn shl<T: num::PrimInt + TryFrom<i64> + WrappingShl + WrappingShr>(
        &self,
        shift_amount: i32,
    ) -> Self {
        match (T::try_from(self.min), T::try_from(self.max)) {
            (Ok(amin), Ok(amax)) => {
                let new_min = amin.wrapping_shl(shift_amount as u32);
                let new_max = amax.wrapping_shl(shift_amount as u32);

                if new_min.wrapping_shr(shift_amount as _) != amin
                    || new_max.wrapping_shr(shift_amount as _) != amax
                {
                    Self::top::<T>()
                } else {
                    Self {
                        min: new_min.to_i64().unwrap(),
                        max: new_max.to_i64().unwrap(),
                    }
                }
            }

            _ => Self::top::<T>(),
        }
    }

    fn shl_ty(&self, shift_amount: i32, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.shl::<i32>(shift_amount),
            TypeKind::Int64 => self.shl::<i64>(shift_amount),
            _ => Self::top::<i64>(),
        }
    }

    fn sshr<T: num::PrimInt + TryFrom<i64> + WrappingShl + WrappingShr>(
        &self,
        shift_amount: i32,
    ) -> Self {
        match (T::try_from(self.min), T::try_from(self.max)) {
            (Ok(amin), Ok(amax)) => {
                let new_min = amin.wrapping_shr(shift_amount as u32);
                let new_max = amax.wrapping_shr(shift_amount as u32);

                Self {
                    min: new_min.to_i64().unwrap(),
                    max: new_max.to_i64().unwrap(),
                }
            }

            _ => Self::top::<T>(),
        }
    }

    pub fn sshr_typ(&self, shift_amount: i32, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.sshr::<i32>(shift_amount),
            TypeKind::Int64 => self.sshr::<i64>(shift_amount),
            _ => Self::top::<i64>(),
        }
    }

    pub fn zshr<
        T: num::PrimInt
            + TryFrom<i64>
            + WrappingShl
            + WrappingShr
            + WrappingSub
            + WrappingAdd
            + Integer,
    >(
        &self,
        shift_amount: i32,
    ) -> Self {
        if shift_amount == 0 {
            return *self;
        }

        if self.min < 0 {
            return Self::range_for_zshrt::<T>(shift_amount);
        }

        match (T::try_from(self.min), T::try_from(self.max)) {
            (Ok(amin), Ok(amax)) => {
                let new_min = amin.wrapping_shr(shift_amount as u32);
                let new_max = amax.wrapping_shr(shift_amount as u32);

                Self {
                    min: new_min.to_i64().unwrap(),
                    max: new_max.to_i64().unwrap(),
                }
            }

            _ => Self::top::<T>(),
        }
    }

    fn zshr_typ(&self, shift_amount: i32, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.zshr::<i32>(shift_amount),
            TypeKind::Int64 => self.zshr::<i64>(shift_amount),
            _ => Self::top::<i64>(),
        }
    }

    fn add<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> Self {
        if self.could_overflow_add::<T>(other) {
            Self::top::<T>()
        } else {
            Self {
                min: self.min + other.min,
                max: self.max + other.max,
            }
        }
    }

    fn add_typ(&self, other: &Self, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.add::<i32>(other),
            TypeKind::Int64 => self.add::<i64>(other),
            _ => Self::top::<i64>(),
        }
    }

    fn sub<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> Self {
        if self.could_overflow_sub::<T>(other) {
            Self::top::<T>()
        } else {
            Self {
                min: self.min - other.max,
                max: self.max - other.min,
            }
        }
    }

    fn sub_typ(&self, other: &Self, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.sub::<i32>(other),
            TypeKind::Int64 => self.sub::<i64>(other),
            _ => Self::top::<i64>(),
        }
    }

    fn mul<T: num::PrimInt + TryFrom<i64>>(&self, other: &Self) -> Self {
        if self.could_overflow_mul::<T>(other) {
            Self::top::<T>()
        } else {
            let amin = T::try_from(self.min).unwrap_or_else(|_| unreachable!());
            let amax = T::try_from(self.max).unwrap_or_else(|_| unreachable!());
            let bmin = T::try_from(other.min).unwrap_or_else(|_| unreachable!());
            let bmax = T::try_from(other.max).unwrap_or_else(|_| unreachable!());

            let mut min = amin * bmin;
            let mut max = amin * bmin;

            min = min.min(amin * bmax);
            max = max.max(amin * bmax);

            min = min.min(amax * bmin);
            max = max.max(amax * bmin);

            min = min.min(amax * bmax);
            max = max.max(amax * bmax);

            Self {
                min: min.to_i64().unwrap(),
                max: max.to_i64().unwrap(),
            }
        }
    }

    fn mul_typ(&self, other: &Self, ty: Type) -> Self {
        match ty.kind() {
            TypeKind::Int32 => self.mul::<i32>(other),
            TypeKind::Int64 => self.mul::<i64>(other),
            _ => Self::top::<i64>(),
        }
    }

    fn sext<T: num::PrimInt + TryFrom<i64>>(&self) -> Self {
        let type_min: i64 = T::min_value().to_i64().unwrap();
        let type_max: i64 = T::max_value().to_i64().unwrap();

        let min = self.min;
        let max = self.max;

        if type_min <= min && min <= type_max && type_min <= max && max <= type_max {
            return Self { min, max };
        }

        // Given type T with N bits, signed extension will turn bit N-1 as
        // a sign bit. If bits N-1 upwards are identical for both min and max,
        // then we're guaranteed that even after the sign extension, min and
        // max will still be in increasing order.
        //
        // For example, when T is int8_t, the space of numbers from highest to
        // lowest are as follows (in binary bits):
        //
        //      highest     0 111 1111  ^
        //                    ...       |
        //            1     0 000 0001  |   top segment
        //            0     0 000 0000  v
        //
        //           -1     1 111 1111  ^
        //           -2     1 111 1110  |   bottom segment
        //                    ...       |
        //       lowest     1 000 0000  v
        //
        // Note that if we exclude the sign bit, the range is made up of 2 segments
        // of contiguous increasing numbers. If min and max are both in the same
        // segment before the sign extension, then min and max will continue to be
        // in a contiguous segment after the sign extension. Only when min and max
        // spans across more than 1 of these segments, will min and max no longer
        // be guaranteed to be in a contiguous range after the sign extension.
        //
        // Hence, we can check if bits N-1 and up are identical for the range min
        // and max. If so, then the new min and max can be be computed by simply
        // applying sign extension to their original values.

        let number_of_bits = T::zero().leading_zeros() as usize;
        let segment_mask = (1i64 << (number_of_bits - 1)) - 1;
        let top_bits_mask = !segment_mask;

        let min_top_bits = top_bits_mask & min;
        let max_top_bits = top_bits_mask & max;

        if min_top_bits == max_top_bits {
            return Self {
                min: min_top_bits | (min & segment_mask),
                max: max_top_bits | (max & segment_mask),
            };
        }

        Self::top::<T>()
    }

    fn zext32(&self) -> Self {
        Self {
            min: self.min as u32 as u64 as i64,
            max: self.max as u32 as u64 as i64,
        }
    }
}

fn sum_overflows<T: num::PrimInt + TryFrom<i64>>(a: T, b: T) -> bool {
    a.checked_add(&b).is_none()
}

fn difference_overflows<T: num::PrimInt + TryFrom<i64>>(a: T, b: T) -> bool {
    a.checked_sub(&b).is_none()
}

fn product_overflows<T: num::PrimInt + TryFrom<i64>>(a: T, b: T) -> bool {
    a.checked_mul(&b).is_none()
}

struct CanonicalizeComparison {
    opcode: Opcode,
    operands: [ValueId; 2],
}

fn canonicalize_comparison(proc: &Procedure, value: ValueId) -> CanonicalizeComparison {
    let flip = |opcode| match opcode {
        Opcode::LessEqual => Opcode::GreaterEqual,
        Opcode::LessThan => Opcode::GreaterThan,
        Opcode::GreaterThan => Opcode::LessThan,
        Opcode::GreaterEqual => Opcode::LessEqual,
        Opcode::Above => Opcode::BelowEqual,
        Opcode::AboveEqual => Opcode::Below,
        Opcode::Below => Opcode::AboveEqual,
        Opcode::BelowEqual => Opcode::Above,
        opcode => opcode,
    };

    if should_swap_binary_operands(proc, value) {
        return CanonicalizeComparison {
            opcode: flip(proc.value(value).kind.opcode()),
            operands: [value.child(proc, 1), value.child(proc, 0)],
        };
    }

    CanonicalizeComparison {
        opcode: proc.value(value).kind.opcode(),
        operands: [value.child(proc, 0), value.child(proc, 1)],
    }
}

fn should_swap_binary_operands(proc: &Procedure, value: ValueId) -> bool {
    // Note that we have commutative operations that take more than two children. Those operations may
    // commute their first two children while leaving the rest unaffected.

    assert!(proc.value(value).children.len() >= 2);

    // Leave it alone if the right child is a constant.
    if proc.value(value.child(proc, 1)).is_constant() {
        return false;
    }

    if proc.value(value.child(proc, 0)).is_constant() {
        return true;
    }

    // Sort the operands. This is an important canonicalization. We use the index instead of
    // the address to make this at least slightly deterministic.
    if value.child(proc, 0) > value.child(proc, 1) {
        return true;
    }

    false
}

fn has_one_bit_set(value: i64) -> bool {
    ((value - 1) & value) == 0 && value != 0
}

fn has_zero_or_one_bit_set(value: i64) -> bool {
    (value & (value - 1)) == 0
}
