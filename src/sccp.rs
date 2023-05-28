//! # Sparse Conditional Constant Propagation
//!
//! Described in
//! Mark N. Wegman, F. Kenneth Zadeck: Constant Propagation with Conditional Branches.
//! TOPLAS 1991.
//!
//! This algorithm uses three level lattice for SSA value
//! ```
//!      Top        undefined
//!     / | \
//! .. 1  2  3 ..   constant
//!     \ | /
//!     Bottom      not constant
//! ```
//! It starts with optimistically assuming that all SSA values are initially Top
//! and then propagates constant facts only along reachable control flow paths.
//! Since some basic blocks are not visited yet, corresponding inputs of phi become
//! Top, we use the [`meet(phi)`](Worklist::meet) to compute its lattice.
//! ```
//! 	  Top ∩ any = any
//! 	  Bottom ∩ any = Bottom
//! 	  ConstantA ∩ ConstantA = ConstantA
//! 	  ConstantA ∩ ConstantB = Bottom
//! ```
//! Each lattice value is lowered most twice(Top to Constant, Constant to Bottom)
//! due to lattice depth, resulting in a fast convergence speed of the algorithm.
//! In this way, sccp can discover optimization opportunities that cannot be found
//! by just combining constant folding and constant propagation and dead code
//! elimination separately.



use std::collections::{HashMap, VecDeque};

use crate::{
    analysis::phi_children::PhiChildren,
    utils::{bitvector::BitVector, index_set::IndexMap},
    BlockId,  Opcode, Procedure, TriState,Value,  ValueId, Frequency,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LatticeLevel {
    Top,
    Constant,
    Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Lattice {
    level: LatticeLevel,
    val: Option<ValueId>,
}

impl Lattice {
    fn equals(&self, other: &Self, proc: &Procedure) -> bool {
        if self.level != other.level {
            return false;
        }

        if self.level == LatticeLevel::Constant {
            let v1 = self.val.unwrap();
            let v2 = other.val.unwrap();

            if proc.value(v1).kind.opcode() == proc.value(v2).kind.opcode()
                && proc.value(v1).as_int().unwrap() == proc.value(v2).as_int().unwrap()
            {
                return true;
            } else {
                return false;
            }
        }

        true
    }
}

/// SCCP worklist
pub struct Worklist<'a> {
    proc: &'a mut Procedure,
    phi_children: PhiChildren,
    edges: VecDeque<BlockId>,
    uses: VecDeque<ValueId>,
    visited: BitVector,
    lattice: IndexMap<Lattice, ValueId>,
    /// def-use chains for some values
    def_use: HashMap<ValueId, Vec<ValueId>>,
    /// Use blocks of def
    def_block: HashMap<ValueId, Vec<BlockId>>,
    visited_block: BitVector,
}

pub fn sccp<'a>(proc: &'a mut Procedure) {
    let visited = BitVector::with_capacity(proc.blocks.len());
    let visited_block = BitVector::with_capacity(proc.blocks.len());
    let phi_children = PhiChildren::new(proc);
    let mut t = Worklist {
        proc,
        phi_children,
        edges: VecDeque::new(),
        uses: VecDeque::new(),
        visited,
        lattice: IndexMap::new(),
        def_use: HashMap::new(),
        def_block: HashMap::new(),
        visited_block,
    };
    t.edges.push_back(BlockId(0));

    for i in 0..t.proc.values.size() {
        t.def_use.insert(ValueId(i), vec![]);
        t.def_block.insert(ValueId(i), vec![]);
    }
    t.build_def_uses();

    // pick up either an edge or SSA value from worklilst, process it

    loop {
        if let Some(edge) = t.edges.pop_front() {
            if true || !t.visited.get(edge.0) {
                let dest_visited = t.visited_block.get(edge.0);
                t.visited.set(edge.0, true);
                t.visited_block.set(edge.0, true);

                for i in 0..t.proc.block(edge).len() {
                    let value = t.proc.block(edge)[i];

                    if value.opcode(t.proc) == Opcode::Phi || !dest_visited {
                        t.visit_value(value);
                    }
                }

                if !dest_visited {
                    t.propagate(edge);
                }
            }

            continue;
        }

        if let Some(use_) = t.uses.pop_front() {
            t.visit_value(use_);
            continue;
        }

        break;
    }

    let (replace_cnt, rewire_cnt) = t.replace_consts();

    if proc.options.dump_b3_at_each_phase {
        eprintln!("SCCP: replaced {} constants, rewired {} blocks", replace_cnt, rewire_cnt);
    }
}

impl<'a> Worklist<'a> {
    fn compute_lattice(&mut self, val: ValueId, args: &[ValueId]) -> Lattice {
        use Opcode::*;

        let wrap_tri = |val: TriState| match val {
            TriState::True => Some(Value::make_const32(1)),
            TriState::False => Some(Value::make_const32(0)),
            _ => None,
        };

        let result = match val.opcode(self.proc) {
            Neg => self.proc.value(args[0]).neg_constant(),
            Floor => self.proc.value(args[0]).floor_constant(),

            Ceil => self.proc.value(args[0]).ceil_constant(),

            Trunc => self.proc.value(args[0]).trunc_constant(),

            ZExt32 => self.proc.value(args[0]).zext32_constant(),

            SExt16 => self.proc.value(args[0]).sext16_constant(),

            SExt16To64 => self.proc.value(args[0]).sext16to64_constant(),

            SExt32 => self.proc.value(args[0]).sext32_constant(),

            SExt8 => self.proc.value(args[0]).sext8_constant(),

            SExt8To64 => self.proc.value(args[0]).sext8to64_constant(),

            DoubleToFloat => Some(Value::make_const_float(
                self.proc.value(args[0]).as_double().unwrap() as f32,
            )),

            FloatToDouble => Some(Value::make_const_double(
                self.proc.value(args[0]).as_float().unwrap() as f64,
            )),

            IToD => Some(Value::make_const_double(
                self.proc.value(args[0]).as_int().unwrap() as f64,
            )),

            DToI => Some(Value::make_const64(
                self.proc.value(args[0]).as_double().unwrap() as i64,
            )),

            IToF => Some(Value::make_const_float(
                self.proc.value(args[0]).as_int().unwrap() as f32,
            )),

            FToI => Some(Value::make_const32(
                self.proc.value(args[0]).as_float().unwrap() as i32,
            )),

            Add => self
                .proc
                .value(args[0])
                .add_constant(self.proc.value(args[1])),

            Sub => self
                .proc
                .value(args[0])
                .sub_constant(self.proc.value(args[1])),

            Mul => self
                .proc
                .value(args[0])
                .mul_constant(self.proc.value(args[1])),

            Div => self
                .proc
                .value(args[0])
                .div_constant(self.proc.value(args[1])),

            Mod => self
                .proc
                .value(args[0])
                .mod_constant(self.proc.value(args[1])),

            Shl => self
                .proc
                .value(args[0])
                .shl_constant(self.proc.value(args[1])),

            SShr => self
                .proc
                .value(args[0])
                .sshr_constant(self.proc.value(args[1])),

            ZShr => self
                .proc
                .value(args[0])
                .zshr_constant(self.proc.value(args[1])),

            BitAnd => self
                .proc
                .value(args[0])
                .bit_and_constant(self.proc.value(args[1])),

            BitOr => self
                .proc
                .value(args[0])
                .bit_or_constant(self.proc.value(args[1])),

            BitXor => self
                .proc
                .value(args[0])
                .bit_xor_constant(self.proc.value(args[1])),

            BitwiseCast => self.proc.value(args[0]).bitwise_cast_constant(),
            Above => wrap_tri(self
                .proc
                .value(args[0])
                .above_constant(self.proc.value(args[1]))),
            AboveEqual => wrap_tri(self
                .proc
                .value(args[0])
                .above_equal_constant(self.proc.value(args[1]))),
            Below => wrap_tri(self
                .proc
                .value(args[0])
                .below_constant(self.proc.value(args[1]))),
            BelowEqual => wrap_tri(self
                .proc
                .value(args[0])
                .below_equal_constant(self.proc.value(args[1]))),
            GreaterThan => wrap_tri(self
                .proc
                .value(args[0])
                .greater_than_constant(self.proc.value(args[1]))),
            GreaterEqual => wrap_tri(self
                .proc
                .value(args[0])
                .greater_than_equal_constant(self.proc.value(args[1]))),
            LessThan => wrap_tri(self
                .proc
                .value(args[0])
                .less_than_constant(self.proc.value(args[1]))),
            LessEqual => wrap_tri(self
                .proc
                .value(args[0])
                .less_than_equal_constant(self.proc.value(args[1]))),
            _ => {
                return Lattice {
                    level: LatticeLevel::Bottom,
                    val: None,
                }
            }
        };

        if let Some(res) = result {
            if res.is_constant() {
                let res = self.proc.add(res);
                return Lattice {
                    level: LatticeLevel::Constant,
                    val: Some(res),
                };
            }
        }

        Lattice {
            level: LatticeLevel::Bottom,
            val: None,
        }
    }

    /// Meets all of phi arguments and computes result lattice
    fn meet(&mut self, val: ValueId) -> Lattice {
       
        let mut optimistic_lt = Lattice {
            level: LatticeLevel::Top,
            val: None,
        };

        let children = self.phi_children.at(val).iter().collect::<Vec<_>>();

        for upsilon in children {
            let arg = self.proc.value(upsilon).children[0];
            let edge = self
                .proc
                .value(arg)
                .owner
                .expect("upsilon must have owner");
            // If incoming edge for phi is not visited, assume top optimistically.
            // According to rules of meet:
            // 		Top ∩ any = any
            // Top participates in meet() but does not affect the result, so here
            // we will ignore Top and only take other lattices into consideration.

            if self.visited.get(edge.0) {
                
                let lt = self.get_lattice_cell(arg);

                if lt.level == LatticeLevel::Constant {
                    if optimistic_lt.level == LatticeLevel::Top {
                        optimistic_lt = lt;
                    } else {
                        if !optimistic_lt.equals(&lt, &self.proc) {
                            // ConstantA ∩ ConstantB = Bottom
                            return Lattice {
                                level: LatticeLevel::Bottom,
                                val: None,
                            };
                        }
                    }
                } else if lt.level == LatticeLevel::Bottom {
                    // Bottom ∩ any = Bottom
                    return Lattice {
                        level: LatticeLevel::Bottom,
                        val: None,
                    };
                } else {
                    // Top ∩ any = any
                    // Revisit this phi node later
                    //self.uses.push_back(val);
                }
            } else {
                // Top ∩ any = any
                // Revisit this phi node later
                //self.uses.push_back(val);
            }
        }

        // ConstantA ∩ ConstantA = ConstantA or Top ∩ any = any
        optimistic_lt
    }

    fn visit_value(&mut self, val: ValueId) {
        if !possible_const(self.proc, val) {
            // fast fail for always worst Values, i.e. there is no lowering happen
            // on them, their lattices must be initially worse Bottom.
            return;
        }

        let old_lt = self.get_lattice_cell(val);

        let cls = |this: &mut Self| {
            let new_lt = this.get_lattice_cell(val);
            if !new_lt.equals(&old_lt, this.proc) {
                this.add_uses(val);
            }
        };
        use Opcode::*;
        match val.opcode(self.proc) {
            Opcode::Const32 | Opcode::Const64 | Opcode::ConstFloat | Opcode::ConstDouble => {
                self.lattice.insert(
                    val,
                    Lattice {
                        level: LatticeLevel::Constant,
                        val: Some(val),
                    },
                );
            }

            Opcode::Identity => {
                let arg = self.proc.value(val).children[0];
                let arg_lt = self.get_lattice_cell(arg);

                self.lattice.insert(val, arg_lt);
            }

            Opcode::Phi => {
                let lattice = self.meet(val);
                self.lattice.insert(val, lattice);
            }

            Neg | Floor | Ceil | Trunc | ZExt32 | SExt32 | SExt16 | SExt8 | SExt16To64
            | SExt8To64 | DoubleToFloat | FloatToDouble | IToD | DToI | IToF | FToI
            | BitwiseCast => {
                let lt = self.get_lattice_cell(self.proc.value(val).children[0]);

                if lt.level == LatticeLevel::Constant {
                    let nlt = self.compute_lattice(val, &[self.proc.value(val).children[0]]);

                    self.lattice.insert(val, nlt);
                } else {
                    self.lattice.insert(
                        val,
                        Lattice {
                            level: lt.level,
                            val: None,
                        },
                    );
                }
            }

            Add | Sub | Mul | Div | Mod | Shl | SShr | ZShr | BitAnd | BitOr | BitXor | Above
            | AboveEqual | Below | BelowEqual | LessEqual | LessThan | GreaterThan
            | GreaterEqual => {
                let lt1 = self.get_lattice_cell(self.proc.value(val).children[0]);
                let lt2 = self.get_lattice_cell(self.proc.value(val).children[1]);

                if lt1.level == LatticeLevel::Constant && lt2.level == LatticeLevel::Constant {
                    let nlt = self.compute_lattice(
                        val,
                        &[
                            self.proc.value(val).children[0],
                            self.proc.value(val).children[1],
                        ],
                    );

                    self.lattice.insert(val, nlt);
                } else {
                    if lt1.level == LatticeLevel::Bottom || lt2.level == LatticeLevel::Bottom {
                        self.lattice.insert(
                            val,
                            Lattice {
                                level: LatticeLevel::Bottom,
                                val: None,
                            },
                        );
                    } else {
                        self.lattice.insert(
                            val,
                            Lattice {
                                level: LatticeLevel::Top,
                                val: None,
                            },
                        );
                    }
                }
            }

            _ => (),
        }

        cls(self);
    }
    
    /// build_def_uses builds def-use chain for some values early, because once the
    /// lattice of a value is changed, we need to update lattices of use. But we don't
    /// need all uses of it, only uses that can become constants would be added into
    /// re-visit worklist since no matter how many times they are revisited, uses which
    /// can't become constants lattice remains unchanged, i.e. Bottom.
    fn build_def_uses(&mut self) {
        for i in 0..self.proc.blocks.len() {
            let block = BlockId(i);

            for i in 0..self.proc.block(block).len() - 1 {
                let val = self.proc.block(block)[i];

                for i in 0..self.proc.value(val).children.len() {
                    let arg = self.proc.value(val).children[i];

                    if possible_const(self.proc, arg) && possible_const(self.proc, val) {
                        self.def_use.entry(arg).or_insert_with(Vec::new).push(val);
                    }
                }

                if val.opcode(&self.proc) == Opcode::Phi {
                    let upsilons = self.phi_children.at(val).iter().collect::<Vec<_>>();

                    for upsilon in upsilons {
                        self.def_use.entry(upsilon).or_insert_with(Vec::new).push(val);
                    }
                }
            }

            let terminator = *self.proc.block(block).last().unwrap();
            let control_value = match terminator.opcode(self.proc) {
                Opcode::Branch | Opcode::Switch => Some(self.proc.value(terminator).children[0]),
                _ => None 
            };
            if let Some(control_value) = control_value.filter(|&control_value| possible_const(self.proc, control_value)) {
                self.def_block
                    .entry(control_value)
                    .or_insert_with(Vec::new)
                    .push(block);
            }
        }
    }
    /// add_uses finds all uses of value and appends them into work list for further process
    fn add_uses(&mut self, val: ValueId) {
        let mut uses = std::mem::replace(&mut self.uses, VecDeque::new());

        for &use_ in self.def_use[&val].iter() {
            if val == use_ {
                // phi may refer to itself as uses, ignore them to avoid
                // re-visiting phi-nodes
                continue;
            }
            uses.push_back(use_);
        }

        if val.opcode(self.proc) == Opcode::Phi {
            let upsilons = self.phi_children.at(val);

            for upsilon in upsilons.iter() {
                if val == upsilon {
                    // phi may refer to itself as uses, ignore them to avoid
                    // re-visiting phi-nodes
                    continue;
                }
                uses.push_back(upsilon);
            }
        }

        self.uses = uses;
    }
    /// Propagates constants facts through CFG. If the block has single successor,
    /// add the successor anyway. If the block has multiple successors, only add the
    /// branch destination corresponding to lattice value of condition value.
    fn propagate(&mut self, block: BlockId) {
        let control_value = *self.proc.block(block).last().unwrap();

        match self.proc.value(control_value).kind.opcode() {
            Opcode::Jump => {
                self.edges
                    .push_back(self.proc.block(block).successor_list[0].0);
            }

            Opcode::Branch | Opcode::Switch => {
                let cond = self.proc.value(control_value).children[0];
                let cond_lattice = self.get_lattice_cell(cond);

                if cond_lattice.level == LatticeLevel::Bottom {
              
                    for i in 0..self.proc.block(block).successor_list.len() {
                        self.edges
                            .push_back(self.proc.block(block).successor_list[i].0);
                    }
                } else if cond_lattice.level == LatticeLevel::Constant {
                    let val = cond_lattice.val.unwrap();
                    if self.proc.value(control_value).kind.opcode() == Opcode::Branch {
                        let constant = self.proc.value(val).as_int().unwrap();
                        if constant != 0 {
                            self.edges
                                .push_back(self.proc.block(block).successor_list[0].0);
                        } else {
                            self.edges
                                .push_back(self.proc.block(block).successor_list[1].0);
                        }
                    } else {
                        let branch_idx = self
                            .proc
                            .value(val)
                            .as_int()
                            .unwrap()
                            .abs()
                            .rem_euclid(self.proc.block(block).successor_list.len() as i64);

                        self.edges.push_back(
                            self.proc.block(block).successor_list[branch_idx as usize].0,
                        );
                    }
                }
            }

            Opcode::Return => {}

            _ => unreachable!(),
        }
    }

    fn get_lattice_cell(&mut self, val: ValueId) -> Lattice {
        if !possible_const(self.proc, val) {
            return Lattice {
                level: LatticeLevel::Bottom,
                val: None,
            };
        }

        self.lattice.get(&val).copied().unwrap_or(Lattice {
            level: LatticeLevel::Top,
            val: None,
        })
    }

    fn replace_consts(&mut self) -> (usize, usize) {
        let mut const_cnt = 0;
        let mut rewire_cnt = 0;

        let cells = std::mem::take(&mut self.lattice);

        for (val, lt) in cells.iter() {
            let val = ValueId(val);

            if lt.level == LatticeLevel::Constant {
                if !is_const(self.proc, val) {
                    let replacement = self.proc.value(lt.val.unwrap()).clone();
                    let val = self.proc.value_mut(val);

                    val.kind = replacement.kind;
                    val.data = replacement.data;
                    val.children = replacement.children;
                    val.num_children = replacement.num_children;
                    const_cnt += 1;
                }

                
                
                if let Some(ctrl_block) = self.def_block.remove(&val) {
                    for block in ctrl_block {
                        if self.rewire_successor(block, val) {
                            rewire_cnt += 1;
                        }
                    }
                }   
            
            }
        }

        (const_cnt, rewire_cnt)
    }

    fn rewire_successor(&mut self, block: BlockId, val: ValueId) -> bool {
        let terminator = self.proc.block(block).last().copied().unwrap();

        match terminator.opcode(self.proc) {
            Opcode::Branch => {
                let value = self.proc.value(val).as_int().unwrap();
                let succ = if value != 0 {
                    self.proc.block(block).successor_list[0].0
                } else {
                    self.proc.block(block).successor_list[1].0
                };

                self.proc.value_mut(terminator).replace_with_jump(block, (succ, Frequency::Normal));
                self.proc.block_mut(block).successor_list.clear();
                self.proc.block_mut(block).successor_list.push((succ, Frequency::Normal));
                true 
            }

            Opcode::Switch => {
                let value = self.proc.value(val).as_int().unwrap();
                if value < 0 {
                    return false;
                }
                if value >= self.proc.block(block).successor_list.len() as i64 {
                    return false;
                }
                let succ = self.proc.block(block).successor_list[value as usize].0;

               
                self.proc.value_mut(terminator).replace_with_jump(block, (succ, Frequency::Normal));
                self.proc.block_mut(block).successor_list.clear();
                self.proc.block_mut(block).successor_list.push((succ, Frequency::Normal));
                true
            }

            _ => false
        }
    }
}

fn is_const(proc: &Procedure, val: ValueId) -> bool {
    proc.value(val).is_constant()
}

fn possible_const(proc: &Procedure, val: ValueId) -> bool {
    if proc.value(val).is_constant() {
        return true;
    }
    use crate::Opcode::*;
    match proc.value(val).kind.opcode() {
        Identity => true,
        Phi => true,
        Upsilon => true,

        Abs | Neg | Floor | Ceil | Trunc | Sqrt | ZExt32 | SExt32 | SExt8 | SExt16
        | SExt16To64 | SExt8To64 | BitwiseCast | DoubleToFloat | FloatToDouble | IToD | IToF
        | DToI | FToI => true,

        Jump | Branch | Switch => true,

        Add | Sub | Mul | Div | Mod | Equal | EqualOrUnordered | NotEqual | Above | AboveEqual
        | Below | BelowEqual | GreaterThan | GreaterEqual | LessThan | LessEqual | Shl | SShr
        | ZShr | BitAnd | BitOr | BitXor => true,
        _ => false,
    }
}
