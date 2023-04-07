use std::cmp::Ordering;

use tinyvec::TinyVec;

use super::{basic_block::*, code::*};

use crate::{block::Frequency, dominators::GraphNodeWorklist, sparse_collection::SparseElement, air::opcode::Opcode};

/// Compute the Block post-order for a Code. In the returned Vec a BasicBlockId will
/// appear after all of its successors.
pub fn compute_po(proc: &Code) -> Vec<BasicBlockId> {
    blocks_in_post_order(BasicBlockId(0), proc)
    //compute_general_po(proc, |succs| succs.split_last())
}

/// Compute the Block reverse-post-order for a Code. In the retuned Vec a
/// BasicBlockId will appear before any of its successors.
pub fn compute_rpo(proc: &Code) -> Vec<BasicBlockId> {
    let mut po = compute_po(proc);
    po.reverse();
    po
}

/// Sort Code's blocks using reverse postorder DFS, renumbering successor edges as needed.
///
/// RPO has the nice property that an Value's operands will always be "earlier" in the ordering.
///
/// Note that blocks unreachable from the entry block will be discarded.
pub fn rpo_sort(proc: &mut Code) {
    let rpo = compute_rpo(proc);

    if block_order_matches(proc, &rpo) {
        return;
    }

    // Create the mapping from old BasicBlockId to new BasicBlockId.
    // Dead blocks will have a usize::MAX entry.
    let remap = order_to_remap(&rpo, proc.blocks.len());

    // Update successor edges for surviving blocks to use the new BlockIds.
    // (Note that this double loop will make it so that the closer to RPO we
    // start the less we have to do.)
    for (old, &new) in remap.iter().enumerate() {
        if new.0 != usize::MAX {
            let bid = BasicBlockId(old);

            for succ in &mut proc.block_mut(bid).successors {
                let target = remap[succ.0 .0];
                debug_assert_ne!(target.0, usize::MAX);
                succ.0 = target;
            }

            for pred in &mut proc.block_mut(bid).predecessors {
                let target = remap[pred.0];
                debug_assert_ne!(target.0, usize::MAX);
                *pred = target;
            }
        }
    }

    permute_and_truncate(&mut proc.blocks, remap);

    for (i, block) in proc.blocks.iter_mut().enumerate() {
        block.index = i;
    }
    assert_eq!(proc.blocks.len(), rpo.len());
}

fn order_to_remap(order: &[BasicBlockId], len: usize) -> Vec<BasicBlockId> {
    let mut remap = vec![BasicBlockId::from(usize::MAX); len];

    for (i, &bid) in order.iter().enumerate() {
        // check for duplicates in `order`
        let bid_as_usize: usize = bid.into();
        debug_assert_eq!(remap[bid_as_usize], BasicBlockId::from(usize::MAX));
        remap[bid_as_usize] = BasicBlockId::from(i);
    }

    remap
}

/// Replace 'value' with a Vec where each entry is moved to the slot specified in remap.
///
/// Entries that map to usize::MAX are discarded, in which case the Vec will shrink.
///
/// Entries other than usize::MAX must be unique, so if there are no usize::MAX then this
/// computes a permutation.
pub fn permute_and_truncate<T: SparseElement>(values: &mut Vec<T>, mut remap: Vec<T::Id>)
where
    T::Id: PartialOrd + Ord,
{
    let mut slot = T::Id::from(0usize);

    while slot < T::Id::from(values.len()) {
        let mut desired_slot = remap[slot.into()];

        while slot != desired_slot {
            if desired_slot.into() != usize::MAX {
                values.swap(slot.into(), desired_slot.into());
                std::mem::swap(&mut remap[desired_slot.into()], &mut desired_slot);
            } else {
                values.swap_remove(slot.into());

                if slot.into() == values.len() {
                    return;
                }

                desired_slot = remap[values.len()];
            }
        }

        slot = T::Id::from(slot.into() + 1);
    }
}

/// Are the Blocks in procedure already in the order specified by rpo?
fn block_order_matches(proc: &Code, order: &[BasicBlockId]) -> bool {
    order.len() == proc.blocks.len()
        && order.iter().enumerate().all(|(i, &b)| {
            let bid_as_usize: usize = b.into();
            i == bid_as_usize
        })
}

struct SortedSuccessors {
    successors: TinyVec<[BasicBlockId; 2]>,
}

impl SortedSuccessors {
    pub fn append(&mut self, succ: BasicBlockId) {
        self.successors.push(succ);
    }

    pub fn process(&mut self, code: &Code, worklist: &mut GraphNodeWorklist<BasicBlockId>) {
        // We prefer a stable sort, and we don't want it to go off the rails if we see NaN. Also, the number
        // of successors is bounded. In fact, it currently cannot be more than 2. :-)
        self.successors.sort_by(|a, b| {
            let a = code.block(*a);
            let b = code.block(*b);
            a.frequency
                .partial_cmp(&b.frequency)
                .unwrap_or(Ordering::Equal)
        });

        // Pushing the successors in ascending order of frequency ensures that the very next block we visit
        // is our highest-frequency successor (unless that successor has already been visited).
        
        for i in 0..self.successors.len() {
            worklist.push(self.successors[i]);
        }

        self.successors.clear();
    }
}

/// Returns a list of blocks sorted according to what would be the current optimal order. This shares
/// some properties with a pre-order traversal. In particular, each block will appear after at least
/// one of its predecessors.
pub fn blocks_in_optimized_order(code: &Code) -> Vec<BasicBlockId> {
    let mut blocks_in_order = Vec::new();

    let mut fast_worklist = GraphNodeWorklist::new();
    let mut sorted_successors = SortedSuccessors {
        successors: TinyVec::new(),
    };
    let mut sorted_slow_successors = SortedSuccessors {
        successors: TinyVec::new(),
    };

    let append_successor = |normal: &mut SortedSuccessors,
                            slow: &mut SortedSuccessors,
                            block: (BasicBlockId, Frequency)| {
        if block.1 == Frequency::Rare {
            slow.append(block.0);
        } else {
            normal.append(block.0);
        }
    };

    for i in 1..code.entrypoints.len() {
        let block = code.entrypoints[i];
        append_successor(&mut sorted_successors, &mut sorted_slow_successors, block);
    }

    fast_worklist.push(BasicBlockId(0));

    while let Some(block) = fast_worklist.pop() {
        blocks_in_order.push(block);
        for succ in code.block(block).successors.iter() {
            append_successor(&mut sorted_successors, &mut sorted_slow_successors, *succ);
        }

        sorted_successors.process(code, &mut fast_worklist);
    }

    let mut slow_worklist = GraphNodeWorklist::new();

    sorted_slow_successors.process(code, &mut slow_worklist);

    while let Some(block) = slow_worklist.pop() {
        
        if fast_worklist.saw(block) {
            continue;
        }

        blocks_in_order.push(block);

        for succ in code.block(block).successors.iter() {
            sorted_successors.append(succ.0);
        }

        sorted_successors.process(code, &mut fast_worklist);
    }

    debug_assert!(fast_worklist.is_empty());
    debug_assert!(slow_worklist.is_empty());

    blocks_in_order
}

/// Reorders the basic blocks to keep hot blocks at the top, and maximize the likelihood that a frequently
/// taken edge is just a fall-through.
pub fn optimize_block_order(code: &mut Code) {
    let blocks_in_order = blocks_in_optimized_order(code);
    
    let remap = order_to_remap(&blocks_in_order, code.blocks.len());

    // Update successor edges for surviving blocks to use the new BlockIds.
    // (Note that this double loop will make it so that the closer to RPO we
    // start the less we have to do.)
    for (old, &new) in remap.iter().enumerate() {
        if new.0 != usize::MAX {
            let bid = BasicBlockId(old);

            for succ in &mut code.block_mut(bid).successors {
                let target = remap[succ.0 .0];
                debug_assert_ne!(target.0, usize::MAX);
                succ.0 = target;
            }

            for pred in &mut code.block_mut(bid).predecessors {
                let target = remap[pred.0];
                debug_assert_ne!(target.0, usize::MAX);
                *pred = target;
            }
        }
    }

    permute_and_truncate(&mut code.blocks, remap);

    for (i, block) in code.blocks.iter_mut().enumerate() {
        block.index = i;
    }

   
    assert_eq!(code.blocks.len(), blocks_in_order.len());
    code.reset_reachability();
    // Finally, flip any branches that we recognize. It's most optimal if the taken successor does not point
    // at the next block.

    for block in (0..code.blocks.len()).map(BasicBlockId) {
        // It's somewhat tempting to just say that if the block has two successors and the first arg is
        // invertible, then we can do the optimization. But that's wagging the dog. The fact that an
        // instruction happens to have an argument that is invertible doesn't mean it's a branch, even though
        // it is true that currently only branches have invertible arguments. It's also tempting to say that
        // the /branch flag in AirOpcode.opcodes tells us that something is a branch - except that there,
        // /branch also means Jump. The approach taken here means that if you add new branch instructions and
        // forget about this phase, then at worst your new instructions won't opt into the inversion
        // optimization.  You'll probably realize that as soon as you look at the disassembly, and it
        // certainly won't cause any correctness issues.
        
        match code.block(block).insts.last().unwrap().kind.opcode {
            Opcode::Branch8 
            | Opcode::Branch32
            | Opcode::Branch64 
            | Opcode::BranchTest8
            | Opcode::BranchTest32
            | Opcode::BranchTest64
            | Opcode::BranchFloat
            | Opcode::BranchDouble
            | Opcode::BranchAdd32
            | Opcode::BranchAdd64
            | Opcode::BranchSub32
            | Opcode::BranchSub64
            | Opcode::BranchMul32
            | Opcode::BranchMul64
            | Opcode::BranchNeg32
            | Opcode::BranchNeg64
            => {

                let arg = code.block(block).insts.last().unwrap().args[0];
                if code.find_next_block_index(block.0) == Some(code.block(block).successors[0].0.0) && arg.is_invertible() {
                    let mut inst = code.block_mut(block).insts.pop().unwrap();
                    inst.args[0] = arg.inverted(true);
                    code.block_mut(block).insts.push(inst);
                    code.block_mut(block).successors.swap(0, 1);
                } 
            }

            _ => ()
        }
    }
}
