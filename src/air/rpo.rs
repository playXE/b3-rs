use super::{
    basic_block::*,
    code::*,
};

use crate::{sparse_collection::SparseElement};

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
