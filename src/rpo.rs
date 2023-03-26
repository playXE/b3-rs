use crate::{
    block::{blocks_in_post_order, BlockId, Frequency},
    procedure::Procedure,
    sparse_collection::SparseElement,
};

/// Compute the Block post-order for a Procedure. In the returned Vec a BlockId will
/// appear after all of its successors.
pub fn compute_po(proc: &Procedure) -> Vec<BlockId> {
    blocks_in_post_order(BlockId(0), proc)
    //compute_general_po(proc, |succs| succs.split_last())
}

/// Compute the Block reverse-post-order for a Procedure. In the retuned Vec a
/// BlockId will appear before any of its successors.
pub fn compute_rpo(proc: &Procedure) -> Vec<BlockId> {
    let mut po = compute_po(proc);
    po.reverse();
    po
}

/// Compute reversed rpo, which is rpo of a dfs that walks children in
/// the opposite order that ordinary rpo does.  This is useful for
/// varying rpo traversal order when doing a fixed point computation
/// such as dominators to avoid quadratic corner cases.
pub fn compute_rrpo(proc: &Procedure) -> Vec<BlockId> {
    let mut po = compute_general_po(proc, |succs| succs.split_first());
    po.reverse();
    po
}

fn compute_general_po(
    proc: &Procedure,
    splitter: impl Fn(
        &[(BlockId, Frequency)],
    ) -> Option<(&(BlockId, Frequency), &[(BlockId, Frequency)])>,
) -> Vec<BlockId> {
    let blocks = &proc.blocks;
    let mut result = Vec::with_capacity(blocks.len());
    if blocks.is_empty() {
        return result;
    }

    // Use an explicit recursion stack seeded with the entry block to avoid
    // blowing the program stack recursing through a large Procedure.
    let mut stack: Vec<((BlockId, Frequency), &[(BlockId, Frequency)])> = Vec::new();
    let mut already_pushed = vec![false; blocks.len()];

    mark_block(&mut stack, &mut already_pushed, proc, BlockId(0));

    fn mark_block<'a>(
        stack: &mut Vec<((BlockId, Frequency), &'a [(BlockId, Frequency)])>,
        already_pushed: &mut [bool],
        proc: &'a Procedure,
        bid: BlockId,
    ) {
        let successors = &proc.block(bid).successor_list;

        stack.push(((bid, Frequency::Normal), successors));
        already_pushed[bid.0] = true;
    }

    fn process_child<'a>(
        proc: &'a Procedure,
        stack: &mut Vec<((BlockId, Frequency), &'a [(BlockId, Frequency)])>,
        already_pushed: &mut Vec<bool>,
        bid: (BlockId, Frequency),
        parent: ((BlockId, Frequency), &'a [(BlockId, Frequency)]),
    ) -> bool {
        let was_pushed = &mut already_pushed[bid.0 .0];

        if *was_pushed {
            return false;
        }

        *was_pushed = true;

        stack.push(parent);

        let child_successors = proc.block(bid.0).successor_list().as_slice();
        stack.push((bid, child_successors));

        true
    }

    'outer: while let Some(((bid, freq), mut successors)) = stack.pop() {
        while !successors.is_empty() {
            let (&child_bid, next_successors) = splitter(successors).unwrap();
            successors = next_successors;
            let parent = ((bid, freq), successors);

            if process_child(proc, &mut stack, &mut already_pushed, child_bid, parent) {
                continue 'outer;
            }
        }

        result.push(bid);
    }

    result
}

/// Sort Procedure's blocks using reverse postorder DFS, renumbering successor edges as needed.
///
/// RPO has the nice property that an Value's operands will always be "earlier" in the ordering.
///
/// Note that blocks unreachable from the entry block will be discarded.
pub fn rpo_sort(proc: &mut Procedure) {
    let rpo = compute_rpo(proc);

    if block_order_matches(proc, &rpo) {
        return;
    }

    // Create the mapping from old BlockId to new BlockId.
    // Dead blocks will have a usize::MAX entry.
    let remap = order_to_remap(&rpo, proc.blocks.len());

    // Update successor edges for surviving blocks to use the new BlockIds.
    // (Note that this double loop will make it so that the closer to RPO we
    // start the less we have to do.)
    for (old, &new) in remap.iter().enumerate() {
        if new.0 != usize::MAX {
            let bid = BlockId(old);

            for succ in proc.block_mut(bid).successor_list_mut() {
                let target = remap[succ.0 .0];
                debug_assert_ne!(target.0, usize::MAX);
                succ.0 = target;
            }

            for pred in proc.block_mut(bid).predecessor_list_mut() {
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

fn order_to_remap(order: &[BlockId], len: usize) -> Vec<BlockId> {
    let mut remap = vec![BlockId::from(usize::MAX); len];

    for (i, &bid) in order.iter().enumerate() {
        // check for duplicates in `order`
        let bid_as_usize: usize = bid.into();
        debug_assert_eq!(remap[bid_as_usize], BlockId::from(usize::MAX));
        remap[bid_as_usize] = BlockId::from(i);
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
fn block_order_matches(proc: &Procedure, order: &[BlockId]) -> bool {
    order.len() == proc.blocks.len()
        && order.iter().enumerate().all(|(i, &b)| {
            let bid_as_usize: usize = b.into();
            i == bid_as_usize
        })
}
