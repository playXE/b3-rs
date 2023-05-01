use crate::{
    block::{blocks_in_pre_order, BasicBlock, BlockId},
    opcode::Opcode,
    procedure::Procedure,
    rpo::rpo_sort,
    typ::TypeKind,
    value::{NumChildren, Value, ValueData},
};

///
/// Breaks critical edges in the IR.
///
/// Critical edges are edges that are neither the only edge leaving a
/// block, nor the only edge entering one.
///
/// When you want something to happen "along" an edge, you can either
/// do at the end of the predecessor block, or at the start of the
/// successor block. Critical edges have to be broken in order to prevent
/// "edge actions" from affecting other edges.
///
/// This function will break those edges by inserting new blocks along them.
///
///
/// # Note
///
/// Simplify SSA pass will happily undo most of the work this pass does.
pub fn break_critical_edges(proc: &mut Procedure) {
    let mut pred_count = vec![0u32; proc.blocks.len()];

    for block in blocks_in_pre_order(BlockId(0), proc) {
        for (tgt, _) in proc.block(block).successor_list().iter().copied() {
            pred_count[tgt.0] += 1;
        }
    }

    let mut work = Vec::new();
    let num_blocks = proc.blocks.len();

    for bid in (0..num_blocks).map(BlockId) {
        let succs = proc.block_mut(bid).successor_list_mut();

        if succs.len() > 1 {
            for succ in succs {
                if pred_count[succ.0 .0] > 1 {
                    let split_bid = BlockId(num_blocks + work.len());

                    work.push((bid, split_bid, *succ));

                    succ.0 = split_bid;
                }
            }
        }
    }

    if !work.is_empty() {
        proc.invalidate_cfg();

        proc.blocks
            .resize_with(num_blocks + work.len(), || BasicBlock::new(usize::MAX, 1.0));

        for (bid, split_bid, old_target) in work.drain(..) {
            let value = proc.add(Value::new(
                Opcode::Jump,
                TypeKind::Void.into(),
                NumChildren::Zero,
                &[],
                ValueData::None,
            ));

            proc.add_to_block(split_bid, value);

            proc.block_mut(split_bid).successor_list.push(old_target);
            proc.block_mut(split_bid).predecessor_list.push(bid);
            proc.block_mut(split_bid).index = split_bid.0;

            assert!(proc
                .block_mut(old_target.0)
                .replace_predecessor(bid, split_bid));
        }

        proc.reset_reachability();
        println!("{}", proc.display_());
        rpo_sort(proc);
    }
}
