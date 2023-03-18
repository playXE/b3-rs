use crate::{
    block::{blocks_in_pre_order, BlockId},
    procedure::Procedure, value::{Value, NumChildren, ValueData}, opcode::Opcode, typ::TypeKind,
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

    for bb in 0..proc.blocks.len() {
        let bb = BlockId(bb);

        let succs = proc.block(bb).successor_list().clone();

        if succs.len() > 1 {
            for (_, (tgt, _freq)) in succs.iter().copied().enumerate() {
                let num_preds = pred_count[tgt.0];

                if num_preds > 1 {
                    let goto = proc.add(Value::new(Opcode::Jump, TypeKind::Void.into(), NumChildren::Zero, &[], ValueData::None));

                    let new_block = proc.add_block(1.0);
                    proc.add_to_block(new_block, goto);

                    proc.block_mut(bb).replace_successor(tgt, new_block);
                }
            }
        }
    }

}
