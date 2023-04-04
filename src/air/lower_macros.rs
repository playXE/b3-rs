use tinyvec::TinyVec;

use crate::{
    typ::TypeKind,
    utils::phase_scope::phase_scope,
    width::{width_for_type, Width},
};

use super::{
    arg::Arg,
    basic_block::BasicBlockId,
    ccalling_convention::{
        build_ccall, ccall_argument_register_count, ccall_result, ccall_result_count,
        compute_ccalling_convention,
    },
    code::Code,
    emit_shuffle::{create_shuffle, ShufflePair},
    insertion_set::InsertionSet,
    inst::Inst,
    opcode::Opcode,
};

/// Air has some opcodes that are very high-level and are meant to reduce the amount of low-level
/// knowledge in the B3->Air lowering. The current example is CCall.
pub fn lower_macros(code: &mut Code) {
    phase_scope("air::lower_macros", || {
        let mut insertion_set = InsertionSet::new();

        for block_id in (0..code.blocks.len()).map(BasicBlockId) {
            for inst_index in 0..code.block(block_id).insts.len() {
                let inst = &code.block(block_id).insts[inst_index];

                match inst.kind.opcode {
                    Opcode::CCall | Opcode::ColdCCall => {
                        let value = inst.origin;
                        let old_kind = inst.kind;
                        let inst = inst.clone();

                        let mut destinations = compute_ccalling_convention(code, value);
                       
                        let result_count = ccall_result_count(code, value);

                        let mut shuffle_pairs = TinyVec::<[ShufflePair; 16]>::new();

                        let mut has_register_source = false;

                        let mut offset = 1;

                        let mut add_next_pair = |width: Width| {
                            let pair = ShufflePair::new(
                                inst.args[offset + result_count + 1],
                                destinations[offset],
                                width,
                            );
                         
                            shuffle_pairs.push(pair);

                            has_register_source |= pair.src.is_reg();
                            offset += 1;
                        };

                        for i in 1..code.proc.value(value).children.len() {
                            let child = code.proc.value(value).children[i];

                            for _ in 0..ccall_argument_register_count(code, child) {
                                let typ = code.proc.value(child).typ();
                                add_next_pair(width_for_type(typ));
                            }
                        }

                        if has_register_source {
                            insertion_set.insert_inst(
                                inst_index,
                                create_shuffle(inst.origin, &shuffle_pairs),
                            );
                        } else {
                            // If none of the inputs are registers, then we can efficiently lower this
                            // shuffle before register allocation. First we lower all of the moves to
                            // memory, in the hopes that this is the last use of the operands. This
                            // avoids creating interference between argument registers and arguments
                            // that don't go into argument registers.

                            for pair in shuffle_pairs.iter() {
                                if pair.dst.is_memory() {
                                    insertion_set
                                        .insert_insts(inst_index, pair.insts(code, inst.origin));
                                }
                            }

                            // Fill the argument registers by starting with the first one. This avoids
                            // creating interference between things passed to low-numbered argument
                            // registers and high-numbered argument registers. The assumption here is
                            // that lower-numbered argument registers are more likely to be
                            // incidentally clobbered.
                            for pair in shuffle_pairs.iter() {
                                if !pair.dst.is_memory() {
                                    insertion_set
                                        .insert_insts(inst_index, pair.insts(code, inst.origin));
                                }
                            }
                        }
                        // Indicate that we're using our original callee argument.
                        destinations[0] = inst.args[1];

                        let result_dst0 = if result_count >= 1 {
                            inst.args[2]
                        } else {
                            Arg::default()
                        };
                        let _result_dst1 = if result_count >= 2 {
                            inst.args[3]
                        } else {
                            Arg::default()
                        };

                        let mut inst = build_ccall(code, inst.origin, &destinations);

                        if old_kind.effects {
                            inst.kind.effects = true;
                        }

                        code.block_mut(block_id).insts[inst_index] = inst;

                        match code.proc.value(value).typ().kind() {
                            TypeKind::Void => {}
                            TypeKind::Float => insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(
                                    Opcode::MoveFloat.into(),
                                    value,
                                    &[Arg::new_tmp(ccall_result(code, value, 0)), result_dst0],
                                ),
                            ),
                            TypeKind::Double => insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(
                                    Opcode::MoveDouble.into(),
                                    value,
                                    &[Arg::new_tmp(ccall_result(code, value, 0)), result_dst0],
                                ),
                            ),
                            TypeKind::Int32 => insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(
                                    Opcode::Move32.into(),
                                    value,
                                    &[Arg::new_tmp(ccall_result(code, value, 0)), result_dst0],
                                ),
                            ),

                            TypeKind::Int64 => insertion_set.insert_inst(
                                inst_index + 1,
                                Inst::new(
                                    Opcode::Move.into(),
                                    value,
                                    &[Arg::new_tmp(ccall_result(code, value, 0)), result_dst0],
                                ),
                            ),

                            _ => todo!(),
                        }
                    }

                    _ => (),
                }
            }

            insertion_set.execute(code, block_id);
        }
    });
}
