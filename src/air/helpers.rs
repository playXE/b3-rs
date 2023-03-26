use crate::typ::{Type, TypeKind};

use super::opcode::Opcode;

pub fn move_for_type(typ: Type) -> Opcode {
    match typ.kind() {
        TypeKind::Int32 => Opcode::Move32,
        TypeKind::Int64 => Opcode::Move,
        TypeKind::Float => Opcode::MoveFloat,
        TypeKind::Double => Opcode::MoveDouble,
        TypeKind::V128 => Opcode::MoveVector,
        _ => Opcode::Oops,
    }
}

pub fn relaxed_move_for_type(typ: Type) -> Opcode {
    match typ.kind() {
        TypeKind::Int32 | TypeKind::Int64 => {
            // For Int32, we could return Move or Move32. It's a trade-off.
            //
            // Move32: Using Move32 guarantees that we use the narrower move, but in cases where the
            //     register allocator can't prove that the variables involved are 32-bit, this will
            //     disable coalescing.
            //
            // Move: Using Move guarantees that the register allocator can coalesce normally, but in
            //     cases where it can't prove that the variables are 32-bit and it doesn't coalesce,
            //     this will force us to use a full 64-bit Move instead of the slightly cheaper
            //     32-bit Move32.
            //
            // Coalescing is a lot more profitable than turning Move into Move32. So, it's better to
            // use Move here because in cases where the register allocator cannot prove that
            // everything is 32-bit, we still get coalescing.

            Opcode::Move
        }
        TypeKind::Float => Opcode::MoveFloat,
        TypeKind::Double => Opcode::MoveDouble,
        TypeKind::V128 => Opcode::MoveVector,
        _ => Opcode::Oops,
    }
}
