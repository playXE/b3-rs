use crate::{block::BlockId, insertion_set::InsertionSet, procedure::Procedure, value::ValueData};

#[allow(dead_code)]
struct LegalizeMemoryOffsets<'a> {
    proc: &'a mut Procedure,
    insertion_set: InsertionSet,
}

impl<'a> LegalizeMemoryOffsets<'a> {
    fn new(proc: &'a mut Procedure) -> Self {
        Self {
            insertion_set: InsertionSet::new(),
            proc,
        }
    }

    fn run(&mut self) {
        for block_id in (0..self.proc.blocks.len()).map(BlockId) {
            for index in 0..self.proc.block(block_id).len() {
                let value_id = self.proc.block(block_id)[index];

                if let ValueData::MemoryValue {
                    offset,
                    range,
                    fence_range,
                } = self.proc.value(value_id).data.clone()
                {
                    let _ = offset;
                    let _ = range;
                    let _ = fence_range;
                    // TODO: X86 does not need legalization, but other targets might need to. Right now we just skip this pass.
                }
            }
        }
    }
}

/// If the offsets of a MemoryValue cannot be represented in the target instruction set,
/// compute it explicitly.
pub fn legalize_memory_offsets(proc: &mut Procedure) {
    let mut pass = LegalizeMemoryOffsets::new(proc);
    pass.run();
}
