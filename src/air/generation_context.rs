use macroassembler::assembler::{abstract_macro_assembler::Label, TargetMacroAssembler};

use crate::{utils::index_set::IndexMap};

use super::{basic_block::BasicBlockId, code::Code};




pub type LatePath= Box<dyn FnOnce(&mut TargetMacroAssembler, &mut GenerationContext)>;


pub struct GenerationContext<'a> {
    pub late_paths: Vec<LatePath>,
    pub block_labels: IndexMap<Box<Label>, BasicBlockId>,
    pub current_block: Option<BasicBlockId>,
    pub index_in_block: usize,
    pub code: &'a mut Code<'a>,
}