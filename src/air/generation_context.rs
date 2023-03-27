use macroassembler::assembler::abstract_macro_assembler::Label;

use crate::{jit::ccall_helpers::CCallHelpers, utils::index_set::IndexMap};

use super::{basic_block::BasicBlockId, code::Code};




pub type LatePath= Box<dyn FnOnce(&mut CCallHelpers, &mut GenerationContext)>;


pub struct GenerationContext<'a> {
    pub late_paths: Vec<LatePath>,
    pub block_labels: IndexMap<Box<Label>, BasicBlockId>,
    pub current_block: Option<BasicBlockId>,
    pub index_in_block: usize,
    pub code: &'a mut Code<'a>,
}