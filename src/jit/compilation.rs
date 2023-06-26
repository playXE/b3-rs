use std::sync::Arc;

use macroassembler::{
    assembler::disassembler::try_to_disassemble, wtf::executable_memory_handle::CodeRef,
};

use crate::data_section::DataSection;

#[derive(Clone)]
pub struct Compilation {
    code_ref: CodeRef,
    byproducts: Vec<Arc<DataSection>>,
    entrypoints: Vec<*const u8>,
}

impl Compilation {
    pub fn new(
        code_ref: CodeRef,
        byproducts: Vec<DataSection>,
        entrypoints: Vec<*const u8>,
    ) -> Self {
        Compilation {
            code_ref,
            byproducts: byproducts.into_iter().map(Arc::new).collect(),
            entrypoints,
        }
    }

    pub fn entrypoint(&self, at: usize) -> *const u8 {
        self.entrypoints[at]
    }

    pub fn code_ref(&self) -> &CodeRef {
        &self.code_ref
    }

    pub fn byproducts(&self) -> &[Arc<DataSection>] {
        &self.byproducts
    }

    pub fn disassembly(&self) -> String {
        let mut out = String::new();

        // SAFETY:
        // `code_ref` is fully initialized
        //
        unsafe {
            try_to_disassemble(
                self.code_ref.start(),
                self.code_ref.size_in_bytes(),
                "  ",
                &mut out,
            )
            .unwrap();
        }

        out
    }
}
