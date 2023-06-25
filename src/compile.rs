use macroassembler::assembler::{link_buffer::LinkBuffer, TargetMacroAssembler};

use crate::{
    generate::prepare_for_generation, jit::compilation::Compilation, procedure::Procedure,
};

/// This is a fool-proof API for compiling a Procedure to code and then running that code. You compile
/// a Procedure using this API by doing:
/// ```mustfail
/// let compilation = b3::compile(proc);
/// ```
/// Then you keep the Compilation object alive for as long as you want to be able to run the code.
/// If this API feels too high-level, you can use `b3::generate()` directly.
pub fn compile(mut proc: Procedure) -> Compilation {
    let mut entrypoints = vec![];
    let code = {
        let mut air = prepare_for_generation(&mut proc);

        let mut jit = TargetMacroAssembler::new();

        super::generate::generate(&mut air, &mut jit);
        let entrypoint_labels = std::mem::take(&mut air.entrypoint_labels);

        let mut link_buffer = LinkBuffer::from_macro_assembler(&mut jit).expect("failed to create link buffer");

        entrypoints.resize(entrypoint_labels.len(), std::ptr::null());

        for i in 0..entrypoint_labels.len() {
            entrypoints[i] = link_buffer.rx_location_of(entrypoint_labels[i]);
        }

        link_buffer.finalize_without_disassembly()
    };
    let byproducts = std::mem::take(&mut proc.data_sections);
    Compilation::new(code, byproducts, entrypoints)
}
