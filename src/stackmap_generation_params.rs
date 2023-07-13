use std::{cell::RefCell, rc::Rc};

use macroassembler::assembler::{abstract_macro_assembler::Label, TargetMacroAssembler};

use crate::{
    air::{code::Code, generation_context::GenerationContext},
    jit::{reg::Reg, register_set::RegisterSetBuilder},
    procedure::Procedure,
    value::{ValueId, ValueRep},
};

/// This is a parameters for the stackmap generation. It is passed to generator callback.
///
/// The following data is passed to the generator callback:
/// - Scratch registers if number of scratch registers is not zero.
/// - Used registers (when `proc.set_needs_used_registers(true)` is called).
/// - Successor labels.
/// - Is it possible to fallthrough to the next successor or not.
/// - Value representation of appended values.
///
// NOTE: It's possible to capture StackmapGenerationParams by value, but not all of the methods will
// work if you do that.
pub struct StackmapGenerationParams<'a, 'b, 'c> {
    value: ValueId,
    reps: Vec<ValueRep>,
    pub(crate) gp_scratch: Vec<u8>,
    pub(crate) fp_scratch: Vec<u8>,
    context: &'a mut GenerationContext<'b, 'c>,
}

impl<'a, 'b, 'c> StackmapGenerationParams<'a, 'b, 'c> {
    pub(crate) fn new(
        value: ValueId,
        reps: Vec<ValueRep>,
        context: &'a mut GenerationContext<'b, 'c>,
    ) -> Self {
        Self {
            value,
            reps,
            context,
            gp_scratch: vec![],
            fp_scratch: vec![],
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn add_late_path(
        &mut self,
        path: Box<dyn FnOnce(&mut TargetMacroAssembler, &mut GenerationContext)>,
    ) {
        self.context.late_paths.push(path);
    }

    pub fn used_registers(&self) -> &RegisterSetBuilder {
        &self
            .context
            .code
            .proc
            .value(self.value)
            .patchpoint()
            .unwrap()
            .used_registers
    }

    pub fn gp_scratch(&self, index: usize) -> u8 {
        self.gp_scratch[index]
    }

    pub fn fp_scratch(&self, index: usize) -> u8 {
        self.fp_scratch[index]
    }
    /// This is a useful helper if you want to do register allocation inside of a patchpoint. The
    /// used_registers() set is not directly useful for this purpose because:
    ///
    /// - You can only use callee-save registers for scratch if they were saved in the prologue. So,
    ///   if a register is callee-save, it's not enough that it's not in usedRegisters().
    ///
    /// - Scratch registers are going to be in usedRegisters() at the patchpoint. So, if you want to
    ///   find one of your requested scratch registers using usedRegisters(), you'll have a bad time.
    ///
    /// This gives you the used register set that's useful for allocating scratch registers. This set
    /// is defined as:
    /// ```mustfail
    ///     (used_registers() | (RegisterSetBuilder::callee_saved_registers() - proc.callee_saved_registers()))
    ///     - gp_scratchRegisters - fp_scratchRegisters
    /// ```
    /// I.e. it is like used_registers() but also includes unsaved callee-saves and excludes scratch
    /// registers.
    ///
    /// NOTE: This will report bogus information if you did proc.set_needs_used_registers(false).
    pub fn unavailable_registers(&self) -> RegisterSetBuilder {
        let mut result = *self.used_registers();

        let mut unsaved_callee_save_registers = RegisterSetBuilder::callee_saved_registers();
        unsaved_callee_save_registers
            .exclude(&self.context.code.callee_save_registers.to_register_set());

        result.merge_regs(&unsaved_callee_save_registers);

        for reg in self.gp_scratch.iter() {
            result.remove(Reg::new_gpr(*reg));
        }

        for reg in self.fp_scratch.iter() {
            result.remove(Reg::new_fpr(*reg));
        }

        result
    }

    pub fn successor_labels(&self) -> Vec<Rc<RefCell<Label>>> {
        let mut result = Vec::new();

        for successor in self
            .context
            .code
            .block(self.context.current_block.unwrap())
            .successors
            .iter()
        {
            let label = self.context.block_labels.get(&successor.0).unwrap();

            result.push(label.clone());
        }

        result
    }

    pub fn fallthrough_to_successor(&self, successor_index: usize) -> bool {
        let successor = self
            .context
            .code
            .block(self.context.current_block.unwrap())
            .successors[successor_index]
            .0;
        let next_block = self
            .context
            .code
            .find_next_block_index(self.context.current_block.unwrap().0)
            .unwrap_or(usize::MAX);

        next_block == successor.0
    }

    
    pub fn code(&self) -> &Code<'_> {
        self.context.code
    }

    pub fn code_mut(&mut self) -> &mut Code<'_> {
        self.context.code
    }

    pub fn proc(&self) -> &Procedure {
        self.context.code.proc
    }

    pub fn proc_mut(&mut self) -> &mut Procedure {
        self.context.code.proc
    }

    pub fn size(&self) -> usize {
        self.reps.len()
    }
}

impl std::ops::Index<usize> for StackmapGenerationParams<'_, '_, '_> {
    type Output = ValueRep;

    fn index(&self, index: usize) -> &Self::Output {
        &self.reps[index]
    }
}

impl std::ops::Deref for StackmapGenerationParams<'_, '_, '_> {
    type Target = [ValueRep];

    fn deref(&self) -> &Self::Target {
        &self.reps
    }
}
