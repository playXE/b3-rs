use std::{hash::Hash, rc::Rc};

use crate::{jit::register_set::RegisterSetBuilder, value::{ValueRep}};

#[derive(Clone)]
pub struct StackMapValue {
    pub reps: Vec<ValueRep>,
    pub generator: Option<Rc<dyn FnOnce()>>,
    pub early_clobbered: RegisterSetBuilder,
    pub late_clobbered: RegisterSetBuilder,
    pub used_registers: RegisterSetBuilder,
}

impl StackMapValue {
    pub fn set_generator(&mut self, generator: impl FnOnce() + 'static) {
        self.generator = Some(Rc::new(generator));
    }

    pub fn generator(&self) -> Option<&dyn FnOnce()> {
        self.generator.as_ref().map(|x| &**x)
    }

    // Stackmaps allow you to specify that the operation may clobber some registers. Clobbering a register
    // means that the operation appears to store a value into the register, but the compiler doesn't
    // assume to know anything about what kind of value might have been stored. In B3's model of
    // execution, registers are read or written at instruction boundaries rather than inside the
    // instructions themselves. A register could be read or written immediately before the instruction
    // executes, or immediately after. Note that at a boundary between instruction A and instruction B we
    // simultaneously look at what A does after it executes and what B does before it executes. This is
    // because when the compiler considers what happens to registers, it views the boundary between two
    // instructions as a kind of atomic point where the late effects of A happen at the same time as the
    // early effects of B.
    //
    // The compiler views a stackmap as a single instruction, even though of course the stackmap may be
    // composed of any number of instructions (if it's a Patchpoint). You can claim that a stackmap value
    // clobbers a set of registers before the stackmap's instruction or after. Clobbering before is called
    // early clobber, while clobbering after is called late clobber.
    //
    // This is quite flexible but it has its limitations. Any register listed as an early clobber will
    // interfere with all uses of the stackmap. Any register listed as a late clobber will interfere with
    // all defs of the stackmap (i.e. the result). This means that it's currently not possible to claim
    // to clobber a register while still allowing that register to be used for both an input and an output
    // of the instruction. It just so happens that B3's sole client (the FTL) currently never wants to
    // convey such a constraint, but it will want it eventually (FIXME:
    // https://bugs.webkit.org/show_bug.cgi?id=151823).
    //
    // Note that a common use case of early clobber sets is to indicate that this is the set of registers
    // that shall not be used for inputs to the value. But B3 supports two different ways of specifying
    // this, the other being LateUse in combination with late clobber (not yet available to stackmaps
    // directly, FIXME: https://bugs.webkit.org/show_bug.cgi?id=151335). A late use makes the use of that
    // value appear to happen after the instruction. This means that a late use cannot use the same
    // register as the result and it cannot use the same register as either early or late clobbered
    // registers. Late uses are usually a better way of saying that a clobbered register cannot be used
    // for an input. Early clobber means that some register(s) interfere with *all* inputs, while LateUse
    // means that some value interferes with whatever is live after the instruction. Below is a list of
    // examples of how the FTL can handle its various kinds of scenarios using a combination of early
    // clobber, late clobber, and late use. These examples are for X86_64, w.l.o.g.
    //
    // Basic ById patchpoint: Early and late clobber of r11. Early clobber prevents any inputs from using
    // r11 since that would mess with the MacroAssembler's assumptions when we
    // AllowMacroScratchRegisterUsage. Late clobber tells B3 that the patchpoint may overwrite r11.
    //
    // ById patchpoint in a try block with some live state: This might throw an exception after already
    // assigning to the result. So, this should LateUse all stackmap values to ensure that the stackmap
    // values don't interfere with the result. Note that we do not LateUse the non-OSR inputs of the ById
    // since LateUse implies that the use is cold: the register allocator will assume that the use is not
    // important for the critical path. Also, early and late clobber of r11.
    //
    // Basic ByIdFlush patchpoint: We could do Flush the same way we did it with LLVM: ignore it and let
    // PolymorphicAccess figure it out. Or, we could add internal clobber support (FIXME:
    // https://bugs.webkit.org/show_bug.cgi?id=151823). Or, we could do it by early clobbering r11, late
    // clobbering all volatile registers, and constraining the result to some register. Or, we could do
    // that but leave the result constrained to SomeRegister, which will cause it to use a callee-save
    // register. Internal clobber support would allow us to use SomeRegister while getting the result into
    // a volatile register.
    //
    // ByIdFlush patchpoint in a try block with some live state: LateUse all for-OSR stackmap values,
    // early clobber of r11 to prevent the other inputs from using r11, and late clobber of all volatile
    // registers to make way for the call. To handle the result, we could do any of what is listed in the
    // previous paragraph.
    //
    // Basic JS call: Force all non-OSR inputs into specific locations (register, stack, whatever).
    // All volatile registers are late-clobbered. The output is constrained to a register as well.
    //
    // JS call in a try block with some live state: LateUse all for-OSR stackmap values, fully constrain
    // all non-OSR inputs and the result, and late clobber all volatile registers.
    //
    // JS tail call: Pass all inputs as a warm variant of Any (FIXME:
    // https://bugs.webkit.org/show_bug.cgi?id=151811).
    //
    // Note that we cannot yet do all of these things because although Air already supports all of these
    // various forms of uses (LateUse and warm unconstrained use), B3 doesn't yet expose all of it. The
    // bugs are:
    // https://bugs.webkit.org/show_bug.cgi?id=151335 (LateUse)
    // https://bugs.webkit.org/show_bug.cgi?id=151811 (warm Any)
    pub fn clobber_early(&mut self, set: &RegisterSetBuilder) {
        self.early_clobbered.merge(set);
    }

    pub fn clobber_late(&mut self, set: &RegisterSetBuilder) {
        self.late_clobbered.merge(set);
    }

    pub fn clobber(&mut self, set: &RegisterSetBuilder) {
        self.clobber_early(set);
        self.clobber_late(set);
    }

}


impl PartialEq for StackMapValue {
    fn eq(&self, other: &Self) -> bool {
        self.reps == other.reps
            && self
                .generator
                .as_ref()
                .zip(other.generator.as_ref())
                .map(|(a, b)| Rc::ptr_eq(a, b))
                .unwrap_or(false)
            && self.early_clobbered == other.early_clobbered
            && self.late_clobbered == other.late_clobbered
            && self.used_registers == other.used_registers
    }
}

impl Eq for StackMapValue {}

impl std::fmt::Debug for StackMapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StackMapValue")
            .field("reps", &self.reps)
            .field("early_clobbered", &self.early_clobbered)
            .field("late_clobbered", &self.late_clobbered)
            .field("used_registers", &self.used_registers)
            .finish()
    }
}

impl Hash for StackMapValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.reps.hash(state);
        self.generator.as_ref().map(|x| Rc::as_ptr(x)).hash(state);
        self.early_clobbered.hash(state);
        self.late_clobbered.hash(state);
        self.used_registers.hash(state);
    }
}
