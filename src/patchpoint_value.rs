use tinyvec::TinyVec;

use crate::{effects::Effects, stackmap_value::StackMapValue, value::ValueRep};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatchpointValue {
    pub(crate) base: StackMapValue,
    /// The effects of the patchpoint. This defaults to Effects::for_call(), but you can set it to anything.
    ///
    /// If there are no effects, B3 is free to assume any use of this PatchpointValue can be replaced with
    /// a use of a different PatchpointValue, so long as the other one also has no effects and has the
    /// same children. Note that this comparison ignores child constraints, the result constraint, and all
    /// other StackmapValue meta-data. If there are read effects but not write effects, then this same sort
    /// of substitution could be made so long as there are no interfering writes.
    pub effects: Effects,

    /// The input representation (i.e. constraint) of the return value. This defaults to WarmAny if the
    /// type is Void and it defaults to SomeRegister otherwise. It's illegal to mess with this if the type
    /// is Void. Otherwise you can set this to any input constraint. If the type of the patchpoint is a tuple
    /// the constrants must be set explicitly.
    pub result_constraints: TinyVec<[ValueRep; 1]>,

    /// The number of scratch registers that this patchpoint gets. The scratch register is guaranteed
    /// to be different from any input register and the destination register. It's also guaranteed not
    /// to be clobbered either early or late. These are 0 by default.
    pub num_gp_scratch_registers: u8,
    pub num_fp_scratch_registers: u8,
}

use std::ops::{Deref, DerefMut};

impl Deref for PatchpointValue {
    type Target = StackMapValue;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for PatchpointValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl std::fmt::Display for PatchpointValue {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
