use crate::{air::{code::Code, inst::Inst, arg::{Arg, ArgRole}}, bank::Bank, width::Width};


/// This is a special that recognizes that there are two uses of Patchpoint: Void and and non-Void.
/// In the Void case, the syntax of the Air Patch instruction is:
///
///     Patch &patchpoint, args...
///
/// Where "args..." are the lowered arguments to the Patchpoint instruction. In the non-Void case
/// we will have:
///
///     Patch &patchpoint, result, args...

pub struct PatchpointSpecial;

impl PatchpointSpecial {
    pub fn for_each_arg(&self, code: &Code<'_>, inst: &Inst, lambda: impl FnMut(&Arg, ArgRole, Bank, Width)) {
        
    }
}