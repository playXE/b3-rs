use crate::{procedure::Procedure, air::code::Code, lower_to_air::lower_to_air};

pub fn generate_to_air<'a>(proc: &'a mut Procedure) -> Code<'a> {
    proc.reset_reachability();
    super::fix_ssa::fix_ssa(proc);
    super::legalize_memory_offsets::legalize_memory_offsets(proc);
    super::move_constants::move_constants(proc);
    

    let code = lower_to_air(proc);

    code
}