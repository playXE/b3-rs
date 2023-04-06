use std::rc::Rc;

use b3::{jit::reg::Reg, Frequency, ValueRep, alloca_to_reg::{get_allocas_without_identity, alloca_to_reg}};
use macroassembler::jit::gpr_info::*;

fn main() {
    let mut opts = b3::Options::default();
    opts.opt_level = b3::OptLevel::O3;
    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.alloca(b3::Type::Int64);

    let i = builder.const64(42);
    builder.store(i, a, 0, None, None);
    let l = builder.load(b3::Type::Int64, a, 0, None, None);
    builder.return_(Some(l)); 
    let compile = b3::compile(proc);

    println!("{}", compile.disassembly());
}
