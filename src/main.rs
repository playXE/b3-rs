use b3::{self, Reg};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;

fn main() {
    let mut opts = b3::Options::default();
    opts.dump_air_at_each_phase = true;
    opts.opt_level = b3::OptLevel::O3;
    opts.air_force_irc_allocator = true;
    //opts.dump_b3_reduce_strength = true;

    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

    let x = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);
    let val = builder.load16z(x, 0, None, None);
    let c = builder.const32(0xffff);
    let new_val = builder.binary(b3::Opcode::BitXor, c, val);
    builder.store16(new_val, x, 0, None, None);

    let c = builder.const64(1);
    builder.return_(Some(c));

    println!("{}", proc.display());
    let code = b3::compile(proc);

    println!("{}", code.disassembly());
}
