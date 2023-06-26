use b3::{self, Reg};
use macroassembler::jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1};

fn main() {
    let mut opts = b3::Options::default();
    opts.dump_air_at_each_phase = true;
    opts.opt_level = b3::OptLevel::O3;
    opts.air_force_irc_allocator = true;

    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

    let x = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);
    let y = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int32);

    let res1 = builder.binary(b3::Opcode::Add, x, y);
    let res2 = builder.binary(b3::Opcode::Add, x, y);

    let res3 = builder.binary(b3::Opcode::Add, res1, res2);

    builder.return_(Some(res3));
    println!("{}", proc.display());
    b3::prepare_for_generation(&mut proc);
}
