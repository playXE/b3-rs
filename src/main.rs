use b3::{self, Reg};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;

fn main() {
    let mut opts = b3::Options::default();
    opts.dump_air_at_each_phase = true;
    opts.opt_level = b3::OptLevel::O1;

    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

    let n = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);

    let switch = builder.switch(n);

    for i in 0..120 {
        let handler = builder.procedure.add_block(1.0);
        builder.switch_to_block(handler);
        let val = builder.const32(i);
        builder.return_(Some(val));

        builder.procedure.switch_append_case(switch, (i as i64, (handler, b3::Frequency::Normal)));
    }
    println!("{}", proc.display_());
    let compilation = b3::compile(proc);

    println!("{}", compilation.disassembly());
}