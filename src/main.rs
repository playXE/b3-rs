use b3::{self, Reg};
use macroassembler::jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1};

fn main() {
    let mut opts = b3::Options::default();
   // opts.dump_air_at_each_phase = true;
    opts.opt_level = b3::OptLevel::O3;
    opts.air_force_irc_allocator = true;
    //opts.dump_b3_reduce_strength = true;

    let mut proc = b3::Procedure::new(opts);
    let var0 = proc.add_variable(b3::Type::Int64);
    let var1 = proc.add_variable(b3::Type::Int64);
    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

    let x = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);
    let y = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int32);
    let l1 = builder.load(b3::Type::Int64, x, 0, None, None);
    let l2 = builder.load(b3::Type::Int64, y, 0, None, None);
    builder.var_set(var0, l1);
    builder.var_set(var1, l2);

    let bb1 = builder.procedure.add_block(0.0);
    let bb2 = builder.procedure.add_block(0.0);
    let bb3 = builder.procedure.add_block(0.0);

    builder.jump(Some(bb1));
    builder.switch_to_block(bb1);
    let c0 = builder.const64(0);
    let gv0 = builder.var_get(var0);
    let z8 = builder.load8z( gv0, 0, None, None);
    let zext = builder.zext32(z8);
    let bitor = builder.binary(b3::Opcode::BitOr, c0, zext);
    builder.branch(bitor, bb2, (bb3, b3::Frequency::Normal));

    builder.switch_to_block(bb2);

    let gv0 = builder.var_get(var0);
    let c1 = builder.const64(1);
    let add = builder.binary(b3::Opcode::Add, gv0, c1);
    builder.var_set(var0, add);
    builder.jump(Some(bb1));

    builder.switch_to_block(bb3);
    let v0 = builder.var_get(var0);
    let v1 = builder.var_get(var1);
    builder.store(v0, x, 0, None, None);
    builder.store(v1, y, 0, None, None);
    let c = builder.const64(1);
    builder.return_(Some(c));

    println!("{}", proc.display());
    let code = b3::compile(proc);

    println!("{}", code.disassembly());
}
