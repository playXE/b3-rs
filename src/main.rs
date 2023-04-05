use b3::jit::reg::Reg;
use macroassembler::jit::gpr_info::*;

extern "C" fn foo(
    v0: i32,
    v1: i32,
    v2: i32,
    v3: i32,
    v4: i32,
    v5: i32,
    v6: i32,
    v7: i32,
    v8: i32,
    v9: i32,
) -> i32 {
    v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
}

fn main() {
    let mut opts = b3::Options::default();
    opts.opt_level = b3::OptLevel::O3;
    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a1 = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
    let a2 = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);
    let a3 = builder.argument(Reg::new_gpr(ARGUMENT_GPR2), b3::Type::Int64);
    let a4 = builder.argument(Reg::new_gpr(ARGUMENT_GPR3), b3::Type::Int64);
    let a5 = builder.argument(Reg::new_gpr(ARGUMENT_GPR4), b3::Type::Int64);
    let a6 = builder.argument(Reg::new_gpr(ARGUMENT_GPR5), b3::Type::Int64);

    let func = builder.const64(foo as i64);

    let v0 = builder.ccall(
        b3::Type::Int32,
        func,
        &[a1, a2, a3, a4, a5, a6, a1, a2, a3, a4],
        b3::effects::Effects::for_call(),
    );

    builder.return_(Some(v0));

    let compilation = b3::compile(proc);

    println!("{}", compilation.disassembly());
    let func: extern "C" fn(i32, i32, i32, i32, i32, i32) -> i32 =
        unsafe { std::mem::transmute(compilation.code_ref().start()) };

    println!("{}", func(1, 2, 3, 4, 5, 6));
    println!("{}", foo(1, 2, 3, 4, 5, 6, 1, 2, 3, 4));
}
