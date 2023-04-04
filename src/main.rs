use b3::jit::reg::Reg;
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;


fn main() {
    let mut opts = b3::Options::default();
    opts.opt_level = b3::OptLevel::O3;
    opts.air_force_linear_scan_allocator = true;
    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let number = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);

    let i = builder.procedure.add_variable(b3::Type::Int32);
    let factorial = builder.procedure.add_variable(b3::Type::Int32);

    
    let for_header = builder.procedure.add_block(1.0);
    let for_body = builder.procedure.add_block(1.0);
    let for_exit = builder.procedure.add_block(1.0);

    
    let one = builder.const32(1);
    builder.var_set(factorial, one);
    builder.var_set(i, one);

    builder.jump(Some(for_header));

    builder.block = for_header;

    let i_value = builder.var_get(i);
    let cmp = builder.binary(b3::Opcode::LessEqual, i_value, number);

    
    builder.branch(cmp, for_body, (for_exit, b3::Frequency::Normal));

    builder.block = for_body;

    let i_value = builder.var_get(i);
    let factorial_value = builder.var_get(factorial);
    let mul = builder.binary(b3::Opcode::Mul, i_value, factorial_value);
    builder.var_set(factorial, mul);

    let i_value = builder.var_get(i);
    let one = builder.const32(1);
    let add = builder.binary(b3::Opcode::Add, i_value, one);
   
    builder.var_set(i, add);

    builder.jump(Some(for_header));

    builder.block = for_exit;

    let factorial_value = builder.var_get(factorial);
    builder.return_(Some(factorial_value));
    println!("{}", builder.procedure.display_());
    let compilation = b3::compile(proc);

    println!("{}", compilation.disassembly());

    let func = unsafe {
        std::mem::transmute::<_, extern "C" fn(i32) -> i32>(compilation.code_ref().start())
    };

    println!("{}", func(5));
}
