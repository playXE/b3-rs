use std::rc::Rc;

use b3::{jit::{reg::Reg, register_set::RegisterSetBuilder}, ValueRep};
use macroassembler::{jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1, RETURN_VALUE_GPR, T0, T1, CALL_FRAME_REGISTER}, assembler::abstract_macro_assembler::Address};

extern "C" {
    fn printf(format: *const u8, ...) -> i32;
}
static FMT: &'static str = "Hello, World!\n\0";
static FMT2: &'static str = "Hello, World! %d %d\n\0";
fn main() {
    let mut opts = b3::Options::default();
    opts.opt_level = b3::OptLevel::O3;
    opts.dump_b3_reduce_strength = true;
    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let number = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);

    let i = builder.procedure.add_variable(b3::Type::Int32);
    let factorial = builder.procedure.add_variable(b3::Type::Int32);

    let printf = builder.const64(printf as i64);
    let fmt = builder.const64(FMT.as_ptr() as i64);


    let for_header = builder.procedure.add_block(1.0);
    let for_body = builder.procedure.add_block(1.0);
    let for_exit = builder.procedure.add_block(1.0);

    let fmt2 = builder.const64(FMT2.as_ptr() as i64);
    let one = builder.const32(1);
    builder.var_set(factorial, one);
    builder.var_set(i, one);

    builder.jump(Some(for_header));

    builder.block = for_header; 
   
    let i_value = builder.var_get(i);
    let cmp = builder.binary(b3::Opcode::LessEqual, i_value, number);

    builder.ccall(b3::Type::Int32, printf, &[fmt2, i_value, number], b3::effects::Effects::for_call());

    builder.branch(cmp, for_body, (for_exit, b3::Frequency::Normal));

    builder.block = for_body;

    let i_value = builder.var_get(i);
    let factorial_value = builder.var_get(factorial);
    let mul = builder.binary(b3::Opcode::Mul, i_value, factorial_value);
    builder.var_set(factorial, mul);

    let out = builder.ccall(b3::Type::Int32, printf, &[fmt], b3::effects::Effects::for_call());
   
   

    let i_value = builder.var_get(i);
    let one = builder.const32(1);
    let add = builder.binary(b3::Opcode::Add, i_value, one);
    builder.ccall(b3::Type::Int32, printf, &[fmt2, out, mul, ], b3::effects::Effects::for_call());
    builder.var_set(i, add);

    builder.jump(Some(for_header));

    builder.block = for_exit;

    let factorial_value = builder.var_get(factorial);
    builder.return_(Some(factorial_value));

    let compilation = b3::compile(proc);

    println!("{}", compilation.disassembly());

    let func = unsafe {
        std::mem::transmute::<_, extern "C" fn(i32) -> i32>(compilation.code_ref().start())
    };

    println!("{}", func(5));
}
