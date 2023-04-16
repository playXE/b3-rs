use b3::{jit::reg::Reg, OptLevel};
use cranelift::{
    prelude::{
        AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, Variable,
    },
};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use macroassembler::{assembler::TargetMacroAssembler, jit::gpr_info::ARGUMENT_GPR0};

fn b3_factorial(opt_level: b3::OptLevel, force_lsra: bool) {
    let mut opts = b3::Options::default();
    opts.opt_level = opt_level;
    opts.air_force_linear_scan_allocator = force_lsra;
    let mut proc = b3::Procedure::new(opts);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

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

    let mut code = black_box(b3::prepare_for_generation(&mut proc));

    let mut jit = TargetMacroAssembler::new();

    b3::air::generate::generate(&mut code, &mut jit);
}

use cranelift::codegen::isa;
use std::str::FromStr;
use target_lexicon::triple;

fn clif_factorial(optimize: bool) {
    use cranelift::codegen::Context;

    let mut context = Context::new();

    let mut builder_ctx = FunctionBuilderContext::new();

    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_ctx);

    let i32 = cranelift::codegen::ir::types::I32;

    builder.func.signature.params.push(AbiParam::new(i32));
    builder.func.signature.returns.push(AbiParam::new(i32));
    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    let number = builder.block_params(entry)[0];

    let i = Variable::from_u32(0);
    let factorial = Variable::from_u32(1);

    builder.declare_var(i, i32);
    builder.declare_var(factorial, i32);

    let one = builder.ins().iconst(i32, 1);
    builder.def_var(i, one);
    builder.def_var(factorial, one);

    let for_header = builder.create_block();
    let for_body = builder.create_block();
    let for_exit = builder.create_block();

    builder.ins().jump(for_header, &[]);

    builder.switch_to_block(for_header);

    let i_value = builder.use_var(i);
    let cmp = builder.ins().icmp(
        cranelift::codegen::ir::condcodes::IntCC::UnsignedLessThanOrEqual,
        i_value,
        number,
    );

    builder.ins().brif(cmp, for_body, &[], for_exit, &[]);

    builder.switch_to_block(for_body);

    let i_value = builder.use_var(i);
    let factorial_value = builder.use_var(factorial);
    let mul = builder.ins().imul(i_value, factorial_value);
    builder.def_var(factorial, mul);

    let i_value = builder.use_var(i);
    let one = builder.ins().iconst(i32, 1);
    let add = builder.ins().iadd(i_value, one);

    builder.def_var(i, add);

    builder.ins().jump(for_header, &[]);

    builder.switch_to_block(for_exit);

    let factorial_value = builder.use_var(factorial);
    builder.ins().return_(&[factorial_value]);
    builder.seal_all_blocks();
    let _ = builder.finalize();
    use cranelift::codegen::settings::{self};
    let mut shared_builder = settings::builder();
    if optimize {
        shared_builder.set("opt_level", "speed").unwrap();
    }
    let shared_flags = settings::Flags::new(shared_builder);

    let code = match isa::lookup(triple!("x86_64")) {
        Err(_) => {
            // The x86_64 target ISA is not available.
            unreachable!()
        }
        Ok(isa_builder) => {
            let isa = isa_builder.finish(shared_flags).unwrap();
            context.optimize(&*isa).unwrap();
            //context.compute_domtree();

            isa.compile_function(&context.func, &context.domtree, true).unwrap()
        }
    };

    let _ = black_box(code);
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Factorial");
    group.sample_size(10000);
    group.bench_with_input("B3(-O0, Linear Scan RA)", &OptLevel::None, |b, i| {
        b.iter(|| b3_factorial(*i, false))
    });

    group.bench_with_input("B3(-O3, IRC RA)", &OptLevel::O3, |b, i| {
        b.iter(|| b3_factorial(*i, false))
    });

    group.bench_with_input("B3(-O3, Linear Scan RA)", &OptLevel::O3, |b, i| {
        b.iter(|| b3_factorial(*i, true))
    });

    group.bench_with_input("CLIF(No opts)", &false, |b, i| {
        b.iter(|| clif_factorial(*i))
    });

    group.bench_with_input("CLIF(Opts)", &true, |b, i| b.iter(|| clif_factorial(*i)));
}
criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
