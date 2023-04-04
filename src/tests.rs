use std::rc::Rc;

use macroassembler::{
    assembler::abstract_macro_assembler::Address,
    jit::{fpr_info::*, gpr_info::*},
};

use crate::{
    self as b3,
    air::{generate::emit_function_epilogue, stack_slot::StackSlotKind},
    jit::reg::Reg,
    ValueRep, ValueRepKind,
};

#[test]
fn test_simple() {
    let mut proc = b3::Procedure::new(Default::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
    let b = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);

    let c = builder.binary(b3::Opcode::Add, a, b);
    let constant = builder.const64(2);
    let d = builder.binary(b3::Opcode::Mul, c, constant);

    builder.return_(Some(d));

    let compilation = b3::compile(proc);

    eprintln!("test_simple:\n{}", compilation.disassembly());

    let func: extern "C" fn(i64, i64) -> i64 =
        unsafe { std::mem::transmute(compilation.code_ref().start()) };

    assert_eq!(func(1, 2), 6);
    assert_eq!(func(2, 3), 10);
    assert_eq!(func(3, 4), 14);
}

#[test]
fn test_simple_double() {
    let mut proc = b3::Procedure::new(Default::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.argument(Reg::new_fpr(ARGUMENT_FPR0), b3::Type::Double);
    let b = builder.argument(Reg::new_fpr(ARGUMENT_FPR1), b3::Type::Double);

    let c = builder.binary(b3::Opcode::Add, a, b);
    let constant = builder.const_double(2.0);
    let d = builder.binary(b3::Opcode::Mul, c, constant);

    builder.return_(Some(d));

    let compilation = b3::compile(proc);

    eprintln!("test_simple_double:\n{}", compilation.disassembly());

    let func: extern "C" fn(f64, f64) -> f64 =
        unsafe { std::mem::transmute(compilation.code_ref().start()) };

    assert_eq!(func(1., 2.0), 6.);
    assert_eq!(func(2., 3.), 10.);
    assert_eq!(func(3.0, 4.), 14.);
}

#[test]
fn test_stack_slot() {
    let mut proc = b3::Procedure::new(Default::default());

    let slot = proc.add_stack_slot(8, StackSlotKind::Locked);

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);

    let slot_base = builder.slot_base(slot);

    builder.store(a, slot_base, 0, None, None);

    let load = builder.load(b3::Type::Int64, slot_base, 0, None, None);

    builder.return_(Some(load));

    let compilation = b3::compile(proc);
    eprintln!("test_stack_slot:{}", compilation.disassembly());
    let func = unsafe {
        std::mem::transmute::<_, extern "C" fn(i64) -> i64>(compilation.code_ref().start())
    };

    assert_eq!(func(1), 1);
    assert_eq!(func(2), 2);
    assert_eq!(func(3), 3);
}

#[test]
fn test_patchpoint() {
    let mut proc = b3::Procedure::new(Default::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);

    let patchpoint = builder.patchpoint(b3::Type::Int64);

    builder
        .procedure
        .stackmap_append(patchpoint, a, ValueRep::new(ValueRepKind::SomeRegister));
    builder
        .procedure
        .patchpoint_set_result_constraints(patchpoint, ValueRep::new(ValueRepKind::StackArgument));

    builder.procedure.stackmap_set_generator(
        patchpoint,
        Rc::new(|jit, params| {
            let output = params[0];
            let input = params[1];

            jit.mul64(input.get_reg().gpr(), input.get_reg().gpr());
            jit.store64(
                input.get_reg().gpr(),
                Address::new(CALL_FRAME_REGISTER, output.offset_from_fp() as i32),
            );
        }),
    );

    builder.return_(Some(patchpoint));

    let compilation = b3::compile(proc);

    eprintln!("test_patchpoint:\n{}", compilation.disassembly());

    let func = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(compilation.code_ref().start()) };

    assert_eq!(func(1), 1);
    assert_eq!(func(2), 4);
    assert_eq!(func(3), 9);
}

#[test]
fn test_check() {
    let mut proc = b3::Procedure::new(Default::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let a = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);

    let constant = builder.const64(10);

    let cmp = builder.binary(b3::Opcode::GreaterEqual, a, constant);

    let check = builder.check(cmp);

    builder
        .procedure
        .stackmap_append(check, a, ValueRep::new(ValueRepKind::SomeRegister));
    builder.procedure.stackmap_set_generator(
        check,
        Rc::new(|jit, params| {
            let input = params[1];
            jit.mul64(input.get_reg().gpr(), input.get_reg().gpr());
            jit.mov(input.get_reg().gpr(), RETURN_VALUE_GPR);
            emit_function_epilogue(jit);
            jit.ret();
        }),
    );

    builder.return_(Some(a));

    let compilation = b3::compile(proc);

    eprintln!("test_check:\n{}", compilation.disassembly());

    let func = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(compilation.code_ref().start()) };

    assert_eq!(func(1), 1);
    assert_eq!(func(9), 9);
    assert_eq!(func(10), 100);
    assert_eq!(func(11), 11 * 11);
}

#[test]
fn test_factorial() {
    let compile = |opt_level, force_lsra| {
        let mut opts = b3::Options::default();
        opts.opt_level = opt_level;
        opts.air_force_linear_scan_allocator = force_lsra;

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

        b3::compile(proc)
    };

    let o1 = compile(b3::OptLevel::O1, false);

    eprintln!("test_factorial(O1):\n{}", o1.disassembly());

    let o1_func = unsafe { std::mem::transmute::<_, fn(i32) -> i32>(o1.code_ref().start()) };

    assert_eq!(o1_func(0), 1);
    assert_eq!(o1_func(1), 1);
    assert_eq!(o1_func(2), 2);
    assert_eq!(o1_func(3), 6);
    assert_eq!(o1_func(4), 24);
    assert_eq!(o1_func(5), 120);

    let o3 = compile(b3::OptLevel::O3, false);

    eprintln!("test_factorial(O3):\n{}", o3.disassembly());

    let o3_func = unsafe { std::mem::transmute::<_, fn(i32) -> i32>(o3.code_ref().start()) };

    assert_eq!(o3_func(0), 1);
    assert_eq!(o3_func(1), 1);
    assert_eq!(o3_func(2), 2);
    assert_eq!(o3_func(3), 6);
    assert_eq!(o3_func(4), 24);
    assert_eq!(o3_func(5), 120);

    let lsra = compile(b3::OptLevel::O3, true);

    eprintln!("test_factorial(O3, force LSRA):\n{}", lsra.disassembly());

    let lsra_func = unsafe { std::mem::transmute::<_, fn(i32) -> i32>(lsra.code_ref().start()) };

    assert_eq!(lsra_func(0), 1);
    assert_eq!(lsra_func(1), 1);
    assert_eq!(lsra_func(2), 2);
    assert_eq!(lsra_func(3), 6);
    assert_eq!(lsra_func(4), 24);
    assert_eq!(lsra_func(5), 120);
}
