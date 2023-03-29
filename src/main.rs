use b3::{
    air::generate::{generate, prepare_for_generation},
    block::{BasicBlockBuilder, Frequency},
    generate::generate_to_air,
    jit::reg::Reg,
    opcode::Opcode,
    procedure::Procedure,
    typ::Type,
    utils::phase_scope::print_scope_info,
};

use macroassembler::{
    assembler::{link_buffer::LinkBuffer, TargetMacroAssembler},
    jit::gpr_info::ARGUMENT_GPR0,
};

#[allow(dead_code)]
extern "C" fn some_func(x: i32) -> i32 {
    println!("i is equal to {}", x);
    x
}

#[allow(dead_code)]
fn factorial() {
    let mut proc = Procedure::new(Default::default());
    // factorial_iterative (Int32) -> Int32
    let root = proc.add_block(1.0);
    let loop_footer = proc.add_block(1.0);
    let loop_body = proc.add_block(1.0);
    let exit = proc.add_block(0.0);
    let n = proc.add_variable(Type::Int32);
    let acc = proc.add_variable(Type::Int32);
    let i = proc.add_variable(Type::Int32);

    BasicBlockBuilder::new(&mut proc, root).add_argument(
        Type::Int32,
        Reg::new_gpr(ARGUMENT_GPR0),
        |inst, arg| {
            inst.add_variable_set(n, arg, |inst, _| {
                inst.add_int_constant(Type::Int32, 2, |inst, iconst| {
                    inst.add_variable_set(i, iconst, |inst, _| {
                        inst.add_int_constant(Type::Int32, 1, |inst, iconst| {
                            inst.add_variable_set(acc, iconst, |inst, _| {
                                inst.add_jump(loop_footer);
                            })
                        })
                    })
                })
            })
        },
    );

    BasicBlockBuilder::new(&mut proc, loop_footer).add_variable_get(i, |inst, ivar| {
        inst.add_variable_get(n, |inst, nvar| {
            inst.add_binary(Opcode::LessEqual, ivar, nvar, |inst, cmp| {
                inst.add_branch(cmp, loop_body, (exit, Frequency::Normal));
            })
        })
    });

    BasicBlockBuilder::new(&mut proc, loop_body).add_variable_get(i, |inst, ivar| {
        inst.add_variable_get(acc, |inst, accvar| {
            inst.add_binary(Opcode::Mul, ivar, accvar, |inst, mul| {
                inst.add_variable_set(acc, mul, |inst, _| {
                    inst.add_variable_get(i, |inst, ivar| {
                        inst.add_int_constant(Type::Int32, 1, |inst, iconst| {
                            inst.add_binary(Opcode::Add, ivar, iconst, |inst, add| {
                                inst.add_variable_set(i, add, |inst, _| {
                                    inst.add_jump(loop_footer);
                                })
                            })
                        })
                    })
                })
            })
        })
    });

    BasicBlockBuilder::new(&mut proc, exit).add_variable_get(acc, |inst, accvar| {
        inst.add_return(accvar);
    });

    proc.dominators_or_compute();

    let mut code = generate_to_air(&mut proc);
    prepare_for_generation(&mut code);

    let mut jit = TargetMacroAssembler::new();
    code.generate_default_prologue(&mut jit);
    generate(&mut code, &mut jit);

    let mut link_buffer = LinkBuffer::from_macro_assembler(&mut jit);
    let mut out = String::new();

    let x = link_buffer
        .finalize_with_disassembly(true, "factorial-iterative", &mut out)
        .unwrap();

    println!("{}", out);

    let factorial_iterative: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(x.start()) };

    println!("factorial_iterative(5) = {}", factorial_iterative(5));

    let _ = x;
    let _ = code;

    print_scope_info();
}
fn main() {
    factorial();
}
