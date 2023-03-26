use b3::{
    block::{BasicBlockBuilder, Frequency},
    fix_ssa::fix_ssa,
    opcode::Opcode,
    procedure::Procedure,
    typ::Type, jit::reg::Reg, lower_to_air::lower_to_air, air::{liveness_adapter::{TmpLiveness, TmpLivenessAdapter}, arg::ArgTemperature, eliminate_dead_code, lsra::allocate_registers_and_stack_by_linear_scan}, bank::Bank,
};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;

#[allow(dead_code)]
fn factorial() {
    let mut proc = Procedure::new();

    // factorial_iterative (Int32) -> Int32
    let root = proc.add_block(1.0);
    let loop_footer = proc.add_block(1.0);
    let loop_body = proc.add_block(1.0);
    let exit = proc.add_block(1.0);
    let n = proc.add_variable(Type::Int32);
    let acc = proc.add_variable(Type::Int32);
    let i = proc.add_variable(Type::Int32);

    BasicBlockBuilder::new(&mut proc, root).add_argument(Type::Int32, Reg::new_gpr(ARGUMENT_GPR0), |inst, arg| {
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
    });

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
    fix_ssa(&mut proc);


    let mut code = lower_to_air(&mut proc);
    println!("Assembly IR before RA:\n{}", code);
    eliminate_dead_code::eliminate_dead_code(&mut code);
    code.reset_reachability();

    allocate_registers_and_stack_by_linear_scan(&mut code);

    println!("Assembly IR after RA:\n{}", code);
    
}

fn main() {
    factorial();
}
