use std::{cell::RefCell, rc::Rc};

use macroassembler::{
    assembler::{abstract_macro_assembler::DataLabelPtr, TargetMacroAssembler},
    jit::gpr_info::ARGUMENT_GPR0,
};

fn main() {
    let mut proc = b3::Procedure::new(b3::Options::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(&mut proc, entry);

    let x = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);

    // emit patchpoint that would resolve address later on
    let patchaddr = builder.patchpoint(b3::Type::Int64);

    // put result in any register
    builder
        .procedure
        .patchpoint_set_result_constraints(patchaddr, b3::ValueRep::new(b3::ValueRepKind::SomeRegister));

    let label = Rc::new(RefCell::new(DataLabelPtr::unset()));
    let lbl2 = label.clone();
    builder.procedure.stackmap_set_generator(
        patchaddr,
        Rc::new(move |jit, params| {
            // emit patchable 64-bit move
            *lbl2.borrow_mut() = jit.move_with_patch(0i64, params[0].gpr());
        }),
    );

    let one = builder.const32(1);
    let inc = builder.binary(b3::Opcode::Add, x, one);
  
    builder.ccall(b3::Type::Void, patchaddr, &[inc], b3::Effects::for_call());

    builder.return_(None);

    let code = b3::compile(proc);

    // now that when we have executable code, we can patch the label.
    // fetch RW pointer to the code and then patch the label
    let start = code.code_ref().start_rw();

    unsafe {
        TargetMacroAssembler::link_pointer(
            start.unwrap(), /* code */
            label.borrow().label, /* label */
            code.code_ref().start(), /* target */
        );
    }

    println!("{}", code.disassembly());

    // stack overflow
    let f: extern "C" fn(i32) -> i64 = unsafe { std::mem::transmute(code.entrypoint(0)) };
    f(0);
}
