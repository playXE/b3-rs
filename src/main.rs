use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Mutex};

use b3::{jit::register_set::RegisterSetBuilder, macroassembler::jit::gpr_info::*, ValueRep};
use macroassembler::assembler::buffer::AssemblerLabel;
use once_cell::sync::Lazy;

static CODE_MAP: Lazy<Mutex<HashMap<usize, Vec<ValueRep>>>> =
    Lazy::new(|| Mutex::new(Default::default()));
use unwind::{Cursor, get_context, RegNum};
extern "C" fn walk_stack() {
    get_context!(context);

    let mut cursor = Cursor::local(context).unwrap();

    loop {
        let ip = cursor.register(RegNum::IP).unwrap();
        if let Some(map) = CODE_MAP.lock().unwrap().get(&(ip as usize)) {
            for rep in map.iter() {
                println!("0x{:x}: {}", ip, rep);
            }
        }
        if !cursor.step().unwrap() {
            break;
        }
    }
}

fn main() {
    let mut proc = b3::Procedure::new(Default::default());

    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    let arg = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
    let arg2 = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);

    let stackmap = builder.patchpoint(b3::Type::Void);
    builder
        .procedure
        .stackmap_append(stackmap, arg, ValueRep::stack_argument(8));
    builder
        .procedure
        .stackmap_append(stackmap, arg2, ValueRep::stack_argument(0));
    let mut rset = RegisterSetBuilder::new();
    rset.add(b3::Reg::new_gpr(T0), b3::Width::W64);
    builder.procedure.stackmap_clobber(stackmap, &rset);

    let gcpoint = Rc::new(RefCell::new((AssemblerLabel::default(), vec![])));
    {
        let gcpoint = gcpoint.clone();
        builder.procedure.stackmap_set_generator(
            stackmap,
            Rc::new(move |jit, params| {
                let arg = params[0];
                let arg2 = params[1];
                gcpoint.borrow_mut().1.push(arg);
                gcpoint.borrow_mut().1.push(arg2);
                jit.mov(walk_stack as i64, T0);
                let label = jit.call_op(Some(T0)).expect("label");
                gcpoint.borrow_mut().0 = label.label;
            }),
        );
    }

    builder.return_(None);

    let compilation = b3::compile(proc);

    println!("{}", compilation.disassembly());
    unsafe {
        let call_addr = compilation
            .entrypoint(0)
            .add(gcpoint.borrow().0.offset as usize);
      
       
        CODE_MAP
            .lock()
            .unwrap()
            .insert(call_addr as usize, gcpoint.borrow().1.clone());

        let func: extern "C" fn(i64, i64) = std::mem::transmute(compilation.entrypoint(0));

        func(1, 2);
    }
}
