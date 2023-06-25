use std::{cell::RefCell, rc::Rc, mem::size_of};

use b3::{BasicBlockBuilder, BlockId, Reg, ValueRep};
use macroassembler::{
    assembler::abstract_macro_assembler::{BaseIndex, DataLabelPtr, Extend, Scale},
    jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1, ARGUMENT_GPR2, T0, T1, T2, T4, T5},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
enum Op {
    Push = 0,
    Add,
    Sub,
    Jmp,
    JmpIf,
    JmpIfNot,
    Gt,
    Lt,
    Eq,
    Neq,
    Ret,
    OpCount,
}

struct InterpreterGenerator {
    proc: b3::Procedure,
    table_loads: Rc<RefCell<Vec<DataLabelPtr>>>,
    opcode_handlers: Vec<BlockId>,
}

impl InterpreterGenerator {
    fn emit_dispatch(
        &mut self,
        ip: b3::ValueId,
        sp: b3::ValueId,
        code: b3::ValueId,
        block: b3::BlockId,
    ) {
        let table_loads = self.table_loads.clone();
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);

        let patchpoint = builder.patchpoint(b3::Type::Void);
        builder
            .procedure
            .patchpoint_effects_mut(patchpoint)
            .control_dependent = true;
        builder
            .procedure
            .patchpoint_effects_mut(patchpoint)
            .terminal = true;

        let ip_rep = ValueRep::reg(Reg::new_gpr(T0)); // save IP in T0 (eax)
        let sp_rep = ValueRep::reg(Reg::new_gpr(T1)); // save SP in T1 (esi)
        let code_rep = ValueRep::reg(Reg::new_gpr(T2)); // save code in T2 (edx)

        builder.procedure.stackmap_append(patchpoint, ip, ip_rep);
        builder.procedure.stackmap_append(patchpoint, sp, sp_rep);
        builder
            .procedure
            .stackmap_append(patchpoint, code, code_rep);

        builder.procedure.stackmap_set_generator(
            patchpoint,
            Rc::new(move |asm, params| {
                assert!(params.len() == 3);
                let ip = params[0].get_reg().gpr();
                let _sp = params[1].get_reg().gpr();
                let code = params[2].get_reg().gpr();

                // load opcode from code pointer
                asm.load32(
                    BaseIndex::new(code, ip, Scale::TimesFour, 0, Extend::None),
                    T5,
                );
                let table_label = asm.move_with_patch(0i64, T4);
                table_loads.borrow_mut().push(table_label);
                // jump to the opcode handler
                asm.far_jump(BaseIndex::new(T4, T5, Scale::TimesEight, 0, Extend::None));
            }),
        );
        for i in 0..self.opcode_handlers.len() {
            let handler = self.opcode_handlers[i];
            self.proc.add_successor(block, handler);
        }
    }

    fn fetch_raw_register(&mut self, reg: Reg, typ: b3::Type, block: b3::BlockId) -> b3::ValueId {
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);

        let patchpoint = builder.patchpoint(typ);
        builder
            .procedure
            .patchpoint_set_result_constraints(patchpoint, ValueRep::reg(reg));
        builder
            .procedure
            .stackmap_set_generator(patchpoint, Rc::new(|_, _| {}));

        patchpoint
    }

    fn emit_dispatch_entry(
        &mut self,
        block: b3::BlockId,
    ) -> (b3::ValueId, b3::ValueId, b3::ValueId) {
        let ip = self.fetch_raw_register(Reg::new_gpr(T0), b3::Type::Int64, block);
        let sp = self.fetch_raw_register(Reg::new_gpr(T1), b3::Type::Int64, block);
        let code = self.fetch_raw_register(Reg::new_gpr(T2), b3::Type::Int64, block);

        (ip, sp, code)
    }

    fn emit_initial_dispatch(
        &mut self,
        ip: b3::ValueId,
        sp: b3::ValueId,
        code: b3::ValueId,
        block: b3::BlockId,
    ) {
        let table_loads = self.table_loads.clone();

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);

        let patchpoint = builder.patchpoint(b3::Type::Void);
        builder
            .procedure
            .patchpoint_effects_mut(patchpoint)
            .control_dependent = true;
        builder
            .procedure
            .patchpoint_effects_mut(patchpoint)
            .terminal = true;

        let ip_rep = ValueRep::reg(Reg::new_gpr(T0)); // save IP in T0 (eax)
        let sp_rep = ValueRep::reg(Reg::new_gpr(T1)); // save SP in T1 (esi)
        let code_rep = ValueRep::reg(Reg::new_gpr(T2)); // save code in T2 (edx)

        builder.procedure.stackmap_append(patchpoint, ip, ip_rep);
        builder.procedure.stackmap_append(patchpoint, sp, sp_rep);
        builder
            .procedure
            .stackmap_append(patchpoint, code, code_rep);

        builder.procedure.stackmap_set_generator(
            patchpoint,
            Rc::new(move |asm, params| {
                assert!(params.len() == 3);
                let (_, jumptable) = params.proc_mut().add_data_section(Op::OpCount as usize * 8);
                let ip = params[0].get_reg().gpr();
                let _sp = params[1].get_reg().gpr();
                let code = params[2].get_reg().gpr();

                // load opcode from code pointer
                asm.load32(
                    BaseIndex::new(code, ip, Scale::TimesFour, 0, Extend::None),
                    T5,
                );
                let table_label = asm.move_with_patch(0i64, T4);
                table_loads.borrow_mut().push(table_label);
                // jump to the opcode handler
                asm.far_jump(BaseIndex::new(T4, T5, Scale::TimesEight, 0, Extend::None));

                let labels = params.successor_labels();
                let table_loads = table_loads.clone();
                asm.add_late_link_task(Box::new(move |link_buffer| {

                    // fill in the jumptable
                    for (i, label) in labels.iter().enumerate() {
                        let label = *label.borrow();
                        let ptr = link_buffer.rx_location_of(label);
                        unsafe {
                            jumptable.cast::<*const u8>().add(i).write(ptr);
                        }
                    }

                    // patch all the table loads
                    for label in table_loads.borrow().iter() {
                        link_buffer.patch(*label, jumptable);
                    }
                }));
            }),
        );
    }

    fn emit(&mut self) {
        let entry = self.proc.add_block(1.0);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, entry);

        let sp = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
        let ip = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);
        let code = builder.argument(Reg::new_gpr(ARGUMENT_GPR2), b3::Type::Int64);

        for _ in 0..Op::OpCount as usize {
            let handler = self.proc.add_block(1.0);

            self.opcode_handlers.push(handler);
            self.proc.add_successor(entry, handler);
        }

        self.emit_initial_dispatch(ip, sp, code, entry);

        self.emit_push();
        self.emit_add();
        self.emit_sub();
        self.emit_jmp();
        self.emit_jmp_if();
        self.emit_jmp_if_not();
        self.emit_eq();
        self.emit_neq();
        self.emit_gt();
        self.emit_lt();
        self.emit_ret();
    }

    fn emit_push(&mut self) {
        let block = self.opcode_handlers[Op::Push as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        let size = builder.const64(4);
        // increment IP (skip op)
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        // fetch the value to push
        let real_offset = builder.binary(b3::Opcode::Mul, new_ip, size);
        let code_ptr = builder.binary(b3::Opcode::Add, code, real_offset);
        let value = builder.load(b3::Type::Int32, code_ptr, 0, None, None);

        // store the value
        builder.store(value, sp, 0,None, None);
        // decrement SP
        let offset = builder.const64(-(size_of::<i32>() as i64));
        let new_sp = builder.binary(b3::Opcode::Add, sp, offset);

        

        // increment IP (skip value)
        let one = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, new_ip, one);

        self.emit_dispatch(new_ip, new_sp, code, block);
    }

    fn emit_add(&mut self) {
        let block = self.opcode_handlers[Op::Add as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::Add, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }

    fn emit_sub(&mut self) {
        let block = self.opcode_handlers[Op::Sub as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::Sub, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }

    fn emit_jmp(&mut self) {
        let block = self.opcode_handlers[Op::Jmp as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);

        // fetch the jump offset
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        let size = builder.const64(4);
        // increment IP (skip op)
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        // fetch the jump offset
        let real_offset = builder.binary(b3::Opcode::Mul, new_ip, size);
        let code_ptr = builder.binary(b3::Opcode::Add, code, real_offset);
        let offset = builder.load(b3::Type::Int32, code_ptr, 0, None, None);

        let upcast = builder.zext32(offset);
        let new_ip = builder.binary(b3::Opcode::Add, ip, upcast);

        self.emit_dispatch(new_ip, sp, code, block);
    }

    fn emit_jmp_if(&mut self) {
        let block = self.opcode_handlers[Op::JmpIf as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);

        // fetch the jump offset
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        let size = builder.const64(4);
        // increment IP (skip op)
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        // fetch the jump offset
        let real_offset = builder.binary(b3::Opcode::Mul, new_ip, size);
        let code_ptr = builder.binary(b3::Opcode::Add, code, real_offset);
        let offset = builder.load(b3::Type::Int32, code_ptr, 0, None, None);

        let (value, sp) = self.pop(block, sp);

        let not_zero_block = self.proc.add_block(1.0);
        let merge = self.proc.add_block(1.0);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        builder.branch(value, not_zero_block, (merge, b3::Frequency::Normal));
        builder.switch_to_block(not_zero_block);
        let upcast = builder.zext32(offset);
        let new_ip = builder.binary(b3::Opcode::Add, ip, upcast);
        builder.jump(Some(merge));
        builder.switch_to_block(merge);
        self.emit_dispatch(new_ip, sp, code, merge);
    }

    fn emit_jmp_if_not(&mut self) {
        let block = self.opcode_handlers[Op::JmpIfNot as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);

        // fetch the jump offset
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        let size = builder.const64(4);
        // increment IP (skip op)
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        // fetch the jump offset
        let real_offset = builder.binary(b3::Opcode::Mul, new_ip, size);
        let code_ptr = builder.binary(b3::Opcode::Add, code, real_offset);
        let offset = builder.load(b3::Type::Int32, code_ptr, 0, None, None);

        let (value, sp) = self.pop(block, sp);

        let zero_block = self.proc.add_block(1.0);
        let merge = self.proc.add_block(1.0);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        builder.branch(value, merge, (zero_block, b3::Frequency::Normal));
        builder.switch_to_block(zero_block);
        let upcast = builder.zext32(offset);
        let new_ip = builder.binary(b3::Opcode::Add, ip, upcast);
        builder.jump(Some(merge));
        builder.switch_to_block(merge);
        self.emit_dispatch(new_ip, sp, code, merge);
    }

    fn emit_gt(&mut self) {
        let block = self.opcode_handlers[Op::Gt as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::GreaterThan, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }

    fn emit_lt(&mut self) {
        let block = self.opcode_handlers[Op::Lt as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::LessThan, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }
    
    fn emit_eq(&mut self) {
        let block = self.opcode_handlers[Op::Eq as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::Equal, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }

    fn emit_neq(&mut self) {
        let block = self.opcode_handlers[Op::Neq as usize];
        let (ip, sp, code) = self.emit_dispatch_entry(block);
         
        // pop two values from the stack
        let (value1, sp) = self.pop(block, sp);
        let (value2, sp) = self.pop(block, sp);

        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // add the values
        let value = builder.binary(b3::Opcode::NotEqual, value1, value2);

        let sp = self.push(block, sp, value);
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        // increment IP
        let offset = builder.const64(1);
        let new_ip = builder.binary(b3::Opcode::Add, ip, offset);

        self.emit_dispatch(new_ip, sp, code, block);
    }


    fn emit_ret(&mut self) {
        let block = self.opcode_handlers[Op::Ret as usize];
        let (_ip, sp, _code) = self.emit_dispatch_entry(block);

        // pop the return value
        let (value, _sp) = self.pop(block, sp);
    
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);
        builder.return_(Some(value));
    }

    
    

    /// Pops the value from stack and returns (value, adjusted SP)
    fn pop(&mut self, block: b3::BlockId, sp: b3::ValueId) -> (b3::ValueId, b3::ValueId) {
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);

        // increment SP
        let offset = builder.const64(size_of::<i32>() as i64);
        let new_sp = builder.binary(b3::Opcode::Add, sp, offset);

        // load the value
        let value = builder.load(b3::Type::Int32, new_sp, 0, None, None);

        (value, new_sp)
    }
    
    fn push(&mut self, block: b3::BlockId, sp: b3::ValueId, value: b3::ValueId) -> b3::ValueId {
        let mut builder = BasicBlockBuilder::new(&mut self.proc, block);

        // store the value
        builder.store(value, sp, 0,None, None);
        // decrement SP
        let offset = builder.const64(-(size_of::<i32>() as i64));
        

        builder.binary(b3::Opcode::Add, sp, offset)
    }

    fn new() -> Self {
        let mut opts = b3::Options::default();
        opts.dump_air_at_each_phase = true;
        opts.dump_b3_at_each_phase = true;
        Self {
            proc: b3::Procedure::new(opts),
            table_loads: Rc::new(RefCell::new(Vec::new())),
            opcode_handlers: Vec::new(),

        }
    }
}

fn main() {
    let mut igen = InterpreterGenerator::new();
    igen.emit();

    let code = b3::compile(igen.proc);

    println!("{}", code.disassembly());

    let func: extern "C" fn(*mut i32, usize, *const i32) -> i32 = unsafe {
        std::mem::transmute(code.entrypoint(0))
    };

    let bcode = vec![
        Op::Push as i32,
        42,
        Op::Push as i32,
        100,
        Op::Add as i32,
        Op::Ret as i32,
    ];
    let mut stack = vec![0xdeadbeefu32 as i32; 32];
    let result = func(unsafe { stack.as_mut_ptr().add(32) }, 0, bcode.as_ptr());

    println!("result: {}", result);

   
    drop(stack);
    drop(bcode);
    drop(code);
}
