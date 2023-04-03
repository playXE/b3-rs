use std::{collections::HashMap, rc::Rc};

use b3::{
    air::generate::emit_function_epilogue,
    jit::{compilation::Compilation, reg::Reg},
    variable::VariableId,
    ValueRep, OptLevel,
};
use macroassembler::{
    assembler::abstract_macro_assembler::Address,
    jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1, CALL_FRAME_REGISTER, RETURN_VALUE_GPR},
};

pub const OP_PUSH: i32 = 0;
pub const OP_ADD: i32 = 1;
pub const OP_JUMP: i32 = 2;
pub const OP_GT: i32 = 3;
pub const OP_HALT: i32 = 4;
pub const OP_POP: i32 = 5;
pub const TRACE_INSTR: i32 = 0;
pub const TRACE_GT_JUMP: i32 = 1;
pub const TRACE_GT_NJUMP: i32 = 2;
pub const TRACE_ENTER_TRACE: i32 = 3;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum Op {
    Movi(u8, i32),
    Gt(u8, i32, usize),
    Jump(usize),
    Add(u8, i32),
    Ret(u8),
}

trait Interpreter {
    fn pc(&self) -> usize;
    fn pc_mut(&mut self) -> &mut usize;
    fn stack(&self) -> &[i32];
    fn stack_mut(&mut self) -> &mut Vec<i32>;
    fn code(&self) -> &[Op];

    fn run_movi(&mut self) {
        match self.code()[self.pc()] {
            Op::Movi(dst, i) => {
                self.stack_mut()[dst as usize] = i;
            }
            _ => unreachable!(),
        }
        *self.pc_mut() += 1;
    }

    fn run_gt(&mut self) {
        match self.code()[self.pc()] {
            Op::Gt(reg, x, target) => {
                if self.stack()[reg as usize] > x {
                    *self.pc_mut() = target;
                
                } else {
                    *self.pc_mut() += 1;
                }
            }
            _ => unreachable!(),
        }
    }

    fn run_add(&mut self) {
        match self.code()[self.pc()] {
            Op::Add(reg, i) => {
                self.stack_mut()[reg as usize] += i;
            }
            _ => unreachable!(),
        }
        *self.pc_mut() += 1;
    }
    fn run_jump(&mut self) {
        match self.code()[self.pc()] {
            Op::Jump(target) => {
                *self.pc_mut() = target;
            }
            _ => unreachable!(),
        }
    }

    fn interpret(&mut self) -> i32 {
        loop {
           
            let ins = self.code()[self.pc()];
            match ins {
                Op::Add { .. } => self.run_add(),
                Op::Jump { .. } => self.run_jump(),
                Op::Ret(r) => return self.stack()[r as usize],
                Op::Gt { .. } => self.run_gt(),
                Op::Movi { .. } => self.run_movi(),
            }
        }
    }
}

struct LoopInfo {
    hotness: usize,
    fails: usize,
    trace_id: usize,
    blacklisted: bool,
    trace: Vec<(Trace, usize)>,
    executable_trace: Option<Compilation>,
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum Trace {
    Instr(Op),
    GuardGtJump(u8, i32, usize /* pc */),
    GuardGtNJump(u8, i32, usize /* pc */),
    EnterTrace,
}

pub struct TracingInterpreter {
    loops: HashMap<(usize, usize), LoopInfo>,
    code: Vec<Op>,
    stack: Vec<i32>,
    recording: bool,
    trace_id: usize,
    pc: usize,
}

impl TracingInterpreter {
    fn new(code: Vec<Op>) -> Self {
        Self {
            loops: HashMap::new(),
            code,
            stack: vec![0; 5],
            recording: false,
            trace_id: 0,
            pc: 0,
        }
    }

    fn translate_trace(&self, range: (usize, usize)) -> Compilation {
        let info = self.loops.get(&range).unwrap();
        let mut opts = b3::Options::default();
        opts.opt_level = OptLevel::O3;
        let mut proc = b3::Procedure::new(opts);

        let entry = proc.add_block(1.0);
        let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

        let stack = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
        let pc = builder.argument(Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);

        let loop_block = builder.procedure.add_block(1.0);

        builder.block = loop_block;

        let mut vars = HashMap::new();

        let load_reg =
            |vars: &mut HashMap<u8, VariableId>, builder: &mut b3::BasicBlockBuilder, r: u8| {
                if let Some(var) = vars.get(&r) {
                    builder.var_get(*var)
                } else {
                    let var = builder.procedure.add_variable(b3::Type::Int32);
                    vars.insert(r, var);
                    builder.var_get(var)
                }
            };

        let store_reg = |vars: &mut HashMap<u8, VariableId>,
                         builder: &mut b3::BasicBlockBuilder,
                         r: u8,
                         val: b3::ValueId| {
            if let Some(var) = vars.get(&r) {
                builder.var_set(*var, val);
            } else {
                let var = builder.procedure.add_variable(b3::Type::Int32);
                vars.insert(r, var);
                builder.var_set(var, val);
            }
        };

        for trace_step in info.trace.iter() {
            match &trace_step.0 {
                Trace::Instr(op) => match op {
                    Op::Jump(_) => {}
                    Op::Add(r, imm) => {
                        let x = builder.const32(*imm);
                        let y = load_reg(&mut vars, &mut builder, *r);
                        let z = builder.binary(b3::Opcode::Add, y, x);
                        store_reg(&mut vars, &mut builder, *r, z);
                    }

                    Op::Movi(r, imm) => {
                        let x = builder.const32(*imm);
                        store_reg(&mut vars, &mut builder, *r, x);
                    }

                    _ => unreachable!(),
                },

                Trace::GuardGtJump(r, imm, pc_) => {
                    let x = load_reg(&mut vars, &mut builder, *r);
                    let imm = builder.const32(*imm);
                    let cmp = builder.binary(b3::Opcode::LessEqual, x, imm);
                    let check = builder.check(cmp);
                    let pc_ = *pc_;
                    builder.procedure.stackmap_append_some_register(check, pc);
                    builder
                        .procedure
                        .stackmap_append_some_register(check, stack);
                    let mut to_restore = vec![];
                    for (&reg, &var) in vars.iter() {
                        let val = builder.var_get(var);
                        to_restore.push(reg);
                        builder.procedure.stackmap_append(
                            check,
                            val,
                            ValueRep::new(b3::ValueRepKind::ColdAny),
                        );
                    }
                    to_restore.sort();

                    builder.procedure.stackmap_set_generator(
                        check,
                        Rc::new(move |jit, params| {
                            let stack = params[2].get_reg().gpr();
                            
                            for (i, param) in params.iter().skip(3).enumerate() {
                              
                                let reg = to_restore[i];
                                let offset = reg as i32 * 4;

                                if param.is_stack() {
                                    jit.load32(
                                        Address::new(
                                            CALL_FRAME_REGISTER,
                                            param.offset_from_fp() as _,
                                        ),
                                        RETURN_VALUE_GPR,
                                    );
                                    jit.store32(RETURN_VALUE_GPR, Address::new(stack, offset));
                                } else {
                                    jit.store32(param.get_reg().gpr(), Address::new(stack, offset));
                                }
                            }
                            let pc = params[1].get_reg().gpr();
                            jit.store64(pc_ as i32, Address::new(pc, 0));
                            jit.mov(1i32, RETURN_VALUE_GPR);
                            emit_function_epilogue(jit);
                            jit.ret();
                        }),
                    );
                }

                Trace::GuardGtNJump(r, imm, pc_) => {
                    let x = load_reg(&mut vars, &mut builder, *r);
                    let imm = builder.const32(*imm);
                    let cmp = builder.binary(b3::Opcode::GreaterThan, x, imm);
                    let check = builder.check(cmp);
                    let pc_ = *pc_;
                    builder.procedure.stackmap_append_some_register(check, pc);
                    builder
                        .procedure
                        .stackmap_append_some_register(check, stack);
                    let mut to_restore = vec![];
                    for (&reg, &var) in vars.iter() {
                        let val = builder.var_get(var);
                        to_restore.push(reg);
                        builder.procedure.stackmap_append(
                            check,
                            val,
                            ValueRep::new(b3::ValueRepKind::ColdAny),
                        );
                    }
                    to_restore.sort();

                    builder.procedure.stackmap_set_generator(
                        check,
                        Rc::new(move |jit, params| {
                            let stack = params[2].get_reg().gpr();
                            
                            for (i, param) in params.iter().skip(3).enumerate() {
                              
                                let reg = to_restore[i];
                                let offset = reg as i32 * 4;

                                if param.is_stack() {
                                    jit.load32(
                                        Address::new(
                                            CALL_FRAME_REGISTER,
                                            param.offset_from_fp() as _,
                                        ),
                                        RETURN_VALUE_GPR,
                                    );
                                    jit.store32(RETURN_VALUE_GPR, Address::new(stack, offset));
                                } else if param.is_constant() {
                                    jit.store32(param.value() as i32, Address::new(stack, offset));
                                } else {
                                    jit.store32(param.get_reg().gpr(), Address::new(stack, offset));
                                }
                            }
                            let pc = params[1].get_reg().gpr();
                            jit.store64(pc_ as i32, Address::new(pc, 0));
                            jit.mov(1i32, RETURN_VALUE_GPR);
                            emit_function_epilogue(jit);
                            jit.ret();
                        }),
                    );
                }

                Trace::EnterTrace {} => {}
            }
        }

        builder.jump(Some(loop_block));

        builder.block = entry;
        for (&reg, &var) in vars.iter() {
            let offset = reg as i32 * 4;
            let load = builder.load(b3::Type::Int64, stack, offset, None, None);
            builder.var_set(var, load);
        }

        builder.jump(Some(loop_block));

        let compilation = b3::compile(proc);

        println!("{}", compilation.disassembly());
        compilation
    }
}

impl Interpreter for TracingInterpreter {
    fn stack(&self) -> &[i32] {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Vec<i32> {
        &mut self.stack
    }

    fn pc(&self) -> usize {
        self.pc
    }

    fn pc_mut(&mut self) -> &mut usize {
        &mut self.pc
    }

    fn code(&self) -> &[Op] {
        &self.code
    }

    fn run_jump(&mut self) {
        let old_pc = self.pc;
        let new_pc = match self.code()[self.pc] {
            Op::Jump(pc) => pc,
            _ => unreachable!(),
        };
        
        
        if new_pc < old_pc {
            if self.loops.contains_key(&(new_pc, old_pc)) {
                let info = self.loops.get_mut(&(new_pc, old_pc)).unwrap();
                if info.blacklisted {
                    self.pc = new_pc;
                    return;
                }
                info.hotness += 1;
                if let Some(ref trace) = info.executable_trace {
                    self.pc = new_pc;
                    
                    let stack = &mut self.stack[0];
                    let mut pc = 0;
                    
                    let func: extern "C" fn(*mut i32, *mut usize) -> i32 =
                        { unsafe { std::mem::transmute(trace.code_ref().start()) } };
                    let is_ok = func(stack, &mut pc);
                    self.pc = pc;

                    if is_ok != 0 {   
                       
                        info.fails += 1;
                        if info.fails == 10 {
                            info.blacklisted = true;
                        }
                        return;
                    }
                } else if info.hotness > 1000 && info.executable_trace.is_none() {
                    
                    if !self.recording {
                        self.recording = true;
                        self.pc = new_pc;
                        let mut trace = vec![];
                        let mut recording = RecordingInterpreter {
                            stack: &mut self.stack,
                            pc: &mut self.pc,
                            done: false,
                            trace: &mut trace,
                            code: &self.code,
                            end_of_trace: old_pc,
                            trace_is_too_big: false,
                        };

                        recording.interpret();
                        if recording.done {
                            self.recording = false;
                            info.trace = trace;
                            info.trace_id = self.trace_id;
                            let f = self.translate_trace((new_pc, old_pc));
                            self.trace_id += 1;
                            let info = self.loops.get_mut(&(new_pc, old_pc)).unwrap();
                            info.executable_trace = Some(f);
                            self.pc = new_pc;
                            
                            return;
                        } else if recording.trace_is_too_big {
                            info.fails += 1;

                            if info.fails == 10 {
                                info.blacklisted = true;
                            }
                        }
                    }
                }
            } else {
                self.loops.insert(
                    (new_pc, old_pc),
                    LoopInfo {
                        executable_trace: None,
                        trace: vec![],
                        trace_id: 0,
                        hotness: 1,
                        blacklisted: false,
                        fails: 0,
                    },
                );
                self.recording = false;
            }
        }
        self.pc = new_pc;
    }
}

pub struct RecordingInterpreter<'a> {
    pc: &'a mut usize,
    stack: &'a mut Vec<i32>,
    code: &'a [Op],
    trace: &'a mut Vec<(Trace, usize)>,
    end_of_trace: usize,
    trace_is_too_big: bool,
    done: bool,
}

impl Interpreter for RecordingInterpreter<'_> {
    fn stack(&self) -> &[i32] {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Vec<i32> {
        &mut self.stack
    }

    fn pc(&self) -> usize {
        *self.pc
    }

    fn pc_mut(&mut self) -> &mut usize {
        &mut self.pc
    }

    fn code(&self) -> &[Op] {
        &self.code
    }

    fn run_movi(&mut self) {
        self.trace
            .push((Trace::Instr(self.code()[self.pc()]), self.pc()));
        match self.code()[self.pc()] {
            Op::Movi(r, x) => self.stack_mut()[r as usize] = x,
            _ => unreachable!(),
        }
        *self.pc_mut() += 1;
    }

    fn run_gt(&mut self) {
        match self.code()[self.pc()] {
            Op::Gt(r, x, target) => {
                if self.stack()[r as usize] > x {
                    self.trace
                        .push((Trace::GuardGtJump(r, x, self.pc()), self.pc()));
                    self.trace.push((Trace::Instr(Op::Jump(target)), self.pc()));
                    *self.pc_mut() = target;
                } else {
                    self.trace
                        .push((Trace::GuardGtNJump(r, x, target), self.pc()));
                    self.trace
                        .push((Trace::Instr(Op::Jump(self.pc() + 1)), self.pc()));
                    *self.pc_mut() += 1;
                }
            }
            _ => unreachable!(),
        }
    }

    fn run_add(&mut self) {
        self.trace
            .push((Trace::Instr(self.code()[self.pc()]), self.pc()));
        match self.code()[self.pc()] {
            Op::Add(r, i) => {
                self.stack_mut()[r as usize] += i;
            }
            _ => unreachable!(),
        }
        *self.pc_mut() += 1;
    }
    fn run_jump(&mut self) {
        let end_of_trace = self.pc() == self.end_of_trace;
        self.trace
            .push((Trace::Instr(self.code()[self.pc()]), self.pc()));
        if end_of_trace {
            self.done = true;
            return;
        }
        match self.code()[self.pc()] {
            Op::Jump(target) => {
                *self.pc_mut() = target;
            }
            _ => unreachable!(),
        }
    }
    fn interpret(&mut self) -> i32 {
        loop {
            let ins = self.code()[self.pc()];

            match ins {
                Op::Add { .. } => self.run_add(),
                Op::Jump { .. } => self.run_jump(),
                Op::Ret(r) => return self.stack()[r as usize],
                Op::Gt { .. } => self.run_gt(),
                Op::Movi { .. } => self.run_movi(),
            }
            if self.trace.len() >= 50 {
                self.trace_is_too_big = true;
                self.done = false;
                return 0;
            }
            if self.done {
                return 0;
            }
        }
    }
}

struct SimpleInterpreter {
    stack: Vec<i32>,
    pc: usize,
    code: Vec<Op>,
}
impl Interpreter for SimpleInterpreter {
    fn stack(&self) -> &[i32] {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Vec<i32> {
        &mut self.stack
    }

    fn pc(&self) -> usize {
        self.pc
    }

    fn pc_mut(&mut self) -> &mut usize {
        &mut self.pc
    }

    fn code(&self) -> &[Op] {
        &self.code
    }
}

#[allow(dead_code)]
impl SimpleInterpreter {
    pub fn new(code: Vec<Op>) -> Self {
        Self {
            code,
            stack: vec![0, 0, 0, 0, 0, 0],
            pc: 0,
        }
    }
}

fn main() {
    let code = vec![
        Op::Movi(0, 0),
        Op::Gt(0, 5000000, 4),
        Op::Add(0, 4),
        Op::Jump(1),
        Op::Ret(0),
    ];
    
    let mut tracing = TracingInterpreter::new(code.clone());
    tracing.interpret();
    println!("{:?}", tracing.stack());

    let mut simple = SimpleInterpreter::new(code.clone());
    simple.interpret();
    println!("{:?}", simple.stack());
    
}
