const SOURCE: &'static str = r#"
A mandelbrot set fractal viewer in brainf*** written by Erik Bosman
+++++++++++++[->++>>>+++++>++>+<<<<<<]>>>>>++++++>--->>>>>>>>>>+++++++++++++++[[
>>>>>>>>>]+[<<<<<<<<<]>>>>>>>>>-]+[>>>>>>>>[-]>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>[-]+
<<<<<<<+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>>+>>>>>>>>>>>>>>>>>>>>>>>>>>
>+<<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+[>>>>>>[>>>>>>>[-]>>]<<<<<<<<<[<<<<<<<<<]>>
>>>>>[-]+<<<<<<++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>+<<<<<<+++++++[-[->>>
>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>+<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[[-]>>>>>>[>>>>>
>>[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>
[>>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<
<<]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]>>>>>>>>>+++++++++++++++[[
>>>>>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[
>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>[
-<<+>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<
<<[>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<
[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>
>>>>[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+
<<<<<<[->>>[-<<<+>>>]<<<[->>>+>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>
>>>>>>>]<<<<<<<<<[>>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<<]>>[->>>>>>>>>+<<<<<<<<<]<<
+>>>>>>>>]<<<<<<<<<[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<
<]<+<<<<<<<<<]>>>>>>>>>[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>
>>>>>>>>>>>>>>>>>>>>>>>]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>
>>>>>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+>>>>>>>>>>>>>>>>>>>>>+<<<[<<<<<<
<<<]>>>>>>>>>[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<<<<<<<<<[<<<<<
<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-<<<+>>>]<<<[->
>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<
<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]<<<<<<<[->+>>>-<<<<]>>>>>>>>>+++++++++++++++++++
+++++++>>[-<<<<+>>>>]<<<<[->>>>+<<[-]<<]>>[<<<<<<<+<[-<+>>>>+<<[-]]>[-<<[->+>>>-
<<<<]>>>]>>>>>>>>>>>>>[>>[-]>[-]>[-]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>>>>>>[>>>>>
[-<<<<+>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>[-<<<<<<<<
<+>>>>>>>>>]>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>>>>>>]+>[-
]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>+>>>>>>>>]<<<
<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<[->>[-<<+>>]<
<[->>+>+<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>[->>>>
>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-]<->>>
[-<<<+>[<->-<<<<<<<+>>>>>>>]<[->+<]>>>]<<[->>+<<]<+<<<<<<<<<]>>>>>>>>>[>>>>>>[-<
<<<<+>>>>>]<<<<<[->>>>>+<<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>+>>>>>>>>
]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<[->>[-<<+
>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>
[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-
]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>>>>>
[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>++++++++
+++++++[[>>>>>>>>>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>>>>>>>>[-<<<<<<<+
>>>>>>>]<<<<<<<[->>>>>>>+<<<<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[
-]>>>]<<<<<<<<<[<<<<<<<<<]>>>>+>[-<-<<<<+>>>>>]>[-<<<<<<[->>>>>+<++<<<<]>>>>>[-<
<<<<+>>>>>]<->+>]<[->+<]<<<<<[->>>>>+<<<<<]>>>>>>[-]<<<<<<+>>>>[-<<<<->>>>]+<<<<
[->>>>->>>>>[>>[-<<->>]+<<[->>->[-<<<+>>>]<<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]
+>>>>>>[>>>>>>>>>]>+<]]+>>>[-<<<->>>]+<<<[->>>-<[-<<+>>]<<[->>+<<<<<<<<<<<[<<<<<
<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<
[<<<<<<<<<]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>>>>>>>]<<<<<
<<<+<[>[->>>>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>>[->>>+<<<]<]>[->>>-<<<<<<<<<
<<<<<+>>>>>>>>>>>]<<]>[->>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>>+<<<]<<
<<<<<<<<<<]>>>>[-]<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>[-<->]<[->+<]>>>>>>>>]<<<
<<<<<+<[>[->>>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>[->>>>+<<<<]>]<[->>>>-<<<<<<<
<<<<<<<+>>>>>>>>>>]<]>>[->>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>]>]<[->>>>+<<<<
]<<<<<<<<<<<]>>>>>>+<<<<<<]]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>>>>>>>>>]<<<<<<<<<
[>[->>>>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>>[->>>+<<<]<]>[->>>-<<<<<<<<<<<<<<
+>>>>>>>>>>>]<<]>[->>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>>+<<<]<<<<<<<
<<<<<]]>[-]>>[-]>[-]>>>>>[>>[-]>[-]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-<
<<<+>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[
[>>>>>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+
[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>
[-<<+>>]<<[->>+>+<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<
<[>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[
>[-]<->>>[-<<<+>[<->-<<<<<<<+>>>>>>>]<[->+<]>>>]<<[->>+<<]<+<<<<<<<<<]>>>>>>>>>[
>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>]>
>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>[-]>>>>+++++++++++++++[[>>>>>>>>>]<<<<<<<<<-<<<<<
<<<<[<<<<<<<<<]>>>>>>>>>-]+[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<
<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-
<<<+>>>]<<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>
>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>>>
[-<<<->>>]<<<[->>>+<<<]>>>>>>>>]<<<<<<<<+<[>[->+>[-<-<<<<<<<<<<+>>>>>>>>>>>>[-<<
+>>]<]>[-<<-<<<<<<<<<<+>>>>>>>>>>>>]<<<]>>[-<+>>[-<<-<<<<<<<<<<+>>>>>>>>>>>>]<]>
[-<<+>>]<<<<<<<<<<<<<]]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>
>>>>>>]<<<<<<<<+<[>[->+>>[-<<-<<<<<<<<<<+>>>>>>>>>>>[-<+>]>]<[-<-<<<<<<<<<<+>>>>
>>>>>>>]<<]>>>[-<<+>[-<-<<<<<<<<<<+>>>>>>>>>>>]>]<[-<+>]<<<<<<<<<<<<]>>>>>+<<<<<
]>>>>>>>>>[>>>[-]>[-]>[-]>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>[-]>>>>>[>>>>>>>[-<<<<<
<+>>>>>>]<<<<<<[->>>>>>+<<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>+>[-<-<<<<+>>>>
>]>>[-<<<<<<<[->>>>>+<++<<<<]>>>>>[-<<<<<+>>>>>]<->+>>]<<[->>+<<]<<<<<[->>>>>+<<
<<<]+>>>>[-<<<<->>>>]+<<<<[->>>>->>>>>[>>>[-<<<->>>]+<<<[->>>-<[-<<+>>]<<[->>+<<
<<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>[-<<->>]+<<[->>->[-<<<+>>>]<
<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<
<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>[-<->]<[->+
<]>>>>>>>>]<<<<<<<<+<[>[->>>>+<<[->>-<<<<<<<<<<<<<+>>>>>>>>>>[->>>+<<<]>]<[->>>-
<<<<<<<<<<<<<+>>>>>>>>>>]<]>>[->>+<<<[->>>-<<<<<<<<<<<<<+>>>>>>>>>>]>]<[->>>+<<<
]<<<<<<<<<<<]>>>>>[-]>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]]>>>>[-<<<<+>
>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>>>>>>>]<<<<<<<<+<[>[->>>>+<<<[->>>-
<<<<<<<<<<<<<+>>>>>>>>>>>[->>+<<]<]>[->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<<]>[->>>+<<[
->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>+<<]<<<<<<<<<<<<]]>>>>[-]<<<<]>>>>[-<<<<+>>
>>]<<<<[->>>>+>[-]>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]>>>>>>>>>[>>>>>>
>>>]<<<<<<<<<[>[->>>>+<<<[->>>-<<<<<<<<<<<<<+>>>>>>>>>>>[->>+<<]<]>[->>-<<<<<<<<
<<<<<+>>>>>>>>>>>]<<]>[->>>+<<[->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>+<<]<<<<<<<<
<<<<]]>>>>>>>>>[>>[-]>[-]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>[-]>>>>>[>>>>>[-<<<<+
>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[-<<<<<+>>>>>
]<<<<<[->>>>>+<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>
>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>+>>
>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>[-<<+
>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>
[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-
]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>>>>>
[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<
<<[->>>[-<<<+>>>]<<<[->>>+>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>
>>>]<<<<<<<<<[>>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<<]>>[->>>>>>>>>+<<<<<<<<<]<<+>>>
>>>>>]<<<<<<<<<[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+
<<<<<<<<<]>>>>>>>>>[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>
>>>>>>>>>>>>>>>>>>>]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>>>>>
>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+>>>>>>>>>>>>>>>>>>>>>+<<<[<<<<<<<<<]
>>>>>>>>>[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<<<<<<<<<[<<<<<<<<<
]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-<<<+>>>]<<<[->>>+<
<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>
>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>->>[-<<<<+>>>>]<<<<[->>>>+<<[-]<<]>>]<<+>>>>[-<<<<
->>>>]+<<<<[->>>>-<<<<<<.>>]>>>>[-<<<<<<<.>>>>>>>]<<<[-]>[-]>[-]>[-]>[-]>[-]>>>[
>[-]>[-]>[-]>[-]>[-]>[-]>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-]>>>>]<<<<<<<<<
[<<<<<<<<<]>+++++++++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>+>>>>>>>>>+<<<<<<<<
<<<<<<[<<<<<<<<<]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+[-]>>[>>>>>>>>>]<<<<<
<<<<[>>>>>>>[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<<<<<<[<<<<<<<<<]>>>>>>>[-]+>>>]<<<<
<<<<<<]]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+>>[>+>>>>[-<<<<->>>>]<<<<[->>>
>+<<<<]>>>>>>>>]<<+<<<<<<<[>>>>>[->>+<<]<<<<<<<<<<<<<<]>>>>>>>>>[>>>>>>>>>]<<<<<
<<<<[>[-]<->>>>>>>[-<<<<<<<+>[<->-<<<+>>>]<[->+<]>>>>>>>]<<<<<<[->>>>>>+<<<<<<]<
+<<<<<<<<<]>>>>>>>-<<<<[-]+<<<]+>>>>>>>[-<<<<<<<->>>>>>>]+<<<<<<<[->>>>>>>->>[>>
>>>[->>+<<]>>>>]<<<<<<<<<[>[-]<->>>>>>>[-<<<<<<<+>[<->-<<<+>>>]<[->+<]>>>>>>>]<<
<<<<[->>>>>>+<<<<<<]<+<<<<<<<<<]>+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>+<<<
<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-<<<<<->>>>>]+<<<<<[->>>>>->>[-<<<<<<<+>>>>>>>]<<<<
<<<[->>>>>>>+<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>>>>[-<
<<<<<<->>>>>>>]+<<<<<<<[->>>>>>>-<<[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<<<<<<<<<[<<<
<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<
<<[<<<<<<<<<]>>>>[-]<<<+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>-<<<<<[<<<<<<<
<<]]>>>]<<<<.>>>>>>>>>>[>>>>>>[-]>>>]<<<<<<<<<[<<<<<<<<<]>++++++++++[-[->>>>>>>>
>+<<<<<<<<<]>>>>>>>>>]>>>>>+>>>>>>>>>+<<<<<<<<<<<<<<<[<<<<<<<<<]>>>>>>>>[-<<<<<<
<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+[-]>[>>>>>>>>>]<<<<<<<<<[>>>>>>>>[-<<<<<<<+>>>>>>
>]<<<<<<<[->>>>>>>+<<<<<<<<[<<<<<<<<<]>>>>>>>>[-]+>>]<<<<<<<<<<]]>>>>>>>>[-<<<<<
<<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+>[>+>>>>>[-<<<<<->>>>>]<<<<<[->>>>>+<<<<<]>>>>>>
>>]<+<<<<<<<<[>>>>>>[->>+<<]<<<<<<<<<<<<<<<]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>[-]<-
>>>>>>>>[-<<<<<<<<+>[<->-<<+>>]<[->+<]>>>>>>>>]<<<<<<<[->>>>>>>+<<<<<<<]<+<<<<<<
<<<]>>>>>>>>-<<<<<[-]+<<<]+>>>>>>>>[-<<<<<<<<->>>>>>>>]+<<<<<<<<[->>>>>>>>->[>>>
>>>[->>+<<]>>>]<<<<<<<<<[>[-]<->>>>>>>>[-<<<<<<<<+>[<->-<<+>>]<[->+<]>>>>>>>>]<<
<<<<<[->>>>>>>+<<<<<<<]<+<<<<<<<<<]>+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>
+>>>>>>>>>>>>>>>>>>>>>>>>>>>+<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[-<<<<<<->>>>>>]+<
<<<<<[->>>>>>->>[-<<<<<<<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+<<<<<<<<<<<<<<<<<[<<<<<<<
<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>>>>>[-<<<<<<<<->>>>>>>>]+<<<<<<<<[->>>>>>>>
-<<[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>
>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>[-]<<<++++
+[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>->>>>>>>>>>>>>>>>>>>>>>>>>>>-<<<<<<[<<<<
<<<<<]]>>>]
"#;

#[derive(Copy, Clone, Debug)]
pub enum Token {
    Forward(u32),
    Backward(u32),
    Add(u8),
    Sub(u8),
    Output,
    Input,
    LoopBegin,
    LoopEnd,

    LoopToZero,
}

pub struct BfJIT {
    ctx: CGContext,
}

pub struct CGContext {
    pub opt_level: OptLevel,
    pub regalloc: RegAlloc,
}

use b3::jit::compilation::Compilation;
use b3::jit::reg::Reg;
use b3::{self, BasicBlockBuilder, Frequency, OptLevel};
use criterion::{criterion_group, criterion_main, Criterion};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;

impl BfJIT {
    pub fn new(ctx: CGContext) -> Self {
        Self { ctx }
    }

    pub fn translate(&self, _disasm: bool, input: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let mut chars = input.chars().peekable();
        loop {
            let c = if let Some(c) = chars.next() { c } else { break };
            match c {
                '>' => {
                    let mut n: u32 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'>') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Forward(n));
                }
                '<' => {
                    let mut n: u32 = 1;
                    if self.ctx.opt_level > 1 {
                        while chars.peek() == Some(&'<') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Backward(n));
                }
                '+' => {
                    let mut n: u8 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'+') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Add(n));
                }
                '-' => {
                    let mut n: u8 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'-') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Sub(n));
                }
                '.' => tokens.push(Token::Output),
                ',' => tokens.push(Token::Input),
                '[' => tokens.push(Token::LoopBegin),
                ']' => tokens.push(Token::LoopEnd),
                _ => {}
            };
        }
        if self.ctx.opt_level > 0 {
            tokens = self.opt_inst_combine(&tokens);
        }

        tokens
    }

    fn opt_inst_combine(&self, tokens: &[Token]) -> Vec<Token> {
        let mut ret: Vec<Token> = vec![];
        let mut i: usize = 0;
        loop {
            if i >= tokens.len() {
                break;
            }
            match tokens[i..] {
                [Token::LoopBegin, Token::Sub(1), Token::LoopEnd, ..] => {
                    ret.push(Token::LoopToZero);
                    i += 3;
                }
                /*[Token::LoopBegin, Token::Sub(1), Token::Forward(1), Token::Add(1), Token::Backward(1), Token::LoopEnd, ..] =>
                {
                    ret.push(Token::LoopToAdd);
                    i += 6;
                }*/
                _ => {
                    ret.push(tokens[i]);
                    i += 1;
                }
            }
        }
        ret
    }

    fn translate_b3(&self, _disasm: bool, input: &[Token]) -> Compilation {
        let mut options = b3::Options::default();
        options.opt_level = self.ctx.opt_level;
        options.air_force_linear_scan_allocator = self.ctx.regalloc == RegAlloc::LinearScan;
        options.air_force_irc_allocator = self.ctx.regalloc == RegAlloc::IRC;
        options.air_force_briggs_allocator = self.ctx.regalloc == RegAlloc::Briggs;
        let mut proc = b3::Procedure::new(options);

        let entry = proc.add_block(1.0);
        let mut jumps_to_end = vec![];
        let tape = proc.add_variable(b3::Type::Int64);

        let mut builder = BasicBlockBuilder::new(&mut proc, entry);
        let callee_getchr = builder.const64(getchr as i64);
        let callee_putchar = builder.const64(putchar as i64);
        let arg = builder.argument(Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
        builder.var_set(tape, arg);
        let zero32 = builder.const32(0);
        let _zero64 = builder.const64(0);
        let one64 = builder.const64(1);
        let one32 = builder.const32(1);
        for t in input {
            match *t {
                Token::Forward(n) => {
                    let vtape = builder.var_get(tape);
                    let n = if n == 1 {
                        one64
                    } else {
                        builder.const64(n as i64)
                    };
                    let new_tape = builder.binary(b3::Opcode::Add, vtape, n);
                    builder.var_set(tape, new_tape);
                }

                Token::Backward(n) => {
                    let vtape = builder.var_get(tape);
                    let n = if n == 1 {
                        one64
                    } else {
                        builder.const64(n as i64)
                    };
                    let new_tape = builder.binary(b3::Opcode::Sub, vtape, n);
                    builder.var_set(tape, new_tape);
                }

                Token::Add(n) => {
                    let vtape = builder.var_get(tape);

                    let load = builder.load8z(vtape, 0, None, None);
                    let n = if n == 1 {
                        one32
                    } else {
                        builder.const32(n as i32)
                    };
                    let new_val = builder.binary(b3::Opcode::Add, load, n);
                    builder.store8(new_val, vtape, 0, None, None);
                }

                Token::Sub(n) => {
                    let vtape = builder.var_get(tape);

                    let load = builder.load8z(vtape, 0, None, None);
                    let n = if n == 1 {
                        one32
                    } else {
                        builder.const32(n as i32)
                    };
                    let new_val = builder.binary(b3::Opcode::Sub, load, n);
                    builder.store8(new_val, vtape, 0, None, None);
                }

                Token::Output => {
                    let vtape = builder.var_get(tape);
                    let load = builder.load8z(vtape, 0, None, None);

                    builder.ccall(
                        b3::Type::Void,
                        callee_putchar,
                        &[load],
                        b3::effects::Effects::for_call(),
                    );
                }

                Token::Input => {
                    let vtape = builder.var_get(tape);

                    let val = builder.ccall(
                        b3::Type::Int32,
                        callee_getchr,
                        &[],
                        b3::effects::Effects::for_call(),
                    );
                    builder.store8(val, vtape, 0, None, None);
                }
                Token::LoopBegin => {
                    let start = builder.procedure.add_block(1.0);
                    let body = builder.procedure.add_block(1.0);
                    let end = builder.procedure.add_block(1.0);

                    builder.jump(Some(start));
                    builder = BasicBlockBuilder::new(&mut proc, start);
                    let vtape = builder.var_get(tape);
                    let load = builder.load8z(vtape, 0, None, None);
                    let zero = builder.const32(0);
                    let cond = builder.binary(b3::Opcode::NotEqual, load, zero);
                    builder.branch(cond, body, (end, Frequency::Normal));
                    jumps_to_end.push((start, end));

                    builder = BasicBlockBuilder::new(&mut proc, body);
                }

                Token::LoopEnd => {
                    let (start, end) = jumps_to_end.pop().unwrap();
                    builder.jump(Some(start));
                    builder = BasicBlockBuilder::new(&mut proc, end);
                }

                Token::LoopToZero => {
                    let vtape = builder.var_get(tape);
                    builder.store8(zero32, vtape, 0, None, None);
                }
            }
        }

        let zero = builder.const64(0);
        builder.return_(Some(zero));
        
        let r = b3::compile(proc);
        r
    }
}

extern "C" fn putchar(x: u8) {
    let mut out = ::std::io::stdout();
    out.write_all(&[x]).unwrap();
    out.flush().unwrap();
}
extern "C" fn getchr() -> u8 {
    let mut buf = [0u8; 1];
    std::io::stdin().read_exact(&mut buf).unwrap();
    buf[0]
}

use std::io::{Read, Write};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RegAlloc {
    LinearScan,
    IRC,
    Briggs,
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Brainfuck Mandelbrot Set Compilation");
    group.sample_size(10);
    let mut bf = BfJIT::new(CGContext {
        opt_level: OptLevel::None,
        regalloc: RegAlloc::LinearScan,
    });

    let tokens = bf.translate(false, SOURCE);

    group.bench_with_input("-O0, Linear Scan RA", &OptLevel::None, |b, _| {
        bf.ctx.opt_level = OptLevel::None;
        bf.ctx.regalloc = RegAlloc::LinearScan;

        b.iter_with_large_drop(|| {
            bf.translate_b3(false, &tokens)
        });
    });

    group.bench_with_input("-O0, IRC RA", &OptLevel::None, |b, _| {
        bf.ctx.opt_level = OptLevel::None;
        bf.ctx.regalloc = RegAlloc::IRC;

        b.iter_with_large_drop(|| {
            bf.translate_b3(false, &tokens)
        });
    });

    group.bench_with_input("-O2, Linear Scan RA", &OptLevel::None, |b, _| {
        bf.ctx.opt_level = OptLevel::O2;
        bf.ctx.regalloc = RegAlloc::LinearScan;

        b.iter_with_large_drop(|| {
             bf.translate_b3(false, &tokens)
        });
    });

    group.bench_with_input("-O2, IRC RA", &OptLevel::None, |b, _| {
        bf.ctx.opt_level = OptLevel::O2;
        bf.ctx.regalloc = RegAlloc::IRC;

        b.iter_with_large_drop(|| {
            bf.translate_b3(false, &tokens)
        });
    });

    group.finish();

    let mut group = c.benchmark_group("Brainfuck Mandelbrot Set Runtime");

    group.sample_size(10);
    /*let no_opt_lsra = {
        bf.ctx.opt_level = OptLevel::None;
        bf.ctx.regalloc = RegAlloc::LinearScan;

        bf.translate_b3(false, &tokens)
    };

    let no_opt_irc = {
        bf.ctx.opt_level = OptLevel::None;
        bf.ctx.regalloc = RegAlloc::IRC;

        bf.translate_b3(false, &tokens)
    };*/

    let opt_lsra = {
        bf.ctx.opt_level = OptLevel::O2;
        bf.ctx.regalloc = RegAlloc::LinearScan;

        bf.translate_b3(false, &tokens)
    };

    let opt_irc = {
        bf.ctx.opt_level = OptLevel::O2;
        bf.ctx.regalloc = RegAlloc::IRC;

        bf.translate_b3(false, &tokens)
    };

    let opt_lsra_f = unsafe {
        std::mem::transmute::<_, extern "C" fn(*mut u8)>(opt_lsra.entrypoint(0))
    };

    let opt_irc_f = unsafe {
        std::mem::transmute::<_, extern "C" fn(*mut u8)>(opt_irc.entrypoint(0))
    };

    /*let noopt_lsra_f = unsafe {
        std::mem::transmute::<_, extern "C" fn(*mut u8)>(no_opt_lsra.entrypoint(0))
    };

    let no_opt_irc_f = unsafe {
        std::mem::transmute::<_, extern "C" fn(*mut u8)>(no_opt_irc.entrypoint(0))
    };


    group.bench_function("-O0, Linear Scan RA", |b| {
        b.iter_with_large_drop(|| {
            let mut tape = vec![0u8; 30000];

            noopt_lsra_f(tape.as_mut_ptr());

            tape
        })
    });

    group.bench_function("-O0, IRC RA", |b| {
        b.iter_with_large_drop(|| {
            let mut tape = vec![0u8; 30000];

            no_opt_irc_f(tape.as_mut_ptr());

            tape
        })
    });*/

    group.bench_function("-O2, Linear Scan RA", |b| {
        b.iter_with_large_drop(|| {
            let mut tape = vec![0u8; 30000];

            opt_lsra_f(tape.as_mut_ptr());

            tape
        })
    });

    group.bench_function("-O2, IRC RA", |b| {
        b.iter_with_large_drop(|| {
            let mut tape = vec![0u8; 30000];

            opt_irc_f(tape.as_mut_ptr());

            tape
        })
    });

}
