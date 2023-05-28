use b3::jit::compilation::Compilation;
use b3::jit::reg::Reg;
use b3::{self, BasicBlockBuilder, Frequency, OptLevel};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;

#[global_allocator]
static MI: mimalloc::MiMalloc = mimalloc::MiMalloc;

pub struct BfJIT {
    ctx: CGContext,
}
pub struct CGContext {
    pub opt_level: OptLevel,
    pub regalloc: RegAlloc,
}
#[derive(Copy, Clone, Debug)]
enum Token {
    Forward(u32),
    Backward(u32),
    Add(u8),
    Sub(u8),
    Output,
    Input,
    LoopBegin,
    LoopEnd,

    LoopToZero,
    #[allow(dead_code)]
    LoopToAdd,
}

impl BfJIT {
    pub fn new(ctx: CGContext) -> Self {
        Self { ctx }
    }

    pub fn translate(&self, disasm: bool, input: &str) -> Compilation {
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

        self.do_translate(disasm, &tokens)
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

    fn do_translate(&self, _disasm: bool, input: &[Token]) -> Compilation {
        let mut options = b3::Options::default();
        options.opt_level = self.ctx.opt_level;
        options.air_force_linear_scan_allocator = self.ctx.regalloc == RegAlloc::LinearScan;
        options.air_force_irc_allocator = self.ctx.regalloc == RegAlloc::IRC;
        options.air_force_briggs_allocator = self.ctx.regalloc == RegAlloc::Briggs;
        options.enable_sccp = true;
        options.dump_b3_at_each_phase = true;
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

                    builder = BasicBlockBuilder::new( &mut proc, body);
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

                Token::LoopToAdd => {
                    /*
                    masm.load8(Address::new(edi, 0), esi);
                    masm.add32(esi, Address::new(edi, 1));
                    masm.store8(0i32, Address::new(edi, 0)); */
                    let vtape = builder.var_get(tape);
                    let load = builder.load8z(vtape, 0, None, None);
                    let load1 = builder.load8z(vtape, 1, None, None);
                    let add = builder.binary(b3::Opcode::Add, load, load1);

                    builder.store8(add, vtape, 0, None, None);
                    builder.store8(zero32, vtape, 0, None, None);
                }
            }
        }

        let zero = builder.const64(0);
        builder.return_(Some(zero));
        let start = std::time::Instant::now();
        let r = b3::compile(proc);
        println!("compile time: {:?}", start.elapsed());
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

impl std::fmt::Display for RegAlloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

use std::ffi::OsStr;
use std::path::PathBuf;
fn parse_path(s: &OsStr) -> Result<PathBuf, &'static str> {
    Ok(s.into())
}


fn main() {
    let mut args = pico_args::Arguments::from_env();
    let regalloc = args
        .opt_value_from_os_str("--regAlloc", |key| {
            Ok::<_, String>(match key.to_string_lossy().to_lowercase().as_str() {
                "linearscan" => RegAlloc::LinearScan,
                "irc" => RegAlloc::IRC,
                "briggs" => RegAlloc::Briggs,
                _ => RegAlloc::IRC,
            })
        })
        .unwrap()
        .unwrap_or_else(|| RegAlloc::LinearScan);
    let opt_level = args.opt_value_from_str::<_, u8>("--optLevel").unwrap();

    let opt_level = match opt_level {
        None => OptLevel::O1,
        Some(n) => match n {
            0 | 1 => OptLevel::O1,
            2 => OptLevel::O2,
            _ => OptLevel::O3,
        },
    };

    let input = args
        .opt_value_from_os_str(["-i", "--input"], parse_path)
        .unwrap();

    if input.is_none() {
        println!(
            "No input provided. To compile brainfuck program run `brainfuck -i <program_name>"
        );
        return;
    }

    let input = input.unwrap();

    println!("Opt level: {:?}, Regalloc: {:?}", opt_level, regalloc);

    let jit = BfJIT::new(CGContext {
        opt_level,
        regalloc,
    });

    let code = jit.translate(false, &std::fs::read_to_string(input).unwrap());

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("jitdump.s")
        .unwrap();
    file.write_all(code.disassembly().as_bytes()).unwrap();

    let fun =
        unsafe { std::mem::transmute::<_, extern "C" fn(*mut u8) -> i64>(code.code_ref().start()) };
    let mut mem = vec![0u8; 100 * 1024];
    let start = std::time::Instant::now();
    unsafe {
        fun(mem.as_mut_ptr().add(50 * 1024));
    }
    drop(code);
    drop(mem);
    println!(
        "Executed in {:.2}ms",
        start.elapsed().as_micros() as f64 / 1000.0
    );
}
