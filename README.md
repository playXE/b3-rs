# b3-rs

Backend based on [Bare Bones Backend](https://webkit.org/docs/b3/) from WebKit. I am making it for educational purposes. 

## Usage

See examples and `src/tests.rs`.

Here's iterative factorial generator:

```rust
// imports ARGUMENT_GPR0
use b3::macroassembler::jit::gpr_info::*;

fn main() {
    let opts = b3::Options::default();
    let mut proc = b3::Procedure::new(opts);

    // Create entry block. The argument to `add_block` is block frequency. 
    // Blocks with different frequences are ordered differently. By default
    // they are recalculated before lowering to Assembly IR.
    // set `opts.estimate_static_execution_counts` to `false` to use user provided frequency.
    let entry = proc.add_block(1.0);

    let mut builder = b3::BasicBlockBuilder::new(entry, &mut proc);

    // Argument management is offloaded to client code. Here we load argument from first GPR argument register,
    // it is RDI on x86-64
    let number = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int32);

    // Declare variables. They are automatically converted to SSA form.
    let i = builder.procedure.add_variable(b3::Type::Int32);
    let factorial = builder.procedure.add_variable(b3::Type::Int32);

    // Create blocks for `for` loop. 
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

    // Conditional branch.
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

    // Compiles B3 IR down to machine code. 
    let compilation = b3::compile(proc);
    // use `entrypoint(0)` to get first entrypoint of code. 
    // NOTE: Multiple entrypoints are not yet supported.
    let func: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(compilation.entrypoint(0)) };

    assert_eq!(func(5), 120);
}
```