use macroassembler::assembler::{
    abstract_macro_assembler::{Address, Jump, JumpList, Label},
    TargetMacroAssembler,
};

use crate::{
    jit::{register_at_offset::RegisterAtOffsetList, register_set::RegisterSetBuilder},
    utils::{index_set::IndexMap, phase_scope::phase_scope},
    width::Width,
};

use super::{
    allocate_registers_and_stack_by_linear_scan::allocate_registers_and_stack_by_linear_scan,
    basic_block::BasicBlockId, block_order::optimize_block_order, code::Code,
    eliminate_dead_code::eliminate_dead_code, form_table::is_return,
    generation_context::GenerationContext, lower_after_regalloc::lower_after_regalloc,
    lower_macros::lower_macros, lower_stack_args::lower_stack_args, opcode::Opcode,
    simplify_cfg::simplify_cfg, lower_entry_switch::lower_entry_switch,
};

pub fn prepare_for_generation(code: &mut Code<'_>) {
    phase_scope("air::prepare_for_generation", || {
        code.reset_reachability();
        simplify_cfg(code);
        lower_macros(code);
        eliminate_dead_code(code);
        // TODO: Port IRC and "fast" -O0 allocator from original source code.

        // When we're compiling quickly, we do register and stack allocation in one linear scan
        // phase. It's fast because it computes liveness only once.
        allocate_registers_and_stack_by_linear_scan(code);
        // We may still need to do post-allocation lowering. Doing it after both register and
        // stack allocation is less optimal, but it works fine.
        lower_after_regalloc(code);
        // This turns all Stack and CallArg Args into Addr args that use the frame pointer.
        lower_stack_args(code);
        lower_entry_switch(code);
        //report_used_registers(code);
        // If we coalesced moves then we can unbreak critical edges. This is the main reason for this
        // phase.
        simplify_cfg(code);
        code.reset_reachability();
        optimize_block_order(code);
    });
}

fn generate_with_already_allocated_registers<'a>(
    code: &'a mut Code<'a>,
    jit: &mut TargetMacroAssembler,
) {
    println!("{}", code);
    //phase_scope("air::generate", || {
    let mut context = GenerationContext {
        late_paths: vec![],
        block_labels: IndexMap::with_capacity(code.blocks.len()),
        current_block: None,
        index_in_block: 0,
        code,
    };

    for block in (0..context.code.blocks.len()).map(BasicBlockId) {
        context.block_labels.insert(block, Box::new(Label::unset()));
    }

    let mut block_jumps = IndexMap::with_capacity(context.code.blocks.len());

    let link = |ctx: &mut GenerationContext,
                block_jumps: &mut IndexMap<JumpList, BasicBlockId>,
                jit: &mut TargetMacroAssembler,
                jump: Jump,
                target| {
        if ctx.block_labels.get(&target).unwrap().is_set() {
            jump.link_to(jit, **ctx.block_labels.get(&target).unwrap());
            return;
        }

        block_jumps
            .entry(target)
            .or_insert_with(|| JumpList::new())
            .push(jump);
    };

    for block_id in (0..context.code.blocks.len()).map(BasicBlockId) {
        context.current_block = Some(block_id);
        context.index_in_block = usize::MAX;


        block_jumps.get(&block_id).map(|jumps: &JumpList| {
            jumps.link(jit);
        });

        let label = jit.label();
        *context.block_labels.get_mut(&block_id).unwrap() = Box::new(label);


        if let Some(entrypoint_index) = context.code.entrypoint_index(block_id) {
            jit.comment(format!("entrypoint {}", entrypoint_index));

            (context.code.prologue_generator_for_entrypoint(entrypoint_index).clone().unwrap())(jit, context.code);
        }

        for i in 0..context.code.block(block_id).insts.len() - 1 {
            context.index_in_block = i;
            let inst = context.code.block(block_id).insts[i].clone();
            //let start = jit.label_ignoring_watchpoints();
            let _jump = inst.generate(jit, &mut context);
        }

        context.index_in_block = context.code.block(block_id).insts.len() - 1;

        // falthrough
        if context.code.block(block_id).last().unwrap().kind.opcode == Opcode::Jump
            && context.code.find_next_block_index(block_id.0)
                == Some(context.code.block(block_id).successors[0].0 .0)
        {
            continue;
        }

        if is_return(context.code.block(block_id).last().unwrap().kind.opcode) {
            context.code.emit_epilogue(jit);
            continue;
        }

        let jump = context
            .code
            .block(block_id)
            .last()
            .cloned()
            .unwrap()
            .generate(jit, &mut context);

        if jump.is_set() {
            match context.code.block(block_id).successors.len() {
                1 => {
                    let succ0 = context.code.block(block_id).successors[0].0;
                    link(&mut context, &mut block_jumps, jit, jump, succ0);
                }

                2 => {
                    let succ0 = context.code.block(block_id).successors[0].0;
                    let succ1 = context.code.block(block_id).successors[1].0;

                    link(&mut context, &mut block_jumps, jit, jump, succ0);
                    if Some(succ1.0) != context.code.find_next_block_index(block_id.0) {
                        let j = jit.jump();
                        link(&mut context, &mut block_jumps, jit, j, succ1);
                    }
                }
                _ => unreachable!(),
            }
        }

        context.current_block = None;

        for late_path in std::mem::take(&mut context.late_paths) {
            late_path(jit, &mut context);
        }
    }
    //});
}

pub fn emit_restore(jit: &mut TargetMacroAssembler, list: &RegisterAtOffsetList, base_gpr: u8) {
    jit.comment(format!("emitRestore {}", list));

    let reg_count = list.register_count();

    let mut i = 0;

    while i < reg_count {
        let entry = list[i];
        if !entry.reg().is_gpr() {
            break;
        }

        jit.load64(
            Address::new(base_gpr, entry.offset() as _),
            entry.reg().gpr(),
        );
        i += 1;
    }

    while i < reg_count {
        let entry = list[i];
        if !entry.reg().is_fpr() {
            break;
        }

        jit.load_double(
            Address::new(base_gpr, entry.offset() as _),
            entry.reg().fpr(),
        );
        i += 1;
    }
}

pub fn generate<'a>(code: &'a mut Code<'a>, jit: &mut TargetMacroAssembler) {
    generate_with_already_allocated_registers(code, jit);
}

pub fn emit_callee_saves_for(jit: &mut TargetMacroAssembler, callee_saves: &RegisterAtOffsetList) {
    let dont_save_registers = RegisterSetBuilder::stack_registers();

    jit.comment(format!(
        "emitCalleeSavesFor: {} dontRestore: {}",
        callee_saves, dont_save_registers
    ));

    let mut i = 0;

    let reg_count = callee_saves.register_count();

    while i < reg_count {
        let entry = callee_saves[i];

        if entry.reg().is_fpr() {
            break;
        }

        if dont_save_registers.contains(entry.reg(), Width::W64) {
            continue;
        }

        jit.store64(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            Address::new(entry.reg().gpr(), entry.offset() as _),
        );
        i += 1;
    }

    while i < reg_count {
        let entry = callee_saves[i];

        if dont_save_registers.contains(entry.reg(), Width::W64) {
            continue;
        }

        jit.store_double(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            Address::new(entry.reg().fpr(), entry.offset() as _),
        );
        i += 1;
    }
}

pub fn emit_restore_callee_saves_for(
    jit: &mut TargetMacroAssembler,
    callee_saves: &RegisterAtOffsetList,
) {
    let dont_restore_registers = RegisterSetBuilder::stack_registers();

    jit.comment(format!(
        "emitRestoreCalleeSavesFor: {} dontSave: {}",
        callee_saves, dont_restore_registers
    ));

    let mut i = callee_saves.register_count();

    while i > 0 {
        i -= 1;

        let entry = callee_saves[i];

        if entry.reg().is_fpr() {
            break;
        }

        if dont_restore_registers.contains(entry.reg(), Width::W64) {
            continue;
        }

        jit.load64(
            Address::new(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                entry.offset() as _,
            ),
            entry.reg().gpr(),
        );
    }

    while i > 0 {
        i -= 1;

        let entry = callee_saves[i];

        if dont_restore_registers.contains(entry.reg(), Width::W64) {
            continue;
        }

        jit.load_double(
            Address::new(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                entry.offset() as _,
            ),
            entry.reg().fpr(),
        );
    }
}

pub fn emit_function_epilogue(jit: &mut TargetMacroAssembler) {
    #[cfg(target_arch = "x86_64")]
    {
        jit.mov(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
        jit.pop(TargetMacroAssembler::FRAME_POINTER_REGISTER);
    }
}

pub fn emit_function_epilogue_with_empty_frame(jit: &mut TargetMacroAssembler) {
    #[cfg(target_arch = "x86_64")]
    {
        jit.pop(TargetMacroAssembler::FRAME_POINTER_REGISTER);
    }
}

pub fn emit_function_prologue(jit: &mut TargetMacroAssembler) {
    #[cfg(target_arch = "x86_64")]
    {
        jit.push(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        jit.mov(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
        );
    }
}

pub fn preserve_return_address_after_call(jit: &mut TargetMacroAssembler, reg: u8) {
    #[cfg(target_arch = "x86_64")]
    {
        jit.push(reg);
    }
}

pub fn restore_return_address_after_call(jit: &mut TargetMacroAssembler, reg: u8) {
    #[cfg(target_arch = "x86_64")]
    {
        jit.pop(reg);
    }
}

pub fn emit_save(jit: &mut TargetMacroAssembler, list: &RegisterAtOffsetList) {
    jit.comment(format!("emitSave {}", list));

    let mut i = 0;

    while i < list.register_count() {
        let entry = list[i];
        if !entry.reg().is_gpr() {
            break;
        }

        jit.store64(
            entry.reg().gpr(),
            Address::new(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                entry.offset() as _,
            ),
        );
        i += 1;
    }

    while i < list.register_count() {
        let entry = list[i];
        if !entry.reg().is_fpr() {
            break;
        }

        jit.store_double(
            entry.reg().fpr(),
            Address::new(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                entry.offset() as _,
            ),
        );
        i += 1;
    }
}
