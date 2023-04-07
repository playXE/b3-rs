use crate::jit::register_at_offset::round_up_to_multiple_of;

use super::{code::Code, stack_slot::StackSlotId};

fn update_frame_size_based_on_stack_slots_impl<'a>(
    code: &mut Code<'_>,
    collection: impl Iterator<Item = StackSlotId> + 'a,
) {
    let mut frame_size = 0;

    for slot in collection {
        frame_size = frame_size.max((-code.stack_slot(slot).offset_from_fp) as usize);
    }

    code.frame_size = round_up_to_multiple_of(16, frame_size as _) as _;
}

pub fn attempt_assignment(
    code: &mut Code<'_>,
    slot: StackSlotId,
    mut offset_from_fp: isize,
    other_slots: &[StackSlotId],
) -> bool {
    offset_from_fp =
        -round_up_to_multiple_of(code.stack_slot(slot).alignment() as _, -offset_from_fp);

    for other_slot in other_slots.iter().copied() {
        if code.stack_slot(other_slot).offset_from_fp == 0 {
            continue;
        }

        let overlap: bool = ranges_overlap(
            offset_from_fp,
            offset_from_fp + code.stack_slot(slot).byte_size as isize,
            code.stack_slot(other_slot).offset_from_fp,
            code.stack_slot(other_slot).offset_from_fp
                + code.stack_slot(other_slot).byte_size as isize,
        );

        if overlap {
            return false;
        }
    }

    code.stack_slot_mut(slot).offset_from_fp = offset_from_fp;

    true
}

pub fn assign(code: &mut Code<'_>, slot: StackSlotId, other_slots: &[StackSlotId]) {
    let byte_size = code.stack_slot(slot).byte_size as isize;
    if attempt_assignment(code, slot, -byte_size, other_slots) {
        return;
    }

    for other_slot in other_slots.iter().copied() {
        if code.stack_slot(other_slot).offset_from_fp == 0 {
            continue;
        }

        let offset_from_fp =
            code.stack_slot(other_slot).offset_from_fp - code.stack_slot(slot).byte_size as isize;

        if attempt_assignment(code, slot, offset_from_fp, other_slots) {
            return;
        }
    }
}

pub fn allocate_and_get_escaped_slots_without_changing_frame_size(
    code: &mut Code<'_>,
) -> Vec<StackSlotId> {
    debug_assert!(code.frame_size == 0);
    let mut assigned_escaped_stack_slots = Vec::new();
    let mut escaped_stack_slot_worklist = Vec::new();

    for slot in (0..code.proc.stack_slots.len()).map(StackSlotId) {
        if code.stack_slot(slot).is_locked() {
            if code.stack_slot(slot).offset_from_fp != 0 {
                assigned_escaped_stack_slots.push(slot);
            } else {
                escaped_stack_slot_worklist.push(slot);
            }
        } else {
            // It would be super strange to have an unlocked stack slot that has an offset already.
            debug_assert!(code.stack_slot(slot).offset_from_fp == 0);
        }
    }

    // This is a fairly expensive loop, but it's OK because we'll usually only have a handful of
    // escaped stack slots.
    while let Some(slot) = escaped_stack_slot_worklist.pop() {
        assign(code, slot, &assigned_escaped_stack_slots);
        assigned_escaped_stack_slots.push(slot);
    }

    assigned_escaped_stack_slots
}

pub fn allocate_escaped_stack_slots(code: &mut Code<'_>) {
    let slots = allocate_and_get_escaped_slots_without_changing_frame_size(code);
    update_frame_size_based_on_stack_slots_impl(code, slots.into_iter())
}

pub fn update_frame_size_based_on_stack_slots(code: &mut Code<'_>) {
    let collection = (0..code.proc.stack_slots.len()).map(StackSlotId);
    update_frame_size_based_on_stack_slots_impl(code, collection);
}

fn ranges_overlap(left_min: isize, left_max: isize, right_min: isize, right_max: isize) -> bool {
    debug_assert!(left_min < left_max);
    debug_assert!(right_min < right_max);

    left_max > right_min && right_max > left_min
}
