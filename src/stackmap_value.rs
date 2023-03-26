use std::{hash::Hash, rc::Rc};

use crate::{jit::register_set::RegisterSetBuilder, value::ValueRep};

#[derive(Clone)]
pub struct StackMapValue {
    pub reps: Vec<ValueRep>,
    pub generator: Option<Rc<dyn FnOnce()>>,
    pub early_clobbered: RegisterSetBuilder,
    pub late_clobbered: RegisterSetBuilder,
    pub used_registers: RegisterSetBuilder,
}

impl PartialEq for StackMapValue {
    fn eq(&self, other: &Self) -> bool {
        self.reps == other.reps
            && self
                .generator
                .as_ref()
                .zip(other.generator.as_ref())
                .map(|(a, b)| Rc::ptr_eq(a, b))
                .unwrap_or(false)
            && self.early_clobbered == other.early_clobbered
            && self.late_clobbered == other.late_clobbered
            && self.used_registers == other.used_registers
    }
}

impl Eq for StackMapValue {}

impl std::fmt::Debug for StackMapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StackMapValue")
            .field("reps", &self.reps)
            .field("early_clobbered", &self.early_clobbered)
            .field("late_clobbered", &self.late_clobbered)
            .field("used_registers", &self.used_registers)
            .finish()
    }
}

impl Hash for StackMapValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.reps.hash(state);
        self.generator.as_ref().map(|x| Rc::as_ptr(x)).hash(state);
        self.early_clobbered.hash(state);
        self.late_clobbered.hash(state);
        self.used_registers.hash(state);
    }
}
