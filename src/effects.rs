use std::ops::Range;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Effects {
    /// True if this cannot continue execution in the current block.
    pub terminal: bool,

    /// True if this value can cause execution to terminate abruptly, and that this abrupt termination is
    /// observable. An example of how this gets used is to limit the hoisting of controlDependent values.
    /// Note that if exitsSideways is set to true but reads is bottom, then codegen is free to assume that
    /// after abrupt termination of this procedure, none of the heap will be read. That's usually false,
    /// so make sure that reads corresponds to the set of things that are readable after this function
    /// terminates abruptly.
    pub exit_sideways: bool,

    /// True if the instruction may change semantics if hoisted above some control flow. For example,
    /// loads are usually control-dependent because we must assume that any control construct (either
    /// a terminal like Branch or anything that exits sideways, like Check) validates whether the
    /// pointer is valid. Hoisting the load above control may cause the load to trap even though it
    /// would not have otherwise trapped.
    pub control_dependant: bool,

    /// True if this writes to the local state. Operations that write local state don't write to anything
    /// in "memory" but they have a side-effect anyway. This is for modeling Upsilons, Sets, and Fences.
    /// This is a way of saying: even though this operation is not a terminal, does not exit sideways,
    /// and does not write to the heap, you still cannot kill this operation.
    pub writes_local_state: bool,

    /// True if this reads from the local state. This is only used for Phi and Get.
    pub reads_local_state: bool,

    /// Memory fences cannot be reordered around each other regardless of their effects. This is flagged
    /// if the operation is a memory fence.
    pub fence: bool,

    pub writes: Range<usize>,
    pub reads: Range<usize>,
}

impl Effects {
    pub fn none() -> Self {
        Self::default()
    }

    pub fn for_call() -> Self {
        let mut this = Self::default();

        this.exit_sideways = true;
        this.control_dependant = true;
        this.writes = 0..usize::MAX;
        this.reads = 0..usize::MAX;
        this.fence = true;

        this
    }

    pub fn for_check() -> Self {
        let mut this = Self::default();

        this.exit_sideways = true;
        this.reads = 0..usize::MAX;
        this
    }

    pub fn must_execute(&self) -> bool {
        self.terminal
            || self.exit_sideways
            || self.writes_local_state
            || self.writes != (0..0)
            || self.fence
    }

    pub fn interferes(&self, other: &Effects) -> bool {
        interferes_with_terminal(self, other)
            || interferes_with_exit_sideways(self, other)
            || interferes_with_writes_local_state(self, other)
            || interferes_with_writes_local_state(other, self)
            || interferes_with_exit_sideways(other, self)
            || interferes_with_terminal(other, self)
            || ((self.writes != (0..0) && other.writes != (0..0))
                && (self.writes.start < other.writes.end && self.writes.end > other.writes.start))
            || ((self.writes != (0..0) && other.writes != (0..0))
                && (self.writes.start < other.reads.end && self.writes.end > other.reads.start))
            || ((self.reads != (0..0) && other.writes != (0..0))
                && (self.reads.start < other.writes.end && self.reads.end > other.writes.start))
            || (self.fence && other.fence)
    }
}

fn interferes_with_terminal(terminal: &Effects, other: &Effects) -> bool {
    if !terminal.terminal {
        return false;
    }

    other.terminal || other.control_dependant || other.writes_local_state || other.writes != (0..0)
}

fn interferes_with_exit_sideways(exit_side_ways: &Effects, other: &Effects) -> bool {
    if !exit_side_ways.exit_sideways {
        return false;
    }

    other.exit_sideways || other.writes != (0..0)
}

fn interferes_with_writes_local_state(writes_local_state: &Effects, other: &Effects) -> bool {
    if !writes_local_state.writes_local_state {
        return false;
    }

    other.writes_local_state || other.reads_local_state
}
