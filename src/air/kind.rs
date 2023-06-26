use super::{form_table::print_internal, opcode::Opcode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Kind {
    pub opcode: Opcode,
    /// This is an opcode-agnostic flag that indicates that we expect that this instruction will do
    /// any of the following:
    /// - Trap.
    // - Perform some non-arg non-control effect.
    pub effects: bool,
}

impl Default for Kind {
    fn default() -> Self {
        Self {
            opcode: Opcode::Oops,
            effects: true,
        }
    }
}

impl Into<Kind> for Opcode {
    fn into(self) -> Kind {
        Kind {
            opcode: self,
            effects: false,
        }
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_internal(self.opcode, f)?;
        if self.effects {
            write!(f, "<Effects>")?;
        }

        Ok(())
    }
}
