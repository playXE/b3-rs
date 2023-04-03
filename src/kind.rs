use crate::opcode::Opcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Kind {
    opcode: Opcode,
    is_chill: bool,
    traps: bool,
    is_sensitive_to_nan: bool,
}

impl Kind {
    pub fn set_is_chill(&mut self, is_chill: bool) {
        self.is_chill = is_chill;
    }

    pub fn set_traps(&mut self, traps: bool) {
        self.traps = traps;
    }

    pub fn with_traps(self, traps: bool) -> Self {
        Self { traps, ..self }
    }

    pub fn set_sensitive_to_nan(&mut self, is_sensitive_to_nan: bool) {
        self.is_sensitive_to_nan = is_sensitive_to_nan;
    }

    pub const fn has_extra_bits(self) -> bool {
        self.is_chill || self.traps
    }

    pub fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = opcode;
    }

    pub fn opcode(&self) -> Opcode {
        self.opcode
    }

    /// Chill bit. This applies to division-based arithmetic ops, which may trap on some
    /// platforms or exhibit bizarre behavior when passed certain inputs. The non-chill
    /// version will behave as unpredictably as it wants. For example, it's legal to
    /// constant-fold Div(x, 0) to any value or to replace it with any effectful operation.
    /// But when it's chill, that means that the semantics when it would have trapped are
    /// the JS semantics. For example, `Div<Chill>(@a, @b)` means:
    /// ```mustfail
    ///     ((a | 0) / (b | 0)) | 0
    /// ```
    /// And `Mod<Chill>(a, b)` means:
    /// ```mustfail
    ///     ((a | 0) % (b | 0)) | 0
    /// ```
    /// Note that `Div<Chill>` matches exactly how ARM handles integer division.
    pub const fn has_chill(&self) -> bool {
        match self.opcode {
            Opcode::Div | Opcode::Mod => true,
            _ => false,
        }
    }
    pub const fn is_chill(&self) -> bool {
        self.is_chill
    }

    pub const fn has_is_sensitive_to_nan_opcode(op: Opcode) -> bool {
        match op {
            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::DoubleToFloat
            | Opcode::FloatToDouble => true,
            _ => false,
        }
    }

    pub const fn has_traps(&self) -> bool {
        match self.opcode {
            Opcode::Load8Z
            | Opcode::Load8S
            | Opcode::Load16Z
            | Opcode::Load16S
            | Opcode::Load
            | Opcode::Store8
            | Opcode::Store16
            | Opcode::Store
            | Opcode::AtomicWeakCAS
            | Opcode::AtomicStrongCAS
            | Opcode::AtomicXchgAdd
            | Opcode::AtomicXchgSub
            | Opcode::AtomicXchgAnd
            | Opcode::AtomicXchgOr
            | Opcode::AtomicXchgXor
            | Opcode::AtomicXchg => true,
            _ => false,
        }
    }

    pub fn traps(&self) -> bool {
        self.traps
    }

    pub fn has_is_sensitive_to_nan(&self) -> bool {
        Self::has_is_sensitive_to_nan_opcode(self.opcode)
    }

    pub fn is_sensitive_to_nan(&self) -> bool {
        self.is_sensitive_to_nan
    }

    pub const fn new(op: Opcode) -> Self {
        Self {
            opcode: op,
            is_chill: false,
            traps: false,
            is_sensitive_to_nan: false,
        }
    }
}

pub fn chill(k: Kind) -> Kind {
    let mut k = k;
    k.set_is_chill(true);
    k
}

pub fn trapping(k: Kind) -> Kind {
    let mut k = k;
    k.set_traps(true);
    k
}

pub fn sensitive_to_nan(k: Kind) -> Kind {
    let mut k = k;
    k.set_sensitive_to_nan(true);
    k
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.opcode)?;

        if self.is_chill || self.traps || self.is_sensitive_to_nan {
            write!(f, "<")?;
            if self.is_chill {
                write!(f, "Chill")?;
                if self.traps || self.is_sensitive_to_nan {
                    write!(f, ", ")?;
                }
            }
            if self.traps {
                write!(f, "Trapping")?;
                if self.is_sensitive_to_nan {
                    write!(f, ", ")?;
                }
            }
            if self.is_sensitive_to_nan {
                write!(f, "SensitiveToNaN")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}
