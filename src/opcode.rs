use crate::{bank::Bank, kind::Kind, typ::Type, width::Width};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    /// A no-op that returns Void, useful for when you want to remove a value.
    Nop,

    /// Polymorphic identity, usable with any value type.
    Identity,

    /// This is an identity, but we prohibit the compiler from realizing this until the bitter end. This can
    /// be used to block reassociation and other compiler reasoning, if we find that it's wrong or
    /// unprofitable and we need an escape hatch.
    Opaque,

    /// Constants. Constants exist in the control flow, so that we can
    /// reason about where we would construct them. Large constants are expensive to create.
    Const32,
    Const64,
    ConstDouble,
    ConstFloat,
    Const128,

    /// Tuple filled with zeros. This appears when Tuple Patchpoints are replaced with Bottom values.
    BottomTuple,

    /// We support non-SSA variables. These are accessed using Get and Set opcodes.
    /// It's a good idea to run fix_ssa() to turn these into SSA. The
    /// optimizer will do that eventually, but if your input tends to use these opcodes, you
    /// should run fixSSA() directly before launching the optimizer.
    Set,
    Get,

    /// Gets the base address of a StackSlot.
    SlotBase,

    /// The magical argument register. This is viewed as executing at the top of the program
    /// regardless of where in control flow you put it, and the compiler takes care to ensure that we
    /// don't clobber the value by register allocation or calls (either by saving the argument to the
    /// stack or preserving it in a callee-save register). Use the ArgumentRegValue class. The return
    /// type is either pointer() (for GPRs) or Double (for FPRs).
    ArgumentReg,

    /// The frame pointer. You can put this anywhere in control flow but it will always yield the
    /// frame pointer, with a caveat: if our compiler changes the frame pointer temporarily for some
    /// silly reason, the FramePointer intrinsic will return where the frame pointer *should* be not
    /// where it happens to be right now.
    FramePointer,

    /// Polymorphic math, usable with any value type.
    Add,
    Sub,
    Mul,
    Div,
    /// All bets are off as to what will happen when you execute this for -2^31/-1 and x/0.
    UDiv,
    Mod,
    /// All bets are off as to what will happen when you execute this for -2^31%-1 and x%0.
    UMod,

    /// Polymorphic negation. Note that we only need this for floating point, since integer negation
    /// is exactly like Sub(0, x). But that's not true for floating point. Sub(0, 0) is 0, while
    /// Neg(0) is -0. Also, we canonicalize Sub(0, x) into Neg(x) in case of integers.
    Neg,

    /// Integer math.
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    SShr,
    /// Arithmetic Shift.
    ZShr,
    /// Logical Shift.
    RotR,
    /// Rotate Right.
    RotL,
    /// Rotate Left.
    Clz,
    /// Count leading zeros.

    /// Floating point math.
    Abs,
    Ceil,
    Floor,
    Sqrt,
    FMax,
    FMin,

    /// Casts and such.
    /// Bitwise Cast of Double->Int64 or Int64->Double
    BitwiseCast,
    /// Takes and returns Int32:
    SExt8,
    SExt16,
    /// Takes Int32 and returns Int64:
    SExt8To64,
    SExt16To64,
    SExt32,
    ZExt32,
    /// Does a bitwise truncation of Int64->Int32 and Double->Float:
    Trunc,
    /// Takes ints and returns floating point value.
    IToD,
    IToF,
    FToI,
    DToI,
    /// Convert between double and float.
    FloatToDouble,
    DoubleToFloat,

    /// Polymorphic comparisons, usable with any value type. Returns int32 0 or 1. Note that "Not"
    /// is just Equal(x, 0), and "ToBoolean" is just NotEqual(x, 0).
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    /// Integer comparisons. Returns int32 0 or 1.
    Above,
    Below,
    AboveEqual,
    BelowEqual,

    /// Unordered floating point compare: values are equal or either one is NaN.
    EqualOrUnordered,

    /// SSA form of conditional move. The first child is evaluated for truthiness. If true, the second child
    /// is returned. Otherwise, the third child is returned.
    Select,

    /// Memory loads. Opcode indicates how we load and the loaded type. These use MemoryValue.
    /// These return Int32:
    Load8Z,
    Load8S,
    Load16Z,
    Load16S,
    /// This returns whatever the return type is:
    Load,

    /// Memory stores. Opcode indicates how the value is stored. These use MemoryValue.
    /// These take an Int32 value:
    Store8,
    Store16,
    /// This is a polymorphic store for Int32, Int64, Float, and Double.
    Store,

    /// Atomic compare and swap that returns a boolean. May choose to do nothing and return false. You can
    /// usually assume that this is faster and results in less code than AtomicStrongCAS, though that's
    /// not necessarily true on Intel, if instruction selection does its job. Imagine that this opcode is
    /// as if you did this atomically:
    /// ```mustfail
    /// template<typename T>
    /// bool AtomicWeakCAS(T expectedValue, T newValue, T* ptr)
    /// {
    ///     if (!rand())
    ///         return false; /// Real world example of this: context switch on ARM while doing CAS.
    ///     if (*ptr != expectedValue)
    ///         return false;
    ///     *ptr = newValue;
    ///     return true;
    /// }
    // ```
    /// Note that all atomics put the pointer last to be consistent with how loads and stores work. This
    /// is a goofy tradition, but it's harmless, and better than being inconsistent.
    //
    /// Note that weak CAS has no fencing guarantees when it fails. This means that the following
    /// transformation is always valid:
    //
    /// Before:
    //
    ///         Branch(AtomicWeakCAS(expected, new, ptr))
    ///       Successors: Then:#success, Else:#fail
    //
    /// After:
    //
    ///         Branch(Equal(Load(ptr), expected))
    ///       Successors: Then:#attempt, Else:#fail
    ///     BB#attempt:
    ///         Branch(AtomicWeakCAS(expected, new, ptr))
    ///       Successors: Then:#success, Else:#fail
    //
    /// Both kinds of CAS for non-canonical widths (Width8 and Width16) ignore the irrelevant bits of the
    /// input.
    AtomicWeakCAS,

    /// Atomic compare and swap that returns the old value. Does not have the nondeterminism of WeakCAS.
    /// This is a bit more code and a bit slower in some cases, though not by a lot. Imagine that this
    /// opcode is as if you did this atomically:
    /// ```mustfail
    /// template<typename T>
    /// T AtomicStrongCAS(T expectedValue, T newValue, T* ptr)
    /// {
    ///     T oldValue = *ptr;
    ///     if (oldValue == expectedValue)
    ///         *ptr = newValue;
    ///     return oldValue
    /// }
    /// ```
    /// AtomicStrongCAS sign-extends its result for subwidth operations.
    //
    /// Note that AtomicWeakCAS and AtomicStrongCAS sort of have this kind of equivalence:
    //
    /// AtomicWeakCAS(@exp, @new, @ptr) == Equal(AtomicStrongCAS(@exp, @new, @ptr), @exp)
    //
    /// Assuming that the WeakCAS does not spuriously fail, of course.
    AtomicStrongCAS,

    /// Atomically ___ a memory location and return the old value. Syntax:
    //
    /// @oldValue = AtomicXchg___(@operand, @ptr)
    //
    /// For non-canonical widths (Width8 and Width16), these return sign-extended results and ignore the
    /// irrelevant bits of their inputs.
    AtomicXchgAdd,
    AtomicXchgAnd,
    AtomicXchgOr,
    AtomicXchgSub,
    AtomicXchgXor,


    /// Atomically exchange a value with a memory location. Syntax:
    //
    /// @oldValue = AtomicXchg(@newValue, @ptr)
    AtomicXchg,

    /// Introduce an invisible dependency for blocking motion of loads with respect to each other. Syntax:
    //
    /// @result = Depend(@phantom)
    //
    /// This is eventually codegenerated to have local semantics as if we did:
    //
    /// @result = $0
    //
    /// But it ensures that the users of @result cannot execute until @phantom is computed.
    //
    /// The compiler is not allowed to reason about the fact that Depend codegenerates this way. Any kind
    /// of transformation or analysis that relies on the insight that Depend is really zero is unsound,
    /// because it unlocks reordering of users of @result and @phantom.
    //
    /// On X86, this is lowered to a load-load fence and @result folds to zero.
    //
    /// On ARM, this is lowered as if like:
    //
    /// @result = BitXor(@phantom, @phantom)
    //
    /// Except that the compiler never gets an opportunity to simplify out the BitXor.
    Depend,

    /// This is used to compute the actual address of a Wasm memory operation. It takes an IntPtr
    /// and a pinned register then computes the appropriate IntPtr address. For the use-case of
    /// Wasm it is important that the first child initially be a ZExt32 so the top bits are cleared.
    /// We do WasmAddress(ZExt32(ptr), ...) so that we can avoid generating extraneous moves in Air.
    WasmAddress,

    /// This is used to represent standalone fences - i.e. fences that are not part of other
    /// instructions. It's expressive enough to expose mfence on x86 and dmb ish/ishst on ARM. On
    /// x86, it also acts as a compiler store-store fence in those cases where it would have been a
    /// dmb ishst on ARM.
    Fence,

    /// This is a regular ordinary C function call, using the system C calling convention. Make sure
    /// that the arguments are passed using the right types. The first argument is the callee.
    CCall,

    /// This is a patchpoint. Use the PatchpointValue class. This is viewed as behaving like a call,
    /// but only emits code via a code generation callback. That callback gets to emit code inline.
    /// You can pass a stackmap along with constraints on how each stackmap argument must be passed.
    /// It's legal to request that a stackmap argument is in some register and it's legal to request
    /// that a stackmap argument is at some offset from the top of the argument passing area on the
    /// stack.
    Patchpoint,

    /// This is a projection out of a tuple. Currently only Patchpoints, Get, Phi, and BottomTuple can produce tuples.
    /// It's assumumed that each entry in a tuple has a fixed Numeric Type (i.e. not Void or Tuple).
    Extract,

    /// Checked math. Use the CheckValue class. Like a Patchpoint, this takes a code generation
    /// callback. That callback gets to emit some code after the epilogue, and gets to link the jump
    /// from the check, and the choice of registers. You also get to supply a stackmap. Note that you
    /// are not allowed to jump back into the mainline code from your slow path, since the compiler
    /// will assume that the execution of these instructions proves that overflow didn't happen. For
    /// example, if you have two CheckAdd's:
    //
    /// a = CheckAdd(x, y)
    /// b = CheckAdd(x, y)
    //
    /// Then it's valid to change this to:
    //
    /// a = CheckAdd(x, y)
    /// b = Identity(a)
    //
    /// This is valid regardless of the callbacks used by the two CheckAdds. They may have different
    /// callbacks. Yet, this transformation is valid even if they are different because we know that
    /// after the first CheckAdd executes, the second CheckAdd could not have possibly taken slow
    /// path. Therefore, the second CheckAdd's callback is irrelevant.
    //
    /// Note that the first two children of these operations have ValueRep's as input constraints but do
    /// not have output constraints.
    CheckAdd,
    CheckSub,
    CheckMul,

    /// Check that side-exits. Use the CheckValue class. Like CheckAdd and friends, this has a
    /// stackmap with a generation callback. This takes an int argument that this branches on, with
    /// full branch fusion in the instruction selector. A true value jumps to the generator's slow
    /// path. Note that the predicate child is has both an input ValueRep. The input constraint must be
    /// WarmAny. It will not have an output constraint.
    Check,

    /// SIMD instructions
    VectorExtractLane,
    VectorReplaceLane,

    /// Currently only some architectures support this.
    /// FIXME: Expand this to identical instructions for the other architectures as a macro.
    VectorDupElement,

    VectorSplat,

    VectorEqual,
    VectorNotEqual,
    VectorLessThan,
    VectorLessThanOrEqual,
    VectorBelow,
    VectorBelowOrEqual,
    VectorGreaterThan,
    VectorGreaterThanOrEqual,
    VectorAbove,
    VectorAboveOrEqual,

    VectorAdd,
    VectorSub,
    VectorAddSat,
    VectorSubSat,
    VectorMul,
    VectorDotProduct,
    VectorDiv,
    VectorMin,
    VectorMax,
    VectorPmin,
    VectorPmax,

    VectorNarrow,

    VectorNot,
    VectorAnd,
    VectorAndnot,
    VectorOr,
    VectorXor,

    VectorShl,
    VectorShr,

    VectorAbs,
    VectorNeg,
    VectorPopcnt,
    VectorCeil,
    VectorFloor,
    VectorTrunc,
    VectorTruncSat,
    VectorConvert,
    VectorConvertLow,
    VectorNearest,
    VectorSqrt,

    VectorExtendLow,
    VectorExtendHigh,

    VectorPromote,
    VectorDemote,

    VectorAnyTrue,
    VectorAllTrue,
    VectorAvgRound,
    VectorBitmask,
    VectorBitwiseSelect,
    VectorExtaddPairwise,
    VectorMulSat,
    VectorSwizzle,

    /// Currently only some architectures support this.
    /// FIXME: Expand this to identical instructions for the other architectures as a macro.
    VectorMulByElement,

    /// SSA support, in the style of DFG SSA.
    Upsilon,
    /// This uses the UpsilonValue class.
    Phi,

    /// Jump.
    Jump,

    /// Polymorphic branch, usable with any integer type. Branches if not equal to zero. The 0-index
    /// successor is the true successor.
    Branch,

    /// Switch. Switches over either Int32 or Int64. Uses the SwitchValue class.
    Switch,

    /// Multiple entrypoints are supported via the EntrySwitch operation. Place this in the root
    /// block and list the entrypoints as the successors. All blocks backwards-reachable from
    /// EntrySwitch are duplicated for each entrypoint.
    EntrySwitch,

    /// Return. Note that procedures don't know their return type, so this can just return any
    /// type.
    Return,

    /// This is a terminal that indicates that we will never get here.
    Oops,
}

impl Opcode {
    pub fn is_binary(self) -> bool {
        use Opcode::*;
        matches!(
            self,
            Add | Sub
                | Mul
                | Div
                | UDiv
                | Mod
                | UMod
                | BitAnd
                | BitOr
                | BitXor
                | Shl
                | SShr
                | ZShr
                | RotR
                | RotL
                | Equal
                | NotEqual
                | LessThan
                | GreaterThan
                | LessEqual
                | GreaterEqual
                | Below
                | Above
                | BelowEqual
                | AboveEqual
                | EqualOrUnordered
                | Select
        )
    }

    

    pub const fn is_check_math(self) -> bool {
        matches!(self, Self::CheckAdd | Self::CheckSub | Self::CheckMul)
    }

    pub const fn const_ptr() -> Self {
        #[cfg(target_pointer_width = "64")]
        {
            Opcode::Const64
        }

        #[cfg(target_pointer_width = "32")]
        {
            Opcode::Const32
        }
    }

    pub const fn is_constant(self) -> bool {
        matches!(
            self,
            Self::Const32 | Self::Const64 | Self::ConstDouble | Self::ConstFloat
        )
    }

    pub const fn is_definitely_terminal(self) -> bool {
        matches!(
            self,
            Self::Return | Self::Oops | Self::Jump | Self::Branch | Self::Switch
        )
    }

    pub const fn is_load(self) -> bool {
        matches!(
            self,
            Self::Load8S | Self::Load8Z | Self::Load16S | Self::Load16Z | Self::Load
        )
    }

    pub const fn is_store(self) -> bool {
        matches!(self, Self::Store8 | Self::Store16 | Self::Store)
    }

    pub const fn is_load_store(self) -> bool {
        self.is_load() || self.is_store()
    }

    pub const fn is_atomic(self) -> bool {
        matches!(
            self,
            Self::AtomicWeakCAS
                | Self::AtomicStrongCAS
                | Self::AtomicXchg
                | Self::AtomicXchgAdd
                | Self::AtomicXchgAnd
                | Self::AtomicXchgOr
                | Self::AtomicXchgSub
                | Self::AtomicXchgXor
        )
    }

    pub const fn is_atomic_xchg(self) -> bool {
        matches!(
            self,
            Self::AtomicXchg
                | Self::AtomicXchgAdd
                | Self::AtomicXchgAnd
                | Self::AtomicXchgOr
                | Self::AtomicXchgSub
                | Self::AtomicXchgXor
        )
    }

    pub const fn is_memory_access(self) -> bool {
        self.is_load_store() || self.is_atomic()
    }

    pub const fn sign_extend_opcode(width: Width) -> Self {
        match width {
            Width::W8 => Opcode::SExt8,
            Width::W16 => Opcode::SExt16,
            _ => Opcode::Oops,
        }
    }

    pub const fn store_opcode(bank: Bank, width: Width) -> Opcode {
        match bank {
            Bank::GP => match width {
                Width::W8 => Opcode::Store8,
                Width::W16 => Opcode::Store16,
                _ => Opcode::Store,
            },

            Bank::FP => Opcode::Store,
        }
    }

    pub const fn invert_opcode(self, typ: Type) -> Option<Opcode> {
        match self {
            Self::Equal => Some(Self::NotEqual),
            Self::NotEqual => Some(Self::Equal),
            Self::LessThan => {
                if typ.is_int() {
                    Some(Self::GreaterEqual)
                } else {
                    None
                }
            }
            Self::GreaterThan => {
                if typ.is_int() {
                    Some(Self::LessEqual)
                } else {
                    None
                }
            }

            Self::LessEqual => {
                if typ.is_int() {
                    Some(Self::GreaterThan)
                } else {
                    None
                }
            }

            Self::GreaterEqual => {
                if typ.is_int() {
                    Some(Self::LessThan)
                } else {
                    None
                }
            }

            Self::Above => {
                if typ.is_int() {
                    Some(Self::BelowEqual)
                } else {
                    None
                }
            }

            Self::Below => {
                if typ.is_int() {
                    Some(Self::AboveEqual)
                } else {
                    None
                }
            }

            Self::AboveEqual => {
                if typ.is_int() {
                    Some(Self::Below)
                } else {
                    None
                }
            }

            Self::BelowEqual => {
                if typ.is_int() {
                    Some(Self::Above)
                } else {
                    None
                }
            }

            _ => None,
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Into<Kind> for Opcode {
    fn into(self) -> Kind {
        Kind::new(self)
    }
}
