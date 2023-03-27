use std::{hash::Hash, ops::Range};

use tinyvec::TinyVec;

use crate::{
    air::stack_slot::StackSlotId,
    bank::{bank_for_type, Bank},
    block::{BasicBlock, BlockId, FrequentBlock},
    effects::Effects,
    jit::reg::Reg,
    kind::Kind,
    opcode::Opcode,
    procedure::Procedure,
    sparse_collection::SparseElement,
    stackmap_value::StackMapValue,
    typ::{Type, TypeKind},
    variable::VariableId,
    width::{width_for_type, Width},
    *, utils::index_set::KeyIndex,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum NumChildren {
    Zero = 0,
    One,
    Two,
    Three,
    VarArgs,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Value {
    pub(crate) index: usize,
    pub(crate) kind: Kind,
    pub(crate) typ: Type,
    pub(crate) num_children: NumChildren,
    pub(crate) data: ValueData,
    pub(crate) owner: Option<BlockId>,
    pub(crate) children: TinyVec<[ValueId; 3]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Ord, PartialOrd)]
#[repr(C)]
pub struct ValueId(pub usize);

impl Into<usize> for ValueId {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for ValueId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

impl SparseElement for Value {
    type Id = ValueId;
    fn id(&self) -> Self::Id {
        ValueId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueData {
    None,
    Const32(i32),
    Const64(i64),
    Const128(i128),
    Double(u64),
    Float(u32),
    MemoryValue {
        offset: i32,
        range: Range<usize>,
        fence_range: Range<usize>,
    },
    Argument(Reg),
    CCallValue(Effects),
    Variable(VariableId),
    Upsilon(Option<ValueId>),
    StackMap(StackMapValue),
    SlotBase(StackSlotId),
}

impl Value {
    pub fn argument_reg(&self) -> Option<Reg> {
        match self.data {
            ValueData::Argument(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn is_int_of(&self, val: i64) -> bool {
        match self.kind.opcode() {
            Opcode::Const32 => self.as_int32() == Some(val as i32),
            Opcode::Const64 => self.as_int64() == Some(val),
            _ => false,
        }
    }
    pub fn returns_bool(&self, proc: &Procedure) -> bool {
        if self.typ().kind() != TypeKind::Int32 {
            return false;
        }

        match self.kind.opcode() {
            Opcode::Const32 => self.as_int32() == Some(0) || self.as_int32() == Some(1),

            Opcode::BitAnd => {
                proc.value(self.children[0]).returns_bool(proc)
                    || proc.value(self.children[1]).returns_bool(proc)
            }

            Opcode::BitOr | Opcode::BitXor => {
                proc.value(self.children[0]).returns_bool(proc)
                    && proc.value(self.children[1]).returns_bool(proc)
            }

            Opcode::Select => {
                proc.value(self.children[1]).returns_bool(proc)
                    && proc.value(self.children[2]).returns_bool(proc)
            }

            Opcode::Equal
            | Opcode::NotEqual
            | Opcode::LessThan
            | Opcode::LessEqual
            | Opcode::GreaterThan
            | Opcode::GreaterEqual
            | Opcode::Above
            | Opcode::AboveEqual
            | Opcode::Below
            | Opcode::BelowEqual
            | Opcode::EqualOrUnordered
            | Opcode::AtomicWeakCAS => true,

            _ => false,
        }
    }

    pub fn access_width(&self, proc: &Procedure) -> Width {
        match self.kind.opcode() {
            Opcode::Load8Z | Opcode::Load8S | Opcode::Store8 => Width::W8,

            Opcode::Load16Z | Opcode::Load16S | Opcode::Store16 => Width::W16,

            Opcode::Load => width_for_type(proc.value(self.children[0]).typ()),
            Opcode::Store => width_for_type(self.typ()),

            _ => Width::W8,
        }
    }

    pub fn access_bank(&self, proc: &Procedure) -> Bank {
        match self.kind.opcode() {
            Opcode::Load8Z | Opcode::Load8S | Opcode::Store8 => Bank::GP,

            Opcode::Load16Z | Opcode::Load16S | Opcode::Store16 => Bank::GP,

            Opcode::Load => bank_for_type(proc.value(self.children[0]).typ()),
            Opcode::Store => bank_for_type(self.typ()),

            _ => Bank::GP,
        }
    }

    pub fn slot_base_value(&self) -> Option<StackSlotId> {
        match self.data {
            ValueData::SlotBase(id) => Some(id),
            _ => None,
        }
    }

    pub fn memory_value(&self) -> Option<(i32, Range<usize>, Range<usize>)> {
        match self.data {
            ValueData::MemoryValue {
                offset,
                ref range,
                ref fence_range,
            } => Some((offset, range.clone(), fence_range.clone())),
            _ => None,
        }
    }

    pub fn effects(&self) -> Effects {
        let mut result = Effects::none();

        match self.kind.opcode() {
            Opcode::Div | Opcode::UDiv | Opcode::Mod | Opcode::UMod => {
                result.control_dependant = true;
            }

            Opcode::Load8Z | Opcode::Load8S | Opcode::Load16S | Opcode::Load16Z | Opcode::Load => {
                let (_offset, range, fence_range) = self.memory_value().unwrap();

                result.reads = range;

                if fence_range.start != 0 {
                    result.writes = fence_range;
                    result.fence = true;
                }

                result.control_dependant = true;
            }

            Opcode::CCall => match self.data {
                ValueData::CCallValue(ref ccall) => result = ccall.clone(),
                _ => unreachable!(),
            },

            Opcode::Patchpoint => {
                todo!()
            }

            Opcode::Phi | Opcode::Upsilon => {
                result.writes_local_state = true;
            }

            Opcode::Jump
            | Opcode::Branch
            | Opcode::Switch
            | Opcode::Return
            | Opcode::Oops
            | Opcode::EntrySwitch => {
                result.terminal = true;
            }

            _ => (),
        }

        result
    }

    pub fn stackmap(&self) -> Option<&StackMapValue> {
        match self.data {
            ValueData::StackMap(ref stackmap) => Some(stackmap),
            _ => None,
        }
    }

    pub fn stackmap_mut(&mut self) -> Option<&mut StackMapValue> {
        match self.data {
            ValueData::StackMap(ref mut stackmap) => Some(stackmap),
            _ => None,
        }
    }

    pub fn is_constant(&self) -> bool {
        match self.data {
            ValueData::Const32(_) => true,
            ValueData::Const64(_) => true,
            ValueData::Const128(_) => true,
            ValueData::Double(_) => true,
            ValueData::Float(_) => true,
            _ => false,
        }
    }

    pub(crate) fn build_adjacency_list(
        num_children: NumChildren,
        args: &[ValueId],
    ) -> TinyVec<[ValueId; 3]> {
        match num_children {
            NumChildren::VarArgs => args.iter().copied().collect(),
            _ => {
                assert!(args.len() == num_children as usize);

                let mut children = TinyVec::new();
                for arg in args {
                    children.push(*arg);
                }

                children
            }
        }
    }

    pub fn make_ccall(typ: Type, effects: Option<Effects>, args: &[ValueId]) -> Self {
        Self {
            index: usize::MAX,
            kind: Kind::new(Opcode::CCall),
            typ,
            owner: None,
            num_children: NumChildren::VarArgs,
            data: ValueData::CCallValue(effects.unwrap_or_else(|| Effects::for_call())),
            children: Self::build_adjacency_list(NumChildren::VarArgs, args),
        }
    }

    pub fn append_args(&mut self, args: &[ValueId]) {
        assert!(self.kind.opcode() == Opcode::CCall);

        self.children.extend_from_slice(args);
    }

    pub fn phi(&self) -> Option<ValueId> {
        assert!(self.kind.opcode() == Opcode::Upsilon);
        match self.data {
            ValueData::Upsilon(phi) => phi,
            _ => None,
        }
    }

    pub fn set_phi(&mut self, new_phi: ValueId) {
        assert!(self.kind.opcode() == Opcode::Upsilon);
        match &mut self.data {
            ValueData::Upsilon(phi) => {
                phi.replace(new_phi);
            }
            _ => unreachable!(),
        }
    }

    pub fn make_upsilon(value: ValueId, phi: Option<ValueId>) -> Self {
        Self {
            index: 0,
            owner: None,
            kind: Kind::new(Opcode::Upsilon),
            typ: TypeKind::Void.into(),
            num_children: NumChildren::One,
            data: ValueData::Upsilon(phi),
            children: Self::build_adjacency_list(NumChildren::One, &[value]),
        }
    }

    pub fn make_const32(value: i32) -> Self {
        Self {
            owner: None,
            index: 0,
            kind: Kind::new(Opcode::Const32),
            typ: TypeKind::Int32.into(),
            num_children: NumChildren::Zero,
            data: ValueData::Const32(value),
            children: TinyVec::new(),
        }
    }

    pub fn make_const64(value: i64) -> Self {
        Self {
            owner: None,
            index: 0,
            kind: Kind::new(Opcode::Const64),
            typ: TypeKind::Int64.into(),
            num_children: NumChildren::Zero,
            data: ValueData::Const64(value),
            children: TinyVec::new(),
        }
    }

    pub fn make_const_double(value: f64) -> Self {
        Self {
            owner: None,
            index: 0,
            kind: Kind::new(Opcode::ConstDouble),
            typ: TypeKind::Double.into(),
            num_children: NumChildren::Zero,
            data: ValueData::Double(value.to_bits()),
            children: TinyVec::new(),
        }
    }

    pub fn make_const_float(value: f32) -> Self {
        Self {
            owner: None,
            index: 0,
            kind: Kind::new(Opcode::ConstFloat),
            typ: TypeKind::Float.into(),
            num_children: NumChildren::Zero,
            data: ValueData::Float(value.to_bits()),
            children: TinyVec::new(),
        }
    }

    pub fn make_const128(value: i128) -> Self {
        Self {
            owner: None,
            index: 0,
            kind: Kind::new(Opcode::Const128),
            typ: TypeKind::V128.into(),
            num_children: NumChildren::Zero,
            data: ValueData::Const128(value),
            children: TinyVec::new(),
        }
    }

    pub fn typ(&self) -> Type {
        self.typ
    }

    pub(crate) fn new(
        kind: impl Into<Kind>,
        typ: Type,
        num_children: NumChildren,
        args: &[ValueId],
        vdata: ValueData,
    ) -> Self {
        Self {
            owner: None,
            index: usize::MAX,
            kind: kind.into(),
            typ,
            num_children,
            data: vdata,
            children: Self::build_adjacency_list(num_children, args),
        }
    }

    pub fn has_double(&self) -> bool {
        match self.kind.opcode() {
            Opcode::ConstDouble => true,
            _ => false,
        }
    }

    pub fn has_float(&self) -> bool {
        match self.kind.opcode() {
            Opcode::ConstFloat => true,
            _ => false,
        }
    }

    pub fn has_int(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const32 | Opcode::Const64 => true,
            _ => false,
        }
    }

    pub fn has_int32(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const32 => true,
            _ => false,
        }
    }

    pub fn has_int64(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const64 => true,
            _ => false,
        }
    }

    pub fn has_int128(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const128 => true,
            _ => false,
        }
    }

    pub fn has_number(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const32 | Opcode::Const64 | Opcode::ConstDouble | Opcode::ConstFloat => true,
            _ => false,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self.kind.opcode() {
            Opcode::Const32 => match self.data {
                ValueData::Const32(value) => Some(value as i64),
                _ => unreachable!(),
            },
            Opcode::Const64 => match self.data {
                ValueData::Const64(value) => Some(value),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn as_int32(&self) -> Option<i32> {
        match self.kind.opcode() {
            Opcode::Const32 => match self.data {
                ValueData::Const32(value) => Some(value),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn as_int64(&self) -> Option<i64> {
        match self.kind.opcode() {
            Opcode::Const64 => match self.data {
                ValueData::Const64(value) => Some(value),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn as_int128(&self) -> Option<i128> {
        match self.kind.opcode() {
            Opcode::Const128 => match self.data {
                ValueData::Const128(value) => Some(value),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn as_double(&self) -> Option<f64> {
        match self.kind.opcode() {
            Opcode::ConstDouble => match self.data {
                ValueData::Double(value) => Some(f64::from_bits(value)),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        match self.kind.opcode() {
            Opcode::ConstFloat => match self.data {
                ValueData::Float(value) => Some(f32::from_bits(value)),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn neg_constant(&self) -> Option<Value> {
        Some(if self.has_float() {
            Self::make_const_float(-self.as_float().unwrap())
        } else if self.has_double() {
            Self::make_const_double(-self.as_double().unwrap())
        } else if self.has_int32() {
            Self::make_const32(-self.as_int32().unwrap())
        } else if self.has_int64() {
            Self::make_const64(-self.as_int64().unwrap())
        } else if self.has_int128() {
            Self::make_const128(-self.as_int128().unwrap())
        } else {
            return None;
        })
    }

    pub fn add_constant(&self, other: &Value) -> Option<Value> {
        if self.has_float() && other.has_float() {
            Some(Self::make_const_float(
                self.as_float().unwrap() + other.as_float().unwrap(),
            ))
        } else if self.has_double() && other.has_double() {
            Some(Self::make_const_double(
                self.as_double().unwrap() + other.as_double().unwrap(),
            ))
        } else if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                self.as_int32()
                    .unwrap()
                    .wrapping_add(other.as_int32().unwrap()),
            ))
        } else if self.has_int64() && other.has_int64() {
            Some(Self::make_const64(
                self.as_int64()
                    .unwrap()
                    .wrapping_add(other.as_int64().unwrap()),
            ))
        } else if self.has_int128() && other.has_int128() {
            Some(Self::make_const128(
                self.as_int128()
                    .unwrap()
                    .wrapping_add(other.as_int128().unwrap()),
            ))
        } else {
            None
        }
    }

    pub fn sub_constant(&self, other: &Value) -> Option<Value> {
        if self.has_float() && other.has_float() {
            Some(Self::make_const_float(
                self.as_float().unwrap() - other.as_float().unwrap(),
            ))
        } else if self.has_double() && other.has_double() {
            Some(Self::make_const_double(
                self.as_double().unwrap() - other.as_double().unwrap(),
            ))
        } else if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                self.as_int32()
                    .unwrap()
                    .wrapping_sub(other.as_int32().unwrap()),
            ))
        } else if self.has_int64() && other.has_int64() {
            Some(Self::make_const64(
                self.as_int64()
                    .unwrap()
                    .wrapping_sub(other.as_int64().unwrap()),
            ))
        } else if self.has_int128() && other.has_int128() {
            Some(Self::make_const128(
                self.as_int128()
                    .unwrap()
                    .wrapping_sub(other.as_int128().unwrap()),
            ))
        } else {
            None
        }
    }

    pub fn mul_constant(&self, other: &Value) -> Option<Value> {
        if self.has_float() && other.has_float() {
            Some(Self::make_const_float(
                self.as_float().unwrap() * other.as_float().unwrap(),
            ))
        } else if self.has_double() && other.has_double() {
            Some(Self::make_const_double(
                self.as_double().unwrap() * other.as_double().unwrap(),
            ))
        } else if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                self.as_int32()
                    .unwrap()
                    .wrapping_mul(other.as_int32().unwrap()),
            ))
        } else if self.has_int64() && other.has_int64() {
            Some(Self::make_const64(
                self.as_int64()
                    .unwrap()
                    .wrapping_mul(other.as_int64().unwrap()),
            ))
        } else if self.has_int128() && other.has_int128() {
            Some(Self::make_const128(
                self.as_int128()
                    .unwrap()
                    .wrapping_mul(other.as_int128().unwrap()),
            ))
        } else {
            None
        }
    }

    pub fn check_add_constant(&self, other: &Value) -> Option<Value> {
        if other.has_int32() && self.has_int32() {
            let lhs = self.as_int32().unwrap();
            let rhs = other.as_int32().unwrap();

            Some(Self::make_const32(lhs.checked_add(rhs)?))
        } else if other.has_int64() && self.has_int64() {
            let lhs = self.as_int64().unwrap();
            let rhs = other.as_int64().unwrap();

            Some(Self::make_const64(lhs.checked_add(rhs)?))
        } else if other.has_int128() && self.has_int128() {
            let lhs = self.as_int128().unwrap();
            let rhs = other.as_int128().unwrap();

            Some(Self::make_const128(lhs.checked_add(rhs)?))
        } else {
            None
        }
    }

    pub fn check_sub_constant(&self, other: &Value) -> Option<Value> {
        if other.has_int32() && self.has_int32() {
            let lhs = self.as_int32().unwrap();
            let rhs = other.as_int32().unwrap();

            Some(Self::make_const32(lhs.checked_sub(rhs)?))
        } else if other.has_int64() && self.has_int64() {
            let lhs = self.as_int64().unwrap();
            let rhs = other.as_int64().unwrap();

            Some(Self::make_const64(lhs.checked_sub(rhs)?))
        } else if other.has_int128() && self.has_int128() {
            let lhs = self.as_int128().unwrap();
            let rhs = other.as_int128().unwrap();

            Some(Self::make_const128(lhs.checked_sub(rhs)?))
        } else {
            None
        }
    }

    pub fn check_mul_constant(&self, other: &Value) -> Option<Value> {
        if other.has_int32() && self.has_int32() {
            let lhs = self.as_int32().unwrap();
            let rhs = other.as_int32().unwrap();

            Some(Self::make_const32(lhs.checked_mul(rhs)?))
        } else if other.has_int64() && self.has_int64() {
            let lhs = self.as_int64().unwrap();
            let rhs = other.as_int64().unwrap();

            Some(Self::make_const64(lhs.checked_mul(rhs)?))
        } else if other.has_int128() && self.has_int128() {
            let lhs = self.as_int128().unwrap();
            let rhs = other.as_int128().unwrap();

            Some(Self::make_const128(lhs.checked_mul(rhs)?))
        } else {
            None
        }
    }

    pub fn checked_neg_constant(&self) -> Option<Value> {
        if self.has_int32() {
            let val = self.as_int32().unwrap();

            Some(Self::make_const32(val.checked_neg()?))
        } else if self.has_int64() {
            let val = self.as_int64().unwrap();

            Some(Self::make_const64(val.checked_neg()?))
        } else if self.has_int128() {
            let val = self.as_int128().unwrap();

            Some(Self::make_const128(val.checked_neg()?))
        } else {
            None
        }
    }

    pub fn div_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(chill_div(
                self.as_int32().unwrap(),
                other.as_int32().unwrap(),
            )));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(chill_div(
                self.as_int64().unwrap(),
                other.as_int64().unwrap(),
            )));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(chill_div(
                self.as_int128().unwrap(),
                other.as_int128().unwrap(),
            )));
        } else {
            None
        }
    }

    pub fn udiv_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(chill_udiv(
                self.as_int32().unwrap() as u32,
                other.as_int32().unwrap() as u32,
            ) as _));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(chill_udiv(
                self.as_int64().unwrap() as u64,
                other.as_int64().unwrap() as u64,
            ) as _));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(chill_udiv(
                self.as_int128().unwrap() as u128,
                other.as_int128().unwrap() as u128,
            ) as _));
        } else {
            None
        }
    }

    pub fn mod_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(chill_mod(
                self.as_int32().unwrap(),
                other.as_int32().unwrap(),
            )));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(chill_mod(
                self.as_int64().unwrap(),
                other.as_int64().unwrap(),
            )));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(chill_mod(
                self.as_int128().unwrap(),
                other.as_int128().unwrap(),
            )));
        } else {
            None
        }
    }

    pub fn umod_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(chill_umod(
                self.as_int32().unwrap() as u32,
                other.as_int32().unwrap() as u32,
            ) as _));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(chill_umod(
                self.as_int64().unwrap() as u64,
                other.as_int64().unwrap() as u64,
            ) as _));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(chill_umod(
                self.as_int128().unwrap() as u128,
                other.as_int128().unwrap() as u128,
            ) as _));
        } else {
            None
        }
    }

    pub fn bit_and_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(
                self.as_int32().unwrap() & other.as_int32().unwrap(),
            ));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(
                self.as_int64().unwrap() & other.as_int64().unwrap(),
            ));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(
                self.as_int128().unwrap() & other.as_int128().unwrap(),
            ));
        } else if self.has_double() && other.has_double() {
            return Some(Self::make_const_double(f64::from_bits(
                self.as_double().unwrap().to_bits() & other.as_double().unwrap().to_bits(),
            )));
        } else if self.has_float() && other.has_float() {
            return Some(Self::make_const_float(f32::from_bits(
                self.as_float().unwrap().to_bits() & other.as_float().unwrap().to_bits(),
            )));
        } else {
            None
        }
    }

    pub fn bit_or_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(
                self.as_int32().unwrap() | other.as_int32().unwrap(),
            ));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(
                self.as_int64().unwrap() | other.as_int64().unwrap(),
            ));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(
                self.as_int128().unwrap() | other.as_int128().unwrap(),
            ));
        } else if self.has_double() && other.has_double() {
            return Some(Self::make_const_double(f64::from_bits(
                self.as_double().unwrap().to_bits() | other.as_double().unwrap().to_bits(),
            )));
        } else if self.has_float() && other.has_float() {
            return Some(Self::make_const_float(f32::from_bits(
                self.as_float().unwrap().to_bits() | other.as_float().unwrap().to_bits(),
            )));
        } else {
            None
        }
    }

    pub fn bit_xor_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            return Some(Self::make_const32(
                self.as_int32().unwrap() ^ other.as_int32().unwrap(),
            ));
        } else if self.has_int64() && other.has_int64() {
            return Some(Self::make_const64(
                self.as_int64().unwrap() ^ other.as_int64().unwrap(),
            ));
        } else if self.has_int128() && other.has_int128() {
            return Some(Self::make_const128(
                self.as_int128().unwrap() ^ other.as_int128().unwrap(),
            ));
        } else if self.has_double() && other.has_double() {
            return Some(Self::make_const_double(f64::from_bits(
                self.as_double().unwrap().to_bits() ^ other.as_double().unwrap().to_bits(),
            )));
        } else if self.has_float() && other.has_float() {
            return Some(Self::make_const_float(f32::from_bits(
                self.as_float().unwrap().to_bits() ^ other.as_float().unwrap().to_bits(),
            )));
        } else {
            None
        }
    }

    pub fn shl_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                self.as_int32()? << (other.as_int32()? & 31),
            ))
        } else if self.has_int64() && other.has_int32() {
            Some(Self::make_const64(
                self.as_int64()? << (other.as_int32()? & 63),
            ))
        } else if self.has_int128() && other.has_int32() {
            Some(Self::make_const128(
                self.as_int128()? << (other.as_int32()? & 127),
            ))
        } else {
            None
        }
    }

    pub fn sshr_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                self.as_int32()? >> (other.as_int32()? & 31),
            ))
        } else if self.has_int64() && other.has_int32() {
            Some(Self::make_const64(
                self.as_int64()? >> (other.as_int32()? & 63),
            ))
        } else if self.has_int128() && other.has_int32() {
            Some(Self::make_const128(
                self.as_int128()? >> (other.as_int32()? & 127),
            ))
        } else {
            None
        }
    }

    pub fn zshr_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                (self.as_int32()? as u32 >> (other.as_int32()? & 31)) as i32,
            ))
        } else if self.has_int64() && other.has_int32() {
            Some(Self::make_const64(
                (self.as_int64()? as u64 >> (other.as_int32()? & 63)) as i64,
            ))
        } else if self.has_int128() && other.has_int32() {
            Some(Self::make_const128(
                (self.as_int128()? as u128 >> (other.as_int32()? & 127)) as i128,
            ))
        } else {
            None
        }
    }

    pub fn rotr_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                (self.as_int32()? >> (other.as_int32()? & 31))
                    | (self.as_int32()? << (32 - (other.as_int32()? & 31))),
            ))
        } else if self.has_int64() && other.has_int32() {
            Some(Self::make_const64(
                (self.as_int64()? >> (other.as_int32()? & 63))
                    | (self.as_int64()? << (64 - (other.as_int32()? & 63))),
            ))
        } else if self.has_int128() && other.has_int32() {
            Some(Self::make_const128(
                (self.as_int128()? >> (other.as_int32()? & 127))
                    | (self.as_int128()? << (128 - (other.as_int32()? & 127))),
            ))
        } else {
            None
        }
    }

    pub fn rotl_constant(&self, other: &Value) -> Option<Value> {
        if self.has_int32() && other.has_int32() {
            Some(Self::make_const32(
                (self.as_int32()? << (other.as_int32()? & 31))
                    | (self.as_int32()? >> (32 - (other.as_int32()? & 31))),
            ))
        } else if self.has_int64() && other.has_int32() {
            Some(Self::make_const64(
                (self.as_int64()? << (other.as_int32()? & 63))
                    | (self.as_int64()? >> (64 - (other.as_int32()? & 63))),
            ))
        } else if self.has_int128() && other.has_int32() {
            Some(Self::make_const128(
                (self.as_int128()? << (other.as_int32()? & 127))
                    | (self.as_int128()? >> (128 - (other.as_int32()? & 127))),
            ))
        } else {
            None
        }
    }

    pub fn bitwise_cast_constant(&self) -> Option<Value> {
        if self.has_int32() {
            Some(Self::make_const_float(f32::from_bits(
                self.as_int32()? as u32
            )))
        } else if self.has_int64() {
            Some(Self::make_const_double(f64::from_bits(
                self.as_int64()? as u64
            )))
        } else if self.has_float() {
            Some(Self::make_const32(self.as_float()?.to_bits() as _))
        } else if self.has_double() {
            Some(Self::make_const64(self.as_double()?.to_bits() as _))
        } else {
            None
        }
    }

    pub fn i2d_constant(&self) -> Option<Value> {
        if self.has_int32() {
            Some(Self::make_const_double(self.as_int32()? as f64))
        } else if self.has_int64() {
            Some(Self::make_const_double(self.as_int64()? as f64))
        } else if self.has_int128() {
            Some(Self::make_const_double(self.as_int128()? as f64))
        } else {
            None
        }
    }

    pub fn i2f_constant(&self) -> Option<Value> {
        if self.has_int32() {
            Some(Self::make_const_float(self.as_int32()? as f32))
        } else if self.has_int64() {
            Some(Self::make_const_float(self.as_int64()? as f32))
        } else if self.has_int128() {
            Some(Self::make_const_float(self.as_int128()? as f32))
        } else {
            None
        }
    }

    pub fn d2i_constant(&self) -> Option<Value> {
        if self.has_double() {
            Some(Self::make_const64(self.as_double()? as i64))
        } else {
            None
        }
    }

    pub fn f2i_constant(&self) -> Option<Value> {
        if self.has_float() {
            Some(Self::make_const32(self.as_float()? as i32))
        } else {
            None
        }
    }

    pub fn d2f_constant(&self) -> Option<Value> {
        if self.has_double() {
            Some(Self::make_const_float(self.as_double()? as f32))
        } else {
            None
        }
    }

    pub fn f2d_constant(&self) -> Option<Value> {
        if self.has_float() {
            Some(Self::make_const_double(self.as_float()? as f64))
        } else {
            None
        }
    }

    pub fn equal_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            TriState::from_bool(self.as_int32().unwrap() == other.as_int32().unwrap())
        } else if self.has_int64() && other.has_int64() {
            TriState::from_bool(self.as_int64().unwrap() == other.as_int64().unwrap())
        } else if self.has_int128() && other.has_int128() {
            TriState::from_bool(self.as_int128().unwrap() == other.as_int128().unwrap())
        } else if self.has_float() && other.has_float() {
            TriState::from_bool(self.as_float().unwrap() == other.as_float().unwrap())
        } else if self.has_double() && other.has_double() {
            TriState::from_bool(self.as_double().unwrap() == other.as_double().unwrap())
        } else {
            TriState::Undeterminate
        }
    }

    pub fn less_than_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            TriState::from_bool(self.as_int32().unwrap() < other.as_int32().unwrap())
        } else if self.has_int64() && other.has_int64() {
            TriState::from_bool(self.as_int64().unwrap() < other.as_int64().unwrap())
        } else if self.has_int128() && other.has_int128() {
            TriState::from_bool(self.as_int128().unwrap() < other.as_int128().unwrap())
        } else if self.has_float() && other.has_float() {
            TriState::from_bool(self.as_float().unwrap() < other.as_float().unwrap())
        } else if self.has_double() && other.has_double() {
            TriState::from_bool(self.as_double().unwrap() < other.as_double().unwrap())
        } else {
            TriState::Undeterminate
        }
    }

    pub fn greater_than_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            TriState::from_bool(self.as_int32().unwrap() > other.as_int32().unwrap())
        } else if self.has_int64() && other.has_int64() {
            TriState::from_bool(self.as_int64().unwrap() > other.as_int64().unwrap())
        } else if self.has_int128() && other.has_int128() {
            TriState::from_bool(self.as_int128().unwrap() > other.as_int128().unwrap())
        } else if self.has_float() && other.has_float() {
            TriState::from_bool(self.as_float().unwrap() > other.as_float().unwrap())
        } else if self.has_double() && other.has_double() {
            TriState::from_bool(self.as_double().unwrap() > other.as_double().unwrap())
        } else {
            TriState::Undeterminate
        }
    }

    pub fn less_than_equal_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            TriState::from_bool(self.as_int32().unwrap() <= other.as_int32().unwrap())
        } else if self.has_int64() && other.has_int64() {
            TriState::from_bool(self.as_int64().unwrap() <= other.as_int64().unwrap())
        } else if self.has_int128() && other.has_int128() {
            TriState::from_bool(self.as_int128().unwrap() <= other.as_int128().unwrap())
        } else if self.has_float() && other.has_float() {
            TriState::from_bool(self.as_float().unwrap() <= other.as_float().unwrap())
        } else if self.has_double() && other.has_double() {
            TriState::from_bool(self.as_double().unwrap() <= other.as_double().unwrap())
        } else {
            TriState::Undeterminate
        }
    }

    pub fn greater_than_equal_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            TriState::from_bool(self.as_int32().unwrap() >= other.as_int32().unwrap())
        } else if self.has_int64() && other.has_int64() {
            TriState::from_bool(self.as_int64().unwrap() >= other.as_int64().unwrap())
        } else if self.has_int128() && other.has_int128() {
            TriState::from_bool(self.as_int128().unwrap() >= other.as_int128().unwrap())
        } else if self.has_float() && other.has_float() {
            TriState::from_bool(self.as_float().unwrap() >= other.as_float().unwrap())
        } else if self.has_double() && other.has_double() {
            TriState::from_bool(self.as_double().unwrap() >= other.as_double().unwrap())
        } else {
            TriState::Undeterminate
        }
    }

    pub fn above_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            let x = self.as_int32().unwrap() as u32;
            let y = other.as_int32().unwrap() as u32;

            TriState::from_bool(x > y)
        } else if self.has_int64() && other.has_int64() {
            let x = self.as_int64().unwrap() as u64;
            let y = other.as_int64().unwrap() as u64;

            TriState::from_bool(x > y)
        } else if self.has_int128() && other.has_int128() {
            let x = self.as_int128().unwrap() as u128;
            let y = other.as_int128().unwrap() as u128;

            TriState::from_bool(x > y)
        } else {
            TriState::Undeterminate
        }
    }

    pub fn above_equal_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            let x = self.as_int32().unwrap() as u32;
            let y = other.as_int32().unwrap() as u32;

            TriState::from_bool(x >= y)
        } else if self.has_int64() && other.has_int64() {
            let x = self.as_int64().unwrap() as u64;
            let y = other.as_int64().unwrap() as u64;

            TriState::from_bool(x >= y)
        } else if self.has_int128() && other.has_int128() {
            let x = self.as_int128().unwrap() as u128;
            let y = other.as_int128().unwrap() as u128;

            TriState::from_bool(x >= y)
        } else {
            TriState::Undeterminate
        }
    }

    pub fn below_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            let x = self.as_int32().unwrap() as u32;
            let y = other.as_int32().unwrap() as u32;

            TriState::from_bool(x < y)
        } else if self.has_int64() && other.has_int64() {
            let x = self.as_int64().unwrap() as u64;
            let y = other.as_int64().unwrap() as u64;

            TriState::from_bool(x < y)
        } else if self.has_int128() && other.has_int128() {
            let x = self.as_int128().unwrap() as u128;
            let y = other.as_int128().unwrap() as u128;

            TriState::from_bool(x < y)
        } else {
            TriState::Undeterminate
        }
    }

    pub fn below_equal_constant(&self, other: &Value) -> TriState {
        if self.has_int32() && other.has_int32() {
            let x = self.as_int32().unwrap() as u32;
            let y = other.as_int32().unwrap() as u32;

            TriState::from_bool(x <= y)
        } else if self.has_int64() && other.has_int64() {
            let x = self.as_int64().unwrap() as u64;
            let y = other.as_int64().unwrap() as u64;

            TriState::from_bool(x <= y)
        } else if self.has_int128() && other.has_int128() {
            let x = self.as_int128().unwrap() as u128;
            let y = other.as_int128().unwrap() as u128;

            TriState::from_bool(x <= y)
        } else {
            TriState::Undeterminate
        }
    }

    pub fn has_variable(&self) -> bool {
        matches!(self.data, ValueData::Variable(_))
    }

    pub fn as_variable(&self) -> Option<VariableId> {
        match &self.data {
            ValueData::Variable(v) => Some(*v),
            _ => None,
        }
    }

    pub fn fmt_successors<W: std::fmt::Write>(
        &self,
        f: &mut W,
        _proc: &Procedure,
        block: &BasicBlock,
    ) -> std::fmt::Result {
        if self.kind.opcode() == Opcode::Branch && block.successor_list().len() == 2 {
            write!(
                f,
                "Then: BB{}, Else: BB{}",
                block.taken().0 .0,
                block.not_taken().0 .0
            )
        } else {
            for (i, succ) in block.successor_list().iter().enumerate() {
                write!(f, "BB{}", succ.0 .0)?;
                if i < block.successor_list().len() - 1 {
                    write!(f, ", ")?;
                }
            }

            Ok(())
        }
    }

    pub(crate) fn fmt<W: std::fmt::Write>(&self, f: &mut W, _proc: &Procedure) -> std::fmt::Result {
        write!(f, "{} v@{} = {}", self.typ, self.index, self.kind)?;
        write!(f, "(")?;

        for (i, child) in self.children.iter().enumerate() {
            write!(f, "v@{}", child.0)?;
            if i < self.children.len() - 1 {
                write!(f, ", ")?;
            }
        }

        match self.data {
            ValueData::None => {}
            ValueData::Const128(x) => write!(f, " ${:x}", x)?,
            ValueData::Const64(x) => write!(f, " ${:x}", x)?,
            ValueData::Const32(x) => write!(f, " ${:x}", x)?,
            ValueData::Float(x) => write!(f, " ${:x}", x)?,
            ValueData::Double(x) => write!(f, " ${:x}", x)?,
            ValueData::Variable(x) => write!(f, " var@{}", x.0)?,
            ValueData::MemoryValue { offset, .. } => write!(f, " ${:x}", offset)?,
            ValueData::Argument(x) => write!(f, "{:?}", x)?,
            ValueData::Upsilon(x) => match x {
                Some(x) => write!(f, " phi=v@{}", x.0)?,
                None => write!(f, " phi=none")?,
            },
            _ => todo!(),
        }

        write!(f, ")")?;

        Ok(())
    }

    pub fn replace_with_value(
        &mut self,
        kind: impl Into<Kind>,
        typ: Type,
        owner: Option<BlockId>,
        value: ValueId,
    ) {
        self.kind = kind.into();
        self.typ = typ;
        self.owner = owner;

        self.children.clear();
        self.data = ValueData::None;
        self.children.push(value);
    }

    pub fn replace_with(&mut self, kind: impl Into<Kind>, typ: Type, owner: Option<BlockId>) {
        self.kind = kind.into();
        self.typ = typ;
        self.owner = owner;

        self.children.clear();
        self.data = ValueData::None;
    }

    pub fn replace_with_nop_ignoring_type(&mut self) {
        self.replace_with(Opcode::Nop, TypeKind::Void.into(), self.owner)
    }

    pub fn replace_with_nop(&mut self) {
        assert_eq!(self.typ.kind(), TypeKind::Void);
        self.replace_with(Opcode::Nop, TypeKind::Void.into(), self.owner)
    }

    pub fn replace_with_phi(&mut self) {
        if self.typ.kind() == TypeKind::Void {
            self.replace_with_nop();
        } else {
            self.replace_with(Opcode::Phi, self.typ, self.owner)
        }
    }

    pub fn replace_with_jump(&mut self, owner: BlockId, target: FrequentBlock) {
        self.replace_with(Opcode::Jump, TypeKind::Void.into(), Some(owner));
        let _ = target;
    }

    pub fn replace_with_identity(&mut self, value: ValueId) {
        if self.typ.kind() == TypeKind::Void {
            self.replace_with_nop();
        } else {
            self.replace_with_value(Opcode::Identity, self.typ, self.owner, value);
        }
    }

    pub fn fold_identity(val: ValueId, proc: &mut Procedure) -> ValueId {
        let mut current = val;

        while proc.value(current).kind.opcode() == Opcode::Identity {
            current = proc.value(current).children[0];
        }

        current
    }

    pub fn perform_substitution(val: ValueId, proc: &mut Procedure) -> bool {
        let mut result = false;

        for (i, child) in proc
            .value(val)
            .children
            .iter()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
            .enumerate()
        {
            if proc.value(child).kind.opcode() == Opcode::Identity {
                result = true;
                proc.value_mut(val).children[i] = Value::fold_identity(child, proc);
            }
        }

        result
    }

    pub fn stackmap_append_with_rep(&mut self, value: ValueId, rep: ValueRep) {
        assert!(self.stackmap().is_some());
        if rep.kind == ValueRepKind::ColdAny {
            self.children.push(value);
            return;
        }
        let nchild = self.children.len();
        let stackmap = self.stackmap_mut().unwrap();

        while stackmap.reps.len() < nchild {
            stackmap.reps.push(ValueRep::new(ValueRepKind::ColdAny));
        }   
        stackmap.reps.push(rep);
        self.children.push(value);
        
    }

    pub fn stackmap_append_some_register(&mut self, value: ValueId,) {
        self.stackmap_append_with_rep(value, ValueRep::new(ValueRepKind::SomeRegister))
    }

    pub fn stackmap_append_some_register_with_clobber(&mut self, value: ValueId) {
        self.stackmap_append_with_rep(value, ValueRep::new(ValueRepKind::SomeRegisterWithClobber))
    }

    pub fn constrained_child(&self, index: usize) -> ConstrainedValue {
        assert!(self.stackmap().is_some());

        let stackmap = self.stackmap().unwrap();

        let rep = if index < stackmap.reps.len() {
            stackmap.reps[index]
        } else {
            ValueRep::new(ValueRepKind::ColdAny)
        };

        ConstrainedValue::new(self.children[index], rep)
    }

    pub fn set_constraint(&mut self, index: usize, rep: ValueRep) {
        assert!(self.stackmap().is_some());

        if rep == ValueRep::new(ValueRepKind::ColdAny) {
            return;
        }

        let stackmap = self.stackmap_mut().unwrap();

        while stackmap.reps.len() <= index {
            stackmap.reps.push(ValueRep::new(ValueRepKind::ColdAny));
        }

        stackmap.reps[index] = rep;
    }

    pub fn set_constrained_child(&mut self, index: usize, child: ConstrainedValue) {
        assert!(self.stackmap().is_some());

        self.children[index] = child.value;
        self.set_constraint(index, child.rep);
    }

    pub fn result_width(&self) -> Width {
        width_for_type(self.typ)
    }

    pub fn result_bank(&self) -> Bank {
        bank_for_type(self.typ)
    }
}

/// We use this class to describe value representations at stackmaps. It's used both to force a
/// representation and to get the representation. When the B3 client forces a representation, we say
/// that it's an input. When B3 tells the client what representation it picked, we say that it's an
/// output.
#[derive(Clone, Copy)]
pub struct ValueRep {
    u: ValueRepU,
    kind: ValueRepKind,
}

impl std::fmt::Debug for ValueRep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ValueRep({:?}: {:x})", self.kind, unsafe {
            self.u.value
        })
    }
}

impl Hash for ValueRep {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            self.u.value.hash(state);
        }
        self.kind.hash(state);
    }
}

impl ValueRep {
    pub fn new(kind: ValueRepKind) -> Self {
        Self {
            u: ValueRepU { value: 0 },
            kind,
        }
    }

    pub fn from_reg(reg: Reg) -> Self {
        Self {
            u: ValueRepU { reg },
            kind: ValueRepKind::Register,
        }
    }

    pub fn reg(reg: Reg) -> Self {
        Self::from_reg(reg)
    }

    pub fn late_reg(reg: Reg) -> Self {
        Self {
            u: ValueRepU { reg },
            kind: ValueRepKind::LateRegister,
        }
    }

    pub fn stack(offset_from_fp: isize) -> Self {
        Self {
            u: ValueRepU { offset_from_fp },
            kind: ValueRepKind::Stack,
        }
    }

    pub fn stack_argument(offset_from_sp: isize) -> Self {
        Self {
            u: ValueRepU { offset_from_sp },
            kind: ValueRepKind::StackArgument,
        }
    }

    pub fn constant(value: i64) -> Self {
        Self {
            u: ValueRepU { value },
            kind: ValueRepKind::Constant,
        }
    }

    pub fn kind(&self) -> ValueRepKind {
        self.kind
    }

    pub fn is_any(&self) -> bool {
        self.kind == ValueRepKind::WarmAny
            || self.kind == ValueRepKind::ColdAny
            || self.kind == ValueRepKind::LateColdAny
    }

    pub fn is_reg(&self) -> bool {
        self.kind == ValueRepKind::Register
            || self.kind == ValueRepKind::LateRegister
            || self.kind == ValueRepKind::SomeLateRegister
    }

    pub fn get_reg(&self) -> Reg {
        assert!(self.is_reg());
        unsafe { self.u.reg }
    }

    pub fn is_gpr(&self) -> bool {
        self.is_reg() && self.get_reg().is_gpr()
    }

    pub fn is_fpr(&self) -> bool {
        self.is_reg() && self.get_reg().is_fpr()
    }

    pub fn gpr(&self) -> u8 {
        assert!(self.is_gpr());
        self.get_reg().gpr()
    }

    pub fn fpr(&self) -> u8 {
        assert!(self.is_fpr());
        self.get_reg().fpr()
    }

    pub fn is_stack(&self) -> bool {
        self.kind == ValueRepKind::Stack
    }
    pub fn is_stack_argument(&self) -> bool {
        self.kind == ValueRepKind::StackArgument
    }

    pub fn is_constant(&self) -> bool {
        self.kind == ValueRepKind::Constant
    }

    pub fn value(&self) -> i64 {
        assert!(self.is_constant());
        unsafe { self.u.value }
    }

    pub fn with_offset(&self, offset: isize) -> ValueRep {
        match self.kind {
            ValueRepKind::Stack => ValueRep::stack(unsafe { self.u.offset_from_fp + offset }),
            ValueRepKind::StackArgument => {
                ValueRep::stack_argument(unsafe { self.u.offset_from_sp + offset })
            }
            _ => *self,
        }
    }

    pub fn offset_from_fp(&self) -> isize {
        assert!(self.is_stack_argument());
        unsafe { self.u.offset_from_fp }
    }
}

impl PartialEq for ValueRep {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && unsafe { self.u.value == other.u.value }
    }
}

#[derive(Clone, Copy)]
union ValueRepU {
    reg: Reg,
    offset_from_fp: isize,
    offset_from_sp: isize,
    value: i64,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum ValueRepKind {
    /// As an input representation, this means that B3 can pick any representation. As an output
    /// representation, this means that we don't know. This will only arise as an output
    /// representation for the active arguments of Check/CheckAdd/CheckSub/CheckMul.
    WarmAny,

    /// Same as WarmAny, but implies that the use is cold. A cold use is not counted as a use for
    /// computing the priority of the used temporary.
    ColdAny,

    /// Same as ColdAny, but also implies that the use occurs after all other effects of the stackmap
    /// value.
    LateColdAny,

    /// As an input representation, this means that B3 should pick some register. It could be a
    /// register that this claims to clobber!
    SomeRegister,

    /// As an input representation, this means that B3 should pick some register but that this
    /// register is then cobbered with garbage. This only works for patchpoints.
    SomeRegisterWithClobber,

    /// As an input representation, this tells us that B3 should pick some register, but implies
    /// that the def happens before any of the effects of the stackmap. This is only valid for
    /// the result constraint of a Patchpoint.
    SomeEarlyRegister,

    /// As an input representation, this tells us that B3 should pick some register, but implies
    /// the use happens after any defs. This is only works for patchpoints.
    SomeLateRegister,

    /// As an input representation, this forces a particular register. As an output
    /// representation, this tells us what register B3 picked.
    Register,

    /// As an input representation, this forces a particular register and states that
    /// the register is used late. This means that the register is used after the result
    /// is defined (i.e, the result will interfere with this as an input).
    /// It's not a valid output representation.
    LateRegister,

    /// As an output representation, this tells us what stack slot B3 picked. It's not a valid
    /// input representation.
    Stack,

    /// As an input representation, this forces the value to end up in the argument area at some
    /// offset. As an output representation this tells us what offset from SP B3 picked.
    StackArgument,

    /// As an output representation, this tells us that B3 constant-folded the value.
    Constant,
}

pub struct ConstrainedValue {
    pub value: ValueId,
    pub rep: ValueRep,
}

impl ConstrainedValue {
    pub fn new(value: ValueId, rep: ValueRep) -> Self {
        Self { value, rep }
    }
}

impl KeyIndex for ValueId {
    fn index(&self) -> usize {
        self.0
    }
}