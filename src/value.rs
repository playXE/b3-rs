use std::{
    hash::{Hash, Hasher},
    ops::Range,
};

use tinyvec::TinyVec;

use crate::{
    air::stack_slot::StackSlotId,
    bank::{bank_for_type, Bank},
    block::{BasicBlock, BlockId, FrequentBlock},
    effects::Effects,
    jit::reg::Reg,
    kind::Kind,
    opcode::Opcode,
    patchpoint_value::PatchpointValue,
    procedure::Procedure,
    sparse_collection::SparseElement,
    stackmap_value::StackMapValue,
    typ::{Type, TypeKind},
    utils::index_set::KeyIndex,
    variable::VariableId,
    width::{width_for_type, Width},
    *,
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

impl ValueId {
    pub fn key(self, proc: &Procedure) -> Option<ValueKey> {
        proc.value(self).key()
    }

    pub fn kind(self, proc: &Procedure) -> Kind {
        proc.value(self).kind
    }

    pub fn opcode(self, proc: &Procedure) -> Opcode {
        proc.value(self).kind.opcode()
    }

    pub fn child(self, proc: &Procedure, index: usize) -> ValueId {
        proc.value(self).children[index]
    }

    pub fn child_mut(self, proc: &mut Procedure, index: usize) -> &mut ValueId {
        &mut proc.value_mut(self).children[index]
    }
}

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
    Patchpoint(PatchpointValue),
    SlotBase(StackSlotId),
    Switch(Vec<i64>),
    Alloca(Type),
    Procedure
}

impl Value {
    pub fn alloca(&self) -> Option<Type> {
        match self.data {
            ValueData::Alloca(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn switch_cases(&self) -> Option<&[i64]> {
        match self.data {
            ValueData::Switch(ref cases) => Some(cases),
            _ => None,
        }
    }

    pub fn is_switch(&self) -> bool {
        match self.data {
            ValueData::Switch(_) => true,
            _ => false,
        }
    }

    pub fn switch_cases_mut(&mut self) -> Option<&mut Vec<i64>> {
        match self.data {
            ValueData::Switch(ref mut cases) => Some(cases),
            _ => None,
        }
    }

    pub fn num_case_values(&self) -> usize {
        match self.data {
            ValueData::Switch(ref cases) => cases.len(),
            _ => 0,
        }
    }

    pub fn case_value(&self, index: usize) -> Option<i64> {
        match self.data {
            ValueData::Switch(ref cases) => Some(cases[index]),
            _ => None,
        }
    }

    pub fn block_has_fallthrough(&self, block: BlockId, proc: &Procedure) -> bool {
        let num_successors = proc.block(block).successor_list().len();
        let num_values = self.switch_cases().unwrap().len();

        num_values + 1 == num_successors
    }

    pub fn has_falltrhough(&self, proc: &Procedure) -> bool {
        let block = self.owner.unwrap();
        self.block_has_fallthrough(block, proc)
    }

    pub fn set_fallthrough(
        this: ValueId,
        proc: &mut Procedure,
        block: BlockId,
        target: FrequentBlock,
    ) {
        assert!(proc.value(this).is_switch());
        if !proc.value(this).has_falltrhough(proc) {
            proc.block_mut(block).successor_list_mut().push(target);
        } else {
            proc.block_mut(block)
                .successor_list_mut()
                .last_mut()
                .unwrap()
                .0 = target.0;
        }
    }

    pub fn is_free(&self) -> bool {
        match self.kind.opcode() {
            Opcode::Const32
            | Opcode::Const64
            | Opcode::ConstDouble
            | Opcode::ConstFloat
            | Opcode::Identity
            | Opcode::Opaque
            | Opcode::Nop => true,
            _ => false,
        }
    }

    pub fn argument_reg(&self) -> Option<Reg> {
        match self.data {
            ValueData::Argument(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn is_int64_of(&self, val: i64) -> bool {
        match self.kind.opcode() {
            Opcode::Const64 => self.as_int64() == Some(val),
            _ => false,
        }
    }

    pub fn is_int32_of(&self, val: i32) -> bool {
        match self.kind.opcode() {
            Opcode::Const64 => self.as_int32() == Some(val),
            _ => false,
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

            Opcode::Store => width_for_type(proc.value(self.children[0]).typ()),
            Opcode::Load => width_for_type(self.typ()),

            _ => Width::W8,
        }
    }

    pub fn access_bank(&self, proc: &Procedure) -> Bank {
        match self.kind.opcode() {
            Opcode::Load8Z | Opcode::Load8S | Opcode::Store8 => Bank::GP,

            Opcode::Load16Z | Opcode::Load16S | Opcode::Store16 => Bank::GP,

            Opcode::Store => bank_for_type(proc.value(self.children[0]).typ()),
            Opcode::Load => bank_for_type(self.typ()),

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

    pub fn memory_value_mut(&mut self) -> Option<(&mut i32, &mut Range<usize>, &mut Range<usize>)> {
        match self.data {
            ValueData::MemoryValue {
                ref mut offset,
                ref mut range,
                ref mut fence_range,
            } => Some((offset, range, fence_range)),
            _ => None,
        }
    }

    pub fn effects(&self) -> Effects {
        let mut result = Effects::none();

        match self.kind.opcode() {
            Opcode::Div | Opcode::UDiv | Opcode::Mod | Opcode::UMod => {
                result.control_dependent = true;
            }

            Opcode::Load8Z | Opcode::Load8S | Opcode::Load16S | Opcode::Load16Z | Opcode::Load => {
                let (_offset, range, fence_range) = self.memory_value().unwrap();

                result.reads = range;

                if fence_range.start != 0 {
                    result.writes = fence_range;
                    result.fence = true;
                }

                result.control_dependent = true;
            }

            Opcode::CCall => match self.data {
                ValueData::CCallValue(ref ccall) => result = ccall.clone(),
                _ => unreachable!(),
            },

            Opcode::Patchpoint => match self.data {
                ValueData::Patchpoint(ref patchpoint) => result = patchpoint.effects.clone(),
                _ => todo!(),
            },

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

            Opcode::Check | Opcode::CheckAdd | Opcode::CheckMul | Opcode::CheckSub => {
                result = Effects::for_check()
            }

            _ => (),
        }

        result
    }

    pub fn stackmap(&self) -> Option<&StackMapValue> {
        match self.data {
            ValueData::StackMap(ref stackmap) => Some(stackmap),
            ValueData::Patchpoint(ref patchpoint) => Some(&patchpoint.base),
            _ => None,
        }
    }

    pub fn stackmap_mut(&mut self) -> Option<&mut StackMapValue> {
        match self.data {
            ValueData::StackMap(ref mut stackmap) => Some(stackmap),
            ValueData::Patchpoint(ref mut patchpoint) => Some(&mut patchpoint.base),
            _ => None,
        }
    }

    pub fn patchpoint(&self) -> Option<&PatchpointValue> {
        match self.data {
            ValueData::Patchpoint(ref patchpoint) => Some(patchpoint),
            _ => None,
        }
    }

    pub fn patchpoint_mut(&mut self) -> Option<&mut PatchpointValue> {
        match self.data {
            ValueData::Patchpoint(ref mut patchpoint) => Some(patchpoint),
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

    pub fn is_integer(&self) -> bool {
        self.typ().is_int()
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

    pub fn floor_constant(&self) -> Option<Value> {
        Some(if self.has_float() {
            Self::make_const_float(self.as_float().unwrap().floor())
        } else if self.has_double() {
            Self::make_const_double(self.as_double().unwrap().floor())
        } else {
            return None;
        })
    }

    pub fn ceil_constant(&self) -> Option<Value> {
        Some(if self.has_float() {
            Self::make_const_float(self.as_float().unwrap().ceil())
        } else if self.has_double() {
            Self::make_const_double(self.as_double().unwrap().ceil())
        } else {
            return None;
        })
    }

    pub fn trunc_constant(&self) -> Option<Value> {
        Some(if self.has_float() {
            Self::make_const_float(self.as_float().unwrap().trunc())
        } else if self.has_double() {
            Self::make_const_double(self.as_double().unwrap().trunc())
        } else if self.has_int64() {
            Self::make_const32(self.as_int64().unwrap() as i32)
        } else {
            return None;
        })
    }

    pub fn zext32_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const64(self.as_int32().unwrap() as i64)
        } else {
            return None;
        })
    }

    pub fn sext16_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const32(self.as_int32().unwrap() as u16 as i16 as i32)
        } else {
            return None;
        })
    }

    pub fn sext8_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const32(self.as_int32().unwrap() as u32 as u8 as i8 as i32)
        } else {
            return None;
        })
    }

    pub fn sext32_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const64(self.as_int32().unwrap() as i64)
        } else {
            return None;
        })
    }

    pub fn sext16to64_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const64(self.as_int32().unwrap() as i64)
        } else {
            return None;
        })
    }

    pub fn sext8to64_constant(&self) -> Option<Value> {
        Some(if self.has_int32() {
            Self::make_const64(self.as_int32().unwrap() as i64)
        } else {
            return None;
        })
    }

    pub fn add_constant_i32(&self, other: i32) -> Option<Value> {
        if self.has_float() {
            Some(Self::make_const_float(
                self.as_float().unwrap() + other as f32,
            ))
        } else if self.has_double() {
            Some(Self::make_const_double(
                self.as_double().unwrap() + other as f64,
            ))
        } else if self.has_int32() {
            Some(Self::make_const32(
                self.as_int32().unwrap().wrapping_add(other),
            ))
        } else if self.has_int64() {
            Some(Self::make_const64(
                self.as_int64().unwrap().wrapping_add(other as i64),
            ))
        } else if self.has_int128() {
            Some(Self::make_const128(
                self.as_int128().unwrap().wrapping_add(other as i128),
            ))
        } else {
            None
        }
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

    pub fn fmax_constant(&self, other: &Value) -> Option<Value> {
        if self.has_float() && other.has_float() {
            Some(Self::make_const_float(
                self.as_float()?.max(other.as_float()?),
            ))
        } else if self.has_double() && other.has_double() {
            Some(Self::make_const_double(
                self.as_double()?.max(other.as_double()?),
            ))
        } else {
            None
        }
    }

    pub fn fmin_constant(&self, other: &Value) -> Option<Value> {
        if self.has_float() && other.has_float() {
            Some(Self::make_const_float(
                self.as_float()?.min(other.as_float()?),
            ))
        } else if self.has_double() && other.has_double() {
            Some(Self::make_const_double(
                self.as_double()?.min(other.as_double()?),
            ))
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

    pub fn as_tri_state(&self) -> TriState {
        match self.kind.opcode() {
            Opcode::Const32 => tri_state(self.as_int32().unwrap() != 0),
            Opcode::Const64 => tri_state(self.as_int64().unwrap() != 0),

            Opcode::ConstDouble => tri_state(self.as_double().unwrap() != 0.0),

            Opcode::ConstFloat => tri_state(self.as_float().unwrap() != 0.0),

            _ => TriState::Undeterminate,
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
            _ => (),
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
        if matches!(rep, ValueRep::ColdAny) {
            self.children.push(value);
            return;
        }
        let nchild = self.children.len();
        let stackmap = self.stackmap_mut().unwrap();

        while stackmap.reps.len() < nchild {
            stackmap.reps.push(ValueRep::ColdAny);
        }
        stackmap.reps.push(rep);
        self.children.push(value);
    }

    pub fn stackmap_append_some_register(&mut self, value: ValueId) {
        self.stackmap_append_with_rep(value, ValueRep::SomeRegister)
    }

    pub fn stackmap_append_some_register_with_clobber(&mut self, value: ValueId) {
        self.stackmap_append_with_rep(value, ValueRep::SomeRegisterWithClobber)
    }

    pub fn constrained_child(&self, index: usize) -> ConstrainedValue {
        assert!(self.stackmap().is_some());

        let stackmap = self.stackmap().unwrap();

        let rep = if index < stackmap.reps.len() {
            stackmap.reps[index]
        } else {
            ValueRep::ColdAny
        };

        ConstrainedValue::new(self.children[index], rep)
    }

    pub fn set_constraint(&mut self, index: usize, rep: ValueRep) {
        assert!(self.stackmap().is_some());

        if rep == ValueRep::ColdAny {
            return;
        }

        let stackmap = self.stackmap_mut().unwrap();

        while stackmap.reps.len() <= index {
            stackmap.reps.push(ValueRep::ColdAny);
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

    pub fn is_negative_zero(&self) -> bool {
        if self.has_double() {
            let value = self.as_double().unwrap();

            value == -0.0
        } else if self.has_float() {
            let value = self.as_float().unwrap();

            value == -0.0
        } else {
            false
        }
    }

    pub fn is_sensitive_to_nan(&self) -> bool {
        self.kind.is_sensitive_to_nan()
    }

    pub fn key(&self) -> Option<ValueKey> {
        Some(match self.kind.opcode() {
            Opcode::FramePointer => ValueKey::new(self.kind, self.typ),
            Opcode::Identity
            | Opcode::Opaque
            | Opcode::Abs
            | Opcode::Floor
            | Opcode::Ceil
            | Opcode::Sqrt
            | Opcode::Neg
            | Opcode::Depend
            | Opcode::SExt8
            | Opcode::SExt16
            | Opcode::SExt8To64
            | Opcode::SExt16To64
            | Opcode::SExt32
            | Opcode::ZExt32
            | Opcode::Clz
            | Opcode::Trunc
            | Opcode::IToD
            | Opcode::IToF
            | Opcode::DToI
            | Opcode::FToI
            | Opcode::FloatToDouble
            | Opcode::DoubleToFloat => ValueKey::new_unary(self.kind, self.typ, self.children[0]),

            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::UDiv
            | Opcode::Mod
            | Opcode::UMod
            | Opcode::FMax
            | Opcode::FMin
            | Opcode::BitAnd
            | Opcode::BitOr
            | Opcode::BitXor
            | Opcode::Shl
            | Opcode::SShr
            | Opcode::ZShr
            | Opcode::RotR
            | Opcode::RotL
            | Opcode::Equal
            | Opcode::NotEqual
            | Opcode::LessThan
            | Opcode::LessEqual
            | Opcode::GreaterThan
            | Opcode::GreaterEqual
            | Opcode::Above
            | Opcode::AboveEqual
            | Opcode::Below
            | Opcode::BelowEqual
            | Opcode::EqualOrUnordered => {
                ValueKey::new_binary(self.kind, self.typ, self.children[0], self.children[1])
            }

            Opcode::Select => ValueKey::new_select(
                self.kind,
                self.typ,
                self.children[0],
                self.children[1],
                self.children[2],
            ),

            Opcode::Const32 => {
                ValueKey::new_int64(self.kind, self.typ(), self.as_int32().unwrap() as _)
            }

            Opcode::Const64 => ValueKey::new_int64(self.kind, self.typ(), self.as_int64().unwrap()),

            Opcode::ConstDouble => {
                ValueKey::new_double(self.kind, self.typ(), self.as_double().unwrap())
            }

            Opcode::ConstFloat => {
                ValueKey::new_float(self.kind, self.typ(), self.as_float().unwrap())
            }

            Opcode::ArgumentReg => ValueKey::new_int64(
                self.kind,
                self.typ(),
                self.argument_reg().unwrap().index() as _,
            ),

            Opcode::SlotBase => ValueKey::new_int64(
                self.kind,
                self.typ(),
                self.slot_base_value().unwrap().index() as _,
            ),

            _ => return None,
        })
    }
}

/// We use this class to describe value representations at stackmaps. It's used both to force a
/// representation and to get the representation. When the B3 client forces a representation, we say
/// that it's an input. When B3 tells the client what representation it picked, we say that it's an
/// output.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub enum ValueRep {
    /// As an input representation, this means that B3 can pick any representation. As an output
    /// representation, this means that we don't know. This will only arise as an output
    /// representation for the active arguments of Check/CheckAdd/CheckSub/CheckMul.
    WarmAny,

    /// Same as WarmAny, but implies that the use is cold. A cold use is not counted as a use for
    /// computing the priority of the used temporary.
    #[default]
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
    SomeLateRegister(Reg),

    /// As an input representation, this forces a particular register. As an output
    /// representation, this tells us what register B3 picked.
    Register(Reg),

    /// As an input representation, this forces a particular register and states that
    /// the register is used late. This means that the register is used after the result
    /// is defined (i.e, the result will interfere with this as an input).
    /// It's not a valid output representation.
    LateRegister(Reg),

    /// As an output representation, this tells us what stack slot B3 picked. It's not a valid
    /// input representation.
    Stack(isize),

    /// As an input representation, this forces the value to end up in the argument area at some
    /// offset. As an output representation this tells us what offset from SP B3 picked.
    StackArgument(isize),

    /// As an output representation, this tells us that B3 constant-folded the value.
    Constant(i64),
}

// type ValueRepKind = std::mem::Discriminant<ValueRep>;

impl std::fmt::Display for ValueRep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ValueRep {
    // pub fn kind(&self) -> ValueRepKind {
    //     std::mem::discriminant(self)
    // }

    pub fn is_any(&self) -> bool {
        match self {
            ValueRep::WarmAny | ValueRep::ColdAny | ValueRep::LateColdAny => true,
            _ => false,
        }
    }

    pub fn is_reg(&self) -> bool {
        match self {
            ValueRep::Register(_) | ValueRep::LateRegister(_) | ValueRep::SomeLateRegister(_) => {
                true
            }
            _ => false,
        }
    }

    pub fn get_reg(&self) -> Reg {
        assert!(self.is_reg());
        match self {
            ValueRep::SomeLateRegister(reg)
            | ValueRep::Register(reg)
            | ValueRep::LateRegister(reg) => *reg,
            _ => unreachable!(),
        }
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
        match self {
            Self::Stack(_) => true,
            _ => false,
        }
    }
    pub fn is_stack_argument(&self) -> bool {
        match self {
            Self::StackArgument(_) => true,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Self::Constant(_) => true,
            _ => false,
        }
    }

    pub fn value(&self) -> i64 {
        assert!(self.is_constant());
        match self {
            Self::Constant(value) => value,
            _ => false,
        }
    }

    pub fn with_offset(&self, offset: isize) -> ValueRep {
        match self {
            ValueRep::Stack(offset_from_fp) => ValueRep::stack(offset_from_fp + offset),
            ValueRep::StackArgument(offset_from_sp) => {
                ValueRep::stack_argument(offset_from_sp + offset)
            }
            _ => *self,
        }
    }

    pub fn offset_from_fp(&self) -> isize {
        assert!(self.is_stack());
        match self {
            Self::Stack(offset) => offset,
            _ => false,
        }
    }

    pub fn offset_from_sp(&self) -> isize {
        assert!(self.is_stack_argument());
        match self {
            Self::StackArgument(offset) => offset,
            _ => false,
        }
    }
}

// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
// pub enum ValueRepKind {
//     /// As an input representation, this means that B3 can pick any representation. As an output
//     /// representation, this means that we don't know. This will only arise as an output
//     /// representation for the active arguments of Check/CheckAdd/CheckSub/CheckMul.
//     WarmAny,

//     /// Same as WarmAny, but implies that the use is cold. A cold use is not counted as a use for
//     /// computing the priority of the used temporary.
//     ColdAny,

//     /// Same as ColdAny, but also implies that the use occurs after all other effects of the stackmap
//     /// value.
//     LateColdAny,

//     /// As an input representation, this means that B3 should pick some register. It could be a
//     /// register that this claims to clobber!
//     SomeRegister,

//     /// As an input representation, this means that B3 should pick some register but that this
//     /// register is then cobbered with garbage. This only works for patchpoints.
//     SomeRegisterWithClobber,

//     /// As an input representation, this tells us that B3 should pick some register, but implies
//     /// that the def happens before any of the effects of the stackmap. This is only valid for
//     /// the result constraint of a Patchpoint.
//     SomeEarlyRegister,

//     /// As an input representation, this tells us that B3 should pick some register, but implies
//     /// the use happens after any defs. This is only works for patchpoints.
//     SomeLateRegister,

//     /// As an input representation, this forces a particular register. As an output
//     /// representation, this tells us what register B3 picked.
//     Register,

//     /// As an input representation, this forces a particular register and states that
//     /// the register is used late. This means that the register is used after the result
//     /// is defined (i.e, the result will interfere with this as an input).
//     /// It's not a valid output representation.
//     LateRegister,

//     /// As an output representation, this tells us what stack slot B3 picked. It's not a valid
//     /// input representation.
//     Stack,

//     /// As an input representation, this forces the value to end up in the argument area at some
//     /// offset. As an output representation this tells us what offset from SP B3 picked.
//     StackArgument,

//     /// As an output representation, this tells us that B3 constant-folded the value.
//     Constant,
// }

pub struct ConstrainedValue {
    pub value: ValueId,
    pub rep: ValueRep,
}

impl std::fmt::Display for ConstrainedValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.value.0, self.rep)
    }
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

/// ValueKeys are useful for CSE. They abstractly describe the value that a Value returns when it
/// executes. Any Value that has the same ValueKey is guaranteed to return the same value, provided
/// that they return a non-empty ValueKey. Operations that have effects, or that can have their
/// behavior affected by other operations' effects, will return an empty ValueKey. You have to use
/// other mechanisms for doing CSE for impure operations.
pub struct ValueKey {
    pub kind: Kind,
    pub typ: Type,
    pub indices: ValueIndices,
}

impl ValueKey {
    pub fn new(kind: Kind, ty: Type) -> Self {
        Self {
            kind,
            typ: ty,
            indices: ValueIndices { indices: [0; 4] },
        }
    }

    pub fn new_int64(kind: Kind, typ: Type, value: i64) -> Self {
        let mut this = Self::new(kind, typ);
        {
            this.indices.value = value;
        }
        this
    }

    pub fn new_double(kind: Kind, typ: Type, value: f64) -> Self {
        let mut this = Self::new(kind, typ);
        {
            this.indices.double_value = value;
        }
        this
    }

    pub fn new_float(kind: Kind, typ: Type, value: f32) -> Self {
        let mut this = Self::new(kind, typ);
        {
            this.indices.float_value = value;
        }
        this
    }

    pub fn new_unary(kind: Kind, typ: Type, child: ValueId) -> Self {
        let mut this = Self::new(kind, typ);
        unsafe {
            this.indices.indices[0] = child.index();
        }
        this
    }

    pub fn new_binary(kind: Kind, typ: Type, left: ValueId, right: ValueId) -> Self {
        let mut this = Self::new(kind, typ);
        unsafe {
            this.indices.indices[0] = left.index();
            this.indices.indices[1] = right.index();
        }
        this
    }

    pub fn new_select(kind: Kind, typ: Type, cond: ValueId, left: ValueId, right: ValueId) -> Self {
        let mut this = Self::new(kind, typ);
        unsafe {
            this.indices.indices[0] = cond.index();
            this.indices.indices[1] = left.index();
            this.indices.indices[2] = right.index();
        }
        this
    }

    pub fn kind(&self) -> Kind {
        assert_ne!(self.opcode(), Opcode::Nop);
        self.kind
    }

    pub fn opcode(&self) -> Opcode {
        self.kind.opcode()
    }

    pub fn typ(&self) -> Type {
        self.typ
    }

    pub fn child_index(&self, index: usize) -> usize {
        unsafe { self.indices.indices[index] }
    }

    pub fn child(&self, _proc: &Procedure, index: usize) -> ValueId {
        ValueId(self.child_index(index))
    }

    pub fn can_materialize(&self) -> bool {
        match self.opcode() {
            Opcode::CheckAdd | Opcode::CheckSub | Opcode::CheckMul => false,
            _ => true,
        }
    }

    pub fn empty() -> Self {
        Self {
            kind: Opcode::Oops.into(),
            typ: Type::Void,
            indices: ValueIndices { indices: [0; 4] },
        }
    }

    pub fn is_constant(&self) -> bool {
        self.opcode().is_constant()
    }

    pub fn value(&self) -> i64 {
        unsafe { self.indices.value }
    }

    pub fn double_value(&self) -> f64 {
        unsafe { self.indices.double_value }
    }

    pub fn float_value(&self) -> f32 {
        unsafe { self.indices.float_value }
    }

    pub fn int_constant(typ: Type, value: i64) -> Self {
        match typ.kind() {
            TypeKind::Int32 => Self::new_int64(Opcode::Const32.into(), typ, value),
            TypeKind::Int64 => Self::new_int64(Opcode::Const64.into(), typ, value),
            _ => unreachable!(),
        }
    }

    pub fn materialize(&self, proc: &mut Procedure) -> ValueId {
        // NOTE: We sometimes cannot return a ValueId for some key, like for Check and friends. That's because
        // though those nodes have side exit effects. It would be weird to materialize anything that has a side
        // exit. We can't possibly know enough about a side exit to know where it would be safe to emit one.
        match self.opcode() {
            Opcode::FramePointer => {
                let value = proc.add(Value::new(
                    self.kind(),
                    self.typ(),
                    NumChildren::Zero,
                    &[],
                    ValueData::None,
                ));

                value
            }

            Opcode::Identity
            | Opcode::Opaque
            | Opcode::Abs
            | Opcode::Floor
            | Opcode::Ceil
            | Opcode::Sqrt
            | Opcode::Neg
            | Opcode::Depend
            | Opcode::SExt8
            | Opcode::SExt16
            | Opcode::SExt8To64
            | Opcode::SExt16To64
            | Opcode::SExt32
            | Opcode::ZExt32
            | Opcode::Clz
            | Opcode::Trunc
            | Opcode::IToD
            | Opcode::IToF
            | Opcode::DToI
            | Opcode::FToI
            | Opcode::FloatToDouble
            | Opcode::DoubleToFloat => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::One,
                &[self.child(proc, 0)],
                ValueData::None,
            )),

            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::UDiv
            | Opcode::Mod
            | Opcode::UMod
            | Opcode::FMax
            | Opcode::FMin
            | Opcode::BitAnd
            | Opcode::BitOr
            | Opcode::BitXor
            | Opcode::Shl
            | Opcode::SShr
            | Opcode::ZShr
            | Opcode::RotR
            | Opcode::RotL
            | Opcode::Equal
            | Opcode::NotEqual
            | Opcode::LessThan
            | Opcode::LessEqual
            | Opcode::GreaterThan
            | Opcode::GreaterEqual
            | Opcode::Above
            | Opcode::AboveEqual
            | Opcode::Below
            | Opcode::BelowEqual
            | Opcode::EqualOrUnordered => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Two,
                &[self.child(proc, 0), self.child(proc, 1)],
                ValueData::None,
            )),

            Opcode::Select => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Three,
                &[
                    self.child(proc, 0),
                    self.child(proc, 1),
                    self.child(proc, 2),
                ],
                ValueData::None,
            )),

            Opcode::Const32 => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::Const32(self.value() as i32),
            )),

            Opcode::Const64 => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::Const64(self.value()),
            )),

            Opcode::ConstDouble => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::Double(self.double_value().to_bits()),
            )),

            Opcode::ConstFloat => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::Float(self.float_value().to_bits()),
            )),

            Opcode::ArgumentReg => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::Argument(Reg::from_index(self.value() as _)),
            )),

            Opcode::SlotBase => proc.add(Value::new(
                self.kind(),
                self.typ(),
                NumChildren::Zero,
                &[],
                ValueData::SlotBase(StackSlotId(self.value() as _)),
            )),

            _ => ValueId(usize::MAX),
        }
    }

    pub fn is_empty(&self) -> bool {
        self != &Self::empty()
    }
}

impl PartialEq for ValueKey {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.typ == other.typ
            && unsafe { self.indices.indices == other.indices.indices }
    }
}

impl Eq for ValueKey {}

impl Hash for ValueKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.typ.hash().hash(state);
        unsafe {
            self.indices.indices.hash(state);
        }
    }
}

pub union ValueIndices {
    pub indices: [usize; 4],
    pub value: i64,
    pub double_value: f64,
    pub float_value: f32,
}
