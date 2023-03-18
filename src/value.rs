use std::ops::Range;

use tinyvec::TinyVec;

use crate::{
    block::{BlockId, FrequentBlock, BasicBlock},
    effects::Effects,
    kind::Kind,
    opcode::Opcode,
    sparse_collection::SparseElement,
    typ::{Type, TypeKind},
    variable::VariableId,
    *, procedure::Procedure, 
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
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
    Argument(usize),
    CCallValue(Effects),
    Variable(VariableId),
    Upsilon(Option<ValueId>),
}

impl Value {
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

    pub fn fmt_successors<W: std::fmt::Write>(&self, f: &mut W, _proc: &Procedure, block: &BasicBlock) -> std::fmt::Result {
        if self.kind.opcode() == Opcode::Branch && block.successor_list().len() == 2 {
            write!(f, "Then: block{}, Else: block{}", block.taken().0.0, block.not_taken().0.0)
        } else {
            for (i, succ) in block.successor_list().iter().enumerate() {
                write!(f, "block{}", succ.0.0)?;
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
            ValueData::None => {},
            ValueData::Const128(x) => write!(f, " ${:x}", x)?,
            ValueData::Const64(x) => write!(f, " ${:x}", x)?,
            ValueData::Const32(x) => write!(f, " ${:x}", x)?,
            ValueData::Float(x) => write!(f, " ${:x}", x)?,
            ValueData::Double(x) => write!(f, " ${:x}", x)?,
            ValueData::Variable(x) => write!(f, " var@{}", x.0)?,
            ValueData::MemoryValue { offset, .. } => write!(f, " ${:x}", offset)?,
            ValueData::Argument(x) => write!(f, "arg@{}", x)?,
            ValueData::Upsilon(x) => match x {
                Some(x) => write!(f, " phi=v@{}", x.0)?,
                None => write!(f, " phi=none")?,
            },
            _ => todo!()
        }

        write!(f, ")")?;

        Ok(())
    }

    pub fn replace_with_value(&mut self, kind: impl Into<Kind>, typ: Type, owner: Option<BlockId>, value: ValueId) {
        self.kind = kind.into();
        self.typ = typ;
        self.owner = owner;

        self.children.clear();
        self.data = ValueData::None;
        self.children.push(value);
    }

    pub fn replace_with(&mut self, kind: impl Into<Kind>, typ: Type, owner: Option<BlockId>,) {
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

        for (i,child) in proc.value(val).children.iter().copied().collect::<Vec<_>>().into_iter().enumerate() {
            
            if proc.value(child).kind.opcode() == Opcode::Identity {
                result = true;
                proc.value_mut(val).children[i] = Value::fold_identity(child, proc);
            }
        }

        result
    }
}
