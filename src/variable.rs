use crate::{bank::{bank_for_type, Bank}, typ::Type, width::{Width, width_for_type}, sparse_collection::SparseElement};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable {
    index: usize,
    typ: Type,
}

impl Variable {
    pub const fn new(index: usize, typ: Type) -> Self {
        Self { index, typ }
    }

    pub const fn index(&self) -> usize {
        self.index
    }

    pub const fn typ(&self) -> Type {
        self.typ
    }

    pub fn bank(&self) -> Bank {
        bank_for_type(self.typ)
    }

    pub fn width(&self) -> Width {
        width_for_type(self.typ)
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} var{}", self.typ, self.index)
    }
}
    
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VariableId(pub usize);

impl From<usize> for VariableId {
    fn from(x: usize) -> Self {
        VariableId(x)
    }
}

impl Into<usize> for VariableId {
    fn into(self) -> usize {
        self.0
    }
}

impl SparseElement for Variable {
    type Id = VariableId;

    fn id(&self) -> Self::Id {
        VariableId(self.index)
    }

    fn set_id(&mut self, id: Self::Id) {
        self.index = id.0;
    }
}