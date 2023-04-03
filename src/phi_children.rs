use tinyvec::TinyVec;

use crate::{
    dominators::GraphNodeWorklist,
    opcode::Opcode,
    procedure::Procedure,
    value::{ValueData, ValueId},
};

#[derive(Debug)]
pub struct PhiChildren {
    upsilons: Vec<Vec<ValueId>>,
    phis: TinyVec<[ValueId; 8]>,
}

impl PhiChildren {
    pub fn new(proc: &Procedure) -> Self {
        let mut upsilons = vec![vec![]; proc.values.size()];
        let mut phis = TinyVec::new();

        for value in (0..proc.values.size()).map(ValueId) {
            if let Some(value_) = proc.values.at(value) {
                if let ValueData::Upsilon(ref phi) = value_.data {
                    let phi = phi.expect("PHIs should be resolved");

                    let vector = &mut upsilons[phi.0];

                    if vector.is_empty() {
                        phis.push(phi);
                    }

                    vector.push(value);
                }
            }
        }

        Self { upsilons, phis }
    }

    pub fn phis(&self) -> &[ValueId] {
        &self.phis
    }

    pub fn at<'a>(&'a self, value: ValueId) -> UpsilonCollection<'a> {
        UpsilonCollection::new(self, value, self.upsilons[value.0].clone())
    }
}

pub struct ValueCollection {
    values: Vec<ValueId>,
}

impl ValueCollection {
    pub fn new(values: Vec<ValueId>) -> Self {
        Self { values }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn at(&self, index: usize, proc: &Procedure) -> ValueId {
        proc.value(self.values[index]).children[0]
    }

    pub fn contains(&self, value: ValueId, proc: &Procedure) -> bool {
        for i in (0..self.values.len()).rev() {
            if self.at(i, proc) == value {
                return true;
            }
        }

        false
    }

    pub fn iter(&self) -> impl Iterator<Item = ValueId> + '_ {
        self.values.iter().copied()
    }
}

pub struct UpsilonCollection<'a> {
    phi_children: &'a PhiChildren,
    value: ValueId,
    values: Vec<ValueId>,
}

impl<'a> UpsilonCollection<'a> {
    pub fn new(phi_children: &'a PhiChildren, value: ValueId, values: Vec<ValueId>) -> Self {
        Self {
            phi_children,
            value,
            values,
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn at(&self, index: usize) -> ValueId {
        self.values[index]
    }

    pub fn contains(&self, value: ValueId) -> bool {
        self.values.contains(&value)
    }

    pub fn values(&self) -> ValueCollection {
        ValueCollection::new(self.values.clone())
    }

    pub fn iter(&self) -> impl Iterator<Item = ValueId> + '_ {
        self.values.iter().copied()
    }

    pub fn for_all_transitively_incoming_values(
        &self,
        mut f: impl FnMut(ValueId),
        proc: &Procedure,
    ) {
        if proc.value(self.value).kind.opcode() != Opcode::Phi {
            f(self.value);
            return;
        }

        let mut worklist = GraphNodeWorklist::new();

        worklist.push(self.value);

        while let Some(phi) = worklist.pop() {
            for child in self.phi_children.at(phi).values().iter() {
                if proc.value(child).kind.opcode() == Opcode::Phi {
                    worklist.push(phi);
                } else {
                    f(child);
                }
            }
        }
    }

    pub fn transitively_uses(&self, candidate: ValueId, proc: &Procedure) -> bool {
        let mut result = false;

        self.for_all_transitively_incoming_values(|child| result |= child == candidate, proc);

        result
    }
}
