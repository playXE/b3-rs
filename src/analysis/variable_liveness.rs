use crate::{
    analysis::liveness::{Liveness, LivenessAdapter},
    block::BlockId,
    opcode::Opcode,
    procedure::Procedure,
    variable::VariableId,
};

pub struct VariableLivenessAdapter<'a> {
    pub cfg: &'a mut Procedure,
}

impl<'a> LivenessAdapter for VariableLivenessAdapter<'a> {
    type CFG = Procedure;
    type Thing = VariableId;
    fn block_size(&self, block: BlockId) -> usize {
        self.cfg.block(block).values.len()
    }

    fn cfg(&self) -> &Self::CFG {
        self.cfg
    }

    fn for_each_def<F>(
        &self,
        block: <<Self as LivenessAdapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        let value = self
            .cfg
            .block(block)
            .values
            .get(value_boundary_index.wrapping_sub(1));

        match value {
            Some(x) if self.cfg.value(*x).kind.opcode() == Opcode::Set => {
                func(self.cfg.value(*x).as_variable().unwrap());
            }

            _ => (),
        }
    }

    fn for_each_use<F>(
        &self,
        block: <<Self as LivenessAdapter>::CFG as crate::analysis::dominators::Graph>::Node,
        value_boundary_index: usize,
        mut func: F,
    ) where
        F: FnMut(Self::Thing),
    {
        let value = self.cfg.block(block).values.get(value_boundary_index);

        match value {
            Some(x) if self.cfg.value(*x).kind.opcode() == Opcode::Get => {
                func(self.cfg.value(*x).as_variable().unwrap());
            }

            _ => (),
        }
    }

    fn prepare_to_compute(&mut self) {}

    fn index_to_value(_: &Self::CFG, index: usize) -> Self::Thing {
        VariableId(index)
    }

    fn num_indices(&self) -> usize {
        self.cfg.variables.size()
    }

    fn value_to_index(_: &Self::CFG, thing: Self::Thing) -> usize {
        thing.0
    }
}

pub type VariableLiveness<'a> = Liveness<'a, VariableLivenessAdapter<'a>>;
