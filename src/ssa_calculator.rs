use std::{
    cell::Cell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use indexmap::IndexMap;

use crate::{
    block::BlockId, dominators::Dominators, procedure::Procedure, value::ValueId,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SSAVariableId(pub usize);

pub struct SSAVariable {
    pub blocks_with_defs: Vec<BlockId>,
    pub index: usize,
}

impl SSAVariable {
    pub fn new(index: usize) -> Self {
        Self {
            blocks_with_defs: Vec::new(),
            index,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug)]
pub struct SSADef {
    pub variable: SSAVariableId,
    pub block: BlockId,
    pub value: Cell<ValueId>,
}

pub struct SSABlockData {
    pub defs: HashMap<SSAVariableId, Rc<SSADef>>,
    pub phis: Vec<Rc<SSADef>>,
}

/// SSACalculator provides a reusable tool for building SSA's.
pub struct SSACalculator {
    pub variables: Vec<SSAVariable>,
    pub defs: LinkedList<Rc<SSADef>>,
    pub phis: LinkedList<Rc<SSADef>>,
    pub data: IndexMap<BlockId, SSABlockData>,
    pub dominators: Option<Dominators<Procedure>>,
}

impl SSACalculator {
    pub fn phis_for_block(&self, block: BlockId) -> &[Rc<SSADef>] {
        &self.data[&block].phis
    }

    pub fn variable(&self, index: SSAVariableId) -> &SSAVariable {
        &self.variables[index.0]
    }

    pub fn variable_mut(&mut self, index: SSAVariableId) -> &mut SSAVariable {
        &mut self.variables[index.0]
    }

    pub fn new(proc: &Procedure) -> Self {
        Self {
            variables: Vec::new(),
            defs: LinkedList::new(),
            phis: LinkedList::new(),
            data: IndexMap::from_iter((0..proc.blocks.len()).map(|i| {
                (
                    BlockId(i),
                    SSABlockData {
                        defs: HashMap::new(),
                        phis: Vec::new(),
                    },
                )
            })),
            dominators: None,
        }
    }

    pub fn reset(&mut self) {
        self.variables.clear();
        self.defs.clear();
        self.phis.clear();

        for block_index in 0..self.data.len() {
            self.data[block_index].defs.clear();
            self.data[block_index].phis.clear();
        }
    }

    pub fn new_variable(&mut self) -> SSAVariableId {
        let index = self.variables.len();
        self.variables.push(SSAVariable::new(index));
        SSAVariableId(index)
    }

    pub fn new_def(
        &mut self,
        variable: SSAVariableId,
        block: BlockId,
        value: ValueId,
    ) -> Rc<SSADef> {
        let def = Rc::new(SSADef {
            variable,
            block,
            value: Cell::new(value),
        });

        self.defs.push_back(def.clone());

        match self.data[&block].defs.insert(variable, def.clone()) {
            Some(r) => {
                r.value.set(value);
            }
            None => {
                self.variables[variable.0].blocks_with_defs.push(block);
            }
        }

        def
    }

    pub fn reaching_def_at_tail(
        &mut self,
        starting_block: BlockId,
        variable: SSAVariableId,
        proc: &Procedure,
    ) -> Option<Rc<SSADef>> {
        let mut block = Some(starting_block);
        while let Some(b) = block {
            if let Some(def) = self.data[&b].defs.get(&variable).cloned() {
                let mut other_block = Some(starting_block);

                while other_block != block {
                    self.data[&b].phis.push(def.clone());
                    other_block = self.dominators.as_ref().unwrap().idom(other_block.unwrap());
                }

                return Some(def.clone());
            }

            block = proc.dominators().idom(b);
        }

        None
    }

    pub fn non_local_reaching_def(
        &mut self,
        starting_block: BlockId,
        variable: SSAVariableId,
        proc: &Procedure,
    ) -> Option<Rc<SSADef>> {
        let block = self.dominators.as_ref().unwrap().idom(starting_block).unwrap();
        self.reaching_def_at_tail(block, variable, proc)
    }

    pub fn reaching_def_at_head(
        &mut self,
        starting_block: BlockId,
        variable: SSAVariableId,
        proc: &Procedure,
    ) -> Option<Rc<SSADef>> {
        self.non_local_reaching_def(starting_block, variable, proc)
    }

    pub fn compute_phis(
        &mut self,
        proc: &mut Procedure,
        mut f: impl FnMut(SSAVariableId, BlockId, &mut Procedure) -> Option<ValueId>,
    ) {
        for i in 0..self.variables.len() {
            let var = SSAVariableId(i);
            let blocks_with_defs = &self.variables[i].blocks_with_defs;
            // this `clone` is cheap, `Dominators` internally uses `Rc` to share the dominator tree
            let dominators = proc.dominators().clone();
            
            dominators.for_all_blocks_in_pruned_iterated_dominance_frontier_of_mut(
                &blocks_with_defs,
                proc,
                |proc, block| {

                    let phi = f(var, block, proc);

                    match phi {
                        Some(value) => {
                            let data = self.data.get_mut(&block).unwrap();

                            let phi_def = Rc::new(SSADef {
                                variable: var,
                                block,
                                value: Cell::new(value),
                            });

                            self.phis.push_back(phi_def.clone());
                            data.phis.push(phi_def.clone());
                            data.defs.insert(var, phi_def);

                            true
                        }
                        None => return false,
                    }
                },
            );
        }
    }

    pub fn display<'a>(&'a self, proc: &'a Procedure) -> SSACalculatorDisplay<'_> {
        SSACalculatorDisplay { calculator: self, proc }
    }
}

pub struct SSACalculatorDisplay<'a> {
    pub calculator: &'a SSACalculator,
    pub proc: &'a Procedure,
}

impl<'a> std::fmt::Display for SSACalculatorDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Variables: [")?;

        for i in 0..self.calculator.variables.len() {
            write!(f, "{}", self.calculator.variables[i])?;

            if i != self.calculator.variables.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "], Defs: [")?;

        for def in self.calculator.defs.iter() {
            write!(
                f,
                "def(var{}, block{}, v@{})",
                def.variable.0,
                def.block.0,
                def.value.get().0
            )?;
        }

        write!(f, "], Block data: [")?;

        for block_index in 0..self.proc.blocks.len() {
            let block = &self.proc.blocks[block_index];

            write!(f, "block{}=>(", block.index)?;
            write!(f, "Defs: {{")?;

            for (i, entry) in self
                .calculator
                .data
                .get(&BlockId(block.index))
                .unwrap()
                .defs
                .iter()
                .enumerate()
            {
                write!(f, "{}->{}", entry.0 .0, entry.1.value.get().0)?;
                if i != self.calculator.data[&BlockId(block.index)].defs.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "}}, Phis: {{")?;

            for (i, def) in self
                .calculator
                .data
                .get(&BlockId(block.index))
                .unwrap()
                .phis
                .iter()
                .enumerate()
            {
                write!(
                    f,
                    "def(var{}, block{}, v@{})",
                    def.variable.0,
                    def.block.0,
                    def.value.get().0
                )?;
                if i != self.calculator.data[&BlockId(block.index)].phis.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, "}})")?;
        }

        write!(f, "]>")?;

        Ok(())
    }
}

impl std::fmt::Display for SSAVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.index)?;
        if !self.blocks_with_defs.is_empty() {
            write!(f, "(defs:")?;

            for (i, block) in self.blocks_with_defs.iter().enumerate() {
                let block: usize = (*block).into();
                write!(f, "block{}", block)?;
                if i != self.blocks_with_defs.len() - 1 {
                    write!(f, ",")?;
                }
            }

            write!(f, ")")?;
        }

        Ok(())
    }
}
