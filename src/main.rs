use std::borrow::Cow;

use scheme_compiler::{
    block::BasicBlockBuilder, dominators::*, fix_ssa::fix_ssa, procedure::Procedure, typ::TypeKind,
};

#[derive(Debug)]
pub struct Block {
    id: usize,
    pred: Vec<usize>,
    succ: Vec<usize>,
}

#[derive(Debug)]
pub struct CFG {
    blocks: Vec<Block>,
}

impl CFG {
    pub fn new() -> Self {
        Self { blocks: vec![] }
    }

    pub fn add_block(&mut self) -> usize {
        let id = self.blocks.len();
        self.blocks.push(Block {
            id,
            pred: vec![],
            succ: vec![],
        });
        id
    }

    pub fn add_block_succ(&mut self, block_id: usize, succ_id: usize) {
        self.blocks[block_id].succ.push(succ_id);
        self.blocks[succ_id].pred.push(block_id);
    }

    pub fn add_block_pred(&mut self, block_id: usize, pred_id: usize) {
        self.blocks[block_id].pred.push(pred_id);
        self.blocks[pred_id].succ.push(block_id);
    }
}

impl Graph for CFG {
    type Node = usize;
    fn node_index(&self, node: Self::Node) -> usize {
        node
    }
    fn node(&self, index: usize) -> Option<Self::Node> {
        Some(self.blocks[index].id)
    }

    fn num_nodes(&self) -> usize {
        self.blocks.len()
    }

    fn predecessors(&self, block: Self::Node) -> Cow<[Self::Node]> {
        Cow::Borrowed(&self.blocks[block].pred)
    }

    fn successors(&self, block: Self::Node) -> Cow<[Self::Node]> {
        Cow::Borrowed(&self.blocks[block].succ)
    }

    fn root(&self) -> Self::Node {
        0
    }
}

fn main() {
    let mut proc = Procedure::new();

    let root = proc.add_block(1.0);
    let next = proc.add_block(1.0);
    let x = proc.add_variable(TypeKind::Int32.into());

    BasicBlockBuilder::new(&mut proc, root).add_argument(TypeKind::Int32.into(), 0, |inst, arg| {
        inst.add_variable_set(x, arg, |inst, _| {
            inst.add_jump(next);
        })
    });

    BasicBlockBuilder::new(&mut proc, next).add_variable_get(x, |inst, var| inst.add_return(var));
    proc.block_mut(next).add_predecessor(root);
    println!("{}", proc.display_());
    proc.dominators_or_compute();
    fix_ssa(&mut proc);
    println!("after fix_ssa:");

    println!("{}", proc.display_());
}
