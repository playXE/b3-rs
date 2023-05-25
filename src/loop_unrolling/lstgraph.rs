use std::collections::HashSet;

use tinyvec::TinyVec;

use crate::{utils::index_set::KeyIndex, BlockId};

/// An analysis to detect strongly connected components.
pub struct Loops {}

/// Represents a single strongly connected component.
pub struct Node {
    pub headers: TinyVec<[BlockId; 1]>,
    pub breakers: TinyVec<[BlockId; 1]>,
    pub backedges: TinyVec<[BlockId; 1]>,

    pub children: TinyVec<[NodeId; 1]>,

    pub entries: TinyVec<[BlockId; 1]>,
    pub exits: TinyVec<[BlockId; 1]>,

    pub parent: Option<NodeId>,

    pub all_members: TinyVec<[BlockId; 1]>,
}

impl Node {
    pub fn new(
        parent: Option<NodeId>,
        header_blocks: TinyVec<[BlockId; 1]>,
        breaker_blocks: TinyVec<[BlockId; 1]>,
        back_edge_blocks: TinyVec<[BlockId; 1]>,
        members: &[BlockId],
        entries: HashSet<BlockId>,
        exits: HashSet<BlockId>,
    ) -> Self {
        Self {
            headers: header_blocks,
            breakers: breaker_blocks,
            backedges: back_edge_blocks,
            children: TinyVec::new(),
            entries: entries.into_iter().collect(),
            exits: exits.into_iter().collect(),
            parent,
            all_members: members.iter().copied().collect(),
        }
    }

    pub fn count(&self) -> usize {
        self.all_members.len()
    }

    pub fn is_nested_loop(&self) -> bool {
        self.parent.is_some()
    }

    pub fn is_innermost_loop(&self) -> bool {
        self.children.len() < 1 
    }

    pub fn contains(&self, block: BlockId) -> bool {
        self.all_members.contains(&block)
    }

    pub fn contains_back_edge_block(&self, blocks: &[BlockId]) -> bool {
        blocks.iter().any(|&block| self.backedges.contains(&block))
    }

    pub fn add_child(&mut self, child: NodeId) {
        self.children.push(child);
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(pub usize);

impl Into<usize> for NodeId {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for NodeId {
    fn from(x: usize) -> Self {
        Self(x)
    }
}

impl KeyIndex for NodeId {
    fn index(&self) -> usize {
        self.0
    }
}
