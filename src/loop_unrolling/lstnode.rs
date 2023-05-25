use crate::{BlockId, utils::{bitvector::BitVector, index_set::KeyIndex}};

#[derive(Debug, Clone)]
pub struct LSTNode {
    /// Basic block which is the loop head
    pub header: BlockId,
    /// Basic blocks in the loop
    pub loop_: BitVector,

    pub depth: usize,

    /// If the loop is entered from the `loop_header` x times
    /// then the `loop_head` is executed `loop_multiplier * x` times
    pub loop_multiplier: f32,

    pub loop_exits: Vec<Edge>,
}

impl LSTNode {
    pub fn new(bb: BlockId) -> Self {
        Self {
            header: bb,
            loop_: BitVector::new(),
            loop_exits: Vec::new(),
            loop_multiplier: 0.0,
            depth: 0
        }
    }

    pub fn get_header(&self) -> BlockId {
        self.header
    }

    pub fn get_loop(&self) -> &BitVector {
        &self.loop_
    }

    pub fn initialize_loop_exits(&mut self) {
        self.loop_exits = Vec::new();
    }

    pub fn add_loop_exit(&mut self, source: BlockId, target: BlockId, probability: f32) {
        self.loop_exits.push(Edge::new(source, target, probability));
    }

    pub fn set_loop(&mut self, loop_: BitVector) {
        self.loop_ = loop_;
    }
}

#[derive(Clone)]
pub struct Edge {
    pub source: BlockId,
    pub target: BlockId,
    pub probability: f32 
}

impl Edge {
    pub const fn new(source: BlockId, target: BlockId, probability: f32) -> Self {
        Self {
            source,
            target,
            probability
        }
    }
}

impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"BB{}->BB{} prob = {}",self.source.0, self.target.0, self.probability)
    }
}

impl std::fmt::Debug for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"BB{}->BB{} prob = {}",self.source.0, self.target.0, self.probability)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LSTNodeId(pub usize);

impl Into<usize> for LSTNodeId {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for LSTNodeId {
    fn from(x: usize) -> Self {
        Self(x)
    }
}

impl KeyIndex for LSTNodeId {
    fn index(&self) -> usize {
        self.0
    }
}