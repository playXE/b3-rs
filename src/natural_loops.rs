use std::collections::HashSet;

use indexmap::IndexMap;

use crate::dominators::{Dominators, Graph};

#[derive(Debug)]
pub struct NaturalLoop<G: Graph> {
    body: Vec<G::Node>,
    header: G::Node,
    outer_loop_index: usize,
    index: usize,
}

impl<G: Graph> NaturalLoop<G> {
    pub fn new(header: G::Node, index: usize) -> Self {
        Self {
            body: vec![],
            header,
            outer_loop_index: usize::MAX,
            index,
        }
    }

    pub fn body(&self) -> &[G::Node] {
        &self.body
    }

    pub fn header(&self) -> G::Node {
        self.header
    }

    pub fn outer_loop_index(&self) -> usize {
        self.outer_loop_index
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn contains(&self, block: G::Node) -> bool {
        self.body.contains(&block)
    }

    pub fn is_outer_most_loop(&self) -> bool {
        self.outer_loop_index == usize::MAX
    }
}

#[derive(Debug)]
pub struct NaturalLoops<G: Graph> {
    loops: Vec<NaturalLoop<G>>,
    inner_most_loop_indices: IndexMap<G::Node, [usize; 2]>,
}

impl<G: Graph> NaturalLoops<G> {
    pub fn new<'a>(graph: &'a G, dominators: &'a Dominators<G>) -> Self {
        let mut this = Self {
            loops: vec![],
            inner_most_loop_indices: IndexMap::new(),
        };

        // Implement the classic dominator-based natural loop finder. The first
        // step is to find all control flow edges A -> B where B dominates A.
        // Then B is a loop header and A is a backward branching block. We will
        // then accumulate, for each loop header, multiple backward branching
        // blocks. Then we backwards graph search from the backward branching
        // blocks to their loop headers, which gives us all of the blocks in the
        // loop body.

        for block_index in (0..graph.num_nodes()).rev() {
            let header = graph.node(block_index);

            if let Some(header) = header {
                for i in (0..graph.predecessors(header).len()).rev() {
                    let footer = graph.predecessors(header)[i];

                    if !dominators.dominates(header, footer) {
                        continue;
                    }

                    let mut found = false;

                    // At this point, we've proven 'header' is actually a loop header and
                    // that 'footer' is a loop footer.

                    for j in (0..this.loops.len()).rev() {
                        let loop_ = &mut this.loops[j];

                        if loop_.header == header {
                            loop_.body.push(footer);
                            found = true;
                            break;
                        }
                    }

                    if found {
                        continue;
                    }

                    let mut l = NaturalLoop::new(header, this.loops.len());
                    l.body.push(footer);
                    this.loops.push(l);
                }
            }
        }

        let mut seen_blocks = HashSet::new();

        let mut block_worklist = vec![];

        seen_blocks.reserve(graph.num_nodes());

        for i in (0..this.loops.len()).rev() {
            let loop_ = &mut this.loops[i];

            seen_blocks.clear();

            for j in (0..loop_.body.len()).rev() {
                seen_blocks.insert(loop_.body[j]);
                block_worklist.push(loop_.body[j]);
            }

            while let Some(block) = block_worklist.pop() {
                if block == loop_.header {
                    continue;
                }

                for j in (0..graph.predecessors(block).len()).rev() {
                    let predecessor = graph.predecessors(block)[j];

                    if seen_blocks.contains(&predecessor) {
                        continue;
                    }

                    loop_.body.push(predecessor);
                    block_worklist.push(predecessor);
                    seen_blocks.insert(predecessor);
                }
            }
        }

        for block_index in (0..graph.num_nodes()).rev() {
            let block = graph.node(block_index);

            if let Some(block) = block {
                this.inner_most_loop_indices.insert(block, [usize::MAX; 2]);
            }
        }

        for loop_index in (0..this.loops.len()).rev() {
            let loop_ = &this.loops[loop_index];

            for block_index_in_loop in (0..loop_.body.len()).rev() {
                let block = loop_.body[block_index_in_loop];

                for i in 0..2 {
                    let this_index = this.inner_most_loop_indices.get(&block).unwrap()[i];

                    if this_index == usize::MAX
                        || loop_.body.len() < this.loops[this_index].body.len()
                    {
                        insert_into_bounded_vector(
                            this.inner_most_loop_indices.get_mut(&block).unwrap(),
                            2,
                            loop_index,
                            i,
                        );
                        break;  
                    }
                }
            }
        }

        for i in (0..this.loops.len()).rev() {
            let loop_ = &mut this.loops[i];

            loop_.outer_loop_index = this.inner_most_loop_indices.get(&loop_.header).unwrap()[1];
        }


        this
    }

    pub fn num_loops(&self) -> usize {
        self.loops.len()
    }

    pub fn loop_(&self, index: usize) -> &'_ NaturalLoop<G> {
        &self.loops[index]
    }

    pub fn inner_most_loop_of(&self, block: G::Node) -> Option<&NaturalLoop<G>> {
        let indices = self.inner_most_loop_indices.get(&block).unwrap();

        if indices[0] == usize::MAX {
            None
        } else {
            Some(&self.loops[indices[0]])
        }
    }

    pub fn inner_most_outer_loop_of(&self, l: &NaturalLoop<G>) -> Option<&NaturalLoop<G>> {
        if l.outer_loop_index == usize::MAX {
            None
        } else {
            Some(&self.loops[l.outer_loop_index])
        }
    }

    pub fn header_of(&self, block: G::Node) -> Option<&NaturalLoop<G>> {
        let l = self.inner_most_loop_of(block);

        if let Some(l) = l {
            if l.header == block {
                return Some(l);
            } else {
                return None;
            }
        } else {
            return None;
        }
    }

    pub fn belongs_to(&self, block: G::Node, candidate_loop: &NaturalLoop<G>) -> bool {
        if candidate_loop.body.len() < 4 {
            return candidate_loop.contains(block);
        }

        let mut l = self.inner_most_loop_of(block);

        while let Some(l_) = l {
            if l_ as *const _ == candidate_loop as *const _ {
                return true;
            }

            l = self.inner_most_outer_loop_of(l_);
        }

        false
    }

    pub fn loop_depth(&self, block: G::Node) -> usize {
        let mut l = self.inner_most_loop_of(block);
        let mut depth = 0;

        while let Some(l_) = l {
            depth += 1;
            l = self.inner_most_outer_loop_of(l_);
        }

        depth
    }

    pub fn loops_of(&self, block: G::Node) -> Vec<&NaturalLoop<G>> {
        let mut l = self.inner_most_loop_of(block);
        let mut loops = vec![];

        while let Some(l_) = l {
            loops.push(l_);
            l = self.inner_most_outer_loop_of(l_);
        }

        loops
    }
}

fn insert_into_bounded_vector<T: Copy>(v: &mut [T], size: usize, element: T, index: usize) {
    let mut i = size;

    while {
        let res = i > index + 1;

        i -= 1;
        res
    } {
        v[i] = v[i - 1];
    }

    v[index] = element;
}
