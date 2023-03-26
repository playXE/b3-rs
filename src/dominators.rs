use std::{borrow::Cow, collections::HashSet, fmt::Debug, hash::Hash, marker::PhantomData, rc::Rc};

use crate::utils::index_set::{IndexMap, IndexSet, KeyIndex};

pub trait Graph {
    type Node: Copy
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + Debug
        + From<usize>
        + Into<usize>
        + KeyIndex;

    fn num_nodes(&self) -> usize;
    fn node(&self, index: usize) -> Option<Self::Node>;
    fn node_index(&self, node: Self::Node) -> usize;
    fn root(&self) -> Self::Node;
    fn successors(&self, block: Self::Node) -> Cow<[Self::Node]>;
    fn predecessors(&self, block: Self::Node) -> Cow<[Self::Node]>;

    fn display(&self, block: Option<Self::Node>) -> String {
        if let Some(block) = block {
            format!("{:?}", block)
        } else {
            format!("null")
        }
    }
}
#[derive(Clone, PartialEq, Eq)]

struct BlockData<G: Graph> {
    parent: Option<G::Node>,
    pre_number: usize,
    semi_number: usize,
    ancestor: Option<G::Node>,
    label: Option<G::Node>,
    bucket: Vec<G::Node>,
    dom: Option<G::Node>,
}

impl<G: Graph> BlockData<G> {
    fn new() -> Self {
        Self {
            parent: None,
            pre_number: usize::MAX,
            semi_number: usize::MAX,
            ancestor: None,
            label: None,
            bucket: Vec::new(),
            dom: None,
        }
    }
}

/// This implements Lengauer and Tarjan's "A Fast Algorithm for Finding Dominators in a Flowgraph"
/// (TOPLAS 1979). It uses the "simple" implementation of LINK and EVAL, which yields an O(n log n)
/// solution. The full paper is linked below; this code attempts to closely follow the algorithm as
/// it is presented in the paper; in particular sections 3 and 4 as well as appendix B.
/// https://www.cs.princeton.edu/courses/archive/fall03/cs528/handouts/a%20fast%20algorithm%20for%20finding.pdf
///
/// This code is very subtle. The Lengauer-Tarjan algorithm is incredibly deep to begin with. The
/// goal of this code is to follow the code in the paper, however our implementation must deviate
/// from the paper when it comes to recursion. The authors had used recursion to implement DFS, and
/// also to implement the "simple" EVAL. We convert both of those into worklist-based solutions.
/// Finally, once the algorithm gives us immediate dominators, we implement dominance tests by
/// walking the dominator tree and computing pre and post numbers. We then use the range inclusion
/// check trick that was first discovered by Paul F. Dietz in 1982 in "Maintaining order in a linked
/// list" (see http://dl.acm.org/citation.cfm?id=802184).
pub struct LengauerTarjan<'a, G: Graph> {
    graph: &'a G,
    data: IndexMap<BlockData<G>, G::Node>,
    block_by_pre_number: Vec<G::Node>,
}

impl<'a, G: Graph> LengauerTarjan<'a, G> {
    pub fn new(graph: &'a G) -> Self {
        let mut data = IndexMap::new();

        for block_index in (0..graph.num_nodes()).rev() {
            let block = graph.node(block_index);

            if let Some(block) = block {
                let mut bd = BlockData::new();
                bd.label = Some(block);
                data.insert(block, bd);
            }
        }

        Self {
            graph,
            data,
            block_by_pre_number: vec![],
        }
    }

    pub fn compute(&mut self) {
        self.compute_depth_first_pre_numbering();
        self.compute_semi_dominators_and_implicit_immediate_dominators();
        self.compute_explicit_immediate_dominators();
    }

    pub fn immediate_dominator(&self, block: G::Node) -> Option<G::Node> {
        self.data.get(&block).and_then(|bd| bd.dom)
    }

    fn compute_depth_first_pre_numbering(&mut self) {
        // Use a block worklist that also tracks the index inside the successor list. This is
        // necessary for ensuring that we don't attempt to visit a successor until the previous
        // successors that we had visited are fully processed. This ends up being revealed in the
        // output of this method because the first time we see an edge to a block, we set the
        // block's parent. So, if we have:
        //
        // A -> B
        // A -> C
        // B -> C
        //
        // And we're processing A, then we want to ensure that if we see A->B first (and hence set
        // B's prenumber before we set C's) then we also end up setting C's parent to B by virtue
        // of not noticing A->C until we're done processing B.

        let mut worklist = ExtendedGraphNodeWorklist::<G::Node, usize>::new();

        worklist.push(self.graph.root(), 0);

        while let Some(item) = worklist.pop() {
            let block = item.0;
            let successor_index = item.1;

            assert!(successor_index == 0 || successor_index < self.graph.successors(block).len());

            if successor_index == 0 {
                self.data.get_mut(&block).unwrap().semi_number = self.block_by_pre_number.len();
                self.block_by_pre_number.push(block);
            }

            if successor_index < self.graph.successors(block).len() {
                let next_successor_index = successor_index + 1;

                if next_successor_index < self.graph.successors(block).len() {
                    worklist.force_push(block, next_successor_index);
                }

                let successor_block = self.graph.successors(block)[successor_index];

                if worklist.push(successor_block, 0) {
                    self.data.get_mut(&successor_block).unwrap().parent = Some(block);
                }
            }
        }
    }

    fn link(&mut self, from: G::Node, to: G::Node) {
        self.data_mut(to).ancestor = Some(from);
    }

    fn compute_semi_dominators_and_implicit_immediate_dominators(&mut self) {
        let mut current_pre_number = self.block_by_pre_number.len();

        while {
            let res = current_pre_number > 1;
            current_pre_number -= 1;

            res
        } {
            let block = self.block_by_pre_number[current_pre_number];

            for predecessor_block in self.graph.predecessors(block).iter().copied() {
                let intermediate_block = self.eval(predecessor_block);

                let min = self
                    .data(intermediate_block)
                    .semi_number
                    .min(self.data(block).semi_number);

                self.data.get_mut(&block).unwrap().semi_number = min;
            }

            let bucket_pre_number = self.data(block).semi_number;

            self.data_mut(self.block_by_pre_number[bucket_pre_number])
                .bucket
                .push(block);

            self.link(self.data(block).parent.unwrap(), block);

            for semi_dominee in self
                .data(self.data(block).parent.unwrap())
                .bucket
                .clone()
                .iter()
                .copied()
            {
                let possible_dominator = self.eval(semi_dominee);
                assert!(
                    self.block_by_pre_number[self.data(semi_dominee).semi_number]
                        == self.data(block).parent.unwrap()
                );
                if self.data(possible_dominator).semi_number < self.data(semi_dominee).semi_number {
                    self.data_mut(semi_dominee).dom = Some(possible_dominator);
                } else {
                    self.data_mut(semi_dominee).dom = Some(self.data(block).parent.unwrap());
                }
            }

            self.data_mut(self.data(block).parent.unwrap())
                .bucket
                .clear();
        }
    }

    fn compute_explicit_immediate_dominators(&mut self) {
        for current_pre_number in 1..self.block_by_pre_number.len() {
            let block = self.block_by_pre_number[current_pre_number];

            if self.data(block).dom != Some(self.block_by_pre_number[self.data(block).semi_number])
            {
                let dom = self.data(block).dom.unwrap();
                self.data_mut(block).dom = self.data(dom).dom;
            }
        }
    }

    fn data(&self, block: G::Node) -> &BlockData<G> {
        self.data.get(&block).unwrap()
    }

    fn data_mut(&mut self, block: G::Node) -> &mut BlockData<G> {
        self.data.get_mut(&block).unwrap()
    }

    fn eval(&mut self, block: G::Node) -> G::Node {
        if self.data.get(&block).unwrap().ancestor.is_none() {
            return block;
        }

        self.compress(block);

        self.data
            .get(&block)
            .unwrap()
            .label
            .expect("label should be set after `compress`")
    }

    fn compress(&mut self, initial_block: G::Node) {
        let ancestor = *self
            .data
            .get(&initial_block)
            .unwrap()
            .ancestor
            .as_ref()
            .unwrap();

        if self.data.get(&ancestor).unwrap().ancestor.is_none() {
            return;
        }

        let mut stack = Vec::with_capacity(16);

        let mut block = Some(initial_block);

        while let Some(bb) = block {
            stack.push(bb);
            block = self.data.get(&bb).unwrap().ancestor;
        }

        // for (unsigned i = stack.len() - 2; i--)

        let mut i = stack.len() - 2;

        while {
            let res = i != 0;
            i -= 1;
            res
        } {
            let block = stack[i];
            let BlockData {
                label: label_of_block,
                ancestor: ancestor_of_block,
                ..
            } = self.data.get(&block).unwrap();
            let label_of_block = label_of_block.unwrap();
            let ancestor_of_block = ancestor_of_block.unwrap();

            let label_of_ancestor_of_block =
                self.data.get(&ancestor_of_block).unwrap().label.unwrap();

            if self
                .data
                .get(&label_of_ancestor_of_block)
                .unwrap()
                .semi_number
                < self.data.get(&label_of_block).unwrap().semi_number
            {
                self.data.get_mut(&block).unwrap().label = Some(label_of_ancestor_of_block);
            }

            let ancestor = self.data.get(&ancestor_of_block).unwrap().ancestor;

            self.data.get_mut(&block).unwrap().ancestor = ancestor;
        }
    }
}

pub struct GraphNodeWorklist<Node: Copy + Clone + PartialEq + Eq + Hash + KeyIndex> {
    seen: IndexSet<Node>,
    stack: Vec<Node>,
}

impl<Node: Copy + Clone + PartialEq + Eq + Hash + KeyIndex> GraphNodeWorklist<Node> {
    pub fn new() -> Self {
        Self {
            seen: IndexSet::new(),
            stack: Vec::new(),
        }
    }

    pub fn push(&mut self, node: Node) -> bool {
        if self.seen.contains(&node) {
            false
        } else {
            self.seen.insert(node);
            self.stack.push(node);
            true
        }
    }

    pub fn push_all<I>(&mut self, iter: I)
    where
        I: Iterator<Item = Node>,
    {
        for node in iter {
            self.push(node);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn not_empty(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn saw(&self, node: Node) -> bool {
        self.seen.contains(&node)
    }

    pub fn seen(&self) -> &IndexSet<Node> {
        &self.seen
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.stack.pop()
    }
}

pub struct ExtendedGraphNodeWorklist<Node, T> {
    seen: HashSet<Node>,
    stack: Vec<(Node, T)>,
}

impl<Node: Copy + Clone + PartialEq + Eq + Hash, T> ExtendedGraphNodeWorklist<Node, T> {
    pub fn new() -> Self {
        Self {
            seen: HashSet::new(),
            stack: Vec::new(),
        }
    }

    pub fn push(&mut self, node: Node, t: T) -> bool {
        if self.seen.contains(&node) {
            false
        } else {
            self.seen.insert(node);
            self.stack.push((node, t));
            true
        }
    }

    pub fn push_all<I>(&mut self, iter: I)
    where
        I: Iterator<Item = (Node, T)>,
    {
        for (node, t) in iter {
            self.push(node, t);
        }
    }

    pub fn force_push(&mut self, node: Node, t: T) {
        self.stack.push((node, t));
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn not_empty(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn pop(&mut self) -> Option<(Node, T)> {
        self.stack.pop()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum GraphVisitOrder {
    Pre,
    Post,
}

pub struct PostOrderGraphNodeWorklist<Node> {
    worklist: ExtendedGraphNodeWorklist<Node, GraphVisitOrder>,
}

impl<Node: Copy + Clone + PartialEq + Eq + Hash> PostOrderGraphNodeWorklist<Node> {
    pub fn new() -> Self {
        Self {
            worklist: ExtendedGraphNodeWorklist::new(),
        }
    }

    pub fn push_pre(&mut self, node: Node) -> bool {
        self.worklist.push(node, GraphVisitOrder::Pre)
    }

    pub fn push_post(&mut self, node: Node) {
        self.worklist.force_push(node, GraphVisitOrder::Post)
    }

    pub fn push(&mut self, node: Node, order: GraphVisitOrder) -> bool {
        match order {
            GraphVisitOrder::Post => {
                self.push_post(node);
                true
            }

            GraphVisitOrder::Pre => self.push_pre(node),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.worklist.is_empty()
    }

    pub fn not_empty(&self) -> bool {
        self.worklist.not_empty()
    }

    pub fn pop(&mut self) -> Option<(Node, GraphVisitOrder)> {
        self.worklist.pop()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct DomBlockData<G: Graph> {
    idom_kids: Vec<G::Node>,
    idom_parent: Option<G::Node>,
    pre_number: usize,
    post_number: usize,
}

impl<G: Graph> DomBlockData<G> {
    pub fn new() -> Self {
        Self {
            idom_kids: vec![],
            idom_parent: None,
            pre_number: usize::MAX,
            post_number: usize::MAX,
        }
    }
}

pub struct Dominators<G: Graph + 'static> {
    data: Rc<IndexMap<DomBlockData<G>, G::Node>>,
    marker: PhantomData<&'static G>,
}

impl<G: Graph + 'static> Clone for Dominators<G> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            marker: PhantomData,
        }
    }
}

impl<G: Graph> Dominators<G> {
    fn data(&self, node: G::Node) -> &DomBlockData<G> {
        self.data.get(&node).unwrap()
    }

    pub fn new(graph: &G) -> Self {
        let mut lengauer_tarjan = LengauerTarjan::new(graph);
        lengauer_tarjan.compute();

        let mut data = IndexMap::new();

        for block_index in (0..graph.num_nodes()).rev() {
            let block = graph.node(block_index);

            if let Some(block) = block {
                let idom_block = lengauer_tarjan.immediate_dominator(block);
                data.entry(block)
                    .or_insert_with(DomBlockData::new)
                    .idom_parent = idom_block;

                if let Some(idom_block) = idom_block {
                    data.entry(idom_block)
                        .or_insert_with(DomBlockData::new)
                        .idom_kids
                        .push(block);
                }
            } else {
                continue;
            }
        }

        let mut next_pre_number = 0;
        let mut next_post_number = 0;

        let mut worklist = Vec::<(G::Node, GraphVisitOrder)>::new();

        worklist.push((graph.root(), GraphVisitOrder::Pre));

        while let Some((node, order)) = worklist.pop() {
            match order {
                GraphVisitOrder::Pre => {
                    data.entry(node)
                        .or_insert_with(DomBlockData::new)
                        .pre_number = next_pre_number;
                    next_pre_number += 1;

                    worklist.push((node, GraphVisitOrder::Post));
                    for kid in data.get(&node).unwrap().idom_kids.iter().copied() {
                        worklist.push((kid, GraphVisitOrder::Pre));
                    }
                }

                GraphVisitOrder::Post => {
                    data.entry(node)
                        .or_insert_with(DomBlockData::new)
                        .post_number = next_post_number;
                    next_post_number += 1;
                }
            }
        }

        Self {
            data: Rc::new(data),
            marker: PhantomData,
        }
    }

    pub fn strictly_dominates(&self, from: G::Node, to: G::Node) -> bool {
        self.data(to).pre_number > self.data(from).pre_number
            && self.data(to).post_number < self.data(from).post_number
    }

    pub fn dominates(&self, from: G::Node, to: G::Node) -> bool {
        from == to || self.strictly_dominates(from, to)
    }

    /// Returns the immediate dominator of this block. Returns `None` for the root block.
    pub fn idom(&self, block: G::Node) -> Option<G::Node> {
        self.data(block).idom_parent
    }

    pub fn for_all_strict_dominators_of<F>(&self, to: G::Node, mut f: F)
    where
        F: FnMut(G::Node),
    {
        let mut block = self.data(to).idom_parent;

        while let Some(bb) = block {
            f(bb);

            block = self.data(bb).idom_parent;
        }
    }

    /// Same as [`for_all_strict_dominators_of`], but also includes `to` itself.
    pub fn for_all_dominators_of<F>(&self, to: G::Node, mut f: F)
    where
        F: FnMut(G::Node),
    {
        f(to);
        self.for_all_strict_dominators_of(to, |bb| f(bb));
    }

    pub fn for_all_blocks_strictly_dominated_by<F>(&self, from: G::Node, mut f: F)
    where
        F: FnMut(G::Node),
    {
        let mut worklist = Vec::with_capacity(16);

        for kid in self.data(from).idom_kids.iter().copied() {
            worklist.push(kid);
        }

        while let Some(block) = worklist.pop() {
            f(block);

            for kid in self.data(block).idom_kids.iter().copied() {
                worklist.push(kid);
            }
        }
    }

    pub fn for_all_blocks_dominated_by<F>(&self, from: G::Node, mut f: F)
    where
        F: FnMut(G::Node),
    {
        let mut worklist = Vec::with_capacity(16);

        worklist.push(from);

        while let Some(block) = worklist.pop() {
            f(block);

            for kid in self.data(block).idom_kids.iter().copied() {
                worklist.push(kid);
            }
        }
    }

    pub fn strict_dominators_of(&self, to: G::Node) -> IndexSet<G::Node> {
        let mut result = IndexSet::new();

        self.for_all_strict_dominators_of(to, |node| {
            result.insert(node);
        });

        result
    }

    pub fn dominators_of(&self, to: G::Node) -> IndexSet<G::Node> {
        let mut result = IndexSet::new();

        self.for_all_dominators_of(to, |node| {
            result.insert(node);
        });

        result
    }

    pub fn blocks_strictly_dominated_by(&self, from: G::Node) -> IndexSet<G::Node> {
        let mut result = IndexSet::new();

        self.for_all_blocks_strictly_dominated_by(from, |node| {
            result.insert(node);
        });

        result
    }

    pub fn blocks_dominated_by(&self, from: G::Node) -> IndexSet<G::Node> {
        let mut result = IndexSet::new();

        self.for_all_blocks_dominated_by(from, |node| {
            result.insert(node);
        });

        result
    }

    pub fn for_all_blocks_in_dominance_frontier_of<F>(&self, from: G::Node, graph: &G, mut f: F)
    where
        F: FnMut(G::Node),
    {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_dominance_frontier_of_impl(from, graph, |block| {
            if set.insert(block) {
                f(block);
            }
        });
    }

    pub fn dominance_frontier_of<F>(&self, from: G::Node, graph: &G) -> IndexSet<G::Node>
    where
        F: FnMut(G::Node) -> bool,
    {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_dominance_frontier_of(from, graph, |block| {
            set.insert(block);
        });

        set
    }

    pub fn for_all_blocks_in_iterated_dominance_frontier_of<F>(
        &self,
        from: &[G::Node],
        graph: &G,
        mut f: F,
    ) where
        F: FnMut(G::Node) -> bool,
    {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_iterated_dominance_frontier_of_impl(from, graph, |block| {
            if set.insert(block) {
                f(block)
            } else {
                false
            }
        });
    }

    pub fn for_all_blocks_in_pruned_iterated_dominance_frontier_of<F>(
        &self,
        from: &[G::Node],
        graph: &G,
        mut f: F,
    ) where
        F: FnMut(G::Node) -> bool,
    {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_iterated_dominance_frontier_of_impl(from, graph, |block| {
            if set.insert(block) {
                f(block)
            } else {
                false
            }
        });
    }

    pub fn for_all_blocks_in_pruned_iterated_dominance_frontier_of_mut<F>(
        &self,
        from: &[G::Node],
        graph: &mut G,
        mut f: F,
    ) where
        F: FnMut(&mut G, G::Node) -> bool,
    {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_iterated_dominance_frontier_of_impl_mut(
            from,
            graph,
            |graph, block| {
                if set.insert(block) {
                    f(graph, block)
                } else {
                    false
                }
            },
        );
    }

    pub fn all_blocks_in_pruned_iterated_dominance_frontier_of(
        &self,
        from: &[G::Node],
        graph: &G,
    ) -> IndexSet<G::Node> {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_iterated_dominance_frontier_of_impl(from, graph, |block| {
            set.insert(block)
        });

        set
    }

    pub fn iterated_dominance_frontier_of(&self, from: &[G::Node], graph: &G) -> IndexSet<G::Node> {
        let mut set = IndexSet::new();

        self.for_all_blocks_in_iterated_dominance_frontier_of_impl(from, graph, |block| {
            set.insert(block)
        });

        set
    }

    fn for_all_blocks_in_dominance_frontier_of_impl<F>(&self, from: G::Node, graph: &G, mut f: F)
    where
        F: FnMut(G::Node),
    {
        self.for_all_blocks_dominated_by(from, move |block| {
            for to in graph.successors(block).iter().copied() {
                if !self.strictly_dominates(from, to) {
                    f(to);
                }
            }
        })
    }

    fn for_all_blocks_in_dominance_frontier_of_impl_mut<F>(
        &self,
        from: G::Node,
        graph: &mut G,
        mut f: F,
    ) where
        F: FnMut(&mut G, G::Node),
    {
        self.for_all_blocks_dominated_by(from, |block| {
            let succs = graph.successors(block).to_vec();
            for to in succs.iter().copied() {
                if !self.strictly_dominates(from, to) {
                    f(graph, to);
                }
            }
        })
    }

    fn for_all_blocks_in_iterated_dominance_frontier_of_impl<F>(
        &self,
        from: &[G::Node],
        graph: &G,
        mut f: F,
    ) where
        F: FnMut(G::Node) -> bool,
    {
        let mut worklist = from.to_vec();

        while let Some(block) = worklist.pop() {
            self.for_all_blocks_in_dominance_frontier_of_impl(block, graph, |other_block| {
                if f(other_block) {
                    worklist.push(other_block);
                }
            })
        }
    }

    fn for_all_blocks_in_iterated_dominance_frontier_of_impl_mut<F>(
        &self,
        from: &[G::Node],
        graph: &mut G,
        mut f: F,
    ) where
        F: FnMut(&mut G, G::Node) -> bool,
    {
        let mut worklist = from.to_vec();

        while let Some(block) = worklist.pop() {
            self.for_all_blocks_in_dominance_frontier_of_impl_mut(
                block,
                graph,
                |graph, other_block| {
                    if f(graph, other_block) {
                        worklist.push(other_block);
                    }
                },
            )
        }
    }

    pub fn display<'a>(&'a self, graph: &'a G) -> DominatorsDisplay<'_, G> {
        DominatorsDisplay { tree: self, graph }
    }
}

pub struct DominatorsDisplay<'a, G: Graph + 'static> {
    tree: &'a Dominators<G>,
    graph: &'a G,
}

impl<'a, G: Graph> std::fmt::Display for DominatorsDisplay<'a, G> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block_index in 0..self.tree.data.len() {
            let data = &self.tree.data.at(block_index).unwrap();

            if data.pre_number == usize::MAX {
                continue;
            }

            write!(
                f,
                "    Block #{}: idom = {}, idomKids = [",
                {
                    let x: usize = block_index;
                    x
                },
                self.graph.display(data.idom_parent)
            )?;

            for i in 0..data.idom_kids.len() {
                write!(f, "{}", self.graph.display(Some(data.idom_kids[i])))?;

                if i != data.idom_kids.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(
                f,
                "], pre/post = {}/{}\n",
                data.pre_number, data.post_number
            )?;
        }

        Ok(())
    }
}

pub struct BackwardsGraph<'a, G: Graph> {
    graph: &'a G,
    root_successor_list: Vec<SingleGraphNode<G::Node>>,
    root_successor_set: IndexSet<G::Node>,
}

impl<'a, G: Graph> BackwardsGraph<'a, G> {
    pub fn num_nodes(&self) -> usize {
        self.graph.num_nodes() + 1
    }

    pub fn new(graph: &'a G) -> Self {
        let mut this = Self {
            root_successor_list: Vec::new(),
            root_successor_set: IndexSet::new(),
            graph,
        };

        let mut worklist = GraphNodeWorklist::new();

        let add_root_successor = |this: &mut Self, worklist: &mut GraphNodeWorklist<_>, node| {
            if worklist.push(node) {
                this.root_successor_list.push(SingleGraphNode::new(node));
                this.root_successor_set.insert(node);

                while let Some(node) = worklist.pop() {
                    for predecessor in graph.predecessors(node).iter().copied() {
                        worklist.push(predecessor);
                    }
                }
            }
        };

        {
            let spanning_tree = SpanningTree::new(graph);
            // Loops are a form of terminality (you can loop forever). To have a loop, you need to
            // have a back edge. An edge u->v is a back edge when u is a descendent of v in the
            // DFS spanning tree of the Graph.
            for i in 0..graph.num_nodes() {
                if let Some(node) = graph.node(i) {
                    for succ in graph.successors(node).iter().copied() {
                        if !spanning_tree.is_descendant(node, succ) {
                            add_root_successor(&mut this, &mut worklist, succ);
                            break;
                        }
                    }
                }
            }
        }

        for i in 0..graph.num_nodes() {
            if let Some(node) = graph.node(i) {
                if graph.successors(node).len() == 0 {
                    add_root_successor(&mut this, &mut worklist, node);
                }
            }
        }

        // At this point there will be some nodes in the graph that aren't known to the worklist. We
        // could add any or all of them to the root successors list. Adding all of them would be a bad
        // pessimisation. Ideally we would pick the ones that have backward edges but no forward
        // edges. That would require thinking, so we just use a rough heuristic: add the highest
        // numbered nodes first, which is totally fine if the input program is already sorted nicely.
        for i in (0..graph.num_nodes()).rev() {
            if let Some(node) = graph.node(i) {
                add_root_successor(&mut this, &mut worklist, node);
            }
        }

        this
    }
}

impl<'a, G: Graph> Graph for BackwardsGraph<'a, G> {
    type Node = SingleGraphNode<G::Node>;

    fn node_index(&self, node: Self::Node) -> usize {
        if node.is_root {
            0
        } else {
            self.graph.node_index(node.node()) + 1
        }
    }

    fn successors(&self, block: Self::Node) -> Cow<[Self::Node]> {
        if block.is_root() {
            Cow::Borrowed(&self.root_successor_list)
        } else {
            Cow::Owned(
                self.graph
                    .predecessors(block.node())
                    .iter()
                    .copied()
                    .map(SingleGraphNode::new)
                    .collect(),
            )
        }
    }

    fn predecessors(&self, block: Self::Node) -> Cow<[Self::Node]> {
        if block.is_root() {
            return Cow::Borrowed(&[]);
        }

        let mut result = vec![];

        if self.root_successor_set.contains(&block.node()) {
            result.push(SingleGraphNode::root());
        }

        for successor in self.graph.successors(block.node()).iter().copied() {
            result.push(SingleGraphNode::new(successor));
        }

        Cow::Owned(result)
    }

    fn num_nodes(&self) -> usize {
        self.graph.num_nodes() + 1
    }

    fn root(&self) -> Self::Node {
        SingleGraphNode::root()
    }

    fn node(&self, index: usize) -> Option<Self::Node> {
        if index == 0 {
            Some(SingleGraphNode::root())
        } else {
            self.graph.node(index - 1).map(SingleGraphNode::new)
        }
    }

    fn display(&self, block: Option<Self::Node>) -> String {
        if let Some(node) = block {
            if node.is_root() {
                "#root".to_string()
            } else {
                self.graph.display(Some(node.node()))
            }
        } else {
            "<null>".to_string()
        }
    }
}

pub struct SpanningTree<'a, G: Graph> {
    graph: &'a G,
    data: IndexMap<(usize, usize), G::Node>,
}

impl<'a, G: Graph> SpanningTree<'a, G> {
    pub fn new(graph: &'a G) -> Self {
        let mut this = Self {
            graph,
            data: IndexMap::new(),
        };

        let mut worklist = ExtendedGraphNodeWorklist::new();

        worklist.push(graph.root(), 0);

        let mut number = 0;

        while let Some((block, successor_index)) = worklist.pop() {
            if successor_index == 0 {
                this.data_mut(block).0 = number;
                number += 1;
            }

            if successor_index < graph.successors(block).len() {
                let next_successor_index = successor_index + 1;

                worklist.force_push(block, next_successor_index);

                let successor_block = this.graph.successors(block)[successor_index];

                worklist.push(successor_block, 0);
            } else {
                this.data_mut(block).1 = number;
                number += 1;
            }
        }

        this
    }

    pub fn is_descendant(&self, a: G::Node, b: G::Node) -> bool {
        self.data(a).0 <= self.data(b).0 && self.data(b).1 >= self.data(a).1
    }

    pub fn data_mut(&mut self, node: G::Node) -> &mut (usize, usize) {
        self.data.entry(node).or_insert((0, 0))
    }

    pub fn data(&self, node: G::Node) -> &(usize, usize) {
        self.data.get(&node).unwrap()
    }
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct SingleGraphNode<
    N: Copy + Clone + PartialEq + Eq + Hash + Copy + From<usize> + Into<usize>,
> {
    node: N,
    is_root: bool,
}

impl<N: Copy + Clone + PartialEq + Eq + Hash + Copy + From<usize> + Into<usize> + KeyIndex> KeyIndex
    for SingleGraphNode<N>
{
    fn index(&self) -> usize {
        self.node.index()
    }
}

impl<N: Copy + Clone + PartialEq + Eq + Hash + Copy + From<usize> + Into<usize>> Into<usize>
    for SingleGraphNode<N>
{
    fn into(self) -> usize {
        self.node.into()
    }
}

impl<N: Copy + Clone + PartialEq + Eq + Hash + Copy + From<usize> + Into<usize>> From<usize>
    for SingleGraphNode<N>
{
    fn from(node: usize) -> Self {
        Self {
            node: node.into(),
            is_root: false,
        }
    }
}

impl<N: Copy + Clone + PartialEq + Eq + Hash + Copy + From<usize> + Into<usize>>
    SingleGraphNode<N>
{
    pub fn new(node: N) -> Self {
        Self {
            node,
            is_root: false,
        }
    }

    pub fn is_root(&self) -> bool {
        self.is_root
    }

    pub fn root() -> Self {
        Self {
            node: 0.into(),
            is_root: true,
        }
    }

    pub fn node(&self) -> N {
        self.node
    }
}

pub struct SingleRootGraphSet<G: Graph> {
    set: IndexSet<SingleGraphNode<G::Node>>,
    has_root: bool,
}

impl<G: Graph> SingleRootGraphSet<G> {
    pub fn new() -> Self {
        Self {
            set: Default::default(),
            has_root: false,
        }
    }

    pub fn insert(&mut self, node: SingleGraphNode<G::Node>) -> bool {
        if node.is_root() {
            return check_and_set(&mut self.has_root, true);
        } else {
            return self.set.insert(node);
        }
    }

    pub fn remove(&mut self, node: SingleGraphNode<G::Node>) -> bool {
        if node.is_root() {
            return check_and_set(&mut self.has_root, false);
        } else {
            return self.set.remove(&node);
        }
    }

    pub fn contains(&self, node: SingleGraphNode<G::Node>) -> bool {
        if node.is_root() {
            return self.has_root;
        } else {
            return self.set.contains(&node);
        }
    }
}

fn check_and_set(left: &mut bool, right: bool) -> bool {
    if *left == right {
        return false;
    }

    *left = right;
    true
}
