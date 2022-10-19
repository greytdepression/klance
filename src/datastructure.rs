use std::cmp::{Ordering, min, max};
use std::fmt::Debug;
use std::num;

/// Insert only binary tree that uses a comparator function to ensure the root is always a
/// maximal element. Assumes total ordering.
pub struct BinaryTree<T, F>
    where T: Copy + Debug, F: Fn(T, T) -> Ordering
{
    nodes: Vec<Node<T>>,
    root: Option<usize>,
    comparator: F,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Left,
    Right
}

#[derive(Debug)]
pub struct Node<T> {
    data: T,
    lhs: Option<usize>,
    rhs: Option<usize>,
    parent: Option<usize>,
}

impl<T> Node<T> {
    pub fn is_leaf(&self) -> bool {
        self.lhs.is_none() && self.rhs.is_none()
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn lhs(&self) -> Option<usize> {
        self.lhs
    }

    pub fn rhs(&self) -> Option<usize> {
        self.rhs
    }
}

impl<T, F> BinaryTree<T, F>
    where T: Copy + Debug, F: Fn(T, T) -> Ordering
{
    pub fn new(comparator: F) -> Self {
        Self {
            nodes: vec![],
            root: None,
            comparator,
        }
    }

    pub fn root(&self) -> usize {
        assert!(self.root.is_some());

        self.root.unwrap()
    }

    pub fn get_root(&self) -> &Node<T> {
        assert!(self.root.is_some());

        let root = self.root.unwrap();

        assert!(self.nodes.len() > root);

        &self.nodes[root]
    }

    pub fn get_node(&self, id: usize) -> &Node<T> {
        assert!(self.nodes.len() > id);

        &self.nodes[id]
    }

    pub fn get_lhs(&self, id: usize) -> Option<&Node<T>> {
        let lhs_id = self.get_node(id).lhs;

        match lhs_id {
            Some(lhs_id) => Some(self.get_node(lhs_id)),
            None => None,
        }
    }

    pub fn get_rhs(&self, id: usize) -> Option<&Node<T>> {
        let rhs_id = self.get_node(id).rhs;

        match rhs_id {
            Some(rhs_id) => Some(self.get_node(rhs_id)),
            None => None,
        }
    }

    pub fn get_parent(&self, id: usize) -> Option<&Node<T>> {
        let parent_id = self.get_node(id).parent;

        match parent_id {
            Some(parent_id) => Some(self.get_node(parent_id)),
            None => None,
        }
    }

    fn rotate_left(&mut self, id: usize) {
        assert!(self.nodes.len() > id);

        let lhs = self.get_node(id).lhs;

        assert!(lhs.is_some());

        let lhs = lhs.unwrap();

        if self.get_node(id).parent.is_some() {
            let parent = self.get_node(id).parent.unwrap();

            if self.nodes[parent].lhs.is_some() && self.nodes[parent].lhs.unwrap() == id {
                self.nodes[parent].lhs = Some(lhs);
            } else {
                assert!(self.nodes[parent].rhs.is_some());
                assert!(self.nodes[parent].rhs.unwrap() == id);
                self.nodes[parent].rhs = Some(lhs);
            }
        }

        if self.root.is_some() && self.root.unwrap() == id {
            self.root = Some(lhs);
        }

        let temp = self.get_node(lhs).rhs;

        self.nodes[lhs].rhs = Some(id);
        self.nodes[id].lhs = temp;

        // check if we need to rotate further
        if let Some(lhsrhs) = temp {
            let id_t = self.nodes[id].data;
            let lhsrhs_t = self.nodes[lhsrhs].data;

            if matches!((self.comparator)(lhsrhs_t, id_t), Ordering::Greater) {
                self.rotate_left(id);
            }
        }

        // we don't need to check the other side since we assume that subtrees are already sorted
    }

    fn rotate_right(&mut self, id: usize) {
        assert!(self.nodes.len() > id);

        let rhs = self.get_node(id).rhs;

        assert!(rhs.is_some());

        let rhs = rhs.unwrap();

        if self.get_node(id).parent.is_some() {
            let parent = self.get_node(id).parent.unwrap();

            if self.nodes[parent].lhs.is_some() && self.nodes[parent].lhs.unwrap() == id {
                self.nodes[parent].lhs = Some(rhs);
            } else {
                assert!(self.nodes[parent].rhs.is_some());
                assert!(self.nodes[parent].rhs.unwrap() == id);
                self.nodes[parent].rhs = Some(rhs);
            }
        }

        if self.root.is_some() && self.root.unwrap() == id {
            self.root = Some(rhs);
        }

        let temp = self.get_node(rhs).lhs;

        self.nodes[rhs].lhs = Some(id);
        self.nodes[id].rhs = temp;

        // check if we need to rotate further
        if let Some(rhslhs) = temp {
            let id_t = self.nodes[id].data;
            let rhslhs_t = self.nodes[rhslhs].data;

            if matches!((self.comparator)(rhslhs_t, id_t), Ordering::Greater) {
                self.rotate_right(id);
            }
        }

        // we don't need to check the other side since we assume that subtrees are already sorted
    }

    pub fn insert_root(&mut self, root_value: T, old_root_side: Side, other_child_value: Option<T>) -> usize {
        let child_id = if let Some(child_t) = other_child_value {
            self.nodes.push(Node { data: child_t, lhs: None, rhs: None, parent: None });

            // Assert that the child should not actually be the root
            assert!(!matches!((self.comparator)(root_value, child_t), Ordering::Less));

            Some(self.nodes.len() - 1)
        } else {
            None
        };

        self.nodes.push(Node {
            data: root_value,
            lhs: if matches!(old_root_side, Side::Left) { self.root } else { child_id },
            rhs: if matches!(old_root_side, Side::Right) { self.root } else { child_id },
            parent: None,
        });

        let new_root = self.nodes.len() - 1;

        if let Some(old_root) = self.root {
            self.nodes[old_root].parent = Some(new_root);
        }

        let old_root = self.root;
        self.root = Some(new_root);

        if let Some(child_id) = child_id {
            self.nodes[child_id].parent = Some(new_root);
        }

        if old_root.is_none() {
            // we're done. no reordering necessary
            return new_root;
        }

        let old_root = old_root.unwrap();

        let new_root_t = self.get_node(new_root).data;
        let old_root_t = self.get_node(old_root).data;

        match old_root_side {
            Side::Left if matches!((self.comparator)(old_root_t, new_root_t), Ordering::Greater) => {
                self.rotate_left(new_root);
            },
            Side::Right if matches!((self.comparator)(old_root_t, new_root_t), Ordering::Greater) => {
                self.rotate_right(new_root);
            },
            _ => {},
        }

        new_root
    }
}

pub struct StaticSparseDigraph<N>
    where N: Clone + Debug
{
    node_data: Vec<N>,
    graph_data: TightlyPackedDatalessSparseDigraph,
}

impl<N: Clone + Debug> StaticSparseDigraph<N> {
    // pub fn new(node_data: Vec<N>) -> Self {
    //     Self {
    //         node_data,
    //         graph_data: None,
    //     }
    // }

    pub fn builder(num_nodes: usize) -> DigraphBuilder {
        DigraphBuilder::new(num_nodes)
    }

    pub fn construct(builder: DigraphBuilder, data: Vec<N>) -> Self {
        assert!(data.len() == builder.n);

        Self {
            node_data: data,
            graph_data: builder.into(),
        }
    }

    pub fn construct_acyclic(builder: DigraphBuilder, data: Vec<N>) -> (Self, Vec<Cycle>) {
        let output = Self::construct(builder, data);

        let graph = &output.graph_data;

        graph.calculate_distances();

        let mut cycles = vec![];

        for i in 0..graph.n {
            for j in 0..i {
                let val_ij = graph.distance(i, j);
                let val_ji = graph.distance(j, i);

                if val_ij.is_some() && val_ji.is_some() {
                    // We have a cycle
                    cycles.push(Cycle(i, j));
                }
            }
        }

        (output, cycles)
    }

    /// If `j` is reachable from `i` it returns `Some(dist(i, j))`, if not `None`.
    ///
    /// If the distance matrix has not been calculated yet, this call will take
    /// O(n^3) time on first execution. Subsequent calls take O(1) time.
    pub fn distance(&mut self, i: usize, j: usize) -> Option<u32> {
        let graph = &self.graph_data;

        if graph.distance_matrix.is_none() {
            graph.calculate_distances();
        }

        graph.distance(i, j)
    }

    pub fn neighbours(&self, i: usize) -> NeighbourhoodIterator {
        let graph = &self.graph_data;

        assert!(graph.distance_matrix.is_some());

        graph.neighbours(i)
    }

    pub fn in_neighbours(&self, i: usize) -> NeighbourhoodIterator {
        let graph = &self.graph_data;

        assert!(graph.distance_matrix.is_some());

        graph.in_neighbours(i)
    }

    pub fn out_neighbours(&self, i: usize) -> NeighbourhoodIterator {
        let graph = &self.graph_data;

        assert!(graph.distance_matrix.is_some());

        graph.out_neighbours(i)
    }

    pub fn degree(&self, i: usize) -> Degree {
        let graph = &self.graph_data;

        assert!(graph.distance_matrix.is_some());

        graph.degree(i)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Cycle(usize, usize);

#[derive(Clone, Debug)]
pub struct DigraphBuilder {
    n: usize,
    edges: Vec<Vec<Edge>>,
}

#[derive(Clone, Debug)]
struct TightlyPackedDatalessSparseDigraph {
    pub n: usize,
    edges: Vec<Edge>,
    distance_matrix: Option<Vec<Option<u32>>>,
}

impl DigraphBuilder {
    pub fn new(n: usize) -> Self {
        Self {
            n,
            edges: vec![],
        }
    }

    pub fn insert_arc(&mut self, from: usize, to: usize) {
        assert!(from != to);
        let i = from;
        let j = to;

        while self.edges.len() < max(i, j) {
            self.edges.push(Vec::new());
        }

        let edge_i = Edge::some(j, true);
        let edge_j = Edge::some(i, false);

        match self.edges[i].binary_search(&edge_i) {
            Ok(index) => self.edges[i][index].add_direction(true),
            Err(index) => self.edges[i].insert(index, edge_i),
        }
        match self.edges[j].binary_search(&edge_j) {
            Ok(index) => self.edges[j][index].add_direction(false),
            Err(index) => self.edges[j].insert(index, edge_j),
        }
    }

    pub fn remove_arc(&mut self, from: usize, to: usize) {
        assert!(from != to);
        let i = from;
        let j = to;

        let edge_i = Edge::some(j, true);
        let edge_j = Edge::some(i, false);

        if i < self.edges.len() {
            match self.edges[i].binary_search(&edge_i) {
                Ok(index) => self.edges[i][index].remove_dir(true),
                Err(index) => {},
            }
        }
        if j < self.edges.len() {
            match self.edges[j].binary_search(&edge_j) {
                Ok(index) => self.edges[j][index].remove_dir(false),
                Err(index) => {},
            }
        }
    }

    fn pack(&mut self) {
        self.edges = self.edges
            .iter()
            .map(|vec| {
                vec.iter()
                    .filter(|&x| x.is_some())
                    .map(|e| *e)
                    .collect()
            })
            .collect();
    }
}

impl Into<TightlyPackedDatalessSparseDigraph> for DigraphBuilder {
    fn into(mut self) -> TightlyPackedDatalessSparseDigraph {
        self.pack();

        let n = self.edges.len();
        let mut counter = n + 1; // First n+1 entries are just a dictionary
        let mut edges: Vec<_> = (0..n)
            .map(|i| {
                let start = counter;
                counter += if self.edges.len() > i {
                    self.edges[i].len()
                } else {
                    0
                };
                Edge::some(start, true)
            })
            .collect();

        // Push a final delimiter
        edges.push(Edge::some(n, true));

        edges.extend(
            self.edges
                .iter()
                .flat_map(|vec| {
                    vec
                })
        );

        TightlyPackedDatalessSparseDigraph {
            n: self.n,
            edges,
            distance_matrix: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Edge(usize);

const OPT_MASK: usize = 1 << (usize::BITS - 1);
const FORWARD_MASK: usize = 1 << (usize::BITS - 2);
const BACKWARD_MASK: usize = 1 << (usize::BITS - 3);
const INT_MASK: usize = BACKWARD_MASK - 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Degree {
    in_deg: usize,
    out_deg: usize,
}

impl Edge {

    fn some(i: usize, from: bool) -> Self {
        assert!(i & !INT_MASK == 0);
        let dir = if from { FORWARD_MASK } else { BACKWARD_MASK };
        Self(i | OPT_MASK | dir)
    }

    fn none() -> Self {
        Self(0)
    }

    fn is_some(self) -> bool {
        (self.0 & OPT_MASK == 1) && (self.0 & (FORWARD_MASK | BACKWARD_MASK) != 0)
    }

    fn is_none(self) -> bool {
        !self.is_some()
    }

    fn id(self) -> usize {
        self.0 & INT_MASK
    }

    fn value(self) -> usize {
        self.0 & INT_MASK
    }

    fn set(&mut self, id: Option<usize>) {
        match id {
            Some(id) => {
                assert!(id & OPT_MASK == 0);
                self.0 = id | OPT_MASK;
            },
            None => {
                self.0 = 0;
            },
        }
    }

    fn add_direction(&mut self, forward: bool) {
        let mask = if forward { FORWARD_MASK } else { BACKWARD_MASK };
        self.0 |= mask;
    }

    fn remove_dir(&mut self, forward: bool) {
        let mask = if forward { FORWARD_MASK } else { BACKWARD_MASK };
        self.0 &= !mask;
    }

    fn has_direction(self, forward: bool) -> bool {
        let dir_mask = if forward { FORWARD_MASK } else { BACKWARD_MASK };
        (self.0 & OPT_MASK) != 0 && (self.0 & dir_mask) != 0
    }

    fn comp_value(self) -> usize {
        self.0 & INT_MASK
    }
}

impl PartialOrd for Edge {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.comp_value().cmp(&other.comp_value()))
    }
}

impl Ord for Edge {
    fn cmp(&self, other: &Self) -> Ordering {
        self.comp_value().cmp(&other.comp_value())
    }
}

impl TightlyPackedDatalessSparseDigraph {
    /// Whether the arc u -> v is part of the graph
    pub fn contains(&self, u: usize, v: usize) -> bool {
        let i = min(u, v);
        let j = max(u, v);

        assert!(i != j && j < self.n);

        let dir = u == i;

        let start_index = self.edges[i].value();
        let end_index = self.edges[i+1].value();

        let slice = &self.edges[start_index..end_index];

        match slice.binary_search(&Edge::some(j, dir)) {
            Err(_) => false,
            Ok(index) => slice[index].has_direction(dir),
        }
    }

    fn neighbours_unchecked(&self, v: usize) -> &[Edge] {
        assert!(v < self.n);

        let start_index = self.edges[v].value();
        let end_index = self.edges[v+1].value();

        &self.edges[start_index..end_index]
    }

    pub fn neighbours(&self, v: usize) -> NeighbourhoodIterator {
        NeighbourhoodIterator {
            incident: self.neighbours_unchecked(v),
            index: 0,
            in_neighbours: true,
            out_neighbours: true,
        }
    }

    pub fn in_neighbours(&self, v: usize) -> NeighbourhoodIterator {
        NeighbourhoodIterator {
            incident: self.neighbours_unchecked(v),
            index: 0,
            in_neighbours: true,
            out_neighbours: false,
        }
    }

    pub fn out_neighbours(&self, v: usize) -> NeighbourhoodIterator {
        NeighbourhoodIterator {
            incident: self.neighbours_unchecked(v),
            index: 0,
            in_neighbours: false,
            out_neighbours: true,
        }
    }

    pub fn degree(&self, v: usize) -> Degree {
        let mut deg = Degree { in_deg: 0, out_deg: 0 };
        self.neighbours_unchecked(v)
            .iter()
            .for_each(|&e| {
                deg.in_deg += if (e.0 & OPT_MASK != 0) && (e.0 & BACKWARD_MASK != 0) { 1 } else { 0 };
                deg.out_deg += if (e.0 & OPT_MASK != 0) && (e.0 & FORWARD_MASK != 0) { 1 } else { 0 };
            });

        deg
    }

    pub fn in_degree(&self, v: usize) -> usize {
        self.neighbours_unchecked(v)
            .iter()
            .filter(|&e| e.has_direction(false))
            .count()
    }

    pub fn out_degree(&self, v: usize) -> usize {
        self.neighbours_unchecked(v)
            .iter()
            .filter(|&e| e.has_direction(true))
            .count()
    }

    /// <b>WARNING</b>: This has Ω(n^2) space and Ω(n^3) time complexity (where n = #nodes).
    pub fn calculate_distances(&mut self) {

        #[derive(Clone, Copy)]
        enum Status {
            Unvisited,
            Active(u32),
            Inactive(u32),
        }

        use Status::*;

        let n = self.n;
        let mut stack: Vec<Status> = vec![Unvisited; n];

        match self.distance_matrix.as_mut() {
            Some(mat) => {
                assert!(mat.len() == n * n);

                mat.fill(None);
            },
            None => {
                let mut dM = vec![None; n * n];
                dM.shrink_to_fit();
                self.distance_matrix = Some(dM);
            },
        }

        for i in 0..self.n {
            // Fix start vertex i

            // Reset the stack
            stack.fill(Unvisited);

            // Set the distance of i to itself and make the node active
            stack[i] = Active(0);

            let mut distance: u32 = 0;

            let mut any = true;

            while any {

                any = false;
                distance += 1;

                for j in 0..n {
                    if !matches!(stack[j], Active(_)) {
                        continue;
                    }
                    any = true;

                    for k in self.out_neighbours(j) {
                        if matches!(stack[k], Unvisited) {
                            stack[k] = Active(distance);
                        }
                    }

                    stack[j] = Inactive(distance - 1);
                }
            }


            let dM = self.distance_matrix.as_mut().unwrap();
            for (j, &dj) in stack.iter().enumerate() {
                dM[i * n + j] = match dj {
                    Unvisited => None,
                    Inactive(d) => Some(d),
                    Active(_) => panic!("Node is not supposed to be active"),
                }
            }
        }
    }

    pub fn distance(&self, i: usize, j: usize) -> Option<u32> {
        assert!(self.distance_matrix.is_some(), "Calculate the distance matrix to access single distances.");

        self.distance_matrix.as_ref().unwrap()[self.index(i, j)]
    }

    fn index(&self, i: usize, j: usize) -> usize {
        i * self.n + j
    }


}

pub struct NeighbourhoodIterator<'a> {
    incident: &'a [Edge],
    index: usize,
    in_neighbours: bool,
    out_neighbours: bool,
}

impl<'a> Iterator for NeighbourhoodIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.incident.len() {
            let e = self.incident[self.index];
            self.index += 1;

            if self.in_neighbours && e.has_direction(false) {
                return Some(e.value());
            }

            if self.out_neighbours && e.has_direction(true) {
                return Some(e.value());
            }
        }

        None
    }
}