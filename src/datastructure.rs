use std::cmp::Ordering;
use std::fmt::Debug;

/// Insert only binary tree that uses a comparator function to ensure the root is always a
/// maximal element. Assumes total ordering.
pub struct BinaryTree<T, F>
    where T: Copy + Debug, F: Fn(T, T) -> Ordering
{
    nodes: Vec<Node<T>>,
    root: Option<usize>,
    comparator: F,
}

#[derive(Debug, Clone, Copy)]
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