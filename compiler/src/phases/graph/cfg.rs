use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[allow(dead_code)]
pub struct ControlFlowGraph<TId>
where
    TId: Hash + std::cmp::Eq + PartialEq + Copy,
{
    graph: HashMap<TId, CfgNode<TId>>,
}

#[allow(dead_code)]
impl<TId: Hash + std::cmp::Eq + PartialEq + Copy> ControlFlowGraph<TId> {
    pub fn new() -> Self {
        Self {
            graph: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: CfgNode<TId>) {
        self.graph.insert(node.id, node);
    }

    pub fn dominates(&self, a_id: &TId, b_id: &TId) -> bool {
        if a_id == b_id {
            return true;
        }

        let b_node = match self.graph.get(b_id) {
            Some(node) => node,
            None => return false,
        };

        if b_node.predecessors.is_empty() {
            return false;
        }

        if b_node.predecessors.len() == 1 && b_node.predecessors.contains(a_id) {
            return true;
        }

        b_node
            .predecessors
            .iter()
            .all(|id| self.does_path_to_entry_contain(id, a_id))
    }

    pub fn strictly_dominates(&self, a_id: &TId, b_id: &TId) -> bool {
        if a_id == b_id {
            false
        } else {
            self.dominates(a_id, b_id)
        }
    }

    pub fn immediately_dominates(&self, a_id: &TId, b_id: &TId) -> bool {
        if a_id == b_id {
            return false;
        }

        let b_node = match self.graph.get(b_id) {
            Some(node) => node,
            None => return false,
        };

        if b_node.predecessors.is_empty() {
            return false;
        }

        let strict_dominators = b_node
            .predecessors
            .iter()
            .filter(|id| self.strictly_dominates(id, b_id))
            .collect::<HashSet<_>>();

        if !strict_dominators.contains(a_id) {
            return false;
        }

        let mut result = true;

        for id in strict_dominators {
            if id == a_id {
                continue;
            }

            result = !self.strictly_dominates(a_id, id);
            if !result {
                break;
            }
        }

        result
    }

    fn does_path_to_entry_contain(&self, start_id: &TId, search_id: &TId) -> bool {
        if start_id == search_id {
            return true;
        }

        let start_node = match self.graph.get(start_id) {
            Some(node) => node,
            None => return false,
        };

        if start_node.predecessors.is_empty() {
            return false;
        }

        let mut visited_result = HashMap::<&TId, bool>::new();
        let mut visited = HashSet::<&TId>::new();
        visited.insert(start_id);

        start_node.predecessors.iter().all(|id| {
            self.does_path_to_entry_contain_recurse(&mut visited_result, &mut visited, id)
        })
    }

    fn does_path_to_entry_contain_recurse<'a>(
        &'a self,
        visited_result: &mut HashMap<&'a TId, bool>,
        visited: &mut HashSet<&'a TId>,
        id: &'a TId,
    ) -> bool {
        visited.insert(id);
        match visited_result.get(id) {
            Some(result) => return *result,
            None => {}
        }

        let node = self.graph.get(id).unwrap();
        if node.predecessors.is_empty() {
            visited_result.insert(id, false);
            return false;
        }

        let mut search_result = true;
        for other_id in node.predecessors.iter() {
            if visited.contains(other_id) {
                continue;
            }

            search_result =
                self.does_path_to_entry_contain_recurse(visited_result, visited, other_id);
            if !search_result {
                break;
            }
        }

        visited_result.insert(id, search_result);
        search_result
    }
}

#[allow(dead_code)]
pub struct CfgNode<TId>
where
    TId: Hash + std::cmp::Eq + PartialEq,
{
    id: TId,
    predecessors: HashSet<TId>,
    successors: HashSet<TId>,
}

#[allow(dead_code)]
impl<TId: Hash + std::cmp::Eq + PartialEq> CfgNode<TId> {
    pub fn new(id: TId) -> Self {
        CfgNode {
            id,
            predecessors: HashSet::new(),
            successors: HashSet::new(),
        }
    }

    pub fn num_of_predecessors(&self) -> usize {
        self.predecessors.len()
    }
    pub fn num_of_successors(&self) -> usize {
        self.successors.len()
    }

    pub fn add_predecessor(&mut self, pred_id: TId) {
        self.predecessors.insert(pred_id);
    }

    pub fn add_predecessors<I>(&mut self, predecessors: I)
    where
        I: Iterator<Item = TId>,
    {
        for predecessor in predecessors {
            self.predecessors.insert(predecessor);
        }
    }

    pub fn add_successor(&mut self, pred_id: TId) {
        self.successors.insert(pred_id);
    }

    pub fn add_successors<I>(&mut self, successors: I)
    where
        I: Iterator<Item = TId>,
    {
        for successor in successors {
            self.successors.insert(successor);
        }
    }
}
