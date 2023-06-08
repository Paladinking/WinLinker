use std::collections::{HashMap, HashSet};
use std::slice::Iter;
use crate::language::amd_win64::compiler::OperationUnit;
use crate::language::amd_win64::operation::Operand;

#[derive(Copy, Clone)]
struct Scope {
    id : usize,
    first_id : usize
}

struct UsageNode {
    row : usize,
    scope : Scope
}

pub enum UsedAfter {
    ValueNeeded, LocationNeeded, None
}

pub struct UsageTracker {
    scope_ids : Vec<usize>,
    first_scope_ids : Vec<usize>,
    usage_queue : HashMap<usize, Vec<UsageNode>>,
    // <first_scope_id, Vec<(scope-id, scope-row)>>
    scopes : HashMap<usize, Vec<(usize, usize)>>,
    // <(operand-id, row), (scope-id, first_scope-id)>
    free_usages : HashMap<(usize, usize), Scope>,
    final_usages  : HashSet<(usize, usize)>,
    // <scope-id, Vec<operand-id>
    initialized_operands : HashMap<usize, Vec<usize>>
}


impl UsageTracker {
    pub fn new() -> UsageTracker {
        UsageTracker {
            scope_ids : Vec::new(), first_scope_ids : Vec::new(), usage_queue : HashMap::new(),
            free_usages : HashMap::new(), final_usages : HashSet::new(), scopes : HashMap::new(),
            initialized_operands : HashMap::new()
        }
    }

    pub fn enter_scope(&mut self, scope_id : usize, scope_row : usize) {
        self.first_scope_ids.push(scope_id);
        self.scope_ids.push(scope_id);
        self.scopes.entry(scope_id).or_insert_with(|| vec![(scope_id, scope_row)]);
    }

    pub fn leave_scope(&mut self) {
        self.scope_ids.pop().unwrap();
        self.first_scope_ids.pop().unwrap();
    }

    pub fn alt_scope(&mut self, scope_id : usize, scope_row : usize) {
        let first_scope_id = self.first_scope_ids.last().unwrap();
        *self.scope_ids.last_mut().unwrap() = scope_id;
        self.scopes.get_mut(first_scope_id).unwrap().push((scope_id, scope_row));
    }

    pub fn add_usage(&mut self, id : usize, row : usize, value_needed : bool) {
        let scope_id = *self.scope_ids.last().unwrap();
        let first_scope_id = *self.first_scope_ids.last().unwrap();
        if let Some(nodes) = self.usage_queue.get_mut(&id) {
            let in_same_block = first_scope_id == nodes.first().unwrap().scope.first_id;
            if !in_same_block {
                if !value_needed {
                    for node in nodes.iter() {
                        self.free_usages.insert((id, node.row), node.scope);
                    }
                }
                nodes.clear();
                nodes.push(UsageNode {
                    row, scope : Scope {id : scope_id, first_id : first_scope_id }
                })
            } else {
                if let Some(node) = nodes.iter_mut()
                    .find(|n| n.scope.id == scope_id) {
                    if !value_needed {
                        self.free_usages.insert((id, node.row), node.scope);
                    }
                    node.row = row;
                } else {
                    nodes.push(UsageNode {
                        row, scope : Scope { id :  scope_id, first_id : first_scope_id }
                    });
                }
            }
        } else {
            self.usage_queue.insert(id, vec![UsageNode {
                row, scope : Scope { id :  scope_id, first_id : first_scope_id }
            }]);
            self.initialized_operands.entry(first_scope_id)
                .or_insert_with(|| Vec::new()).push(id);
        }
    }

    pub fn finalize(&mut self, variable_count : usize) {
        debug_assert!(self.first_scope_ids.is_empty());
        for (id, nodes) in &self.usage_queue {
            let first_scope_id = nodes.first().unwrap().scope.first_id;
            let mut it = nodes.iter().peekable();
            for (scope_id, scope_row) in self.scopes.get(&first_scope_id).unwrap() {
                if let Some(node) = it.next_if(|&n| n.scope.id == *scope_id) {
                    debug_assert!(node.scope.first_id == first_scope_id);
                    self.free_usages.insert((*id, node.row), node.scope);
                    self.final_usages.insert((*id, node.row));
                } else {
                    self.free_usages.insert(
                        (*id, *scope_row),
                        Scope {id : *scope_id, first_id : first_scope_id});
                    self.final_usages.insert((*id, *scope_row));
                }
            }
        }
        self.usage_queue.clear();
        self.usage_queue.shrink_to_fit();
        for (_, v) in &mut self.initialized_operands {
            v.retain(|operand_id| *operand_id < variable_count);
        }
    }

    pub fn get_initializations(&self, scope_id : usize) -> impl Iterator<Item=&usize> {
        if let Some(list) = self.initialized_operands.get(&scope_id) {
            return list.iter();
        }
        return [].iter();
    }

    pub fn used_after(&self, id : usize, row : usize) -> UsedAfter {
        if self.free_usages.contains_key(&(id, row)) {
            if self.final_usages.contains(&(id, row))  {
                return UsedAfter::None;
            }
            return UsedAfter::LocationNeeded;
        }
        return UsedAfter::ValueNeeded;
    }

    pub fn next_free(&self, id : usize, row : usize, scope : usize) -> usize {
        let first_scope_id = self.scopes.iter().find_map(|(k, v)|
            if v.iter().find(|(id, _)| *id == scope).is_some() {Some(*k)} else {None}
        ).unwrap();
        self.free_usages.iter().filter_map(|((operand_id, r), s)|
            if *operand_id == id && *r >= row && (s.id == scope || s.first_id != first_scope_id) {Some(*r)} else {None}
        ).reduce(usize::max).unwrap()
    }

    pub fn final_usage(&self, id : usize) -> usize {
        self.final_usages.iter().filter_map(|(operand_id, row )|
            if *operand_id == id { Some(*row) } else {None}
        ).reduce(usize::max).unwrap()
    }
}