use std::collections::HashMap;
use crate::language::amd_win64::operation::Operand;

struct UsageNode {
    row : usize,
    scope : usize,
    first_scope_id : usize
}

pub struct UsageTracker {
    scope_ids : Vec<usize>,
    first_scope_ids : Vec<usize>,
    usage_queue : HashMap<usize, Vec<UsageNode>>
}


impl UsageTracker {
    pub fn new() -> UsageTracker {
        UsageTracker {
            scope_ids : Vec::new(), first_scope_ids : Vec::new(), usage_queue : HashMap::new()
        }
    }

    pub fn enter_scope(&mut self, scope_id : usize) {
        self.first_scope_ids.push(scope_id);
        self.scope_ids.push(scope_id);
    }

    pub fn leave_scope(&mut self) {
        self.scope_ids.pop().unwrap();
        self.first_scope_ids.pop().unwrap();
    }

    pub fn alt_scope(&mut self, scope_id : usize) {
        *self.scope_ids.last_mut().unwrap() = scope_id;
    }

    pub fn add_usage(&mut self, operands : &mut Vec<Operand>, id : usize, row : usize, value_needed : bool) {
        let operand = &mut operands[id];
        let scope_id = *self.scope_ids.last().unwrap();
        let first_scope_id = *self.first_scope_ids.last().unwrap();
        if let Some(nodes) = self.usage_queue.get_mut(&id) {
            let in_same_block = first_scope_id == nodes.first().unwrap().first_scope_id;
            if !in_same_block {
                if !value_needed {
                    for node in nodes.iter() {
                        operand.free_usages.insert(node.row);
                    }
                }
                nodes.clear();
                nodes.push(UsageNode {
                    row, scope : scope_id, first_scope_id
                })
            } else {
                if let Some(node) = nodes.iter_mut()
                    .find(|n| n.scope == scope_id) {
                    if !value_needed {
                        operand.free_usages.insert(node.row);
                    }
                    node.row = row;
                } else {
                    nodes.push(UsageNode {
                        row, scope : scope_id, first_scope_id
                    });
                }
            }
        } else {
            self.usage_queue.insert(id, vec![UsageNode {
                row, scope : scope_id, first_scope_id
            }]);
        }
    }

    pub fn finalize(self, operands : &mut Vec<Operand>) {
        debug_assert!(self.first_scope_ids.is_empty());
        for (id, nodes) in self.usage_queue {
            for node in nodes {
                operands[id].free_usages.insert(node.row);
                operands[id].final_usage.insert(node.row);
            }
        }
    }
}