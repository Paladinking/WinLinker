use std::collections::{HashMap, HashSet};
use crate::language::amd_win64::compiler::OperationUnit;

struct ScopeBlock {
    scopes : Vec<usize>,
    initialized_variables : Vec<usize>,
    conditional : bool, // True if this scope block can be avoided fully
}

impl ScopeBlock {
    fn new(scopes : Vec<usize>) -> ScopeBlock {
        ScopeBlock {scopes, initialized_variables : Vec::new(), conditional : true}
    }
}

struct Scope {
    block_id : usize,
    last_child : usize,
    freed_variables : Vec<usize>
}

impl Scope {
    fn new(block_id : usize,  last_child : usize) -> Scope {
        Scope {block_id, last_child, freed_variables : Vec::new()}
    }
}

#[derive(Debug)]
struct UsageStatus {
    value_needed : bool,
    invalid_soon : u64,
    used_after : bool,
}

impl UsageStatus {
    fn new(value_needed : bool, invalid_soon : u64, used_after : bool) -> UsageStatus {
        UsageStatus {value_needed, invalid_soon, used_after}
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum UsedAfter {
    ValueNeeded, // Do not free
    LocationNeeded, // Free, but do not allocate variable to location
    DestNeeded, // LocationNeeded if op != dest, ValueNeeded if op == dest
    None // Complete free
}

pub struct UsageTracker {
    scope_ids : Vec<usize>,
    first_scope_ids : Vec<usize>,
    //usage_queue : HashMap<usize, Vec<UsageNode>>,
    // <first_scope_id, Vec<(scope-id, scope-row)>>
    scope_blocks : HashMap<usize, ScopeBlock>,
    scopes : HashMap<usize, Scope>,
    // (id, row), only contains variables
    usages : HashMap<(usize, usize), UsedAfter>,
    // <(operand-id, row)
    used_operands : HashMap<usize, usize>,
    // Same len as operations, all invalidations of dest until next free.
    invalidations: Vec<u64>,
    variable_invalidations : HashMap<usize, u64>,
    outer_scope : usize
}

impl UsageTracker {
    pub fn new() -> UsageTracker {
        UsageTracker {
            scope_ids : Vec::new(), first_scope_ids : Vec::new(),
            usages : HashMap::new(), scopes : HashMap::new(), scope_blocks : HashMap::new(),
            used_operands : HashMap::new(), outer_scope : 0,
            invalidations : Vec::new(), variable_invalidations : HashMap::new()
        }
    }

    pub fn enter_scope(&mut self, scope_id : usize) {
        self.first_scope_ids.push(scope_id);
        self.scope_ids.push(scope_id);
        self.scopes.entry(scope_id).or_insert_with(||
            Scope::new(scope_id, scope_id));
        self.scope_blocks.entry(scope_id).or_insert_with(||
            ScopeBlock::new(vec![scope_id]));
    }

    pub fn leave_scope(&mut self) {
        let id = self.scope_ids.pop().unwrap();
        self.first_scope_ids.pop().unwrap();
        if let Some(scope) = self.scope_ids.last() {
            self.scopes.get_mut(scope).unwrap().last_child = id;
        } else {
            self.outer_scope = id;
        }
    }

    pub fn alt_scope(&mut self, scope_id : usize, condition : bool) {
        let first_scope_id = *self.first_scope_ids.last().unwrap();
        *self.scope_ids.last_mut().unwrap() = scope_id;
        self.scopes.insert(scope_id,Scope::new(
            first_scope_id, scope_id));
        self.scope_blocks.get_mut(&first_scope_id).unwrap().scopes.push(scope_id);
        if !condition {
            self.scope_blocks.get_mut(&first_scope_id).unwrap().conditional = false;
        }
    }

    pub fn add_usage(&mut self, id : usize, row : usize, value_needed : bool) {
        let first_id = *self.first_scope_ids.last().unwrap();
        // Important that dest is added after, so that value_needed is true
        //  when same operand is used both in dest and operand.
        let entry = self.usages.entry((id, row));
        entry.and_modify(|e|{
            debug_assert!(*e == UsedAfter::ValueNeeded);
            if !value_needed {
                *e = UsedAfter::DestNeeded
            }
        }).or_insert(if value_needed {UsedAfter::ValueNeeded} else {UsedAfter::None});
        if !self.used_operands.contains_key(&id) {
            self.scope_blocks.get_mut(&first_id).unwrap()
                .initialized_variables.push(id);
        }
        self.used_operands.insert(id, row);
    }

    fn analyze_usage(&self, id : usize, mut scopes : Vec<usize>, operations : &Vec<OperationUnit>, start : usize) -> UsageStatus {
        let final_usage = *self.used_operands.get(&id).unwrap();
        let mut targets = vec![scopes.last().cloned()];
        let mut index = start;
        let mut visited = HashSet::new();
        let mut needed_invalidations : Option<Option<u64>> = None;
        let mut used_after = false;
        let mut local_invalidations = 0_u64;
        loop {
            if index == operations.len() || index > final_usage {
                break;
            }
            match &operations[index] {
                OperationUnit::Operation(op) => {
                    if targets.last().unwrap().is_some() {
                        if let Some(&needed) = self.usages.get(&(id, index)) {
                            used_after = true;
                            if needed != UsedAfter::None {
                                if let Some(Some(invalidations)) = &mut needed_invalidations {
                                    *invalidations |= local_invalidations;
                                }
                                needed_invalidations.get_or_insert(Some(local_invalidations));
                            }
                            if needed != UsedAfter::ValueNeeded
                            {
                                if targets.len() == 1 {
                                    break;
                                }
                            }
                            *targets.last_mut().unwrap() = None;
                        }
                    }
                    local_invalidations |= op.invalidations;
                }
                OperationUnit::EnterBlock(scope_id) => {
                    scopes.push(*scope_id);
                    let block_id = self.scopes.get(scope_id).unwrap().block_id;
                    if !visited.contains(&block_id) {
                        visited.insert(block_id);
                        let block = &self.scope_blocks[&block_id];
                        if !block.conditional {
                            targets.pop().unwrap();
                        }
                        for &val in block.scopes.iter().rev().filter(|&id| id >= scope_id) {
                            targets.push(Some(val));
                        }
                    }
                }
                OperationUnit::LeaveBlock(has_next) => {
                    scopes.pop().unwrap();
                    targets.pop().unwrap();
                    if targets.len() == 0 {
                        if *has_next {
                            let mut entered = 0;
                            loop {
                                index += 1;
                                match operations.get(index).unwrap() {
                                    OperationUnit::EnterBlock(_) => entered += 1,
                                    &OperationUnit::LeaveBlock(next) => {
                                        entered -= 1;
                                        if entered == 0 && !next {break}
                                    }
                                    _ => {}
                                }
                            }
                        }
                        targets.push(scopes.last().cloned());
                    }
                },
                OperationUnit::SyncPush(_) | OperationUnit::SyncLoad | OperationUnit::SyncPop => {}
            }
            index += 1;
        }

        return if let Some(invalidations) = needed_invalidations.get_or_insert(None) {
            UsageStatus::new(true, *invalidations, used_after)
        } else {
             UsageStatus::new(false, 0, used_after)
        };
    }

    pub fn finalize(&mut self, operations : &Vec<OperationUnit>, variable_count : usize) {
        self.invalidations.resize(operations.len(), 0);
        debug_assert!(self.first_scope_ids.is_empty());
        let mut scopes = Vec::with_capacity(self.scope_ids.capacity());
        for (index, operation) in operations.iter().enumerate() {
            match operation {
                OperationUnit::Operation(operation) => {
                    let mut dest_status = None;
                    let get_usage = |status: &UsageStatus| if status.value_needed {
                        UsedAfter::ValueNeeded
                    } else if status.used_after {
                        UsedAfter::LocationNeeded
                    } else {
                        UsedAfter::None
                    };
                    if let Some(dest) = operation.dest {
                        if dest < variable_count {
                            let status = self.analyze_usage(dest, scopes.clone(), operations, index + 1);
                            self.usages.insert((dest, index), get_usage(&status));
                            self.invalidations[index] = status.invalid_soon;
                            *self.variable_invalidations.entry(dest).or_insert(0) |= status.invalid_soon;
                            dest_status  = Some(status);
                        } else {
                            self.usages.insert((dest, index), UsedAfter::ValueNeeded);
                        }
                    }
                    for operand in &operation.operands {
                        if *operand < variable_count {
                            if operation.dest == Some(*operand) {
                                let dest_status = dest_status.as_ref().unwrap();
                                if dest_status.value_needed {
                                    self.usages.insert((*operand, index), UsedAfter::DestNeeded);
                                } else if dest_status.used_after {
                                    self.usages.insert((*operand, index), UsedAfter::LocationNeeded);
                                } else {
                                    self.usages.insert((*operand, index), UsedAfter::None);
                                }
                            } else {
                                let status = self.analyze_usage(
                                    *operand, scopes.clone(), operations, index + 1);
                                self.usages.insert((*operand, index), get_usage(&status));
                                *self.variable_invalidations.entry(*operand).or_insert(0) |= status.invalid_soon;
                            }
                        } else {
                            self.usages.insert((*operand, index), UsedAfter::None);
                        }
                    }
                }
                OperationUnit::EnterBlock(scope) => {
                    scopes.push(*scope);
                    for operand in 0..variable_count {
                        let status = self.analyze_usage(operand, scopes.clone(), operations, index + 1);
                        if !status.value_needed {
                            self.scopes.get_mut(scope).unwrap().freed_variables.push(operand);
                        }
                    }
                }
                OperationUnit::LeaveBlock(_) => {
                    scopes.pop().unwrap();
                }
                OperationUnit::SyncPush(_) | OperationUnit::SyncLoad | OperationUnit::SyncPop => {}
            }
        }
    }

    pub fn get_initializations(&self, scope_id : usize) -> impl Iterator<Item=&usize> {
        if let Some(block) = self.scope_blocks.get(&scope_id) {
            return block.initialized_variables.iter();
        }
        return [].iter();
    }

    pub fn get_frees(&self, scope_id : usize) -> impl Iterator<Item=&usize> {
        return self.scopes.get(&scope_id).unwrap().freed_variables.iter();
    }


    pub fn used_after(&self, id : usize, row : usize) -> UsedAfter {
        if let Some(usage) = self.usages.get(&(id, row)) {
            return *usage;
        }
        return UsedAfter::None;
    }

    pub fn row_invalidations(&self, row : usize) -> u64 {
        return self.invalidations[row];
    }

    pub fn mark_dest(&mut self, id : usize, row : usize) {
        debug_assert!(*self.usages.get(&(id, row)).unwrap() == UsedAfter::DestNeeded);
        self.usages.insert((id, row), UsedAfter::ValueNeeded);
    }

    pub fn variable_invalidations(&self, id : usize) -> u64 {
        return *self.variable_invalidations.get(&id).unwrap();
    }
}