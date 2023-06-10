use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::slice::Iter;
use crate::language::amd_win64::compiler::OperationUnit;
use crate::language::amd_win64::operation::{Operand, Operation};

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
    row : usize,
    last_child : usize,
    initialized_variables : Vec<usize>,
    freed_variables : HashSet<usize>,
    path : Vec<usize>
}

impl Scope {
    fn new(block_id : usize, row : usize,  last_child : usize, path : Vec<usize>) -> Scope {
        Scope {block_id, row, last_child,
            initialized_variables : Vec::new(), freed_variables : HashSet::new(), path}
    }
}

struct UsageNode {
    row : usize,
    scope : usize,
    value_needed : bool,
}

impl UsageNode {
    fn new(row : usize, scope : usize, value_needed : bool) -> UsageNode {
        UsageNode {row, scope, value_needed}
    }
}

#[derive(PartialEq, Debug)]
pub enum UsedAfter {
    ValueNeeded, LocationNeeded, None
}

pub struct UsageTracker {
    scope_ids : Vec<usize>,
    first_scope_ids : Vec<usize>,
    //usage_queue : HashMap<usize, Vec<UsageNode>>,
    // <first_scope_id, Vec<(scope-id, scope-row)>>
    scope_blocks : HashMap<usize, ScopeBlock>,
    scopes : HashMap<usize, Scope>,
    usages : HashMap<(usize, usize), bool>,
    // <(operand-id, row)
    used_operands : HashMap<usize, usize>,
    // <scope-id, Vec<operand-id>
    initialized_operands : HashMap<usize, Vec<usize>>,
    outer_scope : usize
}

impl UsageTracker {
    pub fn new() -> UsageTracker {
        UsageTracker {
            scope_ids : Vec::new(), first_scope_ids : Vec::new(),
            usages : HashMap::new(), scopes : HashMap::new(), scope_blocks : HashMap::new(),
            initialized_operands : HashMap::new(), used_operands : HashMap::new(), outer_scope : 0
        }
    }

    pub fn enter_scope(&mut self, scope_id : usize, scope_row : usize) {
        self.first_scope_ids.push(scope_id);
        self.scope_ids.push(scope_id);
        self.scopes.entry(scope_id).or_insert_with(||
            Scope::new(scope_id, scope_row, scope_id, self.scope_ids.clone()));
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

    pub fn alt_scope(&mut self, scope_id : usize, scope_row : usize, condition : bool) {
        let first_scope_id = *self.first_scope_ids.last().unwrap();
        *self.scope_ids.last_mut().unwrap() = scope_id;
        self.scopes.insert(scope_id,Scope::new(
            first_scope_id, scope_row, scope_id, self.scope_ids.clone()));
        self.scope_blocks.get_mut(&first_scope_id).unwrap().scopes.push(scope_id);
        if !condition {
            self.scope_blocks.get_mut(&first_scope_id).unwrap().conditional = false;
        }
    }

    fn scope_relation<'a>(scopes : &'a HashMap<usize, Scope>, mut scope : &'a Scope, mut other : &'a Scope) -> (bool, &'a Scope) {
        if scope.path.len() > other.path.len() {
            scope = &scopes[&scope.path[other.path.len() - 1]];
        } else if scope.path.len() < other.path.len() {
            other = &scopes[&other.path[scope.path.len() - 1]];
        }
        loop {
            if scope.path.last() == other.path.last() {
                return (false, scope);
            }
            if scope.block_id == other.block_id {
                return (true, scope);
            }
            scope = &scopes[&scope.path[scope.path.len() - 2]];
            other = &scopes[&other.path[other.path.len() - 2]];
       }
    }

    pub fn add_usage(&mut self, id : usize, row : usize, value_needed : bool) {
        let first_id = *self.first_scope_ids.last().unwrap();
        self.usages.insert((id, row), value_needed);
        if !self.used_operands.contains_key(&id) {
            self.scope_blocks.get_mut(&first_id).unwrap()
                .initialized_variables.push(id);
        }
        self.used_operands.insert(id, row);
    }

    fn value_needed(&self, id : usize, mut scopes : Vec<usize>, operations : &Vec<OperationUnit>, start : usize) -> bool {
        let final_usage = *self.used_operands.get(&id).unwrap();
        let mut targets = vec![scopes.last().cloned()];
        let mut index = start;
        let mut visited = HashSet::new();
        loop {
            if index == operations.len() || index > final_usage {
                return false;
            }
            match &operations[index] {
                OperationUnit::Operation(_) => {
                    if targets.last().unwrap().is_none() {
                        index  += 1;
                        continue;
                    }
                    if let Some(&needed) = self.usages.get(&(id, index)) {
                        if needed {
                            return true;
                        } else {
                            if targets.len() == 1 {
                                return false;
                            }
                            *targets.last_mut().unwrap() = None;
                        }
                    }
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
                OperationUnit::LeaveBlock(mut has_next) => {
                    scopes.pop().unwrap();
                    targets.pop().unwrap();
                    if targets.len() == 0 {
                        if has_next {
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
                }
            }
            index += 1;
        }
    }

    pub fn finalize(&mut self, operations : &Vec<OperationUnit>, variable_count : usize) {
        debug_assert!(self.first_scope_ids.is_empty());
        let mut scopes = Vec::with_capacity(self.scope_ids.capacity());
        for (index, operation) in operations.iter().enumerate() {

            match operation {
                OperationUnit::Operation(operation) => {
                    println!("{:?}", operation);
                    if let Some(dest) = operation.dest {
                        if dest < variable_count {
                            let value_needed = self.value_needed(
                                dest, scopes.clone(), operations, index + 1);
                            println!("Value needed(dest {}) : {}", index, value_needed);
                        }
                    }
                    for (i, operand) in operation.operands.iter().enumerate() {
                        if *operand < variable_count {
                            let value_needed = self.value_needed(
                                *operand, scopes.clone(), operations, index + 1);
                            println!("Value needed({}, {}) : {}", index, *operand, value_needed);
                        }
                    }
                }
                OperationUnit::EnterBlock(scope) => {
                    scopes.push(*scope);
                }
                OperationUnit::LeaveBlock(_) => {
                    scopes.pop().unwrap();
                }
            }
        }
    }

    pub fn get_initializations(&self, scope_id : usize) -> impl Iterator<Item=&usize> {
        if let Some(list) = self.initialized_operands.get(&scope_id) {
            return list.iter();
        }
        return [].iter();
    }

    pub fn used_after(&self, id : usize, row : usize) -> UsedAfter {
        /*if self.free_usages.contains_key(&(id, row)) {
            if self.final_usages.contains(&(id, row))  {
                return UsedAfter::None;
            }
            return UsedAfter::LocationNeeded;
        }*/
        return UsedAfter::ValueNeeded;
    }

    pub fn next_free(&self, id : usize, row : usize, scope : usize) -> usize {
        /*let first_scope_id = self.scopes.iter().find_map(|(k, v)|
            if v.iter().find(|&s| s.id == scope).is_some() {Some(*k)} else {None}
        ).unwrap();
        self.free_usages.iter().filter_map(|((operand_id, r), s)|
            if *operand_id == id && *r >= row && (s.id == scope || s.first_id != first_scope_id) {Some(*r)} else {None}
        ).reduce(usize::max).unwrap()*/0
    }

    pub fn final_usage(&self, id : usize) -> usize {
        /*self.final_usages.iter().filter_map(|(operand_id, row )|
            if *operand_id == id { Some(*row) } else {None}
        ).reduce(usize::max).unwrap()*/0
    }
}