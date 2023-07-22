use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::ptr;
use crate::language::amd_win64::compiler::OperationUnit;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Usage {
    Use, Redefine, UseAndRedefine
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Needed {
    Value, Location, None
}

struct Node {
    parents : Vec<*mut Node>,
    children : Vec<*mut Node>,
    looping : Option<*mut Node>,
    duration : Range<usize>,
    initialized_variables : Vec<usize>,
    freed_variables : Vec<usize>,
}

impl Node {
    unsafe fn new(mut parents : Vec<*mut Node>, start: usize) -> *mut Node {
        let new = Box::into_raw(Box::new(Node {
            parents, children : Vec::new(), duration : start..start,
            looping : None, initialized_variables : Vec::new(),
            freed_variables : Vec::new()
        }));
        for &node in &(*new).parents {
            (*node).children.push(new);
        }
        return new;
    }
}
// ADD(new_child): Add top of open as parents to new node, replace with new child
// BACK(parent): Add parent to open
// CLOSE(n): merge the last n in open
// SKIP(parent): insert parent in top of open
// LOOP(parent): Add top of open as parents to parent, replace with parent
// END: replace top of open with ()

// a = 10;           <-- |Node 1, children[2], parents[]          ADD, open[(root)] => open[(1)]
// b = 5;
// if a > 5 {        <-- Node 2, children[3, 4], parents[1]       ADD, open[(1)] => open[(2)]
//      b = 20;      <-- Node 3, children[9], parents[2]          ADD, BACK, open[(2)] => open[(3)] => open[(3), (2)]
// } else if a < 5 { <-- Node 4, children[5, 9], parents[2]       ADD, open[(3), (2)] => open[(3), (4)]
//      b = 10;      <-- Node 5, children[6], parents[4]          ADD, open[(3), (4)] => open[(3), (5)]
//      if a < 4 {   <-- Node 6, children[7, 8], parents[5]       ADD, open[(3), (5)] => open[(3), (6)]
//          a = 3;   <-- Node 7 children[9], parents[6]           ADD, BACK, open[(3), (6)] => open[(3), (7)] => open[(3), (7), (6)]
//      } else {
//          a = 6;   <-- Node 8 children[9], parents[6]           ADD, CLOSE, open[(3), (7), (6)] => open[(3), (7), (8)] => open[(3), (7, 8)]
//      }
// }                                                              CLOSE, SKIP open[(3), (7, 8)] => open[(3, 7, 8)] => open[(3, 7, 8, 2)]
// b = b + 5;        <-- Node 9 children[10], parents[3, 4, 7, 8] ADD, open[(3, 7, 8, 2)] => open[(9)]
// while b > 0 {     <-- Node 10 children[11, 12], parents[9]     ADD, open[(9)] => open[(10)]
//      a = a + 1;   <-- Node 11 children[10], parents[10]        ADD, LOOP open[(10)] => open[(11)] => open[(10)]
// }
// if a > b {        <-- Node 12 children[13, 14], parents[10]    ADD, open[(10)] => open[(12)]
//      a = 10;      <-- Node 13 children[14], parents[12]        ADD, CLOSE, SKIP, open[(12)] => open[(13)] => open[(13)] => open[(13, 12)]
// }
// b = a + b;        <-- |Node 14 children[], parents[12]         ADD, open[(13, 12)] => open[(14)]
// return a + b;         |                                        END, open[(14)] => open[()]
// b = a + b;        <-- Node 15 children[], parents[]            ADD, open[()] => open[(15)]



pub struct ProgramFlow {
    nodes : Vec<*mut Node>,
    open : Vec<Vec<*mut Node>>,
    current_node : *mut Node,

    initialized: Vec<bool>,
    variable_invalidations: Vec<u64>,
    // (operand-id, row)
    usages: HashMap<(usize, usize), Usage>,
    // (row, index)(dest=0, operand=1+)
    needed: HashMap<(usize, usize), Needed>,
    invalidations : Vec<u64>,

    root_id : usize
}

impl ProgramFlow {
    pub fn new(variable_count : usize) -> ProgramFlow {
        ProgramFlow {
            nodes : Vec::new(),
            open : vec![], current_node : ptr::null_mut(),
            initialized : vec![false; variable_count],
            usages : HashMap::new(),
            needed : HashMap::new(),
            invalidations : Vec::new(),
            variable_invalidations : Vec::with_capacity(variable_count),
            root_id : 0
        }
    }

    fn add_node(&mut self, id : usize, row : usize, parents : Vec<*mut Node>) {
        let new = unsafe {Node::new(parents, row)};
        debug_assert!(self.nodes.len() == id);
        self.nodes.push(new);
        self.open.push(vec![new]);
        self.current_node = new;
    }

    pub fn add_root(&mut self, id : usize, row : usize) {
        print!("Add root {}, {:?} => ", id, &self.open);
        debug_assert!(self.open.is_empty());
        self.add_node(id, row, Vec::new());
        self.root_id = id;
        println!(" {:?}", &self.open);
    }

    pub fn add_new(&mut self, id : usize, row : usize) {
        print!("Add new {}, {:?} => ", id, &self.open);
        // Should be true, probably not necessary though.
        //debug_assert!(self.open.last().unwrap().len() <= 1);
        // Needs to be true.
        debug_assert!(!self.current_node.is_null());
        unsafe {(*self.current_node).duration.end = row};
        let parents = self.open.pop().unwrap();
        self.add_node(id, row, parents);
        println!(" {:?}", &self.open);
    }

    pub fn re_open(&mut self, id : usize) {
        print!("Reopen {}, {:?} => ", id, &self.open);
        let node = *self.nodes.get(id).unwrap();
        self.open.push(vec![node]);
        println!(" {:?}", &self.open);
    }

    pub fn merge(&mut self, n : usize, root : usize) {
        print!("Merge {}, {:?} => ", n, &self.open);
        debug_assert!(self.open.len() >= n);
        debug_assert!(self.open.last().unwrap().len() == 1);
        let target = self.open.len() - n;
        for _ in 1..n {
            let mut others = self.open.pop().unwrap();
            self.open[target].append(&mut others);
        }
        let root = *self.nodes.get(root).unwrap();
        unsafe {
            let mut stack = (*root).children.clone();
            while let Some(node) = stack.pop() {
                (*root).initialized_variables.extend_from_slice(
                    &std::mem::take(&mut (*node).initialized_variables)
                );
                stack.extend_from_slice(&(*node).children);
            }
        }
        println!(" {:?}", &self.open);
    }

    pub fn insert_open(&mut self, id : usize) {
        print!("Insert Open {}, {:?} => ", id, &self.open);
        self.open.last_mut().unwrap().push(*self.nodes.get(id).unwrap());
        println!(" {:?}", &self.open);
    }

    pub fn re_add(&mut self, id : usize) {
        print!("Readd {}, {:?} => ", id, &self.open);
        let node = *self.nodes.get(id).unwrap();
        let last_open = self.open.last_mut().unwrap();
        unsafe {
            for &open in last_open.iter() {
                (*open).looping.replace(node);
            }
        }
        last_open.push(node);
        println!(" {:?}", &self.open);
    }

    pub fn end(&mut self) {
        print!("End, {:?} => ", &self.open);
        self.open.last_mut().unwrap().clear();
        println!(" {:?}", &self.open);
    }

    pub fn add_usage(&mut self, id : usize, row : usize, usage : Usage) {
        assert!(!self.current_node.is_null());
        self.usages.entry((id, row))
            .and_modify(|e| *e = Usage::UseAndRedefine)
            .or_insert(usage);
        if !self.initialized[id] {
            unsafe {
                (*self.current_node).initialized_variables.push(id);
            }
            self.initialized[id] = true;
        }
    }

    unsafe fn analyze_usage(&self, id : usize, operations : &Vec<OperationUnit>, mut row: usize, mut node: *mut Node) -> (Needed, u64) {
        let mut to_visit = vec![];
        let mut needed = false;
        let mut used = false;

        let mut local_invalidations = 0;
        let mut invalidations = 0;
        let mut taken_loops = HashSet::new();

        'nodes: loop {
            let mut found = false;
            for i in row..(*node).duration.end {
                if let Some(&usage) = self.usages.get(&(id, i)) {
                    if usage != Usage::Redefine {
                        needed = needed || !found;
                        invalidations |= local_invalidations;
                    }
                    if usage != Usage::Use {
                        if let Some(new) = to_visit.pop() {
                            node = new;
                            row = (*node).duration.start;
                            continue 'nodes;
                        } else {
                            break 'nodes;
                        }
                    }
                    found = true;
                    used = true;
                }
                local_invalidations |= operations[i].invalidations();
            }
            to_visit.extend_from_slice(&(*node).children);
            if let Some(n) = (*node).looping {
                if taken_loops.insert(n) {
                    to_visit.push(n);
                }
            }
            if let Some(new) = to_visit.pop() {
                node = new;
                row = (*node).duration.start;
            } else {
                break;
            }
        }
        return (match (needed, used) {
            (true, _) => Needed::Value,
            (false, true) => Needed::Location,
            (false, false) => Needed::None
        }, invalidations);
    }

    pub fn finalize(&mut self, operations : &Vec<OperationUnit>, variable_count : usize, row : usize) {
        let mut cur_node = 0;
        self.invalidations.resize(operations.len(), 0);
        self.variable_invalidations.resize(variable_count, 0);
        unsafe {
            (*self.current_node).duration.end = row;
            for (index, op) in operations.iter().enumerate() {
                while index == (*self.nodes[cur_node]).duration.end {
                    cur_node += 1;
                    debug_assert!(index == (*self.nodes[cur_node]).duration.start);
                    if index != (*self.nodes[cur_node]).duration.end {
                        for operand in 0..variable_count {
                            let (needed, _) = self.analyze_usage(operand, operations,
                                                                 index, self.nodes[cur_node]);
                            if needed != Needed::Value {
                                (*self.nodes[cur_node]).freed_variables.push(operand);
                            }
                        }
                    }
                }
                if let OperationUnit::Operation(operation) = op {
                    if let Some(dest) = operation.dest {
                        if dest < variable_count {
                            let (needed, invalidations) = self.analyze_usage(
                                dest, operations, index + 1, self.nodes[cur_node]
                            );
                            self.needed.insert((index, 0), needed);
                            self.invalidations[index] = invalidations;
                            self.variable_invalidations[dest] |= invalidations;
                        }
                    }
                    for (i, &operand) in operation.operands.iter().enumerate() {
                        if operand < variable_count {
                            let (needed, invalidations) = self.analyze_usage(
                                operand, operations, index + 1, self.nodes[cur_node]
                            );
                            self.needed.insert((index, 1 + i), needed);
                            self.variable_invalidations[operand] |= invalidations;
                        }
                    }
                }
            }
        }
    }

    pub fn print(&self) {
        unsafe {
            for (k, &n) in self.nodes.iter().enumerate() {
                let parents = (*n).parents.iter().map(
                    |&ptr| self.nodes.iter().enumerate().find_map(
                        |(key, &node)| if node == ptr {Some(key)} else {None}
                    ).unwrap()
                ).collect::<Vec<_>>();
                let children = (*n).children.iter().map(
                    |&ptr| self.nodes.iter().enumerate().find_map(
                        |(key, &node)| if node == ptr {Some(key)} else {None}
                    ).unwrap()
                ).collect::<Vec<_>>();
                print!("Node {}:", k);
                println!(" {}..{}, {:?}, {:?}", (*n).duration.start, (*n).duration.end, parents, children);
            }
        }
    }

    pub fn get_initializations(&self, node_id : usize) -> impl Iterator<Item=&usize> {
        return unsafe {(*self.nodes[node_id]).initialized_variables.iter()};
    }

    pub fn get_frees(&self, node_id : usize) -> impl Iterator<Item=&usize> {
        return unsafe {(*self.nodes[node_id]).freed_variables.iter()};
    }


    pub fn used_after(&self, index : usize, row : usize) -> Needed {
        return self.needed.get(&(index, row)).cloned().unwrap_or(Needed::None);
    }

    pub fn row_invalidations(&self, row : usize) -> u64 {
        return self.invalidations[row];
    }

    pub fn variable_invalidations(&self, id : usize) -> u64 {
        return self.variable_invalidations[id];
    }
}

impl Drop for ProgramFlow {
    fn drop(&mut self) {
        unsafe {
            for &node in &self.nodes {
                drop(Box::from_raw(node));
            }
        }
    }
}

