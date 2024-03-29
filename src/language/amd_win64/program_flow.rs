use std::collections::{HashSet};
use std::fmt::format;
use std::ops::{Index, Range};
use std::ptr;
use crate::language::amd_win64::compiler::OperationUnit;
use crate::language::amd_win64::operation::Operand;
use crate::language::amd_win64::registers;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Usage {
    Use, Redefine
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Needed {
    Value, Location, None
}

struct Node {
    parents : Vec<*mut Node>,
    children : Vec<*mut Node>,
    duration : Range<usize>,

    live_in : HashSet<usize>,
    live_out : HashSet<usize>,

    gen : HashSet<usize>,
    kill : HashSet<usize>,
}

impl Node {
    unsafe fn new(mut parents : Vec<*mut Node>, start: usize) -> *mut Node {
        let new = Box::into_raw(Box::new(Node {
            parents, children : Vec::new(), duration : start..start,
            live_in : HashSet::new(), live_out : HashSet::new(),
            gen : HashSet::new(), kill : HashSet::new()
        }));
        for &node in &(*new).parents {
            (*node).children.push(new);
        }
        return new;
    }
}

pub struct InterferenceGraph {

}

// ADD(new_child): Add top of open as parents to new node, replace with new child
// BACK(parent): Add parent to open
// CLOSE(n): merge the last n in open
// SKIP(parent): insert parent in top of open
// LOOP(parent): Add top of open as parents to parent, replace with parent
// END: replace top of open with ()

// a = 10;           <-- |Node 1, children[2], parents[]          ADD, open[(root)] => open[(1)]
// b = 5;                |
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
    root_id : usize,
    exit_points : Vec<*mut Node>
}

impl ProgramFlow {
    pub fn new(variable_count : usize) -> ProgramFlow {
        ProgramFlow {
            nodes : Vec::new(),
            open : vec![], current_node : ptr::null_mut(),
            root_id : 0, exit_points : Vec::new()
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
        debug_assert!(self.open.is_empty());
        self.add_node(id, row, Vec::new());
        self.root_id = id;
    }

    pub fn add_new(&mut self, id : usize, row : usize) {
        // Should be true, probably not necessary though.
        //debug_assert!(self.open.last().unwrap().len() <= 1);
        // Needs to be true.
        debug_assert!(!self.current_node.is_null());
        unsafe {(*self.current_node).duration.end = row};
        let parents = self.open.pop().unwrap();
        self.add_node(id, row, parents);
    }

    pub fn re_open(&mut self, id : usize) {
        let node = *self.nodes.get(id).unwrap();
        self.open.push(vec![node]);
    }

    pub fn merge(&mut self, n : usize, _root : usize) {
        debug_assert!(self.open.len() >= n);
        debug_assert!(self.open.last().unwrap().len() == 1);
        let target = self.open.len() - n;
        for _ in 1..n {
            let mut others = self.open.pop().unwrap();
            self.open[target].append(&mut others);
        }
    }

    pub fn insert_open(&mut self, id : usize) {
        self.open.last_mut().unwrap().push(*self.nodes.get(id).unwrap());
    }

    pub fn re_add(&mut self, id : usize) {
        let node = *self.nodes.get(id).unwrap();
        let last_open = self.open.last_mut().unwrap();
        unsafe {
            for &open in last_open.iter() {
                (*open).children.push(node);
            }
            (*node).parents.extend_from_slice(&last_open);
        }
        last_open.clear();
        last_open.push(node);
    }

    pub fn end(&mut self) {
        self.open.last_mut().unwrap().clear();
        self.exit_points.push(self.current_node);
    }

    pub fn add_usage(&mut self, id : usize, row : usize, usage : Usage) {
        assert!(!self.current_node.is_null());
        unsafe {
            if usage == Usage::Use {
                if !(*self.current_node).kill.contains(&id) {
                    (*self.current_node).gen.insert(id);
                }
            } else {
                (*self.current_node).kill.insert(id);
            }
        }
    }

    unsafe fn finalize_internal(&mut self, operations: &Vec<OperationUnit>, operands: &Vec<Operand>) {
        let mut node_stack = std::mem::take(&mut self.exit_points);

        while let Some(node) = node_stack.pop() {
            (*node).live_out = (*node).children.iter().flat_map(
                |&child| (*child).live_in.iter().cloned()
            ).collect();
            let new_in = (*node).gen.iter().chain(
                (*node).live_out.difference(&(*node).kill)
            ).cloned().collect();
            if new_in != (*node).live_in {
                (*node).live_in = new_in;
                node_stack.extend_from_slice(&(*node).parents);
            }
        }

        struct VirtualRegister {
            neighbors : HashSet<*mut VirtualRegister>,
            marked : bool
        }
        let operand_count = operands.len();
        // Add all operands to graph
        let mut graph = vec![];

        // Add all registers + operands to graph
        let register_count = registers::GEN_REG_COUNT;
        graph.resize_with(operand_count + register_count, ||
            Box::into_raw(Box::new(VirtualRegister {
                neighbors: HashSet::new(), marked: false
            }))
        );

        // Add edges between all registers
        let mut registers = registers::GEN_REG;
        while let Some(register) = registers::get_next_register(&mut registers) {
            (*graph[operand_count + register]).neighbors =
                (operand_count..(operand_count + register_count)).filter_map(|i|
                    if i == register {None} else { Some(graph[i]) }
                ).collect();
        }

        // Iterate all nodes:
        for &node in &self.nodes {
            // TODO: Replace with std::mem::take?
            let mut live = (*node).live_out.clone();

            /*for &operand in &live {
                (*graph[operand]).neighbors.extend(
                    live.iter().filter_map(|&i|
                        if i != operand {Some(graph[i])} else {None})
                );
            }*/

            for i in (*node).duration.clone().rev() {
                let operation = if let OperationUnit::Operation(operation) = &operations[i] {
                    operation
                } else {
                    continue;
                };
                // Just for now.
                debug_assert!(operation.operands.len() <= 2);
                println!("{:?}", operation.operator);
                if let Some(dest) = operation.dest {
                    live.remove(&dest);
                    let dest_bitmap = operation.operator.first_bitmap(operands[dest].size);

                    // Get all general purpose registers dest cannot be in.
                    let mut bitmap = (!dest_bitmap) & registers::GEN_REG;
                    while let Some(index) = registers::get_next_register(&mut bitmap) {
                        println!("Dest {}, {}", dest, registers::register_index_to_string(index));
                        (*graph[dest]).neighbors.insert(graph[index + operand_count]);
                        (*graph[index + operand_count]).neighbors.insert(graph[dest]);
                    }
                }
                // Add edges between live and invalidated registers.
                let mut bitmap = operation.invalidations & registers::GEN_REG;
                while let Some(index) = registers::get_next_register(&mut bitmap) {
                    println!("Invalid {}, {}", index, registers::register_index_to_string(index));
                    for &node in &live {
                        (*graph[node]).neighbors.insert(graph[index]);
                        (*graph[index]).neighbors.insert(graph[node]);
                    }
                }
                if let Some(&first) = operation.operands.first() {
                    if live.insert(first) {
                        let first_bitmap = operation.operator.first_bitmap(operands[first].size);
                        // Get all general purpose registers first cannot be in.
                        let mut bitmap = (!first_bitmap) & registers::GEN_REG;
                        while let Some(index) = registers::get_next_register(&mut bitmap) {
                            println!("First {}, {}", first, registers::register_index_to_string(index));
                            (*graph[first]).neighbors.insert(graph[index + operand_count]);
                            (*graph[index + operand_count]).neighbors.insert(graph[first]);
                        }
                    }
                }
                if let Some(&second) = operation.operands.get(1) {
                    let second_bitmap = operation.operator.second_bitmap(operands[second].size);
                    let mut bitmap = (!second_bitmap) & registers::GEN_REG;
                    while let Some(index) = registers::get_next_register(&mut bitmap) {
                        (*graph[second]).neighbors.insert(graph[index + operand_count]);
                        (*graph[index + operand_count]).neighbors.insert(graph[second]);
                    }
                    live.insert(second);
                }
                for &operand in &operation.operands {
                    for &i in live.iter().filter(|&index| *index != operand) {
                        (*graph[i]).neighbors.insert(graph[operand]);
                        (*graph[operand]).neighbors.insert(graph[i]);
                    }
                }
            }
        }
        println!("Count: {}, {:?}, {:?}", operand_count, graph[0], graph[1]);

        for (i, &val) in graph.iter().enumerate() {
            let name = if i < operand_count { format!("{}", i )} else {
                registers::register_index_to_string(i - operand_count).to_owned()
            };
            let neighbors = (*val).neighbors.iter().map(|&val| {
                let i = graph.iter().enumerate().find_map(|(i, &v)| if (val == v) { Some(i) } else { None }).unwrap();
                if i < operand_count { format!("{}", i) } else {
                    registers::register_index_to_string(i - operand_count).to_owned()
                }
            }).collect::<Vec<_>>();
            println!("{}, {:?}", name, neighbors);
        }


    }

    pub fn finalize(&mut self, row: usize, operations: &Vec<OperationUnit>, operands: &Vec<Operand>) {
        unsafe {
            (*self.current_node).duration.end = row;
            self.finalize_internal(operations, operands);
        }
    }

    pub fn print(&self) {
        unsafe {
            for (i, &node) in self.nodes.iter().enumerate() {
                println!("Node {}: ", i);
                let children = (*node).children.iter().map(
                    |&child| self.nodes.iter().enumerate()
                        .find_map(|(i, &n)| if child == n {Some(i)} else {None}).unwrap()
                ).collect::<Vec<_>>();
                let parents = (*node).parents.iter().map(
                    |&child| self.nodes.iter().enumerate()
                        .find_map(|(i, &n)| if child == n {Some(i)} else {None}).unwrap()
                ).collect::<Vec<_>>();
                println!("\tChildren: {:?}", children);
                println!("\tParents: {:?}", parents);
                println!("\tLive_in: {:?}", (*node).live_in);
                println!("\tLive_out: {:?}", (*node).live_out);
                println!("\tGen: {:?}", (*node).gen);
                println!("\tKill: {:?}", (*node).kill);
            }
        }
    }

    pub fn get_initializations(&self, node_id : usize) -> impl Iterator<Item=&usize> {
        return [].iter();
    }

    pub fn get_frees(&self, node_id : usize) -> impl Iterator<Item=&usize> {
        return [].iter();
    }


    pub fn used_after(&self, index : usize, row : usize) -> Needed {
        return Needed::None;
        //return self.needed.get(&(row, index)).cloned().unwrap_or(Needed::None);
    }

    pub fn row_invalidations(&self, row : usize) -> u64 {
        return 0;
        //return self.invalidations[row];
    }

    pub fn variable_invalidations(&self, id : usize) -> u64 {
        return 0;
        //return self.variable_invalidations[id];
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

