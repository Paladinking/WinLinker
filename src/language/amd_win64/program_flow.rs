use std::collections::HashMap;
use std::ops::Range;
use std::ptr;
use std::ptr::null_mut;

pub enum Usage {
    Use, Redefine
}

struct Node {
    parents : Vec<*mut Node>,
    children : Vec<*mut Node>,
    duration : Range<usize>
}

impl Node {
    unsafe fn new(mut parents : Vec<*mut Node>, start: usize) -> *mut Node {
        let new = Box::into_raw(Box::new(Node {
            parents, children : Vec::new(), duration : start..start
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
    nodes : HashMap<usize, *mut Node>,
    open : Vec<Vec<*mut Node>>,
    current_node : *mut Node,

    // (operand-id, row, index) (dest=0, operand=1+)
    committed_usages: HashMap<(usize, usize, usize), Usage>,
}

impl ProgramFlow {
    pub fn new() -> ProgramFlow {
        ProgramFlow {
            nodes : HashMap::new(),
            open : vec![], current_node : null_mut(),
            committed_usages : HashMap::new()
        }
    }

    fn add_node(&mut self, id : usize, row : usize, parents : Vec<*mut Node>) {
        let new = unsafe {Node::new(parents, row)};
        self.nodes.insert(id, new);
        self.open.push(vec![new]);
        self.current_node = new;
    }

    pub fn add_root(&mut self, id : usize, row : usize) {
        print!("Add root {}, {:?} => ", id, &self.open);
        debug_assert!(self.nodes.is_empty());
        debug_assert!(self.open.is_empty());
        self.add_node(id, row, Vec::new());
        println!(" {:?}", &self.open);
    }

    pub fn add_new(&mut self, id : usize, row : usize) {
        print!("Add new {}, {:?} => ", id, &self.open);
        // Should be true, probably not necessary though.
        //debug_assert!(self.open.last().unwrap().len() <= 1);
        // Needs to be true.
        debug_assert!(!self.nodes.contains_key(&id));
        debug_assert!(!self.current_node.is_null());
        unsafe {(*self.current_node).duration.end = row};
        let parents = self.open.pop().unwrap();
        self.add_node(id, row, parents);
        println!(" {:?}", &self.open);
    }

    pub fn re_open(&mut self, id : usize) {
        print!("Reopen {}, {:?} => ", id, &self.open);
        let node = *self.nodes.get(&id).unwrap();
        self.open.push(vec![node]);
        println!(" {:?}", &self.open);
    }

    pub fn merge(&mut self, n : usize) {
        print!("Merge {}, {:?} => ", n, &self.open);
        debug_assert!(self.open.len() >= n);
        let target = self.open.len() - n;
        for _ in 1..n {
            let mut others = self.open.pop().unwrap();
            self.open[target].append(&mut others);
        }
        println!(" {:?}", &self.open);
    }

    pub fn insert_open(&mut self, id : usize) {
        print!("Insert Open {}, {:?} => ", id, &self.open);
        self.open.last_mut().unwrap().push(*self.nodes.get(&id).unwrap());
        println!(" {:?}", &self.open);
    }

    pub fn re_add(&mut self, id : usize) {
        print!("Readd {}, {:?} => ", id, &self.open);
        let node = *self.nodes.get(&id).unwrap();
        let last_open = self.open.last_mut().unwrap();
        unsafe {
            for &open in last_open.iter() {
                (*open).children.push(node);
            }
            (*node).parents.append(last_open);
        }
        last_open.push(node);
        println!(" {:?}", &self.open);
    }

    pub fn end(&mut self) {
        print!("End, {:?} => ", &self.open);
        self.open.last_mut().unwrap().clear();
        println!(" {:?}", &self.open);
    }

    pub fn print(&self) {
        unsafe {
            let mut sorted = self.nodes.iter()
                .map(|(k, v)| *k)
                .collect::<Vec<_>>();
            sorted.sort_unstable();
            for k in sorted {
                let &n = self.nodes.get(&k).unwrap();
                let parents = (*n).parents.iter().map(
                    |&ptr| self.nodes.iter().find_map(
                        |(key, &node)| if node == ptr {Some(*key)} else {None}
                    ).unwrap()
                ).collect::<Vec<_>>();
                let children = (*n).children.iter().map(
                    |&ptr| self.nodes.iter().find_map(
                        |(key, &node)| if node == ptr {Some(*key)} else {None}
                    ).unwrap()
                ).collect::<Vec<_>>();
                print!("Node {}:", k);
                println!(" {}..{}, {:?}, {:?}", (*n).duration.start, (*n).duration.end, parents, children);
            }
        }
    }
}

impl Drop for ProgramFlow {
    fn drop(&mut self) {
        unsafe {
            for (_, &node) in &self.nodes {
                drop(Box::from_raw(node));
            }
        }
    }
}

