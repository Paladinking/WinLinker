use super::{Expression, ExpressionData};
use std::cmp::Ordering;
use crate::language::operator::{DualOperator, SingleOperator};


enum ExpressionBuilderType{
    Atom(Expression),
    SingleOperator(SingleOperator),
    DualOperator(DualOperator)
}


struct ExpressionBuilderNode {
    expression_type: ExpressionBuilderType,
    par : usize,
    parent : *mut ExpressionBuilderNode,
    first_child : *mut ExpressionBuilderNode,
    second_child : *mut ExpressionBuilderNode,
    pos : (usize, usize)
}

impl ExpressionBuilderNode {
    fn priority(&self) -> isize {
        match self.expression_type {
            ExpressionBuilderType::Atom(_) => isize::MIN,
            ExpressionBuilderType::DualOperator(s) => s as isize,
            ExpressionBuilderType::SingleOperator(s) => s as isize
        }
    }
}

impl Eq for ExpressionBuilderNode {}

impl PartialEq for ExpressionBuilderNode {
    fn eq(&self, other: &Self) -> bool {
        self.priority() == other.priority() && self.par == other.par
    }
}

impl Ord for ExpressionBuilderNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialOrd for ExpressionBuilderNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let par_cmp = other.par.cmp(&self.par);
        if par_cmp.is_eq() {
            self.priority().partial_cmp(&other.priority())
        } else {
            Some(par_cmp)
        }
    }
}

pub(crate) struct ExpressionBuilder {
    root : *mut ExpressionBuilderNode,
    open_paren : Vec<usize>,
    prev : *mut ExpressionBuilderNode,
    nodes : usize
}

impl ExpressionBuilder {
    pub(crate) fn new() -> ExpressionBuilder {
        let root = Box::into_raw(Box::new( ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::SingleOperator(SingleOperator::Pass),
            par : 0,
            parent : std::ptr::null_mut(),
            first_child : std::ptr::null_mut(),
            second_child: std::ptr::null_mut(),
            pos : (0, 0)
        }));
        ExpressionBuilder {
            root,
            open_paren : Vec::new(),
            prev : root,
            nodes : 0
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        return self.open_paren.is_empty()
    }

    pub(crate) fn into_expression(self) -> Vec<ExpressionData> {
        let mut res : usize = 0;
        let val = unsafe { // Root never changes, is always valid
            if (*self.root).second_child.is_null() {
                panic!("No expression created");
            }
            (*self.root).second_child
        };
        let mut stack = vec![(val, &mut res as *mut usize)];
        let mut expressions = Vec::with_capacity(self.nodes);
        while let Some((top, dest)) = stack.pop() {
            let mut top = unsafe {Box::from_raw(top)}; // Tree is valid
            match &mut top.expression_type {
                ExpressionBuilderType::Atom(e) => {
                    unsafe {*dest = expressions.len()};
                    expressions.push(ExpressionData::new(std::mem::replace(e, Expression::None), top.pos));
                    drop(top);
                },
                ExpressionBuilderType::SingleOperator(s) => {
                    let e = Expression::SingleOperator {
                        operator: *s, expr: 0
                    };
                    top.expression_type = ExpressionBuilderType::Atom(e);
                    let child = match &mut top.expression_type {
                        ExpressionBuilderType::Atom(Expression::SingleOperator {expr, ..}) =>
                            (top.second_child, expr as *mut usize),
                        _ => unreachable!()
                    };
                    stack.push((Box::into_raw(top), dest));
                    stack.push(child);
                },
                ExpressionBuilderType::DualOperator(s) => {
                    let e = Expression::Operator {
                        operator: *s, first: 0, second : 0
                    };
                    top.expression_type = ExpressionBuilderType::Atom(e);
                    let (first_child, second_child) = match &mut top.expression_type {
                        ExpressionBuilderType::Atom(Expression::Operator {first, second, ..}) =>
                            ((top.first_child, first as *mut usize), (top.second_child, second as *mut usize)),
                        _ => unreachable!()
                    };
                    stack.push((Box::into_raw(top), dest));
                    stack.push(second_child);
                    stack.push(first_child);
                }
            }
        }
        expressions
    }

    pub(crate) fn open_parentheses(&mut self) {
        self.open_paren.push(0);
    }

    pub(crate) fn close_parentheses(&mut self) -> Result<(), ()> {
        self.open_paren.pop().ok_or(())?;
        Ok(())
    }

    pub(crate) fn add_atom(&mut self, expr : Expression, pos : (usize, usize)) {
        let atom = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type: ExpressionBuilderType::Atom(expr),
            par: self.open_paren.len(),
            parent: self.prev,
            first_child: std::ptr::null_mut(),
            second_child: std::ptr::null_mut(),
            pos
        }));
        unsafe {
            if let ExpressionBuilderType::Atom(_) = (*self.prev).expression_type {
                panic!("Multiple atoms in a row");
            }
            (*self.prev).second_child = atom;
        }
        self.nodes += 1;
        self.prev = atom;
    }

    pub(crate) fn add_single_operator(&mut self, s : SingleOperator, pos : (usize, usize)) {
        let node = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::SingleOperator(s),
            par : self.open_paren.len(),
            parent : self.prev,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut(),
            pos
        }));
        unsafe {
            if let ExpressionBuilderType::Atom(_) = (*self.prev).expression_type {
                panic!("Single operator following atom");
            }
            (*self.prev).second_child = node;
        }
        self.nodes += 1;
        self.prev = node;
    }

    pub(crate) fn add_dual_operator(&mut self, op : DualOperator) {
        let node = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::DualOperator(op),
            par : self.open_paren.len(),
            parent : self.prev,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut(),
            pos : (0, 0)
        }));
        unsafe {
            if !matches!((*self.prev).expression_type, ExpressionBuilderType::Atom(_)) {
                panic!("Dual operator not following atom");
            }
            let mut owner = (*self.prev).parent;
            while *node >= *owner {
                owner = (*owner).parent;
                debug_assert!(!owner.is_null(), "Node cannot be higher than root");
            }
            (*node).first_child = (*owner).second_child;
            (*(*owner).second_child).parent = node;
            (*owner).second_child = node;
            (*node).parent = owner;
            (*node).pos = (*(*node).first_child).pos;
        }
        self.nodes += 1;
        self.prev = node;
    }
}

