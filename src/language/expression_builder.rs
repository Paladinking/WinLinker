use super::parser::SingleOperator;
use crate::language::parser::{Expression, DualOperator};
use std::cmp::Ordering;


enum ExpressionBuilderType <'a>{
    Atom(Expression<'a>),
    SingleOperator(SingleOperator),
    DualOperator(DualOperator)
}


struct ExpressionBuilderNode<'a> {
    expression_type: ExpressionBuilderType<'a>,
    par : usize,
    parent : *mut ExpressionBuilderNode<'a>,
    first_child : *mut ExpressionBuilderNode<'a>,
    second_child : *mut ExpressionBuilderNode<'a>
}

impl <'a> ExpressionBuilderNode<'a> {
    fn priority(&self) -> isize {
        match self.expression_type {
            ExpressionBuilderType::Atom(_) => isize::MIN,
            ExpressionBuilderType::DualOperator(s) => s as isize,
            ExpressionBuilderType::SingleOperator(s) => s as isize
        }
    }
}

impl <'a> Eq for ExpressionBuilderNode<'a> {}

impl <'a> PartialEq for ExpressionBuilderNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.priority() == other.priority() && self.par == other.par
    }
}

impl <'a> Ord for ExpressionBuilderNode<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a> PartialOrd for ExpressionBuilderNode<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let par_cmp = other.par.cmp(&self.par);
        if par_cmp.is_eq() {
            self.priority().partial_cmp(&other.priority())
        } else {
            Some(par_cmp)
        }
    }
}

pub(crate) struct ExpressionBuilder <'a> {
    root : *mut ExpressionBuilderNode<'a>,
    open_paren : Vec<usize>,
    prev : *mut ExpressionBuilderNode<'a>
}

impl<'a> ExpressionBuilder <'a> {
    pub(crate) fn new() -> ExpressionBuilder<'a> {
        let root = Box::into_raw(Box::new( ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::SingleOperator(SingleOperator::Pass),
            par : 0,
            parent : std::ptr::null_mut(),
            first_child : std::ptr::null_mut(),
            second_child: std::ptr::null_mut()
        }));
        ExpressionBuilder {
            root,
            open_paren : Vec::new(),
            prev : root
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        return self.open_paren.is_empty()
    }

    pub(crate) fn into_expression(self, expressions : &mut Vec<Expression<'a>>) {
        let mut res : usize = 0;
        let val = unsafe { // Root never changes, is always valid
            if (*self.root).second_child.is_null() {
                panic!("No expression created");
            }
            (*self.root).second_child
        };
        let mut stack = vec![(val, &mut res as *mut usize)];
        while let Some((top, dest)) = stack.pop() {
            let mut top = unsafe {Box::from_raw(top)}; // Tree is valid
            match &mut top.expression_type {
                ExpressionBuilderType::Atom(e) => {
                    unsafe {*dest = expressions.len()};
                    expressions.push(std::mem::replace(e, Expression::None));
                    drop(top);
                },
                ExpressionBuilderType::SingleOperator(s) => {
                    let mut e = Expression::SingleOperator {
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
                    let mut e = Expression::Operator {
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
    }

    pub(crate) fn open_parentheses(&mut self) {
        self.open_paren.push(0);
    }

    pub(crate) fn close_parentheses(&mut self) -> Result<(), ()> {
        self.open_paren.pop().ok_or(())?;
        Ok(())
    }

    pub(crate) fn add_atom(&mut self, expr : Expression<'a>) {
        let atom = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type: ExpressionBuilderType::Atom(expr),
            par: self.open_paren.len(),
            parent: self.prev,
            first_child: std::ptr::null_mut(),
            second_child: std::ptr::null_mut()
        }));
        unsafe {
            if let ExpressionBuilderType::Atom(_) = (*self.prev).expression_type {
                panic!("Multiple atoms in a row");
            }
            (*self.prev).second_child = atom;
        }
        self.prev = atom;
    }

    pub(crate) fn add_single_operator(&mut self, s : SingleOperator) {
        let node = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::SingleOperator(s),
            par : self.open_paren.len(),
            parent : self.prev,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut()
        }));
        unsafe {
            if let ExpressionBuilderType::Atom(_) = (*self.prev).expression_type {
                panic!("Single operator following atom");
            }
            (*self.prev).second_child = node;
        }
        self.prev = node;
    }

    pub(crate) fn add_dual_operator(&mut self, op : DualOperator) {
        let node = Box::into_raw(Box::new(ExpressionBuilderNode {
            expression_type : ExpressionBuilderType::DualOperator(op),
            par : self.open_paren.len(),
            parent : self.prev,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut()
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
        }
        self.prev = node;
    }
}

