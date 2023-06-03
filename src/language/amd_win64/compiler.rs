use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::rc::Rc;
use bumpalo::Bump;
use crate::language::amd_win64::operation::IdTracker;
use crate::language::amd_win64::usages::UsageTracker;
use super::instruction::InstructionCompiler;
use super::operation::{Operand, OperandSize, Operation, OperationType};
use super::registers::*;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::parser::{Expression, ExpressionData, Variable};
use crate::language::parser::statement::{IfStatement, Statement, StatementData};
use crate::language::types::Type;

pub enum OperationUnit {
    Operation(Operation), // Real operation
    EnterBlock(HashSet<usize>),
    LeaveBlock
}

impl OperationUnit {
    fn operation(operator : OperationType, operands : Vec<usize>, dest : Option<usize>) -> OperationUnit {
        OperationUnit::Operation(Operation::new(
            operator, operands, dest
        ))
    }
}

trait BlockCompiler<'a> {
    fn begin<'b>(&'b mut self, builder: &mut InstructionBuilder, usages : &mut UsageTracker) -> std::slice::Iter<'a, StatementData>;

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder, usages : &mut UsageTracker) -> Option<std::slice::Iter<'a, StatementData>>;
}

struct IfBlockBuilder<'a> {
    statements : &'a Vec<IfStatement>,
    block_ids : Vec<usize>,
    index : usize
}

impl <'a> IfBlockBuilder<'a> {
    fn new(builder : &mut InstructionBuilder, statements : &'a Vec<IfStatement>) -> Self {
        IfBlockBuilder {
            statements,
            block_ids : statements.iter().map(|_| builder.tracker.get_id()).collect(),
            index : 0
        }
    }
}

impl <'a> BlockCompiler<'a> for IfBlockBuilder<'a> {
    fn begin<'b>(&'b mut self, builder : &mut InstructionBuilder, usages : &mut UsageTracker) -> std::slice::Iter<'a, StatementData> {
        if let Some(condition) = &self.statements[0].condition {
            builder.add_condition(usages, condition);
        }
        usages.enter_scope(self.block_ids[0]);
        self.statements[0].block.iter()
    }

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder, usages : &mut UsageTracker) -> Option<std::slice::Iter<'a, StatementData>> {
        self.index += 1;
        if let Some(s) = self.statements.get(self.index) {
            usages.alt_scope(self.block_ids[self.index]);
            Some(s.block.iter())
        } else {
            usages.leave_scope();
            None
        }
    }
}

pub struct InstructionBuilder <'a> {
    operations: Vec<OperationUnit>,
    invalidations : Vec<(usize, u64)>,

    operands : Vec<Operand>,
    arena : &'a Bump,
    register_state : RegisterState,
    tracker : IdTracker,
    exit_code : usize
}

impl <'a> InstructionBuilder <'a> {
    pub fn new<'b> (arena : &'a Bump, vars: &'b HashMap<String, Rc<Variable>>) -> InstructionBuilder<'a> {
        let register_state = RegisterState::new();
        let mut tracker = IdTracker::new();
        let operands = vars.iter().enumerate().map(|(i, (n, v))|{
            println!("{} : {}", i, n);
            v.id.replace(Some(i));
            Operand::new(OperandSize::from(v.var_type))
        }).collect();
        let exit_code = vars.get("exit_code").unwrap().id.get().unwrap();
        InstructionBuilder {
            operations: Vec::new(), invalidations : Vec::new(),
            operands, arena, register_state, tracker, exit_code
        }
    }

    fn create_operand(&mut self, size : OperandSize) -> usize {
        self.operands.push(Operand::new(size));
        return self.operands.len() - 1;
    }

    fn add_condition(&mut self, usages : &mut UsageTracker, expr : &Vec<ExpressionData>) {
        todo!();

    }

    fn add_dual_operator(&mut self, locations : &mut Vec<usize>, usages : &mut UsageTracker, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
        usages.add_usage(&mut self.operands, locations[first], self.operations.len(), true);
        usages.add_usage(&mut self.operands, locations[second], self.operations.len(), true);
        let dest;
        if op.is_cmp() {

            self.operations.push(OperationUnit::operation(OperationType::Cmp,
                                                vec![locations[first], locations[second]],
                                                None));
            let basic_operator = match op {
                DualOperator::Equal => OperationType::SetE,
                DualOperator::NotEqual => OperationType::SetNe,
                DualOperator::GreaterEqual => if t.is_signed() { OperationType::SetGE } else { OperationType::SetAE},
                DualOperator::LesserEqual => if t.is_signed() { OperationType::SetLE} else { OperationType::SetBE},
                DualOperator::Greater => if t.is_signed() { OperationType::SetG } else { OperationType::SetA},
                DualOperator::Lesser => if t.is_signed() { OperationType::SetL } else { OperationType::SetB},
                _ => unreachable!("Not is_cmp")
            };
            dest = self.create_operand(size);
            // Allocation assumes all instructions have operand1 = dest
            // This means an unused operand needs to be allocated to represent
            //  the first operand. The allocation will merge this with dest since it will be unallocated.
            let oper = self.create_operand(size);
            usages.add_usage(&mut self.operands, oper, self.operations.len(), true);
            locations.push(dest);
            self.operations.push(OperationUnit::operation(basic_operator, vec![oper], Some(dest)));
        } else {
            let operator = match op {
                DualOperator::Divide => {
                    self.invalidations.push((self.operations.len(), RDX | RAX));
                    if t.is_signed() { OperationType::IDiv } else { OperationType::Div }
                },
                DualOperator::Multiply => if t.is_signed() {
                    if size == OperandSize::BYTE {
                        self.invalidations.push((self.operations.len(), RAX));
                    }
                    OperationType::IMul
                } else {
                    if size == OperandSize::BYTE {
                        self.invalidations.push((self.operations.len(), RAX));
                    } else {
                        self.invalidations.push((self.operations.len(), RDX | RAX));
                    }

                    OperationType::Mul
                },
                DualOperator::Minus => OperationType::Sub,
                DualOperator::Plus => OperationType::Add,
                DualOperator::BoolAnd => OperationType::And,
                DualOperator::BoolOr => OperationType::Or,
                _ => unreachable!("Covered by is_cmp")
            };
            dest = self.create_operand(size);
            locations.push(dest);
            self.operations.push(OperationUnit::operation(
                operator, vec![locations[first], locations[second]], Some(dest)
            ));
        }
        usages.add_usage(&mut self.operands, dest, self.operations.len() - 1, false);
    }

    fn add_single_operator(&mut self, locations : &mut Vec<usize>, usages : &mut UsageTracker, expr : usize, op : SingleOperator) {
        usages.add_usage(&mut self.operands, locations[expr], self.operations.len(), true);
        let dest;
        match op {
            SingleOperator::Not => {
                dest = self.create_operand(OperandSize::BYTE);
                let imm = self.create_operand(OperandSize::BYTE);
                usages.add_usage(&mut self.operands, imm, self.operations.len(), true);
                self.register_state.allocate_imm(&self.operands[imm], 1, OperandSize::BYTE);
                locations.push(dest);
                self.operations.push(OperationUnit::operation(
                    OperationType::Xor,
                    vec![locations[expr], imm], Some(dest))
                );
            }
            SingleOperator::Pass => unreachable!("Should not exist at this point.")
        }
        usages.add_usage(&mut self.operands, dest, self.operations.len() - 1, false);
    }

    fn add_assigment(&mut self, usages : &mut UsageTracker, dest : &Variable, expr : &Vec<ExpressionData>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = self.operations.len();
        for e in expr.iter() {
            match &e.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(id);
                },
                &Expression::Operator { first, operator, second } => {
                    let size = OperandSize::from(e.t);
                    self.add_dual_operator(&mut locations, usages, first, second, size, expr[first].t, operator);
                }
                &Expression::SingleOperator { operator, expr } => {
                    self.add_single_operator(&mut locations, usages, expr, operator);
                }
                &Expression::IntLiteral(val) => {
                    let literal = self.create_operand(OperandSize::from(e.t));
                    println!("{} : int({})", literal, val);
                    self.register_state.allocate_imm(&mut self.operands[literal],
                                                     val, OperandSize::from(e.t));
                    locations.push(literal);
                }
                &Expression::BoolLiteral(b) => {
                    let literal = self.create_operand(OperandSize::BYTE);
                    println!("{} : bool({})", literal, b);
                    self.register_state.allocate_imm(&mut self.operands[literal],
                                                     if b {1} else {0}, OperandSize::BYTE);
                    locations.push(literal);
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
        let dest_index = dest.id.get().unwrap();
        if prev_len ==  self.operations.len() { // Whole expression was only a variable or immediate value.
            let operand = *locations.last().unwrap();
            let first = self.create_operand(OperandSize::from(dest.var_type));
            usages.add_usage(&mut self.operands, operand, self.operations.len(), true);
            usages.add_usage(&mut self.operands, first, self.operations.len(), true);
            let instruction = OperationUnit::operation(OperationType::Mov,
                                             vec![first, operand], Some(dest_index));
            self.operations.push(instruction);
        } else if let Some(OperationUnit::Operation(instruction)) =  self.operations.last_mut() {
            instruction.dest = Some(dest_index);
        } else {
            unreachable!("An operation was added..");
        }
        usages.add_usage(&mut self.operands, dest_index, self.operations.len(), false);
    }

    pub fn with(mut self, statements : &Vec<StatementData>) -> Self {
        let mut usage_tracker = UsageTracker::new();
        let mut statement_stack = vec![statements.iter()];
        let mut builder_stack = Vec::new();
        let outer_scope_id = self.tracker.get_id();
        usage_tracker.enter_scope(outer_scope_id);
        while let Some(iter) = statement_stack.last_mut() {
            if let Some(val) = iter.next() {
                match &val.statement {
                    Statement::Assignment { var, expr } => {
                        self.add_assigment(&mut usage_tracker,var, expr);
                    }
                    Statement::IfBlock(if_statement) => {
                        let mut builder : Box<dyn BlockCompiler> = Box::new(
                            IfBlockBuilder::new(&mut self, if_statement)
                        );
                        statement_stack.push(builder.begin(&mut self, &mut usage_tracker));
                        builder_stack.push(builder);
                    }
                    Statement::Block(_) => todo!()
                }
            } else {
                statement_stack.pop();
                if let Some(builder) = builder_stack.last_mut() {
                    if let Some(iter) = builder.end(&mut self, &mut usage_tracker) {
                        statement_stack.push(iter);
                    } else {
                        builder_stack.pop();
                    }
                } else {
                    break;
                }
            }
        }
        let exit = &self.operands[self.exit_code];
        let out = self.create_operand(exit.size);
        usage_tracker.add_usage(&mut self.operands, self.exit_code, self.operations.len(), true);
        usage_tracker.add_usage(&mut self.operands, out, self.operations.len(), false);
        self.operations.push(OperationUnit::operation(OperationType::MovRet,
                                                      vec![self.exit_code], Some(out)));
        usage_tracker.leave_scope();
        usage_tracker.finalize(&mut self.operands);
        self
    }

    // Allocate hints for what registers should be used
    // Allows e.g using rax if later instruction uses mul
    fn allocate_hints(&mut self) {
        /*for operation in self.operations.iter() {
            let operation = if let OperationUnit::Operation(operation) = operation {
                operation
            } else {
                panic!("Not yet implemented");
            };
            for (i, &operand) in operation.operands.iter().enumerate() {
                let hint = operation.operator.bitmap_hint(i, operand.size);
                self.register_state.allocate_hint(operand, hint);
            }
            // Propagate the hint to dest to allow combining with future instructions
            if let (Some(dest), &first) = (operation.dest, operation.operands.first().unwrap()) {
                self.register_state.propagate_hint(first, dest);
            }
        }*/
    }
    pub fn compile(mut self) {
        for (index, operation) in self.operations.iter().enumerate() {
            if let OperationUnit::Operation(o) = operation {
                print!("{:?}", o.operator);
                for &op in &o.operands {
                    let used = self.operands[op].free_usages.contains(&index);
                    print!(" {}:{}",op, used);
                }
                if let Some(op) = o.dest {
                    let used = self.operands[op].free_usages.contains(&index);
                    print!(" {}:{}",op, used);
                }
                println!();
            }
        }
    }
/*
    pub fn compile(mut self) {
        fn used_after(scope : usize, index : usize, usages : &HashMap<usize, Vec<(usize, usize)>>, operand : &Operand) -> bool {
            if let Some(vec) = usages.get(&operand.id) {
                if let Some(&(_, row)) = vec.iter().find(|(s, _)| *s == scope) {
                    row > index
                } else {
                    true
                }
            } else {
                false
            }
        }

        self.allocate_hints();

        let mut invalidation = 0;
        let mut used_stable = 0_u64;

        for (index, operation) in self.operations.iter_mut().enumerate() {
            let operation = if let OperationUnit::Operation(operation) = operation {
                operation
            } else {
                panic!("Not yet implemented");
            };
            // Get bitmap of all registers invalidated by this instruction
            let mut invalid_now = 0;
            if let Some((i, map)) = self.invalidations.get(invalidation) {
                if *i == index {
                    invalid_now = *map;
                    invalidation += 1;
                }
            }

            // Get bitmap of registers that will be invalidated while dest is still needed.
            let mut invalid_soon = if let Some(dest) = operation.dest {
                //self.invalidations[invalidation..].iter()
                //    .take_while(|(i, _)| dest.used_after(*i))
                //    .fold(0, |prev, (_, map)| *map | prev)
                0
            } else {
                0
            };

            let mut bitmap = 1;
            let destroyed = operation.operator.destroyed();
            println!("{:?} : index {}", operation.operator, index);
            for (i, operand) in operation.operands.iter_mut().enumerate() {
                println!("i: {}", operand.id);
                bitmap = operation.operator.next_bitmap(
                    bitmap, i, operand.size
                );

                if self.register_state.is_free(operand) {
                    self.register_state.allocate(*operand, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                }
                let mut allocation_bitmap = self.register_state.allocation_bitmap(operand);
                if (used_after(0, index, &self.usages, operand) && destroyed & (1 << i) != 0) || allocation_bitmap & bitmap == 0 {
                    let new = self.arena.alloc(
                        Operand::local(&mut self.tracker,operand.size));
                    let location = self.register_state.allocate(new, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                    // Allocating might take the previous register and move the original value
                    // In that case no move is needed.
                    if location != allocation_bitmap {
                        println!("Mov0 {}, {}", self.register_state.to_string(new.allocation.get()), self.register_state.to_string(operand.allocation.get()));
                        self.register_state.build_instruction(OperationType::Mov, &[new, operand]);
                    }
                    allocation_bitmap = location;

                    if !used_after(0, index, &self.usages, operand) {
                        self.register_state.free(operand);
                    }
                    *operand = new;
                }
                if destroyed & (1 << i) != 0 {
                    used_stable |= allocation_bitmap & NON_VOL_GEN_REG
                }
                // Only the first operand uses invalid_soon, since that is the destination register
                invalid_soon = 0;
                // If this operand is going to be freed there is no need to invalidate it.
                // It cannot be freed until after invalidation is done since otherwise
                //  they might be overridden while saving needed invalidated registers.
                if !used_after(0, index, &self.usages, operand){
                    invalid_now &= !allocation_bitmap;
                }
                bitmap = allocation_bitmap;
            }
            self.register_state.invalidate_registers(invalid_now);
            let mut free = true;
            if let Some(dest) = operation.dest {
                if self.register_state.is_free(dest) {
                    let first = *operation.operands.first().unwrap();
                    first.merge_into(dest);
                    self.usages.insert(first.id, self.usages[&dest.id].clone());
                } else {
                    used_stable |= self.register_state.allocation_bitmap(dest) & NON_VOL_GEN_REG;
                    free = false;
                }
            }
            print!("{:?}", operation.operator);
            for o in &operation.operands {
                print!(" {}", self.register_state.to_string(o.allocation.get()));
            }
            println!();
            self.register_state.build_instruction(operation.operator, &operation.operands);

            if let Some(dest) = operation.dest {
                if !free {
                    let first = *operation.operands.first().unwrap();
                    println!("Mov1 {}, {}", self.register_state.to_string(dest.allocation.get()), self.register_state.to_string(first.allocation.get()));
                    self.register_state.build_instruction(OperationType::Mov, &[dest, first]);
                }
            }
            for &operand in &operation.operands {
                if !used_after(0, index, &self.usages, operand) {
                    self.register_state.free(operand);
                }
            }
        }

        let compiler = InstructionCompiler::new();
        let mut res = Vec::new();
        for instruction in &self.register_state.output {
            compiler.compile_instruction(instruction, &mut res);
        }
        std::fs::write("out.bin", &res).unwrap();

        println!();
    }*/
}