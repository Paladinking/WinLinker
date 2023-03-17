use std::collections::HashMap;
use std::rc::Rc;
use bumpalo::Bump;
use crate::language::amd_win64::operation::IdTracker;
use super::instruction::InstructionCompiler;
use super::operation::{Operand, OperandSize, Operation, OperationType};
use super::registers::*;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::parser::{Expression, ExpressionData, Variable};
use crate::language::parser::statement::{IfStatement, Statement, StatementData};
use crate::language::types::Type;

// Represents one function
pub struct ProgramFrame {
    stack_size : usize,
    saved_registers : u64,
    binary : Vec<u8>,
}


pub enum OperationUnit <'a> {
    Operation(Operation<'a>), // Real operation
    EnterBlock(HashMap<usize, usize>),
    LeaveBlock
}

impl <'a> OperationUnit <'a> {
    fn operation(operator : OperationType, operands : Vec<&'a Operand>, dest : Option<&'a Operand>) -> OperationUnit {
        OperationUnit::Operation(Operation::new(
            operator, operands, dest
        ))
    }
}


pub struct InstructionBuilder <'a> {
    operations: Vec<OperationUnit<'a>>,
    invalidations : Vec<(usize, u64)>,
    operands : Vec<&'a Operand>,
    arena : &'a Bump,
    register_state : RegisterState,
    tracker : IdTracker,
    exit_code : usize
}

trait BlockCompiler<'a> {
    fn begin<'b>(&'b mut self, builder: &mut InstructionBuilder) -> std::slice::Iter<'a, StatementData>;

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder) -> Option<std::slice::Iter<'a, StatementData>>;
}

struct IfBlockBuilder<'a> {
    statements : &'a Vec<IfStatement>,
    index : usize
}

impl <'a> BlockCompiler<'a> for IfBlockBuilder<'a> {
    fn begin<'b>(&'b mut self, builder : &mut InstructionBuilder) -> std::slice::Iter<'a, StatementData> {
        self.statements[0].block.iter()
    }

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder) -> Option<std::slice::Iter<'a, StatementData>> {
        self.index += 1;
        if let Some(s) = self.statements.get(self.index) {
            Some(s.block.iter())
        } else {
            None
        }
    }
}

impl <'a> InstructionBuilder <'a> {
    pub fn new<'b> (arena : &'a Bump, vars: &'b HashMap<String, Rc<Variable>>) -> InstructionBuilder<'a> {
        let register_state = RegisterState::new();
        let mut tracker = IdTracker::new();
        let operands = vars.iter().enumerate().map(|(i, (_, v))|{
            v.id.replace(Some(i));
            &*arena.alloc(Operand::local(&mut tracker,OperandSize::from(v.var_type)))
        }).collect();
        let exit_code = vars.get("exit_code").unwrap().id.get().unwrap();
        InstructionBuilder {
            operations: Vec::new(), invalidations : Vec::new(),
            operands, arena, register_state, tracker, exit_code
        }
    }

    fn add_condition(&mut self, expr : &Vec<ExpressionData>, usages : &mut HashMap<usize, usize>) {
        let mut locations = Vec::with_capacity(expr.len());
        let mut stack = Vec::with_capacity(expr.len());
        stack.push(expr.last().unwrap());
        while let Some(expr) = stack.pop() {
            match expr.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(self.operands[id].clone());
                }
                Expression::Operator { .. } => {}
                Expression::SingleOperator { .. } => {}
                Expression::IntLiteral(_) => {}
                Expression::BoolLiteral(_) => {}
                Expression::None => {}
            }
        }
    }

    fn add_dual_operator(&mut self, locations : &mut Vec<&'a Operand>, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
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
            let dest =  self.arena.alloc(Operand::local(&mut self.tracker, size));
            let oper = self.arena.alloc(Operand::local(&mut self.tracker, size));
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
            let new = self.arena.alloc(Operand::local(&mut self.tracker, size));
            locations.push(new);
            self.operations.push(OperationUnit::operation(
                operator, vec![locations[first], locations[second]], Some(new)
            ));
        }
    }

    fn add_single_operator(&mut self, locations : &mut Vec<&'a Operand>, expr : usize, op : SingleOperator) {
        match op {
            SingleOperator::Not => {
                let new = self.arena.alloc(Operand::local(&mut self.tracker,OperandSize::BYTE));
                let new2 = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::BYTE));
                self.register_state.allocate_imm(new2, 1, OperandSize::BYTE);
                locations.push(new);
                self.operations.push(OperationUnit::operation(
                    OperationType::Xor,
                    vec![locations[expr], new2], Some(new))
                );
            }
            SingleOperator::Pass => unreachable!("Should not exist at this point.")
        }
    }

    fn add_assigment(&mut self, dest : &Variable, expr : &Vec<ExpressionData>, usages : &mut HashMap<usize, usize>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = self.operations.len();
        for e in expr.iter() {
            match &e.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(self.operands[id].clone());
                },
                &Expression::Operator { first, operator, second } => {
                    usages.insert(locations[first].id, self.operations.len());
                    usages.insert(locations[second].id, self.operations.len());
                    let size = OperandSize::from(e.t);
                    self.add_dual_operator(&mut locations, first, second, size, expr[first].t, operator);
                }
                &Expression::SingleOperator { operator, expr } => {
                    usages.insert(locations[expr].id, self.operations.len());
                    self.add_single_operator(&mut locations, expr, operator);
                }
                &Expression::IntLiteral(val) => {
                    let new = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::from(e.t)));
                    self.register_state.allocate_imm(new, val, OperandSize::from(e.t));
                    locations.push(new);
                }
                &Expression::BoolLiteral(b) => {
                    let new = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::BYTE));
                    self.register_state.allocate_imm(new, if b {1} else {0}, OperandSize::BYTE);
                    locations.push(new);
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
        let index = dest.id.get().unwrap();
        let new_dest = self.arena.alloc(
            Operand::local(&mut self.tracker, OperandSize::from(dest.var_type)));
        self.operands[index] = new_dest;
        if prev_len ==  self.operations.len() { // Whole expression was only a variable or immediate value.
            let operand = *locations.last().unwrap();
            usages.insert(operand.id, self.operations.len());
            let first = self.arena.alloc(
                Operand::local(&mut self.tracker,OperandSize::from(dest.var_type)));

            let instruction = OperationUnit::operation(OperationType::Mov,
                                             vec![first, operand], Some(new_dest));
            self.operations.push(instruction);
        } else if let Some(OperationUnit::Operation(instruction)) =  self.operations.last_mut() {
            instruction.dest = Some(new_dest);
        } else {
            unreachable!("An operation was added..");
        }
    }

    pub fn with(mut self, statements : &Vec<StatementData>) -> Self {
        let mut statement_stack = vec![(statements.iter(), HashMap::new())];
        let mut builder_stack = Vec::new();
        while let Some((iter, usage_map)) = statement_stack.last_mut() {
            if let Some(val) = iter.next() {
                match &val.statement {
                    Statement::Assignment { var, expr } => {
                        self.add_assigment(var, expr, usage_map);
                    }
                    Statement::IfBlock(if_statement) => {
                        let mut builder : Box<dyn BlockCompiler> = Box::new(IfBlockBuilder {
                            statements : if_statement, index : 0
                        });
                        statement_stack.push((builder.begin(&mut self), HashMap::new()));
                        builder_stack.push(builder);
                    }
                    Statement::Block(_) => todo!()
                }
            } else {
                let (_, usages) = statement_stack.pop().unwrap();
                if let Some(builder) = builder_stack.last_mut() {
                    let parent_map = &mut statement_stack.last_mut().unwrap().1;
                    for (k, v) in usages {
                        parent_map.insert(k, v);
                    }
                    if let Some(iter) = builder.end(&mut self) {
                        statement_stack.push((iter, HashMap::new()));
                    } else {
                        builder_stack.pop();
                    }
                }
            }

        }
        for statement in statements {
            match &statement.statement {
                Statement::Assignment { var, expr } => {
                    self.add_assigment(var, &expr, &mut HashMap::new());
                },
                _ => todo!()
            }
        }
        let exit = self.operands[self.exit_code];
        let out = self.arena.alloc(Operand::local(&mut self.tracker,
                                                  exit.size));
        //exit.add_use(self.operations.len());
        self.operations.push(OperationUnit::operation(OperationType::MovRet, vec![exit], Some(out)));
        self
    }

    // Allocate hints for what registers should be used
    // Allows e.g using rax if later instruction uses mul
    fn allocate_hints(&mut self) {
        for operation in self.operations.iter() {
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
        }
    }

    pub fn compile(mut self) {
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
            for (i, operand) in operation.operands.iter_mut().enumerate() {
                bitmap = operation.operator.next_bitmap(
                    bitmap, i, operand.size
                );

                if self.register_state.is_free(operand) {
                    self.register_state.allocate(*operand, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                }
                let mut allocation_bitmap = self.register_state.allocation_bitmap(operand);
                if (operand.used_after(index) && destroyed & (1 << i) != 0) || allocation_bitmap & bitmap == 0 {
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

                    if !operand.used_after(index) {
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
                if !operand.used_after(index) {
                    invalid_now &= !allocation_bitmap;
                }
                bitmap = allocation_bitmap;
            }

            self.register_state.invalidate_registers(invalid_now);
            let mut free = true;
            if let Some(dest) = operation.dest {
                if self.register_state.is_free(dest) {
                    operation.operands.first().unwrap().merge_into(dest);
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
                if !operand.used_after(index) {
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
    }
}