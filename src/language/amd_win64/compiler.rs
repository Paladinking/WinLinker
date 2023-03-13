use std::collections::HashMap;
use std::rc::Rc;
use bumpalo::Bump;
use super::instruction::InstructionCompiler;
use super::operation::{Operand, OperandSize, Operation, OperationType};
use super::registers::*;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::parser::{Expression, ExpressionData, Statement, StatementData, Variable};
use crate::language::types::Type;

// Represents one function
pub struct ProgramFrame {
    stack_size : usize,
    saved_registers : u64,
    binary : Vec<u8>,
}


pub struct InstructionBuilder <'a> {
    operations: Vec<Operation<'a>>,
    invalidations : Vec<(usize, u64)>,
    operands : Vec<&'a Operand>,
    arena : &'a Bump,
    register_state : RegisterState,
    exit_code : usize
}

impl <'a> InstructionBuilder <'a> {
    pub fn new<'b> (arena : &'a Bump, vars: &'b HashMap<String, Rc<Variable>>) -> InstructionBuilder<'a> {
        let register_state = RegisterState::new();
        let operands = vars.iter().enumerate().map(|(i, (_, v))|{
            v.id.replace(Some(i));
            &*arena.alloc(Operand::local(OperandSize::from(v.var_type)))
        }).collect();
        let exit_code = vars.get("exit_code").unwrap().id.get().unwrap();
        InstructionBuilder {
            operations: Vec::new(), invalidations : Vec::new(),
            operands, arena, register_state, exit_code
        }
    }

    fn add_dual_operator(&mut self, locations : &mut Vec<&'a Operand>, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
        if op.is_cmp() {
            self.operations.push(Operation::new(OperationType::Cmp,
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
            let dest =  self.arena.alloc(Operand::local(size));
            let oper = self.arena.alloc(Operand::local(size));
            locations.push(dest);
            self.operations.push(Operation::new(basic_operator, vec![oper], Some(dest)));
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
            let new = self.arena.alloc(Operand::local(size));
            locations.push(new);
            self.operations.push(Operation::new(
                operator, vec![locations[first], locations[second]], Some(new)
            ));
        }
    }

    fn add_single_operator(&mut self, locations : &mut Vec<&'a Operand>, expr : usize, op : SingleOperator) {
        match op {
            SingleOperator::Not => {
                let new = self.arena.alloc(Operand::local(OperandSize::BYTE));
                let new2 = self.arena.alloc(Operand::new(0, OperandSize::BYTE));
                self.register_state.allocate_imm(new2, 1, OperandSize::BYTE);
                locations.push(new);
                self.operations.push(Operation::new(
                    OperationType::Xor,
                    vec![locations[expr], new2], Some(new))
                );
            }
            SingleOperator::Pass => unreachable!("Should not exist at this point.")
        }
    }

    fn add_assigment(&mut self, dest : &Variable, expr : &Vec<ExpressionData>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = self.operations.len();
        for e in expr.iter() {
            match &e.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(self.operands[id].clone());
                },
                Expression::Operator { first, operator, second } => {
                    locations[*first].add_use(self.operations.len());
                    locations[*second].add_use(self.operations.len());
                    let size = OperandSize::from(e.t);
                    self.add_dual_operator(&mut locations, *first, *second, size, expr[*first].t, *operator);
                }
                Expression::SingleOperator { operator, expr } => {
                    locations[*expr].add_use(self.operations.len());
                    self.add_single_operator(&mut locations, *expr, *operator);
                }
                Expression::IntLiteral(val) => {
                    let new = self.arena.alloc(Operand::new(0, OperandSize::from(e.t)));
                    self.register_state.allocate_imm(new, *val, OperandSize::from(e.t));
                    locations.push(new);
                }
                Expression::BoolLiteral(b) => {
                    let new = self.arena.alloc(Operand::new(0, OperandSize::BYTE));
                    self.register_state.allocate_imm(new, if *b {1} else {0}, OperandSize::BYTE);
                    locations.push(new);
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
        let index = dest.id.get().unwrap();
        let new_dest = self.arena.alloc(Operand::local(OperandSize::from(dest.var_type)));
        self.operands[index] = new_dest;
        if prev_len ==  self.operations.len() { // Whole expression was only a variable or immediate value.
            let operand = *locations.last().unwrap();
            operand.add_use(self.operations.len());
            let first = self.arena.alloc(Operand::local(OperandSize::from(dest.var_type)));

            let instruction = Operation::new(OperationType::Mov,
                                             vec![first, operand], Some(new_dest));
            self.operations.push(instruction);
        } else if let Some(instruction) =  self.operations.last_mut() {
            instruction.dest = Some(new_dest);
        }
    }

    pub fn with(mut self, statements : &Vec<StatementData>) -> Self {
        for statement in statements {
            match &statement.statement { Statement::Assignment { var, expr } => {
                self.add_assigment(var, &expr);
            }}
        }
        let exit = self.operands[self.exit_code];
        let out = self.arena.alloc(Operand::local(exit.size));
        exit.add_use(self.operations.len());
        self.operations.push(Operation::new(OperationType::MovRet, vec![exit], Some(out)));
        self
    }

    // Allocate hints for what registers should be used
    // Allows e.g using rax if later instruction uses mul
    fn allocate_hints(&mut self) {
        for instruction in self.operations.iter() {
            for (i, &operand) in instruction.operands.iter().enumerate() {
                let hint = instruction.operator.bitmap_hint(i, operand.size);
                self.register_state.allocate_hint(operand, hint);
            }
            // Propagate the hint to dest to allow combining with future instructions
            if let (Some(dest), &first) = (instruction.dest, instruction.operands.first().unwrap()) {
                self.register_state.propagate_hint(first, dest);
            }
        }
    }

    pub fn compile(mut self) {
        self.allocate_hints();

        let mut invalidation = 0;
        let mut used_stable = 0_u64;

        for (index, operation) in self.operations.iter_mut().enumerate() {
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
                self.invalidations[invalidation..].iter()
                    .take_while(|(i, _)| dest.used_after(*i))
                    .fold(0, |prev, (_, map)| *map | prev)
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
                    let new = self.arena.alloc(Operand::local(operand.size));
                    let location = self.register_state.allocate(new, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                    // Allocating might take the previous register and move the original value
                    // In that case no move is needed.
                    if location != allocation_bitmap {
                        println!("Mov0 {}, {}", self.register_state.to_string(new.id.get()), self.register_state.to_string(operand.id.get()));
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
                print!(" {}", self.register_state.to_string(o.id.get()));
            }
            println!();
            self.register_state.build_instruction(operation.operator, &operation.operands);

            if let Some(dest) = operation.dest {
                if !free {
                    let first = *operation.operands.first().unwrap();
                    println!("Mov1 {}, {}", self.register_state.to_string(dest.id.get()), self.register_state.to_string(first.id.get()));
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