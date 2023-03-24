use std::collections::{HashMap, HashSet, VecDeque};
use std::io::Write;
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

pub enum OperationUnit <'a> {
    Operation(Operation<'a>), // Real operation
    EnterBlock(usize),
    LeaveBlock
}

impl <'a> OperationUnit <'a> {
    fn operation(operator : OperationType, operands : Vec<&'a Operand>, dest : Option<&'a Operand>) -> OperationUnit<'a> {
        OperationUnit::Operation(Operation::new(
            operator, operands, dest
        ))
    }
}

trait BlockCompiler<'a> {
    fn begin<'b>(&'b mut self, builder: &mut InstructionBuilder<'a>) -> std::slice::Iter<'a, StatementData>;

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder<'a>) -> Option<std::slice::Iter<'a, StatementData>>;
}

struct IfBlockBuilder<'a> {
    statements : &'a Vec<IfStatement>,
    block_ids : Vec<usize>,
    label_queue : Vec<(&'a Operand, usize)>,
    label_locations : Vec<Option<usize>>,
    index : usize
}

impl <'a> IfBlockBuilder<'a> {
    fn new(builder : &mut InstructionBuilder, statements : &'a Vec<IfStatement>) -> Self {
        IfBlockBuilder {
            statements,
            block_ids : statements.iter().map(|_| builder.tracker.get_id()).collect(),
            label_queue : Vec::new(),
            label_locations : vec![None; (statements.len() * 2) + 1], // All conditions start and end + after all blocks
            index : 0
        }
    }

    // Returns the location of the current blocks else clause in the label_locations vec.
    fn else_location(&self) -> usize {
       self.index * 2 + 2
    }

    // Returns the location of the current block (after condition) in the label_locations vec.
    fn block_location(&self) -> usize {
        self.index * 2 + 1
    }

    fn add_condition<'b>(&'b mut self, builder : &mut InstructionBuilder<'a>) {
        if let Some(expr) = &self.statements[self.index].condition {
            let mut location_offset = self.label_locations.len();
            self.label_locations.resize(location_offset + expr.len(), None);
            let mut stack = vec![(expr.len() - 1, None, Some(self.else_location()), self.block_location())];
            while let Some((index, true_location, false_location, after)) = stack.pop() {
                match &expr[index].expression {
                    Expression::Operator { operator : DualOperator::BoolAnd, first, second} => {
                        stack.push((*second, true_location, false_location, after));
                        stack.push((*first, None, Some(false_location.unwrap_or(after)), location_offset + *second));

                    },
                    Expression::Operator {operator : DualOperator::BoolOr, first, second} => {
                        stack.push((*second, true_location, false_location, after));
                        stack.push((*first, Some(true_location.unwrap_or(after)), None, location_offset + *second));
                    },
                    Expression::SingleOperator {operator : SingleOperator::Not, expr} => {
                        stack.push((*expr, false_location, true_location, after));
                    },
                    e => {
                        self.label_locations[index + location_offset] = Some(builder.operations.len());
                        let jmp = match e {
                            Expression::Variable(v) => {
                                let im = builder.arena.alloc(Operand::new(&mut builder.tracker, OperandSize::from(v.var_type)));
                                builder.register_state.allocate_imm(im, 0, OperandSize::from(v.var_type));
                                builder.usages.insert(im.id, VecDeque::from([Usage::Free]));
                                builder.add_usage(builder.operands[v.id.get().unwrap()]);
                                builder.operations.push(OperationUnit::operation(
                                    OperationType::Cmp, vec![builder.operands[v.id.get().unwrap()], &*im], None
                                ));
                                OperationType::JmpE
                            }
                            Expression::Operator { operator, first, second} => {
                                let mut locations = Vec::new();
                                let mut pos = *first;
                                loop {
                                    match &expr[pos].expression {
                                        Expression::Operator { first, .. } => pos = *first,
                                        Expression::SingleOperator { expr, .. } => pos = *expr,
                                        _ => break
                                    }
                                }
                                builder.add_expressions(&expr[pos..index], &mut locations, pos);
                                builder.add_usage(locations[*first - pos]);
                                builder.add_usage(locations[*second - pos]);
                                let singed = || expr[*first].t.is_signed();
                                builder.operations.push(OperationUnit::operation(
                                    OperationType::Cmp, vec![locations[*first - pos], locations[*second - pos]], None
                                ));
                                match operator {
                                    DualOperator::Equal => OperationType::JmpE,
                                    DualOperator::NotEqual => OperationType::JmpNE,
                                    DualOperator::GreaterEqual => if singed() {OperationType::JmpGE} else {OperationType::JmpAE},
                                    DualOperator::LesserEqual => if singed() {OperationType::JmpLE} else {OperationType::JmpBE},
                                    DualOperator::Greater => if singed() {OperationType::JmpG} else {OperationType::JmpA},
                                    DualOperator::Lesser => if singed() {OperationType::JmpL} else {OperationType::JmpB},
                                    _ => panic!("Not a boolean operator")
                                }
                            },
                            Expression::BoolLiteral(b) => {
                                if *b {
                                    OperationType::Jmp
                                } else {
                                    OperationType::JmpNop
                                }
                            }
                            _ => panic!("Non boolean condition")
                        };
                        let jmp_dest = builder.arena.alloc(Operand::new(&mut builder.tracker, OperandSize::QWORD));
                        builder.usages.insert(jmp_dest.id, VecDeque::from([Usage::Free]));
                        if let Some(location) = true_location {
                            self.label_queue.push((jmp_dest, location));
                            builder.operations.push(OperationUnit::operation(
                                jmp, vec![jmp_dest], None
                            ));
                            if let Some(location) = false_location {
                                let jmp_dest = builder.arena.alloc(Operand::new(&mut builder.tracker, OperandSize::QWORD));
                                builder.usages.insert(jmp_dest.id, VecDeque::from([Usage::Free]));
                                self.label_queue.push((jmp_dest, location));
                                builder.operations.push(OperationUnit::operation(
                                    jmp.inverse(), vec![jmp_dest], None
                                ));
                            }
                        } else {
                            let location = false_location.unwrap();
                            self.label_queue.push((jmp_dest, location));
                            builder.operations.push(OperationUnit::operation(
                                jmp.inverse(), vec![jmp_dest], None
                            ));
                        }
                    }
                }
            }
        }
    }
}

impl <'a> BlockCompiler<'a> for IfBlockBuilder<'a> {
    fn begin<'b>(&'b mut self, builder : &mut InstructionBuilder<'a>) -> std::slice::Iter<'a, StatementData> {
        self.label_locations[0] = Some(builder.operations.len());
        self.add_condition(builder);
        self.label_locations[1] = Some(builder.operations.len() + 1); // + 1 to skip EnterBlock
        builder.operations.push(OperationUnit::EnterBlock(self.block_ids[0]));
        self.statements[0].block.iter()
    }

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder<'a>) -> Option<std::slice::Iter<'a, StatementData>> {
        builder.operations.push(OperationUnit::LeaveBlock);
        self.index += 1;
        if let Some(s) = self.statements.get(self.index) {
            let jmp_dest = builder.arena.alloc(Operand::new(&mut builder.tracker, OperandSize::QWORD));
            builder.usages.insert(jmp_dest.id, VecDeque::from([Usage::Free]));
            self.label_queue.push((jmp_dest, self.statements.len() * 2));
            builder.operations.push(OperationUnit::operation(
               OperationType::Jmp, vec![&*jmp_dest], None
            ));

            self.label_locations[self.index * 2].replace(builder.operations.len());
            self.add_condition(builder);
            self.label_locations[self.index * 2 + 1].replace(builder.operations.len() + 1); // + 1 to skip EnterBlock
            builder.operations.push(OperationUnit::EnterBlock(self.block_ids[self.index]));
            Some(s.block.iter())
        } else {
            self.label_locations[self.statements.len() * 2].replace(builder.operations.len());
            for &(o, index) in &self.label_queue {
                let addr = self.label_locations[index].unwrap();
                builder.register_state.allocate_addr(o, addr);
            }
            None
        }
    }
}

// Struct used for changing an address to the real one.
// Important that the address is the last part of instruction.
pub struct AddressRelocation {
    pub index : usize, // Location of address in output bytes
    pub target : usize, // Location of target in the address list
    pub size : OperandSize // Size of the address
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Usage {
    Use, // Regular usage
    Free, // Last usage, operand can be permanently freed
    TempFree,  // Temporary last usage, restore will come later
    Restore // Need to get allocation before TempFree again
}

pub struct InstructionBuilder <'a> {
    operations: Vec<OperationUnit<'a>>,
    invalidations : Vec<(usize, u64)>,
    // Usages map, operand id to vec of (scope_id, operations_index).
    usages : HashMap<usize, VecDeque<Usage>>,
    operands : Vec<&'a Operand>,
    arena : &'a Bump,
    register_state : RegisterState,
    tracker : IdTracker,
    exit_code : usize
}

impl <'a> InstructionBuilder <'a> {
    pub fn new<'b> (arena : &'a Bump, vars: &'b HashMap<String, Rc<Variable>>) -> InstructionBuilder<'a> {
        let register_state = RegisterState::new();
        let mut tracker = IdTracker::new();
        let operands = vars.iter().enumerate().map(|(i, (_, v))|{
            v.id.replace(Some(i));
            &*arena.alloc(Operand::new(&mut tracker,OperandSize::from(v.var_type)))
        }).collect();
        let exit_code = vars.get("exit_code").unwrap().id.get().unwrap();
        let mut builder = InstructionBuilder {
            operations: Vec::new(), invalidations : Vec::new(), usages : HashMap::new(),
            operands, arena, register_state, tracker, exit_code
        };
        for operand in &builder.operands {
            builder.usages.insert(operand.id, VecDeque::new());
        }
        builder
    }

    fn add_dual_operator(&mut self, locations : &mut Vec<&'a Operand>, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
        if op.is_cmp() {
            self.operations.push(OperationUnit::operation(OperationType::Cmp,
                                                vec![locations[first], locations[second]],
                                                None));
            let basic_operator = match op {
                DualOperator::Equal => OperationType::SetE,
                DualOperator::NotEqual => OperationType::SetNE,
                DualOperator::GreaterEqual => if t.is_signed() { OperationType::SetGE } else { OperationType::SetAE},
                DualOperator::LesserEqual => if t.is_signed() { OperationType::SetLE} else { OperationType::SetBE},
                DualOperator::Greater => if t.is_signed() { OperationType::SetG } else { OperationType::SetA},
                DualOperator::Lesser => if t.is_signed() { OperationType::SetL } else { OperationType::SetB},
                _ => unreachable!("Not is_cmp")
            };
            let dest =  self.arena.alloc(Operand::new(&mut self.tracker, size));
            let oper = self.arena.alloc(Operand::new(&mut self.tracker, size));
            self.usages.insert(dest.id, VecDeque::from([Usage::Free]));
            self.usages.insert(oper.id, VecDeque::from([Usage::Free]));
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
            let new = self.arena.alloc(Operand::new(&mut self.tracker, size));
            self.usages.insert(new.id, VecDeque::from([Usage::Free]));
            locations.push(new);
            self.operations.push(OperationUnit::operation(
                operator, vec![locations[first], locations[second]], Some(new)
            ));
        }
    }

    fn add_single_operator(&mut self, locations : &mut Vec<&'a Operand>, expr : usize, op : SingleOperator) {
        match op {
            SingleOperator::Not => {
                let new = self.arena.alloc(Operand::new(&mut self.tracker,OperandSize::BYTE));
                let new2 = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::BYTE));
                self.usages.insert(new.id, VecDeque::from([Usage::Free]));
                self.usages.insert(new2.id, VecDeque::from([Usage::Free]));
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

    fn add_expressions(&mut self, expr : &[ExpressionData], locations : &mut Vec<&'a Operand>, offset : usize) {
        for e in expr.iter() {
            match &e.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(self.operands[id].clone());
                },
                &Expression::Operator { first, operator, second } => {
                    self.add_usage(locations[first - offset]);
                    self.add_usage(locations[second - offset]);
                    let size = OperandSize::from(e.t);
                    self.add_dual_operator(locations, first - offset, second - offset, size, expr[first].t, operator);

                }
                &Expression::SingleOperator { operator, expr } => {
                    self.add_usage(locations[expr - offset]);
                    self.add_single_operator(locations, expr - offset, operator);
                }
                &Expression::IntLiteral(val) => {
                    let new = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::from(e.t)));
                    self.usages.insert(new.id, VecDeque::from([Usage::Free]));
                    self.register_state.allocate_imm(new, val, OperandSize::from(e.t));
                    locations.push(new);
                }
                &Expression::BoolLiteral(b) => {
                    let new = self.arena.alloc(Operand::new(&mut self.tracker, OperandSize::BYTE));
                    self.usages.insert(new.id, VecDeque::from([Usage::Free]));
                    self.register_state.allocate_imm(new, if b {1} else {0}, OperandSize::BYTE);
                    locations.push(new);
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
    }

    fn add_assigment(&mut self, dest : &Variable, expr : &Vec<ExpressionData>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = self.operations.len();
        self.add_expressions(&expr, &mut locations, 0);
        let index = dest.id.get().unwrap();
        let new_dest = self.operands[index];
        self.add_restore(new_dest);

        if prev_len ==  self.operations.len() { // Whole expression was only a variable or immediate value.
            let operand = *locations.last().unwrap();
            self.add_usage(operand);
            let first = self.arena.alloc(
                Operand::new(&mut self.tracker,OperandSize::from(dest.var_type)));
            let instruction = OperationUnit::operation(OperationType::Mov,
                                             vec![first, operand], Some(new_dest));
            self.usages.insert(first.id, VecDeque::from([Usage::Free]));
            self.operations.push(instruction);
        } else if let Some(OperationUnit::Operation(instruction)) =  self.operations.last_mut() {
            instruction.dest = Some(new_dest);
        } else {
            unreachable!("An operation was added..");
        }
    }

    fn add_usage(&mut self, operand : &Operand) {
        let usages = self.usages.get_mut(&operand.id).unwrap();
        if let Some(usage) = usages.back_mut() {
            *usage = Usage::Use;
        }
        usages.push_back(Usage::Free);
        println!("Added usage: {}, {:?}",operand.id,  usages);
    }

    fn add_restore(&mut self, operand : &Operand) {
        let usages = self.usages.get_mut(&operand.id).unwrap();
        if let Some(usage) = usages.back_mut() {
            *usage = Usage::TempFree;
        }
        usages.push_back(Usage::Restore);
        usages.push_back(Usage::Free);
        println!("Added restore: {}, {:?}",operand.id,  usages);
    }

    pub fn with(mut self, statements : &'a Vec<StatementData>) -> Self {
        let mut statement_stack = vec![statements.iter()];
        let mut builder_stack = Vec::new();
        let outer_scope_id = self.tracker.get_id();
        self.operations.push(OperationUnit::EnterBlock(outer_scope_id));
        while let Some(iter) = statement_stack.last_mut() {
            if let Some(val) = iter.next() {
                match &val.statement {
                    Statement::Assignment { var, expr } => {
                        self.add_assigment(var, expr);
                    }
                    Statement::IfBlock(if_statement) => {
                        let mut builder : Box<dyn BlockCompiler> = Box::new(
                            IfBlockBuilder::new(&mut self, if_statement)
                        );
                        let iter = builder.begin(&mut self);
                        statement_stack.push(iter);
                        builder_stack.push(builder);
                    }
                    Statement::Block(_) => todo!()
                }
            } else {
                if let Some(builder) = builder_stack.last_mut() {
                    if let Some(iter) = builder.end(&mut self) {
                        statement_stack.push(iter );
                    } else {
                        builder_stack.pop();
                    }
                } else {
                    break;
                }
            }
        }

        let exit = self.operands[self.exit_code];
        let out = self.arena.alloc(Operand::new(&mut self.tracker,
                                                  exit.size));
        self.add_restore(exit);

        self.usages.insert(out.id, VecDeque::from([Usage::Free]));
        self.operations.push(OperationUnit::operation(OperationType::MovRet, vec![exit], Some(out)));
        self.operations.push(OperationUnit::LeaveBlock);
        self
    }

    // Allocate hints for what registers should be used
    // Allows e.g using rax if later instruction uses mul
    fn allocate_hints(&mut self) {
        for operation in self.operations.iter() {
            let operation = if let OperationUnit::Operation(operation) = operation {
                operation
            } else {
                continue;
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
        // self.operands contains all variable operands, these are pre-allocated and then freed.
        // This means they can be restored to their home positions later.
        for operand in &self.operands {
            self.register_state.allocate(operand, MEM_GEN_REG, 0, 0);
        }
        for operand in &self.operands {
            self.register_state.free(operand, true);
        }
        for operation in self.operations.iter() {
            let operation = if let OperationUnit::Operation(operation) = operation {
                operation
            } else {
                continue;
            };
            // Propagate the hint to dest to allow combining with future instructions
            if let (Some(dest), &first) = (operation.dest, operation.operands.first().unwrap()) {
                self.register_state.hint_from_allocation(first, dest);
            }
        }
    }

    pub fn compile(mut self) {
        self.allocate_hints();

        let mut invalidation = 0;
        let mut used_stable = 0_u64;

        let mut scope = 0;
        let mut block_stack = vec![0];
        let mut operation_index_list = Vec::with_capacity(self.operations.len());

        for (index, operation) in self.operations.iter_mut().enumerate() {
            operation_index_list.push(self.register_state.output.len());
            let operation = match operation {
                OperationUnit::Operation(o) => o,
                OperationUnit::EnterBlock(block) => {
                    block_stack.push(scope);
                    scope = *block;
                    println!("New block : {}", scope);
                    continue;
                },
                OperationUnit::LeaveBlock => {
                    scope = block_stack.pop().unwrap();
                    continue;
                }
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
            println!("\n{:?} : index {}", operation.operator, index);
            for (i, operand) in operation.operands.iter_mut().enumerate() {
                bitmap = operation.operator.next_bitmap(
                    bitmap, i, operand.size
                );

                if self.register_state.is_free(operand) {
                    self.register_state.allocate(*operand, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                }
                let mut allocation_bitmap = self.register_state.allocation_bitmap(operand);

                let usages = self.usages.get_mut(&operand.id).unwrap();
                print!("{:?}", operand);
                std::io::stdout().flush().unwrap();
                let mut first_use = *usages.front().unwrap();
                println!(", {:?}", usages);
                if (first_use == Usage::Use && destroyed & (1 << i) != 0) || allocation_bitmap & bitmap == 0 {
                    let new = self.arena.alloc(
                        Operand::new(&mut self.tracker,operand.size));
                    let location = self.register_state.allocate(new, bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                    // Allocating might take the previous register and move the original value
                    // In that case no move is needed.
                    if location != allocation_bitmap {
                        println!("Mov0 {}, {}", self.register_state.to_string(new.allocation.get()), self.register_state.to_string(operand.allocation.get()));
                        self.register_state.build_instruction(OperationType::Mov, &[new, operand]);
                    }
                    allocation_bitmap = location;

                    usages.pop_front();
                    if first_use != Usage::Use {
                        self.register_state.free(operand, first_use == Usage::TempFree);
                    }
                    self.usages.insert(new.id, VecDeque::from([Usage::Free]));
                    first_use = Usage::Free;
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
                if first_use != Usage::Use {
                    invalid_now &= !allocation_bitmap;
                }
                bitmap = allocation_bitmap;
            }
            self.register_state.invalidate_registers(invalid_now);

            print!("{:?}", operation.operator);
            for o in &operation.operands {
                print!(" {}", self.register_state.to_string(o.allocation.get()));
            }
            println!();
            self.register_state.build_instruction(operation.operator, &operation.operands);

            if let Some(dest) = operation.dest {
                let usage = *self.usages.get_mut(&dest.id).unwrap().front().unwrap();
                let first = *operation.operands.first().unwrap();
                match usage {
                    Usage::Use => {
                        debug_assert!(self.register_state.is_free(dest));
                        first.merge_into(dest);
                        self.usages.insert(first.id, self.usages[&dest.id].clone());
                    },
                    Usage::Restore => {
                        let first_location = self.register_state.allocation_bitmap(first);
                        let dest_location = self.register_state.allocation_bitmap(dest);
                        if first_location != dest_location {
                            println!("Mov1 {}, {}", self.register_state.to_string(dest.allocation.get()), self.register_state.to_string(first.allocation.get()));
                            self.register_state.build_instruction(OperationType::Mov, &[dest, first]);
                            self.register_state.restore_allocation(dest);
                        }
                        let dest_usages = self.usages.get_mut(&dest.id).unwrap();
                        dest_usages.pop_front().unwrap();
                        let clone = dest_usages.clone();
                        dest_usages.pop_front().unwrap();
                        self.usages.insert(first.id, clone);
                    },
                    Usage::Free | Usage::TempFree => panic!("Unused dest")
                }
            }
            for &operand in &operation.operands {
                let usage = self.usages.get_mut(&operand.id).unwrap().pop_front().unwrap();
                if usage != Usage::Use {
                    self.register_state.free(operand, usage == Usage::TempFree);
                }
            }
        }

        let compiler = InstructionCompiler::new();
        let mut res = Vec::new();
        let mut address_list = Vec::with_capacity(self.register_state.output.len());
        let mut label_queue = Vec::new();
        for instruction in &self.register_state.output {
            address_list.push(res.len());
            compiler.compile_instruction(instruction, &mut res, &mut label_queue);
        }
        for relocation in label_queue {
            let target = address_list[operation_index_list[relocation.target]] as isize;

            // Relative address is relative to PC after instruction
            let diff = target - ((relocation.index + relocation.size as usize) as isize);
            let index = relocation.index;
            match relocation.size {
                OperandSize::BYTE => {
                    let jmp = i8::try_from(diff).unwrap();
                    let bytes : &[u8; 1] = &jmp.to_le_bytes();
                    res[index] = bytes[0];
                },
                OperandSize::WORD => {
                    let jmp = i16::try_from(diff).unwrap();
                    let bytes : &[u8; 2] = &jmp.to_le_bytes();
                    for (i, &b) in bytes.iter().enumerate() {
                        res[index + i] = b;
                    }
                }
                OperandSize::DWORD => {
                    let jmp = i32::try_from(diff).unwrap();
                    let bytes : &[u8; 4] = &jmp.to_le_bytes();
                    for (i, &b) in bytes.iter().enumerate() {
                        res[index + i] = b;
                    }
                }
                OperandSize::QWORD => {
                    let jmp = i64::try_from(diff).unwrap();
                    let bytes : &[u8; 8] = &jmp.to_le_bytes();
                    for (i, &b) in bytes.iter().enumerate() {
                        res[index + i] = b;
                    }
                }
            }
        }

        std::fs::write("out.bin", &res).unwrap();

        println!();
    }
}