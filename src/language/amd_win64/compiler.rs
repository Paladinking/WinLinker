use std::collections::{HashMap};
use std::rc::Rc;
use crate::language::amd_win64::operation::IdTracker;
use crate::language::amd_win64::usages::{UsageTracker, UsedAfter};
use super::instruction::InstructionCompiler;
use super::operation::{Operand, OperandSize, Operation, OperationType};
use super::registers::*;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::parser::{Expression, ExpressionData, Variable};
use crate::language::parser::statement::{IfStatement, Statement, StatementData};
use crate::language::types::Type;

#[derive(Debug)]
pub enum OperationUnit {
    Operation(Operation), // Real operation
    EnterBlock(usize),
    LeaveBlock(bool),
    SyncPush(usize),
    SyncLoad,
    SyncPop
}

impl OperationUnit {
    fn operation(&self) -> Option<&Operation> {
        if let OperationUnit::Operation(ref op) = self {
            return Some(op);
        }
        return None;
    }
}

trait BlockCompiler<'a> {
    fn begin<'b>(&'b mut self, builder: &mut InstructionBuilder) -> std::slice::Iter<'a, StatementData>;

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder) -> Option<std::slice::Iter<'a, StatementData>>;
}

struct IfBlockBuilder<'a> {
    statements : &'a Vec<IfStatement>,
    block_ids : Vec<usize>,
    label_queue : Vec<(usize, usize)>,
    label_locations : Vec<Option<usize>>,
    index : usize
}

impl <'a> IfBlockBuilder<'a> {
    fn new(builder : &mut InstructionBuilder, statements : &'a Vec<IfStatement>) -> Self {
        let mut block_ids : Vec<_> = statements.iter().map(|_| 0).collect();
        block_ids[0] = builder.block_tracker.get_id();
        IfBlockBuilder {
            statements,
            block_ids,
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

    fn add_condition(&mut self, builder : &mut InstructionBuilder) {
        let expr = if let Some(e) = &self.statements[self.index].condition { e } else { return; };
        let location_offset = self.label_locations.len();
        // Extend label_locations with jumps within expression
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
                            let im = builder.create_operand(OperandSize::from(v.var_type));
                            builder.register_state.allocate_imm(&builder.operands[im], 0, OperandSize::from(v.var_type));
                            builder.add_operation(OperationType::Cmp,
                                                  vec![v.id.get().unwrap(), im], None);
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
                            let singed = || expr[*first].t.is_signed();
                            builder.add_operation(
                                OperationType::Cmp,
                                vec![locations[*first - pos], locations[*second - pos]],
                                None);
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
                    let jmp_dest = builder.create_operand(OperandSize::QWORD);
                    if let Some(location) = true_location {
                        self.label_queue.push((jmp_dest, location));
                        builder.add_operation(jmp, vec![jmp_dest], None);
                        if let Some(location) = false_location {
                            let jmp_dest = builder.create_operand(OperandSize::QWORD);
                            self.label_queue.push((jmp_dest, location));
                            builder.add_operation(jmp.inverse(), vec![jmp_dest], None);
                        }
                    } else {
                        let location = false_location.unwrap();
                        self.label_queue.push((jmp_dest, location));
                        builder.add_operation(jmp.inverse(), vec![jmp_dest], None);
                    }
                }
            }
        }
    }
}

impl <'a> BlockCompiler<'a> for IfBlockBuilder<'a> {
    fn begin<'b>(&'b mut self, builder : &mut InstructionBuilder) -> std::slice::Iter<'a, StatementData> {
        self.label_locations[0] = Some(builder.operations.len());
        builder.operations.push(OperationUnit::SyncPush(self.block_ids[0]));
        self.add_condition(builder);
        self.label_locations[1] = Some(builder.operations.len());
        builder.usage_tracker.enter_scope(self.block_ids[0]);
        builder.operations.push(OperationUnit::EnterBlock(self.block_ids[0]));

        self.statements[0].block.iter()
    }

    fn end<'b>(&'b mut self, builder : &mut InstructionBuilder) -> Option<std::slice::Iter<'a, StatementData>> {
        builder.operations.push(OperationUnit::LeaveBlock(self.index < self.statements.len() - 1));
        builder.usage_tracker.leave_scope();
        self.index += 1;
        if let Some(s) = self.statements.get(self.index) {
            self.block_ids[self.index] = builder.block_tracker.get_id();
            // Add jump to end of scope
            let jmp_dest = builder.create_operand(OperandSize::QWORD);
            self.label_queue.push((jmp_dest, self.statements.len() * 2));
            builder.add_operation(OperationType::Jmp, vec![jmp_dest], None);

            self.label_locations[self.index * 2].replace(builder.operations.len());
            builder.operations.push(OperationUnit::SyncLoad);
            self.add_condition(builder);
            self.label_locations[self.index * 2 + 1].replace(builder.operations.len());
            builder.usage_tracker.enter_scope(self.block_ids[0]);
            builder.usage_tracker.alt_scope(
                self.block_ids[self.index],
                self.statements[self.index].condition.is_some());
            builder.operations.push(OperationUnit::EnterBlock(self.block_ids[self.index]));
            Some(s.block.iter())
        } else {
            self.label_locations[self.statements.len() * 2].replace(builder.operations.len());
            if self.statements.last().unwrap().condition.is_some() {
                // Sync only needed if program execution might not enter any case of if-statement.
                builder.operations.push(OperationUnit::SyncLoad);
            }
            builder.operations.push(OperationUnit::SyncPop);
            for &(o, index) in &self.label_queue {
                let addr = self.label_locations[index].unwrap();
                builder.register_state.allocate_addr(&builder.operands[o], addr);
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

pub struct InstructionBuilder {
    operations: Vec<OperationUnit>,
    usage_tracker : UsageTracker,
    operands : Vec<Operand>,
    variable_count : usize,
    register_state : RegisterState,
    block_tracker : IdTracker
}

impl InstructionBuilder {
    pub fn new(vars: &HashMap<String, Rc<Variable>>) -> InstructionBuilder {
        let register_state = RegisterState::new();
        let block_tracker = IdTracker::new();
        let operands = vars.iter().enumerate().map(|(i, (_, v))|{
            v.id.replace(Some(i));
            Operand::new(OperandSize::from(v.var_type))
        }).collect();
        let builder = InstructionBuilder {
            operations: Vec::new(), usage_tracker : UsageTracker::new(),
            operands, register_state, block_tracker, variable_count : vars.len()
        };
        builder
    }

    fn add_operation(&mut self, operation : OperationType, operands : Vec<usize>, dest : Option<usize>) {
        let mut size = OperandSize::QWORD;
        for (i, &operand) in operands.iter().enumerate() {
            if operand < self.variable_count {
                self.usage_tracker.add_usage(operand, self.operations.len(), true);
            }
            let hint = operation.bitmap_hint(i, self.operands[operand].size);
            let new_hint = hint & self.operands[operand].hint;
            if new_hint != 0 {
                self.operands[operand].hint = new_hint;
            }
        }

        if let Some(op) = &dest {
            size = self.operands[*op].size;
            if *op < self.variable_count {
                self.usage_tracker.add_usage(*op, self.operations.len(), false);
            }
            if let Some(first) = operands.first() {
                let new_hint = self.operands[*op].hint & self.operands[*first].hint;
                if new_hint != 0 {
                    self.operands[*op].hint = new_hint;
                }
            }
        }

        self.operations.push(OperationUnit::Operation(Operation::new(
            operation, operands, dest, operation.invalidations(size))));
    }

    fn create_operand(&mut self, size : OperandSize) -> usize {
        self.operands.push(Operand::new(size));
        return self.operands.len() - 1;
    }

    fn add_dual_operator(&mut self, locations : &mut Vec<usize>, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
        if op.is_cmp() {
            self.add_operation(OperationType::Cmp,
                               vec![locations[first], locations[second]], None);
            let basic_operator = match op {
                DualOperator::Equal => OperationType::SetE,
                DualOperator::NotEqual => OperationType::SetNE,
                DualOperator::GreaterEqual => if t.is_signed() { OperationType::SetGE } else { OperationType::SetAE},
                DualOperator::LesserEqual => if t.is_signed() { OperationType::SetLE} else { OperationType::SetBE},
                DualOperator::Greater => if t.is_signed() { OperationType::SetG } else { OperationType::SetA},
                DualOperator::Lesser => if t.is_signed() { OperationType::SetL } else { OperationType::SetB},
                _ => unreachable!("Not is_cmp")
            };
            let dest =  self.create_operand(size);
            let oper = self.create_operand(size);
            locations.push(dest);
            self.add_operation(basic_operator, vec![oper], Some(dest));
        } else {
            let operator = match op {
                DualOperator::Divide => {
                    if t.is_signed() { OperationType::IDiv } else { OperationType::Div }
                },
                DualOperator::Multiply => if t.is_signed() {
                    OperationType::IMul
                } else {
                    OperationType::Mul
                },
                DualOperator::Minus => OperationType::Sub,
                DualOperator::Plus => OperationType::Add,
                DualOperator::BoolAnd => OperationType::And,
                DualOperator::BoolOr => OperationType::Or,
                _ => unreachable!("Covered by is_cmp")
            };
            let dest = self.create_operand(size);
            locations.push(dest);
            self.add_operation(operator,
                               vec![locations[first], locations[second]], Some(dest));
        }
    }

    fn add_single_operator(&mut self, locations : &mut Vec<usize>, expr : usize, op : SingleOperator) {
        match op {
            SingleOperator::Not => {
                let dest = self.create_operand(OperandSize::BYTE);
                let imm = self.create_operand(OperandSize::BYTE);
                self.register_state.allocate_imm(&self.operands[imm], 1, OperandSize::BYTE);
                locations.push(dest);
                self.add_operation(OperationType::Xor, vec![locations[expr], imm], Some(dest));
            }
            SingleOperator::Pass => unreachable!("Should not exist at this point.")
        }
    }

    fn add_expressions(&mut self, expr : &[ExpressionData], locations : &mut Vec<usize>, offset : usize) {
        for e in expr.iter() {
            match &e.expression {
                Expression::Variable(v) => {
                    let id = v.id.get().unwrap();
                    locations.push(id);
                },
                &Expression::Operator { first, operator, second } => {
                    let size = OperandSize::from(e.t);
                    self.add_dual_operator(locations, first - offset, second - offset, size, expr[first].t, operator);

                }
                &Expression::SingleOperator { operator, expr } => {
                    self.add_single_operator(locations, expr - offset, operator);
                }
                &Expression::IntLiteral(val) => {
                    let literal = self.create_operand(OperandSize::from(e.t));
                    self.register_state.allocate_imm(&self.operands[literal], val, OperandSize::from(e.t));
                    locations.push(literal);
                }
                &Expression::BoolLiteral(b) => {
                    let literal = self.create_operand(OperandSize::BYTE);
                    self.register_state.allocate_imm(&self.operands[literal], if b {1} else {0}, OperandSize::BYTE);
                    locations.push(literal);
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
    }

    fn add_assigment(&mut self, dest_index : usize, dest_size : OperandSize, expr : &Vec<ExpressionData>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = self.operations.len();
        self.add_expressions(&expr, &mut locations, 0);

        if prev_len ==  self.operations.len() { // Whole expression was only a variable or immediate value.
            let operand = *locations.last().unwrap();
            let first = self.create_operand(dest_size);
            self.add_operation(OperationType::Mov, vec![first, operand], Some(dest_index));
        } else if let Some(OperationUnit::Operation(instruction)) =  self.operations.last_mut() {
            instruction.dest = Some(dest_index);
            if dest_index < self.variable_count {
                self.usage_tracker.add_usage(dest_index, self.operations.len() - 1, false);
            }
        } else {
            unreachable!("An operation was added..");
        }
    }

    pub fn with(mut self, statements : &Vec<StatementData>) -> Self {
        let mut statement_stack = vec![statements.iter()];
        let mut builder_stack = Vec::new();
        let outer_scope_id = self.block_tracker.get_id();
        self.operations.push(OperationUnit::SyncPush(outer_scope_id));
        self.operations.push(OperationUnit::EnterBlock(outer_scope_id));
        self.usage_tracker.enter_scope(outer_scope_id);
        let mut return_addr_list = Vec::new();
        while let Some(iter) = statement_stack.last_mut() {
            if let Some(val) = iter.next() {
                match &val.statement {
                    Statement::Assignment { var, expr } => {
                        self.add_assigment( var.id.get().unwrap(),
                                            OperandSize::from(var.var_type), expr);
                    }
                    Statement::IfBlock(if_statement) => {
                        let mut builder : Box<dyn BlockCompiler> = Box::new(
                            IfBlockBuilder::new(&mut self, if_statement)
                        );
                        let iter = builder.begin(&mut self);
                        statement_stack.push(iter);
                        builder_stack.push(builder);
                    },
                    Statement::Return(expr) => {
                        let dest = self.create_operand(OperandSize::DWORD);
                        self.add_assigment(dest, OperandSize::DWORD, expr);
                        let out = self.create_operand(OperandSize::DWORD);
                        self.add_operation(OperationType::MovRet, vec![dest],
                                           Some(out));
                        let jmp_ret = self.create_operand(OperandSize::QWORD);
                        self.add_operation(OperationType::Jmp, vec![jmp_ret], None);
                        return_addr_list.push(jmp_ret);
                    },
                    Statement::Block(_) => todo!()
                }
            } else {
                statement_stack.pop();
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

        for op in return_addr_list {
            self.register_state.allocate_addr(&self.operands[op],
            self.operations.len());
        }
        self.operations.push(OperationUnit::LeaveBlock(false));
        self.operations.push(OperationUnit::SyncPop);

        self.operations.iter().rev()
            .filter_map(|op| op.operation())
            .for_each(|op| {
               if let (Some(dest), Some(&first)) = (op.dest, op.operands.first()) {
                   let hint = self.operands[dest].hint & self.operands[first].hint;
                   if  hint != 0 {
                       self.operands[first].hint = hint;
                   }
               }
            });

        self.usage_tracker.leave_scope();
        self.usage_tracker.finalize(&mut self.operations, self.variable_count);
        self
    }

    pub fn compile(mut self) -> Vec<u8> {
        // Remove last leave_block and syncPop
        self.operations.truncate(self.operations.len() - 2);
        let mut operation_index_list = Vec::with_capacity(self.operations.len());

        for index in 0..self.operations.len() {
            operation_index_list.push(self.register_state.output.len());
            match std::mem::replace(&mut self.operations[index], OperationUnit::EnterBlock(0)) {
                OperationUnit::EnterBlock(ref id) => {
                    self.register_state.enter_block(&self.operands);
                    let to_free = self.usage_tracker.get_frees(*id);
                    for &operand in to_free {
                        if !self.register_state.is_free(&self.operands[operand]) {
                            self.register_state.free(&self.operands[operand]);
                        }
                    }
                },
                OperationUnit::LeaveBlock(has_next) => {
                    self.register_state.leave_block(&self.operands, has_next);
                },
                OperationUnit::SyncPush(scope) => {
                    let operands = self.usage_tracker.get_initializations(scope);
                    self.register_state.reserve_variables(operands.size_hint().0);
                    for operand_id in operands {
                        let invalid_soon = self.usage_tracker.variable_invalidations(*operand_id);
                        let operand = &mut self.operands[*operand_id];
                        self.register_state.allocate_variable(*operand_id, operand, invalid_soon);
                    }
                    self.register_state.push_state(&self.operands);
                }
                OperationUnit::SyncLoad => {
                    self.register_state.load_state(&self.operands);
                },
                OperationUnit::SyncPop => {
                  self.register_state.pop_state();
                },
                OperationUnit::Operation(mut operation) => {
                    let mut invalid_now =  operation.invalidations;
                    let mut invalid_soon = self.usage_tracker.row_invalidations(index);
                    let mut bitmap = 1;
                    let destroyed = operation.operator.destroyed();
                    for (i, id) in operation.operands.iter_mut().enumerate() {
                        bitmap = operation.operator.next_bitmap(
                            bitmap, i, self.operands[*id].size
                        );

                        let used_after = self.usage_tracker.used_after(*id, index);

                        // Merge home location with first operand
                        // Needed for moves, a bit ugly.
                        if used_after == UsedAfter::None && *id >= self.variable_count {
                            if let Some(dest) = operation.dest {
                                self.operands[*id].home = self.operands[dest].home;
                            }
                        }

                        if self.register_state.is_free(&self.operands[*id]) {
                            self.register_state.allocate(
                                &self.operands[*id],
                                bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                        }
                        let mut allocation_bitmap = self.register_state.allocation_bitmap(&self.operands[*id]);


                        let copy_needed = used_after == UsedAfter::ValueNeeded && destroyed & (1 << i) != 0;
                        let invalid_location = allocation_bitmap & bitmap == 0;
                        if copy_needed || invalid_location {
                            let new = self.create_operand(self.operands[*id].size);
                            let location = self.register_state.allocate(
                                &self.operands[new],
                                bitmap & MEM_GEN_REG, invalid_now, invalid_soon);
                            // Allocating might take the previous register and move the original value
                            // In that case no move is needed.
                            if location != allocation_bitmap {
                                self.register_state.build_instruction(
                                    OperationType::Mov,
                                    &[&self.operands[new], &self.operands[*id]]);
                            }

                            allocation_bitmap = location;
                            if used_after != UsedAfter::ValueNeeded {
                                self.register_state.free(&self.operands[*id]);
                            }
                            *id = new;
                        }
                        // Only the first operand uses invalid_soon, since that is the destination register
                        invalid_soon = 0;
                        // If this operand is going to be freed there is no need to invalidate it.
                        // It cannot be freed until after invalidation is done since otherwise
                        //  they might be overridden while saving needed invalidated registers.
                        if used_after != UsedAfter::ValueNeeded {
                            invalid_now &= !allocation_bitmap;
                        }
                        bitmap = allocation_bitmap;
                    }
                    self.register_state.invalidate_registers(invalid_now);

                    let operands : Vec<_> = operation.operands.iter()
                        .map(|id| &self.operands[*id]).collect();
                    self.register_state.build_instruction(operation.operator, &operands);

                    if let Some(dest) = operation.dest {
                        let first = *operation.operands.first().unwrap();
                        self.operands[first].merge_into(&self.operands[dest]);
                        let usage = self.usage_tracker.used_after(dest, index);
                        if usage == UsedAfter::DestNeeded {
                            // Make sure this reg does not get freed later
                            self.usage_tracker.mark_dest(dest, index);
                        } else if usage != UsedAfter::ValueNeeded {
                            self.register_state.free(&self.operands[dest]);
                        }
                    } else if let Some(&first) = operation.operands.first() {
                        let usage = self.usage_tracker.used_after(first, index);
                        if usage != UsedAfter::ValueNeeded {
                            self.register_state.free(&self.operands[first]);
                        }
                    }
                    for operand in &operation.operands[1..] {
                        let usage = self.usage_tracker.used_after(*operand, index);
                        if usage != UsedAfter::ValueNeeded {
                            if !self.register_state.is_free(&self.operands[*operand]) {
                                self.register_state.free(&self.operands[*operand]);
                            }
                        }
                    }
                }
            }
        }
        // Include address of jump to epilogue.
        operation_index_list.push(self.register_state.output.len());
        let (prologue, epilogue) = self.register_state.get_prologue_and_epilogue();

        let compiler = InstructionCompiler::new();
        let mut res = Vec::new();
        let mut address_list = Vec::with_capacity(self.register_state.output.len());
        let mut label_queue = Vec::new();
        for instruction in &prologue {
            compiler.compile_instruction(instruction, &mut res, &mut label_queue);
        }
        for instruction in &self.register_state.output {
            address_list.push(res.len());
            compiler.compile_instruction(instruction, &mut res, &mut label_queue);
        }

        address_list.push(res.len());
        for instruction in epilogue.iter().rev() {
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
        return res;
    }
}