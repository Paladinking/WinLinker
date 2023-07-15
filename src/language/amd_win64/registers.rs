use std::collections::{HashMap, HashSet};
use crate::language::amd_win64::instruction::{Instruction, InstructionOperand};
use crate::language::amd_win64::operation::{Operand, OperandSize, Operation, OperationType};


// Bitmaps of all locations an operand can be allocated at.
// Registers are ordered in terms of priority, with higher values being allocated before lower ones.
// The ordering is based on: prefer volatile registers, prefer avoiding rex-prefix, and lower rax
//  a bit since it is required for many instructions, and is therefore good to keep free when possible.
//
// The invalidation checking + register hinting makes it so that allocations are not allocated
//  in this order when it does not work well, this ordering is only a tiebreaker.
//
// These bitmaps are also used to determine what registers can be used for what operation.
// Important that 2^0 is not a register, otherwise subtraction overflow in get_register()
//  in case of empty bitmap.
pub const MEM : u64 = 2_u64.pow(0);
pub const R15 : u64 = 2_u64.pow(1);
pub const R14 : u64 = 2_u64.pow(2);
pub const R13 : u64 = 2_u64.pow(3);
pub const R12 : u64 = 2_u64.pow(4);
pub const RSP : u64 = 2_u64.pow(5);
pub const RSI : u64 = 2_u64.pow(6);
pub const RDI : u64 = 2_u64.pow(7);
pub const RBP : u64 = 2_u64.pow(8);
pub const RBX : u64 = 2_u64.pow(9);
pub const RAX : u64 = 2_u64.pow(10);
pub const R11 : u64 = 2_u64.pow(11);
pub const R10 : u64 = 2_u64.pow(12);
pub const R9 : u64 = 2_u64.pow(13);
pub const R8 : u64 = 2_u64.pow(14);
pub const RDX : u64 = 2_u64.pow(15);
pub const RCX : u64 = 2_u64.pow(16);

pub const IMM8 : u64  = 2_u64.pow(60);
pub const IMM16 : u64 = 2_u64.pow(61);
pub const IMM32 : u64 = 2_u64.pow(62);
pub const IMM64 : u64 = 2_u64.pow(63);


pub const VOL_GEN_REG : u64 = RAX | RCX | RDX | R8 | R9 | R10 | R11;
pub const NON_VOL_GEN_REG : u64 = RBX | RBP | RDI | RSI | R12 | R13 | R14 | R15;
pub const GEN_REG : u64 = VOL_GEN_REG | NON_VOL_GEN_REG;
pub const MEM_GEN_REG : u64 = GEN_REG | MEM;

pub mod encoding {
    pub const RAX : u8 = 0b0000;
    pub const RCX : u8 = 0b0001;
    pub const RDX : u8 = 0b0010;
    pub const RBX : u8 = 0b0011;
    pub const RBP : u8 = 0b0101;
    pub const RSI : u8 = 0b0110;
    pub const RDI : u8 = 0b0111;
    pub const R8 : u8 = 0b1000;
    pub const R9 : u8 = 0b1001;
    pub const R10 : u8 = 0b1010;
    pub const R11 : u8 = 0b1011;
    pub const R12 : u8 = 0b1100;
    pub const R13 : u8 = 0b1101;
    pub const R14 : u8 = 0b1110;
    pub const R15 : u8 = 0b1111;
}

// Represents a single register during allocation.
struct Register {
    // The bitmap matching this register
    bitmap : u64,
    // index to the MemoryAllocation the operand using this register has, None if this register is free.
    operand : Option<usize>,
    // 4 bit value, how this register is represented by the processor.
    encoding : u8
}

impl Register {
    fn new_general(bitmap : u64, encoding : u8) -> Register {
        Register {bitmap, operand : None, encoding}
    }
}

pub struct RegisterState {
    registers : Vec<Register>,
    memory : Vec<bool>,
    allocations : Vec<MemoryAllocation>,
    free_gen : u64, // Bitmap of all free general purpose registers
    variables : Vec<(usize, MemoryAllocation)>,
    variable_map : u64, // Bitmap of all variables in registers
    saved_variables : Vec<HashMap<usize, (MemoryAllocation, bool)>>,
    pub output : Vec<Instruction>
}

pub fn register_string(bitmap : u64) -> &'static str {
    match bitmap {
        RCX => "rcx", RDX => "rdx", RAX => "rax",
        R8 => "r8", R9 => "r9", R10 => "r10",
        R11 => "r11", RBX => "rbx", RBP => "rbp",
        RDI => "rdi", RSI => "rsi", R12 => "r12",
        R13 => "r13", R14 => "r14", R15 => "r15",
        _ => "???"
    }
}

pub fn register_bitmap(encoding : u8) -> u64 {
    match encoding {
        encoding::RAX => RAX, encoding::RCX => RCX, encoding::RDX => RDX,
        encoding::RBX => RBX, encoding::RBP => RBP, encoding::RSI => RSI,
        encoding::RDI => RDI, encoding::R8 => R8, encoding::R9 => R9,
        encoding::R10 => R10, encoding::R11 => R11, encoding::R12 => R12,
        encoding::R13 => R13, encoding::R14 => R14, encoding::R15 => R15,
        _ => panic!("Invalid encoding")
    }
}


#[derive(Debug, PartialEq, Clone)]
enum MemoryAllocation {
    Register(usize, OperandSize), // Represents a register
    Memory(usize, OperandSize), // Represents stack memory
    Immediate(u64, u64), // Represents an immediate value, (val, bitmap)
    Address(usize), // Represents a jump label, containing an operand index.
    //Hint(u64), // Hint with a bitmap containing good registers to allocate
    None // Unallocated
}

impl MemoryAllocation {
    // Returns the size of this allocation.
    // Used when moving a previous allocation to memory.
    fn size(&self) -> OperandSize {
        match self {
            MemoryAllocation::Register(_, size) |
            MemoryAllocation::Memory(_, size) => *size,
            MemoryAllocation::Immediate(_, bitmap) => match *bitmap {
                IMM8 => OperandSize::BYTE,
                IMM16 => OperandSize::WORD,
                IMM32 => OperandSize::DWORD,
                IMM64 => OperandSize::QWORD,
                _ => panic!("Bad immediate bitmap")
            },
            // Not always correct, but no usage 'should' care
            MemoryAllocation::Address(_) => OperandSize::QWORD,
            MemoryAllocation::None => panic!("Size on non allocation")
        }
    }

    fn is_free(&self) -> bool {
        match &self {
            MemoryAllocation::Register(..) | MemoryAllocation::Memory(..) |
            MemoryAllocation::Immediate(..) | MemoryAllocation::Address(..) => false,
            MemoryAllocation::None => true
        }
    }
}

impl RegisterState {

    pub fn new() -> RegisterState {
        let registers = vec![
            Register::new_general(RCX, encoding::RCX),
            Register::new_general(RDX, encoding::RDX),
            Register::new_general(RAX, encoding::RAX),
            Register::new_general(R8, encoding::R8),
            Register::new_general(R9, encoding::R9),
            Register::new_general(R10, encoding::R10),
            Register::new_general(R11, encoding::R11),
            Register::new_general(RBX, encoding::RBX),
            Register::new_general(RBP, encoding::RBP),
            Register::new_general(RDI, encoding::RDI),
            Register::new_general(RSI, encoding::RSI),
            Register::new_general(R12, encoding::R12),
            Register::new_general(R13, encoding::R13),
            Register::new_general(R14, encoding::R14),
            Register::new_general(R15, encoding::R15)
        ];
        // All operands have a 0-initialized id field, make sure this means no allocation.
        let allocations = vec![MemoryAllocation::None];
        RegisterState {
            registers, memory : Vec::new(), allocations, free_gen : GEN_REG,
            variables : Vec::new(), variable_map : 0, saved_variables : Vec::new(), output : Vec::new()
        }
    }

    // Returns the index in the register list of the highest priority register in bitmap,
    //  or None if the bitmap has no registers in it.
    fn get_register(bitmap : u64) -> Option<usize> {
        Some(match bitmap & (u64::MAX << (63 - (bitmap | 1).leading_zeros())) {
            RCX => 0, RDX => 1, RAX => 2,
            R8 => 3, R9 => 4, R10 => 5,
            R11 => 6, RBX => 7, RBP => 8,
            RDI => 9, RSI => 10, R12 => 11,
            R13 => 12, R14 => 13, R15 => 14,
            0 => {return None;}
            _ => {
                panic!("Bad bitmap")
            }
        })
    }


    // Finds a memory location large enough for size, marks it as taken and returns the index.
    fn get_memory(&mut self, size : OperandSize) -> usize {
        for i in (0..self.memory.len()).step_by(size as usize) {
            if self.memory[i..].iter().take(size as usize).all(|b|!*b) {
                for j in i..(i + size as usize) {
                    self.memory[j] = true;
                }
                return i;
            }
        }
        self.memory.reserve(size as usize);
        for _ in 0..(size as usize) {
            self.memory.push(true);
        }
        return self.memory.len() - size as usize;
    }

    fn get_val(&self, id : usize) -> InstructionOperand {
        match self.allocations[id] {
            MemoryAllocation::Register(i, _) => {
                InstructionOperand::Reg(self.registers[i].encoding)
            }
            MemoryAllocation::Memory(i, _) => {
                InstructionOperand::Mem(i as u32)
            }
            MemoryAllocation::Immediate(val, _) => {
                InstructionOperand::Imm(val)
            },
            MemoryAllocation::Address(adr) => {
                InstructionOperand::Addr(adr)
            }
            MemoryAllocation::None => panic!("No allocation")
        }
    }

    fn add_instruction(&mut self, operation : OperationType, size : OperandSize, operands : Vec<InstructionOperand>) {
        let instruction = Instruction::new(operation, size, operands);
        println!("{:?}", instruction);
        self.output.push(instruction);
    }

    pub fn build_instruction(&mut self, operation : OperationType, operands : &[&Operand]) {
        let size = operands[0].size;
        // Need to collect to avoid borrowing self, probably fix.
        let vals : Vec<_> = operands.iter().map(|o|
            self.get_val(o.allocation.get())
        ).collect();
        self.add_instruction(operation, size, vals);
    }

    // Allocates a location for operand to a location contained in bitmap.
    // Potentially inserts a move in order to free a register, moving the value to another
    // register or memory location. The destination of such a move will not be to
    // any location contained in invalidated.
    pub fn allocate(&mut self, operand : &Operand, bitmap : u64, invalidated : u64, invalid_soon : u64) -> u64 {
        // Helper for allocating a register
        fn allocate_reg(state : &mut RegisterState, index : usize, operand : &Operand) -> u64 {
            state.registers[index].operand = Some(state.allocations.len());
            operand.allocation.replace(state.allocations.len());
            state.allocations.push(MemoryAllocation::Register(index, operand.size));
            state.free_gen &= !state.registers[index].bitmap;
            return state.registers[index].bitmap;
        }

        // The operand might not be free if TempFree and Restore are used incorrectly
        debug_assert!(self.is_free(operand));
        // Try to allocate a free register, preferring hinted and not soon invalid.
        for map in [operand.hint & !invalid_soon, operand.hint, !invalid_soon, GEN_REG] {
            let bitmap = map & bitmap & self.free_gen;
            if let Some(index) = Self::get_register(bitmap) {
                return allocate_reg(self, index, operand);
            }
        }
        // Try to allocate to memory
        if bitmap & MEM != 0 {
            let index = operand.home.unwrap_or_else(|| self.get_memory(operand.size));
            operand.allocation.replace(self.allocations.len());
            self.allocations.push(MemoryAllocation::Memory(index, operand.size));
            return MEM;
        }

        // Steal register from some other allocation
        let location = Self::get_register(bitmap).unwrap();
        let prev_owner = self.registers[location].operand.unwrap();
        // Try moving previous allocation to some other register
        let size = self.allocations[prev_owner].size();
        if let Some(reg) = Self::get_register(self.free_gen & !self.registers[location].bitmap & !invalidated) {
            // Insert Move
            self.allocations[prev_owner] = MemoryAllocation::Register(reg, size);
            self.registers[reg].operand = Some(prev_owner);
            self.free_gen &= !self.registers[reg].bitmap;
        } else { // Move it to memory instead
            // Insert Move
            let pos = self.get_memory(size);
            self.allocations[prev_owner] = MemoryAllocation::Memory(pos, size);
        }

        let bitmap = allocate_reg(self, location, operand);
        let vals = vec![self.get_val(prev_owner), self.get_val(operand.allocation.get())];
        self.add_instruction(OperationType::Mov, operand.size, vals);
        return bitmap;
    }

    fn free_allocation(&mut self, allocation : usize, free_mem : bool) {
        match self.allocations[allocation] {
            MemoryAllocation::Register(reg, _) => {
                self.registers[reg].operand = None;
                self.free_gen |= self.registers[reg].bitmap;
            }
            MemoryAllocation::Memory(index, size) => {
                if free_mem {
                    self.memory[index..(index + size as usize)].fill(false);
                }
            },
            MemoryAllocation::Immediate(..) | MemoryAllocation::Address(_) => {},
            MemoryAllocation::None => panic!("Double free")
        }
    }

    // Make register / memory used by operand available again.
    pub fn free(&mut self, operand : &Operand) {
        self.free_allocation(operand.allocation.get(), operand.home.is_none());
        // This allocation might be restored, no reason to clear the allocation
        self.allocations[operand.allocation.get()] = MemoryAllocation::None;
    }

    // Move all allocations that are contained in map to some register not in map.
    pub fn invalidate_registers(&mut self, map : u64) {
        if map == 0 {
            return;
        }
        for i in 0..self.registers.len() {
            if self.registers[i].bitmap & map != 0 {
                if let Some(index) = self.registers[i].operand {
                    let val = self.get_val(index);
                    let size = self.allocations[index].size();
                    // Try to find free register, if none exists use memory
                    if let Some(reg) = Self::get_register(self.free_gen & !map) {
                        self.allocations[index] = MemoryAllocation::Register(reg, size);
                        self.registers[reg].operand = Some(index);
                        self.free_gen &= !self.registers[reg].bitmap;
                    } else {
                        let mem = self.get_memory(size);
                        self.allocations[index] = MemoryAllocation::Memory(mem, size);
                    }
                    self.add_instruction(OperationType::Mov, size,
                                         vec![self.get_val(index), val]);
                    self.registers[i].operand = None;
                    self.free_gen |= self.registers[i].bitmap;
                }
            }
        }
    }

    pub fn is_free(&self, operand : &Operand) -> bool {
        self.allocations[operand.allocation.get()].is_free()
    }

    pub fn allocation_bitmap(&self, operand : &Operand) -> u64 {
        match self.allocations[operand.allocation.get()] {
            MemoryAllocation::Register(i, _) => self.registers[i].bitmap,
            MemoryAllocation::Memory(_, _) => MEM,
            MemoryAllocation::Immediate(_, bitmap) => bitmap,
            // Since no operation uses both imm and address values in the same slot,
            // Addresses uses the IMM64 bitmap
            MemoryAllocation::Address(_) => IMM64,
            MemoryAllocation::None => panic!("Allocation bitmap on non-allocated operand")
        }
    }

    fn allocation_string(&self, allocation : &MemoryAllocation) -> String {
        match allocation {
            MemoryAllocation::Register(i, _) => {
                register_string(self.registers[*i].bitmap).to_owned()
            }
            MemoryAllocation::Memory(i, _) => {
                "mem(".to_owned() + &i.to_string() + ")"
            },
            MemoryAllocation::Immediate(i, _) => {
                i.to_string()
            },
            MemoryAllocation::Address(i) => {
                "addr(".to_owned() + &i.to_string() + ")"
            },
            MemoryAllocation::None => {
                "None".to_owned()
            }
        }
    }

    pub fn allocate_addr(&mut self, operand : &Operand, addr : usize) {
        self.allocations.push(MemoryAllocation::Address(addr));
        operand.allocation.replace(self.allocations.len() - 1);
    }

    // Allocates an immediate value for operand
    pub fn allocate_imm(&mut self, operand : &Operand, imm : u64, mut size : OperandSize) {
        // Most instructions working with 64 bit register + 32-bit immediate
        //  force sign-extension.
        if size == OperandSize::QWORD && imm <= (u32::MAX >> 1) as u64 {
            size = OperandSize::DWORD
        }
        self.allocations.push(MemoryAllocation::Immediate(imm, size.to_imm()));
        operand.allocation.replace(self.allocations.len() - 1);
    }

    // Call first when entering block
    pub fn reserve_variables(&mut self, capacity : usize) {
        self.saved_variables.push(HashMap::with_capacity(capacity));
    }

    // Call for each variable initialized in this block after calling reserve_variables
    pub fn allocate_variable(&mut self, id : usize, operand : &mut Operand, invalid_soon : u64) {
        for bitmap in [operand.hint & !invalid_soon, operand.hint, !invalid_soon, GEN_REG] {
            if let Some(location) = Self::get_register(self.free_gen & !self.variable_map & bitmap) {
                let allocation = MemoryAllocation::Register(location, operand.size);
                self.variable_map |= self.registers[location].bitmap;
                self.saved_variables.last_mut().unwrap().insert(id, (allocation, true));
                if operand.hint & self.registers[location].bitmap != 0 {
                    operand.hint &= self.registers[location].bitmap;
                }
                return;
            }
        }
        let index = self.get_memory(operand.size);
        operand.home.replace(index);
        self.saved_variables.last_mut().unwrap().insert(
            id, (MemoryAllocation::Memory(index, operand.size), true));
    }

    // Call after allocate_variables when entering block
    pub fn push_state(&mut self, operands : &Vec<Operand>) {
        let size = self.variables.len();
        for (id, (allocation, _ )) in self.saved_variables.last().unwrap() {
            self.variables.push((*id, allocation.clone()));
        }
        for index in 0..size {
            let (id, ref allocation) = self.variables[index];
            if !self.is_free(&operands[id]) {
                self.saved_variables.last_mut().unwrap().insert(
                    id, (self.allocations[operands[id].allocation.get()].clone(), false)
                );
            } else {
                self.saved_variables.last_mut().unwrap().insert(
                    id, (allocation.clone(), true)
                );
            }
        }
    }

    pub fn enter_block(&mut self, operands : &Vec<Operand>) {
        let mut saved = self.saved_variables.pop().unwrap();
        for (id, (_, is_free)) in &mut saved {
            *is_free = self.is_free(&operands[*id]);
        }
        self.saved_variables.push(saved);
        self.load_state(operands);
    }

    pub fn load_state(&mut self, operands : &Vec<Operand>) {
        let to_restore = self.saved_variables.pop().unwrap();
        for (id, (allocation, _)) in &to_restore {
            let cur_allocation = operands[*id].allocation.get();
            if !self.is_free(&operands[*id]) && self.allocations[cur_allocation].ne(allocation) {
                self.restore_allocation(cur_allocation, allocation);
            }
        }
        for (&id, (allocation, is_free)) in &to_restore {
            let index = operands[id].allocation.get();
            if *is_free {
                if !self.allocations[index].is_free() {
                    self.free_allocation(index, false);
                }
            } else {
                self.allocations[index] = allocation.clone();
                if let MemoryAllocation::Register(reg, _) = &self.allocations[index] {
                    self.registers[*reg].operand.replace(index);
                }
            }
        }
        self.saved_variables.push(to_restore);
    }

    pub fn pop_state(&mut self) {
        self.saved_variables.pop().unwrap();
    }


    fn restore_allocation(&mut self, cur_allocation : usize, target_allocation : &MemoryAllocation) {
        let val = self.get_val(cur_allocation);
        match &target_allocation {
            MemoryAllocation::Register(index, size) => {
                if let Some(all) = self.registers[*index].operand {
                    let target = self.get_val(all);
                    let scratch_size = self.allocations[all].size();
                    if let Some(i) = Self::get_register(self.free_gen) {
                        self.registers[i].operand.replace(all);
                        self.allocations[all] = MemoryAllocation::Register(i, scratch_size);
                    } else {
                        let mem = self.get_memory(scratch_size);
                        self.allocations[all] = MemoryAllocation::Memory(mem, scratch_size);
                    };
                    let scratch = self.get_val(all);
                    self.add_instruction(OperationType::Mov, scratch_size,
                                         vec![scratch, target.clone()]);
                    self.add_instruction(OperationType::Mov, *size,
                                         vec![target, val]);
                    self.free_allocation(cur_allocation, true);
                    self.allocations[cur_allocation] = target_allocation.clone();
                } else {
                    self.free_allocation(cur_allocation, true);
                    self.allocations[cur_allocation] = target_allocation.clone();
                    self.registers[*index].operand.replace(cur_allocation);
                    self.add_instruction(
                        OperationType::Mov,
                        *size, vec![self.get_val(cur_allocation), val]);
                }
            }
            MemoryAllocation::Memory(_, size) => {
                debug_assert!(
                    if let MemoryAllocation::Register(..) = self.allocations[cur_allocation] {
                        true
                    } else {
                        false
                    }
                );
                self.free_allocation(cur_allocation, true);
                self.allocations[cur_allocation] = target_allocation.clone();
                self.add_instruction(OperationType::Mov,
                                     *size, vec![self.get_val(cur_allocation), val]);
            }
            _ => panic!("Invalid location")
        }
    }

    pub fn leave_block(&mut self, operands : &Vec<Operand>, has_next : bool) {
        let to_restore = self.saved_variables.pop().unwrap();
        for (&id, (allocation, _ )) in &to_restore {
            let cur_allocation = operands[id].allocation.get();
            if !self.is_free(&operands[id]) && self.allocations[cur_allocation].ne(allocation) {
                self.restore_allocation(cur_allocation, allocation);
            }
        }
        self.saved_variables.push(to_restore);
    }



}