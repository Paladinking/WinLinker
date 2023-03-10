use super::instruction::{Operand, OperandSize};

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

struct Register {
    bitmap : u64, operand : Option<usize>
}

impl Register {
    fn new_general(bitmap : u64) -> Register {
        Register {bitmap, operand : None}
    }
}

pub struct RegisterState {
    registers : Vec<Register>,
    memory : Vec<bool>,
    allocations : Vec<MemoryAllocation>,
    free_gen : u64
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


#[derive(Debug, Clone, Copy)]
enum MemoryAllocation {
    Register(usize), Memory(usize), Immediate(u64, u64),
    Hint(u64), None
}

impl RegisterState {
    const VOLATILE_REGISTERS : usize = 7;
    const GENERAL_REGISTERS : usize = 15;

    pub fn new() -> RegisterState {
        let registers = vec![
            Register::new_general(RCX),
            Register::new_general(RDX),
            Register::new_general(RAX),
            Register::new_general(R8),
            Register::new_general(R9),
            Register::new_general(R10),
            Register::new_general(R11),
            Register::new_general(RBX),
            Register::new_general(RBP),
            Register::new_general(RDI),
            Register::new_general(RSI),
            Register::new_general(R12),
            Register::new_general(R13),
            Register::new_general(R14),
            Register::new_general(R15)
        ];
        let allocations = vec![MemoryAllocation::None];
        RegisterState {
            registers, memory : Vec::new(), allocations, free_gen : GEN_REG
        }
    }

    fn get_register(bitmap : u64) -> Option<usize> {
        Some(match bitmap & (u64::MAX << (63 - (bitmap | 1).leading_zeros())) {
            RCX => 0, RDX => 1, RAX => 2,
            R8 => 3, R9 => 4, R10 => 5,
            R11 => 6, RBX => 7, RBP => 8,
            RDI => 9, RSI => 10, R12 => 11,
            R13 => 12, R14 => 13, R15 => 14,
            0 => {return None;}
            _ => panic!("Bad bitmap")
        })
    }



    fn get_memory(&mut self) -> usize {
        for i in 0..self.memory.len() {
            if !self.memory[i] {
                self.memory[i] = true;
                return i;
            }
        }
        self.memory.push(true);
        return self.memory.len() - 1;
    }

    // Allocates a location for operand to a location contained in bitmap.
    // Potentially inserts a move to free a register, moving it to another
    // register or memory location. The destination of such a move will not be to
    // any location contained in invalidated.
    pub fn allocate(&mut self, operand : &Operand, bitmap : u64, invalidated : u64, invalid_soon : u64) -> u64 {
        fn allocate_reg(state : &mut RegisterState, index : usize, operand : &Operand) -> u64 {
            state.registers[index].operand = Some(state.allocations.len());
            operand.id.replace(state.allocations.len());
            state.allocations.push(MemoryAllocation::Register(index));
            state.free_gen &= !state.registers[index].bitmap;
            return state.registers[index].bitmap;
        }

        if let MemoryAllocation::Hint(map) = self.allocations[operand.id.get()] {
            if let Some(index) = Self::get_register(map & bitmap & self.free_gen & !invalid_soon) {
                return allocate_reg(self, index, operand);
            }
        }
        if let Some(index) = Self::get_register(bitmap & self.free_gen & !invalid_soon) {
            return allocate_reg(self, index, operand);
        }
        if let Some(index) = Self::get_register(bitmap & self.free_gen) {
            return allocate_reg(self, index, operand);
        }
        if bitmap & MEM != 0 {
            let index = self.get_memory();
            operand.id.replace(self.allocations.len());
            self.allocations.push(MemoryAllocation::Memory(index));
            return MEM;
        }
        let location = Self::get_register(bitmap).unwrap();
        let prev_owner = self.registers[location].operand.unwrap();
        let s = self.to_string(prev_owner);
        if let Some(reg) = Self::get_register(self.free_gen & !self.registers[location].bitmap & !invalidated) {
            // Insert Move
            self.allocations[prev_owner] = MemoryAllocation::Register(reg);
            self.registers[reg].operand = Some(prev_owner);
            self.free_gen &= !self.registers[reg].bitmap;
        } else {
            // Insert Move
            let pos = self.get_memory();
            self.allocations[prev_owner] = MemoryAllocation::Memory(pos);
        }
        println!("Move {}, {}", self.to_string(prev_owner), s);
        return allocate_reg(self, location, operand);
    }

    pub fn free(&mut self, operand : &Operand) {
        match self.allocations[operand.id.get()] {
            MemoryAllocation::Register(reg)=> {
                self.registers[reg].operand = None;
                self.free_gen |= self.registers[reg].bitmap;
            }
            MemoryAllocation::Memory(index) => {
                self.memory[index] = false;
            },
            MemoryAllocation::Immediate(..) => {},
            MemoryAllocation::Hint(_) | MemoryAllocation::None => panic!("Double free")
        }
        self.allocations[operand.id.get()] = MemoryAllocation::None;
    }

    pub fn invalidate_registers(&mut self, map : u64) {
        if map == 0 {
            return;
        }
        for i in 0..self.registers.len() {
            if self.registers[i].bitmap & map != 0 {
                if let Some(index) = self.registers[i].operand {
                    let s = self.to_string(index);
                    if let Some(reg) = Self::get_register(self.free_gen & !map) {
                        self.allocations[index] = MemoryAllocation::Register(reg);
                        self.registers[reg].operand = Some(index);
                        self.free_gen &= !self.registers[reg].bitmap;
                    } else {
                        let mem = self.get_memory();
                        self.allocations[index] = MemoryAllocation::Memory(mem);
                    }
                    println!("Move3 {}, {}", self.to_string(index), s);
                    self.registers[i].operand = None;
                    self.free_gen |= self.registers[i].bitmap;
                }
            }
        }
    }

    pub fn is_free(&self, operand : &Operand) -> bool {
        match self.allocations[operand.id.get()] {
            MemoryAllocation::Register(_) | MemoryAllocation::Memory(_) |
            MemoryAllocation::Immediate(_, _) => false,
            MemoryAllocation::Hint(_) | MemoryAllocation::None => true
        }
    }

    pub fn allocation_bitmap(&self, operand : &Operand) -> u64 {
        match self.allocations[operand.id.get()] {
            MemoryAllocation::Register(i) => self.registers[i].bitmap,
            MemoryAllocation::Memory(_) => MEM,
            MemoryAllocation::Immediate(_, bitmap) => bitmap,
            MemoryAllocation::Hint(_) |
            MemoryAllocation::None => panic!("Allocation bitmap on non-allocated operand")
        }
    }

    pub fn to_string(&self, id : usize) -> String {
        match self.allocations[id] {
            MemoryAllocation::Register(i) => {
                match self.registers[i].bitmap {
                    RCX => "rcx", RDX => "rdx", RAX => "rax",
                    R8 => "r8", R9 => "r9", R10 => "r10",
                    R11 => "r11", RBX => "rbx", RBP => "rbp",
                    RDI => "rdi", RSI => "rsi", R12 => "r12",
                    R13 => "r13", R14 => "r14", R15 => "r15",
                    _ => "???"
                }.to_owned()
            }
            MemoryAllocation::Memory(i) => {
                "mem(".to_owned() + &i.to_string() + ")"
            },
            MemoryAllocation::Immediate(i, _) => {
                i.to_string()
            },
            MemoryAllocation::Hint(_) => {
                "Hint".to_string()
            }
            MemoryAllocation::None => {
                "None".to_owned()
            }
        }
    }

    pub fn allocate_imm(&mut self, operand : &Operand, imm : u64, size : OperandSize) {
        self.allocations.push(MemoryAllocation::Immediate(imm, size.to_imm()));
        operand.id.replace(self.allocations.len() - 1);
    }

    pub fn allocate_hint(&mut self, operand : &Operand, hint : u64) -> bool {
        if let MemoryAllocation::None = self.allocations[operand.id.get()] {
            self.allocations.push(MemoryAllocation::Hint(hint));
            operand.id.replace(self.allocations.len() - 1);
            return true;
        } else if let MemoryAllocation::Hint(ref mut prev_hint) = self.allocations[operand.id.get()] {
            if *prev_hint & hint == 0 {
                self.allocations.push(MemoryAllocation::Hint(hint));
                operand.id.replace(self.allocations.len() - 1);
            } else {
                *prev_hint &= hint;
            }
            return true;
        }
        return false;
    }

    pub fn propagate_hint(&mut self, source : &Operand, dest : &Operand) {
        if let MemoryAllocation::None = self.allocations[dest.id.get()] {
            if let MemoryAllocation::Hint(_) = self.allocations[source.id.get()] {
                dest.id.replace(source.id.get());
            }
        }
    }
}