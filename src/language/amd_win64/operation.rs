use std::cell::Cell;
use crate::language::amd_win64::instruction::InstructionOperand::Imm;
use crate::language::types::Type;
use super::registers::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum OperandSize {
    BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum OperationType {
    IMul, Mul, IDiv, Div,
    Add, Sub, And, Or, Xor,
    Push, Pop,
    Cmp,
    SetE, SetNE, SetA, SetB, SetAE, SetBE, SetG, SetL, SetGE, SetLE,
    JmpE, JmpNE, JmpA, JmpB, JmpAE, JmpBE, JmpG, JmpL, JmpGE, JmpLE, Jmp,
    JmpNop, // A nop used for inverting Jmp
    Mov,
    MovRet, Ret
}

impl OperationType {

    pub fn bitmap_hint(&self, n : usize, size : OperandSize) -> u64 {
        match n {
            0 => self.first_bitmap(size),
            1 => self.second_bitmap(size),
            _ => panic!("To many operands")
        }
    }

    pub fn inverse(&self) -> Self {
        match self {
            OperationType::JmpE => OperationType::JmpNE,
            OperationType::JmpNE => OperationType::JmpE,
            OperationType::JmpA => OperationType::JmpBE,
            OperationType::JmpB => OperationType::JmpAE,
            OperationType::JmpAE => OperationType::JmpB,
            OperationType::JmpBE => OperationType::JmpA,
            OperationType::JmpG => OperationType::JmpLE,
            OperationType::JmpL => OperationType::JmpGE,
            OperationType::JmpGE => OperationType::JmpL,
            OperationType::JmpLE => OperationType::JmpG,
            OperationType::Jmp => OperationType::JmpNop,
            OperationType::JmpNop => OperationType::Jmp,
            _ => panic!("Not a jump instruction")
        }
    }

    // Returns a bitmap to all operands that are overridden by the operation
    // 00000001 means first, 00000010 means second, 00000101 means first and third etc
    // Does not include dest.
    pub fn destroyed(&self) -> u8 {
        match self {
            OperationType::Cmp | OperationType::Push |
            OperationType::Pop | OperationType::SetE | OperationType::SetNE |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE |
            OperationType::SetBE | OperationType::SetG | OperationType::SetL |
            OperationType::SetGE | OperationType::SetLE | OperationType::JmpLE |
            OperationType::JmpE | OperationType::JmpNE | OperationType::JmpA |
            OperationType::JmpB | OperationType::JmpAE | OperationType::JmpBE |
            OperationType::JmpG | OperationType::JmpL | OperationType::JmpGE |
            OperationType::JmpNop | OperationType::Jmp => 0,

            OperationType::Mov | OperationType::MovRet | OperationType::IMul |
            OperationType::Mul | OperationType::Add | OperationType::Sub |
            OperationType::IDiv | OperationType::Div | OperationType::Or |
            OperationType::And | OperationType::Xor => 1,

            OperationType::Ret => panic!("Bad state")
        }
    }

    // Return if the instruction destroys operand at index.
    // Operands here means operands as given to
    pub fn is_destroyed(&self, index : usize) -> bool {
        match self {
            OperationType::IMul | OperationType::Mul | OperationType::IDiv |
            OperationType::Div | OperationType::Add | OperationType::Sub |
            OperationType::And | OperationType::Or | OperationType::Xor |
            OperationType::Pop | OperationType::SetE | OperationType::SetNE |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE |
            OperationType::SetBE | OperationType::SetG | OperationType::SetL |
            OperationType::SetGE | OperationType::SetLE | OperationType::Mov => index == 1,
            OperationType::Cmp | OperationType::Push | OperationType::JmpE |
            OperationType::JmpNE | OperationType::JmpA | OperationType::JmpB |
            OperationType::JmpAE | OperationType::JmpBE | OperationType::JmpG |
            OperationType::JmpL | OperationType::JmpGE | OperationType::JmpLE |
            OperationType::Jmp | OperationType::JmpNop | OperationType::MovRet => false,
            // MovRet is wierd.. but it does not matter since RAX is volatile
            OperationType::Ret => panic!("No operands")
        }
    }

    pub fn invalidations(&self, size : OperandSize) -> u64 {
        match self {
            OperationType::IMul => if size == OperandSize::BYTE { RAX } else { 0 },
            OperationType::Mul => if size == OperandSize::BYTE { RAX } else {RDX | RAX},
            OperationType::IDiv | OperationType::Div => RDX | RAX,
            OperationType::Add | OperationType::Sub | OperationType::And |
            OperationType::Or | OperationType::Xor | OperationType::Push |
            OperationType::Pop | OperationType::Cmp | OperationType::SetE |
            OperationType::SetNE | OperationType::SetA | OperationType::SetB |
            OperationType::SetAE | OperationType::SetBE | OperationType::SetG |
            OperationType::SetL | OperationType::SetGE | OperationType::SetLE |
            OperationType::JmpE | OperationType::JmpNE | OperationType::JmpA |
            OperationType::JmpB | OperationType::JmpAE | OperationType::JmpBE |
            OperationType::JmpG | OperationType::JmpL | OperationType::JmpGE |
            OperationType::JmpLE | OperationType::Jmp | OperationType::JmpNop |
            OperationType::Mov | OperationType::MovRet | OperationType::Ret => 0,
        }
    }

    pub fn first_bitmap(&self, size : OperandSize) -> u64 {
        match self {
            OperationType::Div | OperationType::IDiv |
            OperationType::Mul => RAX,

            OperationType::IMul if size == OperandSize::BYTE => RAX,

            OperationType::IMul | OperationType::Add | OperationType::Sub |
            OperationType::And | OperationType::Or | OperationType::Xor |
            OperationType::Cmp  => MEM_GEN_REG,

            OperationType::Push | OperationType::SetE | OperationType::SetNE |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE |
            OperationType::SetBE | OperationType::SetG | OperationType::SetL |
            OperationType::SetGE | OperationType::SetLE => GEN_REG,

            OperationType::JmpE | OperationType::JmpNE | OperationType::JmpA |
            OperationType::JmpB | OperationType::JmpAE | OperationType::JmpBE |
            OperationType::JmpG | OperationType::JmpL | OperationType::JmpGE |
            OperationType::JmpLE | OperationType::JmpNop => IMM64,

            OperationType::Mov | OperationType::MovRet => MEM_GEN_REG | IMM64,

            OperationType::Jmp => GEN_REG | IMM64,

            OperationType::Ret | OperationType::Pop  => panic!("No first operand")
        }
    }

    pub fn second_bitmap(&self, _size : OperandSize) -> u64 {
        match self {
            OperationType::IDiv | OperationType::Div => MEM_GEN_REG & !RDX,
            OperationType::Mul | OperationType::IMul => MEM_GEN_REG,
            OperationType::Add | OperationType::Sub | OperationType::Cmp |
            OperationType::And | OperationType::Or | OperationType::Xor =>
                MEM_GEN_REG | IMM32 | IMM32 | IMM16 | IMM8,
            OperationType::Mov => MEM_GEN_REG | IMM64 | IMM32 | IMM32 | IMM16 | IMM8,
            OperationType::Push | OperationType::Pop | OperationType::SetE | OperationType::SetNE |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE | OperationType::SetBE |
            OperationType::SetG | OperationType::SetL | OperationType::SetGE |
            OperationType::SetLE | OperationType::MovRet |OperationType::JmpE |
            OperationType::JmpNE | OperationType::JmpA | OperationType::JmpB |
            OperationType::JmpAE | OperationType::JmpBE | OperationType::JmpG |
            OperationType::JmpL | OperationType::JmpGE | OperationType::JmpLE |
            OperationType::Jmp | OperationType::JmpNop | OperationType::Ret => panic!("No second operand")
        }
    }

    pub fn next_bitmap(&self, first_map: u64, n : usize, size : OperandSize) -> u64 {
        debug_assert!(first_map.count_ones() == 1);
        match n {
            0 => self.first_bitmap(size),
            1 => ((first_map ^ MEM) | !MEM) & self.second_bitmap(size),
            _ => panic!("Too many operands")
        }
    }
}

impl OperandSize {
    pub fn to_imm(&self) -> u64 {
        match self {
            OperandSize::BYTE => IMM8,
            OperandSize::WORD => IMM16,
            OperandSize::DWORD => IMM32,
            OperandSize::QWORD => IMM64
        }
    }
}

impl From<Type> for OperandSize {
    fn from(t: Type) -> Self {
        match t {
            Type::U32 | Type::S32 => OperandSize::DWORD,
            Type::Bool => OperandSize::BYTE,
            Type::Plain { .. } => todo!("Not yet implemented"),
            Type::AnyInt | Type::Any => panic!("Not proper type {}", t)
        }
    }
}

pub struct IdTracker {
    count: usize
}

impl IdTracker {
    pub fn new() -> IdTracker {
        IdTracker {count : 0}
    }

    pub fn get_id(&mut self) -> usize {
        self.count += 1;
        self.count - 1
    }
}


#[derive(Debug)]
pub struct Operand {
    pub allocation: Cell<usize>, // Index of allocated MemoryLocation
    pub size : OperandSize,
    pub hint : u64,
    pub home : Option<usize>,
    pub last_use : Option<usize>
}


impl Operand {
    pub fn new(size : OperandSize) -> Operand {
        Operand {
            allocation: Cell::new(0),
            size,
            hint : u64::MAX, // Full bitmap to allow bitwise and
            home : None,
            last_use : None
        }
    }

    pub(crate) fn merge_into(&self, other: &Operand) {
        other.allocation.replace(self.allocation.get());
        self.allocation.replace(0);
    }
}

#[derive(Debug, Clone)]
pub struct Operation {
    pub operator : OperationType,
    pub operands : Vec<usize>,
    pub dest : Option<usize>, // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
    pub invalidations : u64
}


impl Operation {
    pub fn new(operator : OperationType, operands : Vec<usize>, dest : Option<usize>, invalidations : u64) -> Operation {
        Operation {
            operator, operands, dest, invalidations
        }
    }
}