use std::cell::Cell;
use crate::language::types::Type;
use super::registers::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum OperandSize {
    BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum OperationType {
    IMul, Mul, Add, Sub, IDiv, Div, Mov, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
    SetGE, SetLE, And, Or, Xor, MovRet
}

impl OperationType {

    pub fn bitmap_hint(&self, n : usize, size : OperandSize) -> u64 {
        match n {
            0 => self.first_bitmap(size),
            1 => self.second_bitmap(size),
            _ => panic!("To many operands")
        }
    }

    // Returns a bitmap to all operands that are overridden by the operation
    // 00000001 means first, 00000010 means second, 00000101 means first and third etc
    pub fn destroyed(&self) -> u8 {
        match self {
            OperationType::Cmp | OperationType::Push |
            OperationType::Pop | OperationType::SetE | OperationType::SetNe |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE |
            OperationType::SetBE | OperationType::SetG | OperationType::SetL |
            OperationType::SetGE | OperationType::SetLE => 0,

            OperationType::Mov | OperationType::MovRet | OperationType::IMul |
            OperationType::Mul | OperationType::Add | OperationType::Sub |
            OperationType::IDiv | OperationType::Div | OperationType::Or |
            OperationType::And | OperationType::Xor => 1
        }
    }

    pub fn first_bitmap(&self, size : OperandSize) -> u64 {
        match self {
            OperationType::Div | OperationType::IDiv |
            OperationType::Mul | OperationType::MovRet => RAX,

            OperationType::IMul if size == OperandSize::BYTE => RAX,

            OperationType::IMul | OperationType::Add | OperationType::Sub |
            OperationType::Mov | OperationType::And | OperationType::Or |
            OperationType::Xor | OperationType::Cmp  => MEM_GEN_REG,

            OperationType::Push | OperationType::Pop | OperationType::SetE | OperationType::SetNe |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE |
            OperationType::SetBE | OperationType::SetG | OperationType::SetL |
            OperationType::SetGE | OperationType::SetLE => GEN_REG
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
            OperationType::Push | OperationType::Pop | OperationType::SetE | OperationType::SetNe |
            OperationType::SetA | OperationType::SetB | OperationType::SetAE | OperationType::SetBE |
            OperationType::SetG | OperationType::SetL | OperationType::SetGE |
            OperationType::SetLE | OperationType::MovRet => panic!("No second operand")
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
            Type::AnyInt | Type::Any => panic!("Not proper type")
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
    pub id : usize,
    //last_use : Cell<usize> // Index of the last usage of this operand in instructions vector
}


impl Operand {
    pub fn new(tracker : &mut IdTracker, size : OperandSize) -> Operand {
        Operand {
            allocation: Cell::new(0), size, id : tracker.get_id()
        }
    }

    pub fn local(tracker : &mut IdTracker, size : OperandSize) -> Operand {
        Self::new(tracker, size)
    }

    /*pub fn add_use(&self, index : usize) {
        self.last_use.replace(index);
    }*/

    /*pub(crate) fn used_after(&self, index: usize) -> bool {
        false//index < self.last_use.get()
    }*/

    pub(crate) fn merge_into(&self, other: &Operand) {
        other.allocation.replace(self.allocation.get());
    }
}

#[derive(Debug, Clone)]
pub struct Operation<'a> {
    pub operator : OperationType,
    pub operands : Vec<&'a Operand>,
    pub dest : Option<&'a Operand> // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
}


impl <'a> Operation<'a> {
    pub fn new(operator : OperationType, operands : Vec<&'a Operand>, dest : Option<&'a Operand>) -> Operation<'a> {
        Operation {
            operator, operands, dest
        }
    }
}