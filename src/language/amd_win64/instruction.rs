use std::cell::Cell;
use super::registers::*;
use crate::language::types::Type;

#[derive(Debug, Copy, Clone)]
pub enum BasicOperation {
    IMul, Mul, Add, Sub, IDiv, Div, Mov, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
    SetGE, SetLE, And, Or, Xor, MovRet
}

impl BasicOperation {
    // Returns true if swapping first and second operand does not affect the result
    pub fn is_symmetric(&self) -> bool {
        match self {
            BasicOperation::IMul | BasicOperation::Mul | BasicOperation::Add |
            BasicOperation::And | BasicOperation::Or | BasicOperation::Xor => true,

            BasicOperation::Sub | BasicOperation::IDiv | BasicOperation::Div |
            BasicOperation::Mov | BasicOperation::Push | BasicOperation::Pop |
            BasicOperation::Cmp | BasicOperation::SetE | BasicOperation::SetNe |
            BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
            BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
            BasicOperation::SetGE | BasicOperation::SetLE | BasicOperation::MovRet => false,
        }
    }

    pub fn bitmap_hint(&self, n : usize) -> u64 {
        match n {
            0 => self.first_bitmap(),
            1 => self.second_bitmap(),
            _ => panic!("To many operands")
        }
    }

    // Returns a bitmap to all operands that are overridden by the operation
    // 00000001 means first, 00000010 means second, 00000101 means first and third etc
    pub fn destroyed(&self) -> u8 {
        match self {
            BasicOperation::Cmp | BasicOperation::Push |
            BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
            BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
            BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
            BasicOperation::SetGE | BasicOperation::SetLE => 0,

            BasicOperation::Mov | BasicOperation::MovRet |BasicOperation::IMul |
            BasicOperation::Mul | BasicOperation::Add | BasicOperation::Sub |
            BasicOperation::IDiv | BasicOperation::Div | BasicOperation::Or |
            BasicOperation::And | BasicOperation::Xor => 1
        }
    }

    pub fn first_bitmap(&self) -> u64 {
        match self {
            BasicOperation::Div | BasicOperation::IDiv |
            BasicOperation::Mul | BasicOperation::MovRet => RAX,

            BasicOperation::IMul | BasicOperation::Add | BasicOperation::Sub |
            BasicOperation::Mov | BasicOperation::And | BasicOperation::Or |
            BasicOperation::Xor | BasicOperation::Cmp  => MEM_GEN_REG,

            BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
            BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
            BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
            BasicOperation::SetGE | BasicOperation::SetLE => GEN_REG
        }
    }

    pub fn second_bitmap(&self) -> u64 {
        match self {
            BasicOperation::IDiv | BasicOperation::Div => MEM_GEN_REG & !RDX,
            BasicOperation::Mul | BasicOperation::IMul => MEM_GEN_REG,
            BasicOperation::Add | BasicOperation::Sub | BasicOperation::Cmp |
            BasicOperation::And | BasicOperation::Or | BasicOperation::Xor =>
                MEM_GEN_REG | IMM32 | IMM32 | IMM16 | IMM8,
            BasicOperation::Mov => MEM_GEN_REG | IMM64 | IMM32 | IMM32 | IMM16 | IMM8,
            BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
            BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE | BasicOperation::SetBE |
            BasicOperation::SetG | BasicOperation::SetL | BasicOperation::SetGE |
            BasicOperation::SetLE | BasicOperation::MovRet => panic!("No second operand")
        }
    }

    pub fn next_bitmap(&self, first_map: u64, n : usize) -> u64 {
        debug_assert!(first_map.count_ones() == 1);
        match n {
            0 => self.first_bitmap(),
            1 => ((first_map ^ MEM) | !MEM) & self.second_bitmap(),
            _ => panic!("Too many operands")
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum OperandSize {
    BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
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

#[derive(Debug)]
pub struct Operand {
    pub id : Cell<usize>, // Index of allocated MemoryLocation
    pub size : OperandSize,
    last_use : Cell<usize> // Index of the last usage of this operand in instructions vector
}


impl Operand {
    pub fn new(id : usize, size : OperandSize) -> Operand {
        Operand {
            id : Cell::new(id), last_use : Cell::new(0), size
        }
    }

    pub fn local(size : OperandSize) -> Operand {
        Self::new(0, size)
    }

    pub fn add_use(&self, index : usize) {
        self.last_use.replace(index);
    }

    pub(crate) fn used_after(&self, index: usize) -> bool {
        index < self.last_use.get()
    }

    pub(crate) fn merge_into(&self, other: &Operand) {
        other.id.replace(self.id.get());
        self.last_use.replace(other.last_use.get());
    }
}

#[derive(Debug, Clone)]
pub struct Instruction <'a> {
    pub operator : BasicOperation,
    pub operands : Vec<&'a Operand>,
    pub dest : Option<&'a Operand> // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
}


impl <'a> Instruction<'a> {
    pub fn new(operator : BasicOperation, operands : Vec<&'a Operand>, dest : Option<&'a Operand>) -> Instruction<'a> {
        Instruction {
            operator, operands, dest
        }
    }
}