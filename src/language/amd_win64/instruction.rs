use std::cell::Cell;
use std::collections::HashMap;
use std::ops::Index;
use crate::language::amd_win64::instruction::OperandType::Reg;
use super::registers::*;
use crate::language::types::Type;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BasicOperation {
    IMul, Mul, Add, Sub, IDiv, Div, Mov, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
    SetGE, SetLE, And, Or, Xor, MovRet
}

impl BasicOperation {

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

    pub fn first_bitmap(&self, size : OperandSize) -> u64 {
        match self {
            BasicOperation::Div | BasicOperation::IDiv |
            BasicOperation::Mul | BasicOperation::MovRet => RAX,

            BasicOperation::IMul if size == OperandSize::BYTE => RAX,

            BasicOperation::IMul | BasicOperation::Add | BasicOperation::Sub |
            BasicOperation::Mov | BasicOperation::And | BasicOperation::Or |
            BasicOperation::Xor | BasicOperation::Cmp  => MEM_GEN_REG,

            BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
            BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
            BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
            BasicOperation::SetGE | BasicOperation::SetLE => GEN_REG
        }
    }

    pub fn second_bitmap(&self, _size : OperandSize) -> u64 {
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

    pub fn next_bitmap(&self, first_map: u64, n : usize, size : OperandSize) -> u64 {
        debug_assert!(first_map.count_ones() == 1);
        match n {
            0 => self.first_bitmap(size),
            1 => ((first_map ^ MEM) | !MEM) & self.second_bitmap(size),
            _ => panic!("Too many operands")
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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

#[macro_export]
macro_rules! mnemonic {
    ($e:expr) => {
        $e as u64
    };
    ($e:expr $(, $more:expr)*) => {
      ((mnemonic!($($more),*)) << 8) | ($e as u64)
    };
}

type Identifier = (BasicOperation, OperandSize, u64);

fn size_repeat_instr(map : &mut HashMap<Identifier, Vec<Mnemonic>>, op : BasicOperation, id : u64, mnemonics : Vec<Mnemonic>) {
    map.insert((op, OperandSize::WORD, id),mnemonics.clone());
    let mut word_mnemonics = mnemonics.clone();
    let index = mnemonics.iter().enumerate().find(|(_, &mn)| mn.is_opcode()).unwrap().0;
    word_mnemonics.insert(index, Mnemonic::Prefix(0x66));
    let mut qword_mnemonics = mnemonics.clone();
    qword_mnemonics.insert(index, Mnemonic::Rex(0x48));
    map.insert((op, OperandSize::WORD, id), word_mnemonics);
    map.insert((op, OperandSize::DWORD, id), mnemonics);
    map.insert((op, OperandSize::QWORD, id), qword_mnemonics);
}

fn reg_rm_instr(map : &mut HashMap<Identifier, Vec<Mnemonic>>, op : BasicOperation, opcode : Mnemonic) {
    size_repeat_instr(map, op, mnemonic!(OperandType::Reg, OperandType::Reg),
        vec![opcode, Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::RmReg]
    );
    size_repeat_instr(map, op, mnemonic!(OperandType::Reg, OperandType::Mem),
        vec![opcode, Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::Mem]
    );
}

fn rm_reg_instr(map : &mut HashMap<Identifier, Vec<Mnemonic>>, op : BasicOperation, opcode : Mnemonic) {
    size_repeat_instr(map, op, mnemonic!(OperandType::Mem, OperandType::Reg),
        vec![opcode, Mnemonic::ModRm(0), Mnemonic::Mem, Mnemonic::RegReg]
    );
}

fn rm_imm_instr(map : &mut HashMap<Identifier, Vec<Mnemonic>>, op : BasicOperation, opcode : Mnemonic, im : u8) {
    map.insert(
        (op, OperandSize::WORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Mnemonic::Prefix(0x66), opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::WORD)]
    );
    map.insert(
        (op, OperandSize::WORD, mnemonic!(OperandType::Mem, OperandType::Imm)),
        vec![Mnemonic::Prefix(0x66), opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::WORD)]
    );

    map.insert(
        (op, OperandSize::DWORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::DWORD)]
    );
    map.insert(
        (op, OperandSize::DWORD, mnemonic!(OperandType::Mem, OperandType::Imm)),
        vec![opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::DWORD)]
    );

    map.insert(
        (op, OperandSize::QWORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Mnemonic::Rex(0x48), opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::DWORD)]
    );
    map.insert(
        (op, OperandSize::QWORD, mnemonic!(OperandType::Mem, OperandType::Imm)),
        vec![Mnemonic::Rex(0x48), opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::DWORD)]
    );
}

fn standard_instr(map :&mut HashMap<Identifier, Vec<Mnemonic>>, op : BasicOperation,
    rm_r8 : u8, r_rm8 : u8, rm_i8 : u8, rm_r : u8, r_rm : u8, rm_i : u8, im : u8
) {
    reg_rm_instr(map, op, Mnemonic::Opcode(r_rm));
    rm_reg_instr(map, op, Mnemonic::Opcode(rm_r));
    rm_imm_instr(map, op, Mnemonic::Opcode(rm_i), im);
    map.insert(
        (op, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Mnemonic::Opcode(r_rm8), Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::RmReg]
    );
    map.insert(
        (op, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Mnemonic::Opcode(r_rm8), Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::Mem]
    );
    map.insert(
        (op, OperandSize::BYTE, mnemonic!(OperandType::Mem, OperandType::Reg)),
        vec![Mnemonic::Opcode(rm_r8), Mnemonic::ModRm(0), Mnemonic::Mem, Mnemonic::RegReg]
    );

    map.insert(
        (op, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Mnemonic::Opcode(rm_i8), Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::BYTE)]
    );
    map.insert(
        (op, OperandSize::BYTE, mnemonic!(OperandType::Mem, OperandType::Imm)),
        vec![Mnemonic::Opcode(rm_i8), Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::BYTE)]
    );
}



#[derive(Clone, Copy)]
enum Mnemonic {
    Prefix(u8),
    Value(u8), // Can be used for injecting extra instructions before / after main one
    Rex(u8),
    Opcode(u8),
    Opcode2(u8), // Opcode from secondary opcode map
    ModRm(u8),
    RegReg, // Register in ModRm.reg
    PlusReg, // Register in second opcode byte, has to come directly after Opcode / Opcode2
    Mem, // Memory in ModRm.r/m (+ potentially SIB)
    RmReg, //  Register in ModRm.r/m
    RegImm(OperandSize), //Immediate value
    Pass, // Ignore operand
}

impl Mnemonic {
    fn is_opcode(&self) -> bool {
        match self {
            Mnemonic::Opcode(_) | Mnemonic::Opcode2(_) => true,
            _ => false
        }
    }
}

#[repr(u8)]
pub enum OperandType {
    Reg = 0, Mem = 1, Imm = 2
}

pub struct InstructionCompiler {
    map : HashMap<(BasicOperation, OperandSize, u64), Vec<Mnemonic>>
}

impl InstructionCompiler {
    pub fn new() -> Self {
        InstructionCompiler {
            map : create_instruction_map()
        }
    }

    pub fn compile_instruction(&self, operand : BasicOperation, size : OperandSize, mnemonic : u64, iter : &mut dyn Iterator<Item=u64>, res : &mut Vec<u8>) {
        let start = res.len();
        let mut rex : Option<u8> = None;
        let mut opcode_index = 0;
        let mut mod_rm_index = 0;

        for op in self.map.get(&(operand, size, mnemonic)).unwrap() {
            match op {
                Mnemonic::Prefix(val) | Mnemonic::Value(val) => res.push(*val),
                Mnemonic::Rex(r) => rex = Some(*r),
                Mnemonic::Opcode(opcode) => {
                    opcode_index = res.len();
                    res.push(*opcode);
                },
                Mnemonic::Opcode2(opcode) => {
                    opcode_index = res.len();
                    res.push(0x0f); // Secondary opcode map escape
                    res.push(*opcode);
                }
                Mnemonic::ModRm(initial) => {
                    mod_rm_index = res.len();
                    res.push(*initial);
                },
                Mnemonic::RegReg => {
                    let reg = iter.next().unwrap() as u8;
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= (1 << 2);
                    }
                    res[mod_rm_index] |= ((reg & 0b111) << 3);
                },
                Mnemonic::PlusReg => {
                    let reg = iter.next().unwrap() as u8;
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= 1;
                    }
                    // PlusReg always follows Opcode / Opcode2
                    let len = res.len();
                    res[len - 1] |= (reg & 0b111);
                }
                Mnemonic::Mem => {
                    let offset = iter.next().unwrap() as u32; // Offset from rsp
                    res.push(0x24); // SIB base rsp scale 1, index 4
                    let mod_rm_mask = match offset {
                        0 => 0b00000100, // ModRm.md = 00 (no offset), ModRm.r/m = 100 (address by SIB)
                        1..=255 => {
                            res.push(offset as u8);
                            0b01000100  // ModRm.md = 01 (8-bit offset), ModRm.r/m = 100 (address by SIB)
                        },
                        256.. => {
                            res.extend_from_slice(&offset.to_le_bytes());
                            0b10000100
                        }, // ModRm.md = 10 (32-bit offset), ModRm.r/m = 100 (address by SIB)
                    };
                    res[mod_rm_index] = (res[mod_rm_index] & 0b00111000) | mod_rm_mask;

                }
                Mnemonic::RmReg => {
                    let reg = iter.next().unwrap() as u8;
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= 1;
                    }
                    res[mod_rm_index] |= 0b11000000 | (reg & 0b111);
                }
                Mnemonic::RegImm(size) => {
                    let imm : &[u8; 8] = &iter.next().unwrap().to_le_bytes();
                    res.extend_from_slice(&imm[0..(*size as usize)]);
                },
                Mnemonic::Pass => {
                    iter.next().unwrap();
                }
            }
        }
        if let Some(rex) = rex {
            res.insert(opcode_index, rex);
        }
        for b in &res[start..] {
            print!("{:02X}", b);
        }
        println!();
    }
}

fn create_instruction_map() -> HashMap<(BasicOperation, OperandSize, u64), Vec<Mnemonic>> {
    use Mnemonic::*;
    let mut map = HashMap::new();
    map.insert(
        (BasicOperation::IMul, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Opcode(0xF6), ModRm(5 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::IMul, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Opcode(0xF6), ModRm(5 << 3), Pass, Mem]
    );
    reg_rm_instr(&mut map, BasicOperation::IMul, Opcode2(0xAF));
    map.insert(
        (BasicOperation::Mul, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Opcode(0xF6), ModRm(4 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::Mul, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Opcode(0xF6), ModRm(4 << 3), Pass, Mem]
    );
    size_repeat_instr(&mut map, BasicOperation::Mul, mnemonic!(OperandType::Reg, OperandType::Reg),
        vec![Opcode(0xF7), ModRm(4 << 3), Pass, RmReg]
    );
    size_repeat_instr(&mut map, BasicOperation::Mul, mnemonic!(OperandType::Reg, OperandType::Mem),
        vec![Opcode(0xF7), ModRm(4 << 3), Pass, Mem]
    );
    standard_instr(&mut map, BasicOperation::Add, 0x00, 0x02, 0x80, 0x01, 0x03, 0x81, 0);
    standard_instr(&mut map, BasicOperation::Sub, 0x28, 0x2A, 0x80, 0x29, 0x2B, 0x81, 5);

    // Value(0x98 / 0x99) is a sign-extend instruction
    map.insert(
        (BasicOperation::IDiv, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Value(0x66), Value(0x98), Opcode(0xF6), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Value(0x66), Value(0x98), Value(0xF6), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::WORD, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Value(0x66), Value(0x99), Prefix(0x66), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::WORD, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Value(0x66), Value(0x99), Prefix(0x66), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::DWORD, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Value(0x99), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::DWORD, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Value(0x99), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::QWORD, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Value(0x48), Value(0x99), Rex(0x48), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::IDiv, OperandSize::QWORD, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Value(0x48), Value(0x99), Rex(0x48), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );

    // 0x31 0xD2 zeros the RDX-register (xor rdx, rdx)
    map.insert(
        (BasicOperation::Div, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Reg)),
        vec![Value(0x31), Value(0xD2), Opcode(0xF6), ModRm(6 << 3), Pass, RmReg]
    );
    map.insert(
        (BasicOperation::Div, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Mem)),
        vec![Value(0x31), Value(0xD2), Value(0xF6), ModRm(6 << 3), Pass, Mem]
    );
    size_repeat_instr(&mut map, BasicOperation::Div, mnemonic!(OperandType::Reg, OperandType::Reg),
                      vec![Value(0x31), Value(0xD2), Opcode(0xF7), ModRm(6 << 3), Pass, RmReg]
    );
    size_repeat_instr(&mut map, BasicOperation::Div, mnemonic!(OperandType::Reg, OperandType::Mem),
                      vec![Value(0x31), Value(0xD2), Opcode(0xF7), ModRm(6 << 3), Pass, Mem]
    );

    standard_instr(&mut map, BasicOperation::Mov, 0x88, 0x8A, 0xC6, 0x89, 0x8B, 0xC7, 0);
    map.insert(
        (BasicOperation::Mov, OperandSize::BYTE, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Opcode(0xB0), PlusReg, RegImm(OperandSize::BYTE)]
    );
    map.insert(
        (BasicOperation::Mov, OperandSize::WORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Prefix(0x66), Opcode(0xB8), PlusReg, RegImm(OperandSize::WORD)]
    );
    map.insert(
        (BasicOperation::Mov, OperandSize::DWORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Opcode(0xB8), PlusReg, RegImm(OperandSize::DWORD)]
    );
    map.insert(
        (BasicOperation::Mov, OperandSize::QWORD, mnemonic!(OperandType::Reg, OperandType::Imm)),
        vec![Rex(0x48), Opcode(0xB8), PlusReg, RegImm(OperandSize::QWORD)]
    );
    standard_instr(&mut map, BasicOperation::Cmp, 0x38, 0x3A, 0x80, 0x39, 0x3B, 0x81, 7);
    for (opcode, op) in vec![
        (0x94, BasicOperation::SetE), (0x95, BasicOperation::SetNe), (0x97, BasicOperation::SetA),
        (0x92, BasicOperation::SetB), (0x93, BasicOperation::SetAE), (0x96, BasicOperation::SetBE),
        (0x9F, BasicOperation::SetG), (0x9C, BasicOperation::SetL), (0x9D, BasicOperation::SetGE),
        (0x9E, BasicOperation::SetLE),
    ] {
        map.insert(
            (op, OperandSize::BYTE, mnemonic!(OperandType::Reg)),
            vec![Opcode2(opcode), ModRm(0), RmReg]
        );
        map.insert(
            (op, OperandSize::BYTE, mnemonic!(OperandType::Mem)),
            vec![Opcode2(opcode), ModRm(0), RmReg]
        );
    }

    standard_instr(&mut map, BasicOperation::And, 0x20, 0x22, 0x80, 0x21, 0x23, 0x81, 4);
    standard_instr(&mut map, BasicOperation::Or, 0x08, 0x0A, 0x80, 0x09, 0x0B, 0x81, 1);
    standard_instr(&mut map, BasicOperation::Xor, 0x30, 0x32, 0x80, 0x31, 0x33, 0x81, 6);
    for size in vec![OperandSize::BYTE, OperandSize::WORD, OperandSize::DWORD, OperandSize::QWORD] {
        map.insert( // MovRet does nothing itself, only makes sure the return value is moved to rax.
            (BasicOperation::MovRet, size, mnemonic!(OperandType::Reg)),
            vec![]
        );
    }
    map
}