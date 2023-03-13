use std::cell::Cell;
use std::collections::HashMap;
use std::hash::Hash;
use derivative::Derivative;
use crate::language::amd_win64::operation::{OperationType, OperandSize};
use super::registers::*;



#[macro_export]
macro_rules! mnemonic {
    ($e:expr) => {
        $e as u64
    };
    ($e:expr $(, $more:expr)*) => {
      ((mnemonic!($($more),*)) << 8) | ($e as u64)
    };
}


fn size_repeat_instr(map : &mut HashMap<Instruction, Vec<Mnemonic>>, op : OperationType, operands : &[OperandType], mnemonics : Vec<Mnemonic>) {
    map.insert(Instruction::mnemonic(op, OperandSize::WORD, operands),mnemonics.clone());
    let mut word_mnemonics = mnemonics.clone();
    let index = mnemonics.iter().enumerate().find(|(_, &mn)| mn.is_opcode()).unwrap().0;
    word_mnemonics.insert(index, Mnemonic::Prefix(0x66));
    let mut qword_mnemonics = mnemonics.clone();
    qword_mnemonics.insert(index, Mnemonic::Rex(0x48));
    map.insert(Instruction::mnemonic(op, OperandSize::WORD, operands), word_mnemonics);
    map.insert(Instruction::mnemonic(op, OperandSize::DWORD, operands), mnemonics);
    map.insert(Instruction::mnemonic(op, OperandSize::QWORD, operands), qword_mnemonics);
}

fn reg_rm_instr(map : &mut HashMap<Instruction, Vec<Mnemonic>>, op : OperationType, opcode : Mnemonic) {
    size_repeat_instr(map, op, &[OperandType::Reg, OperandType::Reg],
                      vec![opcode, Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::RmReg]
    );
    size_repeat_instr(map, op, &[OperandType::Reg, OperandType::Mem],
        vec![opcode, Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::Mem]
    );
}

fn rm_reg_instr(map : &mut HashMap<Instruction, Vec<Mnemonic>>, op : OperationType, opcode : Mnemonic) {
    size_repeat_instr(map, op, &[OperandType::Mem, OperandType::Reg],
        vec![opcode, Mnemonic::ModRm(0), Mnemonic::Mem, Mnemonic::RegReg]
    );
}

fn rm_imm_instr(map : &mut HashMap<Instruction, Vec<Mnemonic>>, op : OperationType, opcode : Mnemonic, im : u8) {
    map.insert(
        Instruction::mnemonic(op, OperandSize::WORD, &[OperandType::Reg, OperandType::Imm]),
        vec![Mnemonic::Prefix(0x66), opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::WORD)]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::WORD, &[OperandType::Mem, OperandType::Imm]),
        vec![Mnemonic::Prefix(0x66), opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::WORD)]
    );

    map.insert(
        Instruction::mnemonic(op, OperandSize::DWORD, &[OperandType::Reg, OperandType::Imm]),
        vec![opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::DWORD)]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::DWORD, &[OperandType::Mem, OperandType::Imm]),
        vec![opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::DWORD)]
    );

    map.insert(
        Instruction::mnemonic(op, OperandSize::QWORD, &[OperandType::Reg, OperandType::Imm]),
        vec![Mnemonic::Rex(0x48), opcode, Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::DWORD)]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::QWORD, &[OperandType::Mem, OperandType::Imm]),
        vec![Mnemonic::Rex(0x48), opcode, Mnemonic::ModRm(im << 3), Mnemonic::Mem, Mnemonic::RegImm(OperandSize::DWORD)]
    );
}

fn standard_instr(map :&mut HashMap<Instruction, Vec<Mnemonic>>, op : OperationType,
                  rm_r8 : u8, r_rm8 : u8, rm_i8 : u8, rm_r : u8, r_rm : u8, rm_i : u8, im : u8
) {
    reg_rm_instr(map, op, Mnemonic::Opcode(r_rm));
    rm_reg_instr(map, op, Mnemonic::Opcode(rm_r));
    rm_imm_instr(map, op, Mnemonic::Opcode(rm_i), im);
    map.insert(
        Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Reg, OperandType::Reg]),
        vec![Mnemonic::Opcode(r_rm8), Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::RmReg]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Reg, OperandType::Mem]),
        vec![Mnemonic::Opcode(r_rm8), Mnemonic::ModRm(0), Mnemonic::RegReg, Mnemonic::Mem]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Mem, OperandType::Reg]),
        vec![Mnemonic::Opcode(rm_r8), Mnemonic::ModRm(0), Mnemonic::Mem, Mnemonic::RegReg]
    );

    map.insert(
        Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Reg, OperandType::Imm]),
        vec![Mnemonic::Opcode(rm_i8), Mnemonic::ModRm(im << 3), Mnemonic::RmReg, Mnemonic::RegImm(OperandSize::BYTE)]
    );
    map.insert(
        Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Mem, OperandType::Imm]),
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

// Used to generate empty Instructions as keys for the instruction map
enum OperandType {
    Reg = 0, Mem = 1, Imm = 2
}

#[derive(Derivative, Eq)]
#[derivative(PartialEq, Hash)]
pub enum InstructionOperand {
    Reg(
        #[derivative(Hash="ignore", PartialEq="ignore")]
        u8
    ),
    Mem(
        #[derivative(Hash="ignore", PartialEq="ignore")]
        u32
    ),
    Imm(
        #[derivative(Hash="ignore", PartialEq="ignore")]
        u64
    )
}


#[derive(PartialEq, Hash, Eq)]
pub struct Instruction {
    operation : OperationType,
    size : OperandSize,
    operands : Vec<InstructionOperand>,
}

impl Instruction {
    pub fn new(operation : OperationType, size : OperandSize, operands : Vec<InstructionOperand>) -> Instruction {
        Instruction {
            operation, size, operands
        }
    }

    fn mnemonic(operation : OperationType, size : OperandSize, operands : &[OperandType]) -> Instruction {
        Instruction {
            operation, size, operands : operands.iter().map(|ot| match ot {
                OperandType::Reg => InstructionOperand::Reg(0),
                OperandType::Mem => InstructionOperand::Mem(0),
                OperandType::Imm => InstructionOperand::Imm(0)
            }).collect()
        }
    }

    fn get_reg(&self, i : &mut usize) -> u8 {
        *i += 1;
        match self.operands[*i - 1] {
            InstructionOperand::Reg(val) => val,
            _ => panic!("Not a register operand")
        }
    }

    fn get_mem(&self, i : &mut usize) -> u32 {
        *i += 1;
        match self.operands[*i - 1] {
            InstructionOperand::Mem(val) => val,
            _ => panic!("Not a memory operand")
        }
    }

    fn get_imm(&self, i : &mut usize) -> u64 {
        *i += 1;
        match self.operands[*i - 1] {
            InstructionOperand::Imm(val) => val,
            _ => panic!("Not an immediate operand")
        }
    }
}

pub struct InstructionCompiler {
    map : HashMap<Instruction, Vec<Mnemonic>>
}

impl InstructionCompiler {
    pub fn new() -> Self {
        InstructionCompiler {
            map : create_instruction_map()
        }
    }

    pub fn compile_instruction(&self, instruction : &Instruction, res : &mut Vec<u8>) {
        let start = res.len();
        let mut rex : Option<u8> = None;
        let mut opcode_index = 0;
        let mut mod_rm_index = 0;
        let mut index = 0;

        for op in self.map.get(instruction).unwrap() {
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
                    let reg = instruction.get_reg(&mut index);
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= 1 << 2;
                    }
                    res[mod_rm_index] |= (reg & 0b111) << 3;
                },
                Mnemonic::PlusReg => {
                    let reg = instruction.get_reg(&mut index);
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= 1;
                    }
                    // PlusReg always follows Opcode / Opcode2
                    let len = res.len();
                    res[len - 1] |= reg & 0b111;
                }
                Mnemonic::Mem => {
                    let offset = instruction.get_mem(&mut index); // Offset from rsp
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
                    let reg = instruction.get_reg(&mut index);
                    if reg > 7 {
                        *rex.get_or_insert(0x40) |= 1;
                    }
                    res[mod_rm_index] |= 0b11000000 | (reg & 0b111);
                }
                Mnemonic::RegImm(size) => {
                    let imm : &[u8; 8] = &instruction.get_imm(&mut index).to_le_bytes();
                    res.extend_from_slice(&imm[0..(*size as usize)]);
                },
                Mnemonic::Pass => {
                    index += 1;
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

fn create_instruction_map() -> HashMap<Instruction, Vec<Mnemonic>> {
    use Mnemonic::*;
    let mut map = HashMap::new();
    map.insert(
        Instruction::mnemonic(OperationType::IMul, OperandSize::BYTE, &[OperandType::Reg, OperandType::Reg]),
        vec![Opcode(0xF6), ModRm(5 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IMul, OperandSize::BYTE, &[OperandType::Reg, OperandType::Mem]),
        vec![Opcode(0xF6), ModRm(5 << 3), Pass, Mem]
    );
    reg_rm_instr(&mut map, OperationType::IMul, Opcode2(0xAF));
    map.insert(
        Instruction::mnemonic(OperationType::Mul, OperandSize::BYTE, &[OperandType::Reg, OperandType::Reg]),
        vec![Opcode(0xF6), ModRm(4 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::Mul, OperandSize::BYTE, &[OperandType::Reg, OperandType::Mem]),
        vec![Opcode(0xF6), ModRm(4 << 3), Pass, Mem]
    );
    size_repeat_instr(&mut map, OperationType::Mul, &[OperandType::Reg, OperandType::Reg],
                      vec![Opcode(0xF7), ModRm(4 << 3), Pass, RmReg]
    );
    size_repeat_instr(&mut map, OperationType::Mul, &[OperandType::Reg, OperandType::Mem],
                      vec![Opcode(0xF7), ModRm(4 << 3), Pass, Mem]
    );
    standard_instr(&mut map, OperationType::Add, 0x00, 0x02, 0x80, 0x01, 0x03, 0x81, 0);
    standard_instr(&mut map, OperationType::Sub, 0x28, 0x2A, 0x80, 0x29, 0x2B, 0x81, 5);

    // Value(0x98 / 0x99) is a sign-extend instruction
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::BYTE, &[OperandType::Reg, OperandType::Reg]),
        vec![Value(0x66), Value(0x98), Opcode(0xF6), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::BYTE, &[OperandType::Reg, OperandType::Mem]),
        vec![Value(0x66), Value(0x98), Value(0xF6), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::WORD, &[OperandType::Reg, OperandType::Reg]),
        vec![Value(0x66), Value(0x99), Prefix(0x66), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::WORD, &[OperandType::Reg, OperandType::Mem]),
        vec![Value(0x66), Value(0x99), Prefix(0x66), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::DWORD, &[OperandType::Reg, OperandType::Reg]),
        vec![Value(0x99), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::DWORD, &[OperandType::Reg, OperandType::Mem]),
        vec![Value(0x99), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::QWORD, &[OperandType::Reg, OperandType::Reg]),
        vec![Value(0x48), Value(0x99), Rex(0x48), Opcode(0xF7), ModRm(7 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::IDiv, OperandSize::QWORD, &[OperandType::Reg, OperandType::Mem]),
        vec![Value(0x48), Value(0x99), Rex(0x48), Opcode(0xF7), ModRm(7 << 3), Pass, Mem]
    );

    // 0x31 0xD2 zeros the RDX-register (xor rdx, rdx)
    map.insert(
        Instruction::mnemonic(OperationType::Div, OperandSize::BYTE, &[OperandType::Reg, OperandType::Reg]),
        vec![Value(0x31), Value(0xD2), Opcode(0xF6), ModRm(6 << 3), Pass, RmReg]
    );
    map.insert(
        Instruction::mnemonic(OperationType::Div, OperandSize::BYTE, &[OperandType::Reg, OperandType::Mem]),
        vec![Value(0x31), Value(0xD2), Value(0xF6), ModRm(6 << 3), Pass, Mem]
    );
    size_repeat_instr(&mut map, OperationType::Div, &[OperandType::Reg, OperandType::Reg],
                      vec![Value(0x31), Value(0xD2), Opcode(0xF7), ModRm(6 << 3), Pass, RmReg]
    );
    size_repeat_instr(&mut map, OperationType::Div, &[OperandType::Reg, OperandType::Mem],
                      vec![Value(0x31), Value(0xD2), Opcode(0xF7), ModRm(6 << 3), Pass, Mem]
    );

    standard_instr(&mut map, OperationType::Mov, 0x88, 0x8A, 0xC6, 0x89, 0x8B, 0xC7, 0);
    map.insert(
        Instruction::mnemonic(OperationType::Mov, OperandSize::BYTE, &[OperandType::Reg, OperandType::Imm]),
        vec![Opcode(0xB0), PlusReg, RegImm(OperandSize::BYTE)]
    );
    map.insert(
        Instruction::mnemonic(OperationType::Mov, OperandSize::WORD, &[OperandType::Reg, OperandType::Imm]),
        vec![Prefix(0x66), Opcode(0xB8), PlusReg, RegImm(OperandSize::WORD)]
    );
    map.insert(
        Instruction::mnemonic(OperationType::Mov, OperandSize::DWORD, &[OperandType::Reg, OperandType::Imm]),
        vec![Opcode(0xB8), PlusReg, RegImm(OperandSize::DWORD)]
    );
    map.insert(
        Instruction::mnemonic(OperationType::Mov, OperandSize::QWORD, &[OperandType::Reg, OperandType::Imm]),
        vec![Rex(0x48), Opcode(0xB8), PlusReg, RegImm(OperandSize::QWORD)]
    );
    standard_instr(&mut map, OperationType::Cmp, 0x38, 0x3A, 0x80, 0x39, 0x3B, 0x81, 7);
    for (opcode, op) in vec![
        (0x94, OperationType::SetE), (0x95, OperationType::SetNe), (0x97, OperationType::SetA),
        (0x92, OperationType::SetB), (0x93, OperationType::SetAE), (0x96, OperationType::SetBE),
        (0x9F, OperationType::SetG), (0x9C, OperationType::SetL), (0x9D, OperationType::SetGE),
        (0x9E, OperationType::SetLE),
    ] {
        map.insert(
            Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Reg]),
            vec![Opcode2(opcode), ModRm(0), RmReg]
        );
        map.insert(
            Instruction::mnemonic(op, OperandSize::BYTE, &[OperandType::Mem]),
            vec![Opcode2(opcode), ModRm(0), RmReg]
        );
    }

    standard_instr(&mut map, OperationType::And, 0x20, 0x22, 0x80, 0x21, 0x23, 0x81, 4);
    standard_instr(&mut map, OperationType::Or, 0x08, 0x0A, 0x80, 0x09, 0x0B, 0x81, 1);
    standard_instr(&mut map, OperationType::Xor, 0x30, 0x32, 0x80, 0x31, 0x33, 0x81, 6);
    for size in vec![OperandSize::BYTE, OperandSize::WORD, OperandSize::DWORD, OperandSize::QWORD] {
        map.insert( // MovRet does nothing itself, only makes sure the return value is moved to rax.
                    Instruction::mnemonic(OperationType::MovRet, size, &[OperandType::Reg]),
                    vec![]
        );
    }
    map
}