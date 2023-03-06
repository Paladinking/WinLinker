pub mod amd_win64 {
    use std::cell::{RefCell, RefMut};
    use crate::language::parser::{self, Statement, Expression, ExpressionData, StatementData, Variable};
    use std::collections::HashMap;
    use std::ops::{BitXor, Deref, Not};
    use std::rc::Rc;
    use crate::language::operator::{DualOperator, SingleOperator};
    use crate::language::types::Type;

    #[derive(Debug, Copy, Clone)]
    enum BasicOperation {
        IMul, Mul, Add, Sub, IDiv, Div, Move, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
        SetGE, SetLE, And, Or, Xor
    }

    impl BasicOperation {
        fn is_symmetric(&self) -> bool {
            match self {
                BasicOperation::IMul => true,
                BasicOperation::Mul => true,
                BasicOperation::Add => true,
                BasicOperation::Sub => false,
                BasicOperation::IDiv => false,
                BasicOperation::Div => false,
                BasicOperation::Move => false,
                BasicOperation::Push => false,
                BasicOperation::Pop => false,
                BasicOperation::Cmp => false,
                BasicOperation::SetE => false,
                BasicOperation::SetNe => false,
                BasicOperation::SetA => false,
                BasicOperation::SetB => false,
                BasicOperation::SetAE => false,
                BasicOperation::SetBE => false,
                BasicOperation::SetG => false,
                BasicOperation::SetL => false,
                BasicOperation::SetGE => false,
                BasicOperation::SetLE => false,
                BasicOperation::And => true,
                BasicOperation::Or => true,
                BasicOperation::Xor => true
            }
        }

        fn imm_2(&self, size : OperandSize) -> bool {
            match self {
                BasicOperation::IMul => false,
                BasicOperation::Mul => false,
                BasicOperation::Add => size as usize <= OperandSize::DWORD as usize,
                BasicOperation::Sub => size as usize <= OperandSize::DWORD as usize,
                BasicOperation::IDiv => false,
                BasicOperation::Div => false,
                BasicOperation::Move => true,
                BasicOperation::Push => false,
                BasicOperation::Pop => false,
                BasicOperation::Cmp => size as usize <= OperandSize::DWORD as usize,
                BasicOperation::SetE => false,
                BasicOperation::SetNe => false,
                BasicOperation::SetA => false,
                BasicOperation::SetB => false,
                BasicOperation::SetAE => false,
                BasicOperation::SetBE => false,
                BasicOperation::SetG => false,
                BasicOperation::SetL => false,
                BasicOperation::SetGE => false,
                BasicOperation::SetLE => false,
                BasicOperation::And => size as usize <= OperandSize::DWORD as usize,
                BasicOperation::Or => size as usize <= OperandSize::DWORD as usize,
                BasicOperation::Xor => size as usize <= OperandSize::DWORD as usize
            }
        }

        fn dest_bitmap(&self) -> u64 {
            self.first_bitmap()
        }

        fn first_bitmap(&self) -> u64 {
            match self {
                BasicOperation::Div | BasicOperation::IDiv | BasicOperation::Mul => RAX,

                BasicOperation::IMul | BasicOperation::Add | BasicOperation::Sub |
                BasicOperation::Move | BasicOperation::And | BasicOperation::Or |
                BasicOperation::Xor | BasicOperation::Cmp  => MEM_GEN_REG,

                BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
                BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
                BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
                BasicOperation::SetGE | BasicOperation::SetLE => GEN_REG
            }
        }

        fn second_bitmap(&self, first_map: u64) -> u64 {
            debug_assert!(first_map.count_ones() == 1);
            match self {
                BasicOperation::IDiv | BasicOperation::Div | BasicOperation::Mul | //First is always rax
                BasicOperation::Add | BasicOperation::Sub | BasicOperation::IMul | BasicOperation::Move |
                BasicOperation::Cmp | BasicOperation::And | BasicOperation::Or | BasicOperation::Xor => (first_map ^ MEM_GEN_REG) | GEN_REG,

                BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
                BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE | BasicOperation::SetBE |
                BasicOperation::SetG | BasicOperation::SetL | BasicOperation::SetGE |
                BasicOperation::SetLE => panic!("No second operand")
            }
        }
    }



    #[derive(Copy, Clone, Debug)]
    enum OperandSize {
        BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
    }

    struct Register {
       bitmap : u64, index : usize, operand : Option<usize>
    }

    impl Register {
        pub fn new_general(bitmap : u64, index : usize) -> Register {
            Register {bitmap, index, operand : None}
        }

        pub fn bitmap(&self) -> u64 {
            return self.bitmap;
            panic!("Not general register")
        }
    }

    struct RegisterState {
        registers : Vec<Register>,
        memory : Vec<bool>,
        allocations : Vec<MemoryAllocation>,
        free_gen : u64
    }


    const MEM : u64 = 1;
    const R15 : u64 = 2;
    const R14 : u64 = 4;
    const R13 : u64 = 8;
    const R12 : u64 = 16;
    const RSP : u64 = 32;
    const RSI : u64 = 64;
    const RDI : u64 = 128;
    const RBP : u64 = 256;
    const RBX : u64 = 512;
    const R11 : u64 = 1024;
    const R10 : u64 = 2048;
    const R9 : u64 = 4096;
    const R8 : u64 = 8192;
    const RAX : u64 = 16384;
    const RDX : u64 = 32768;
    const RCX : u64 = 65536;


    const VOL_GEN_REG : u64 = RAX | RCX | RDX | R8 | R9 | R10 | R11;
    const NON_VOL_GEN_REG : u64 = RBX | RBP | RDI | RSI | R12 | R13 | R14 | R15;
    const GEN_REG : u64 = VOL_GEN_REG | NON_VOL_GEN_REG;
    const MEM_GEN_REG : u64 = GEN_REG | MEM;

    #[derive(Debug, Clone, Copy)]
    enum MemoryAllocation{
        Register(usize), Memory(usize), None
    }

    impl RegisterState {
        const VOLATILE_REGISTERS : usize = 7;
        const GENERAL_REGISTERS : usize = 15;

        fn new(mut locations : usize) -> RegisterState {
            let registers = vec![
               Register::new_general(RCX, 0),
               Register::new_general(RDX, 1),
               Register::new_general(RAX, 2),
               Register::new_general(R8, 3),
               Register::new_general(R9, 4),
               Register::new_general(R10, 5),
               Register::new_general(R11, 6),
               Register::new_general(RBX, 7),
               Register::new_general(RBP, 8),
               Register::new_general(RDI, 9),
               Register::new_general(RSI, 10),
               Register::new_general(R12, 11),
               Register::new_general(R13, 12),
               Register::new_general(R14, 13),
               Register::new_general(R15, 14)
            ];
            let mut allocations = vec![MemoryAllocation::None; locations];
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

        fn register_string(bitmap : u64) -> String {
            match bitmap {
                RCX => "rcx", RDX => "rdx", RAX => "rax",
                R8 => "r8", R9 => "r9", R10 => "r10",
                R11 => "r11", RBX => "rbx", RBP => "rbp",
                RDI => "rdi", RSI => "rsi", R12 => "r12",
                R13 => "r13", R14 => "r14", R15 => "r15",
                _ => "???"
            }.to_owned()
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

        fn allocate(&mut self, id : usize, bitmap : u64) -> u64 {
            let location = Self::get_register(bitmap & self.free_gen);

            if let Some(index) = location {
                self.registers[index].operand = Some(id);
                self.allocations[id] = MemoryAllocation::Register(index);
                self.free_gen &= !self.registers[index].bitmap;
                return self.registers[index].bitmap;
            }
            if bitmap & MEM != 0 {
                let index = self.get_memory();
                self.allocations[id] = MemoryAllocation::Memory(index);
                return MEM;
            }
            let location = Self::get_register(bitmap).unwrap();
            let prev_owner = self.registers[location].operand.unwrap();
            let s = self.to_string(prev_owner);
            if let Some(reg) = Self::get_register(self.free_gen & !self.registers[location].bitmap) {
                // Insert Move

                self.allocations[prev_owner] = MemoryAllocation::Register(reg);
                self.registers[reg].operand = Some(prev_owner);
            } else {
                // Insert Move
                let pos = self.get_memory();
                self.allocations[prev_owner] = MemoryAllocation::Memory(pos);
            }
            println!("Move {}, {}", self.to_string(prev_owner), s);
            self.allocations[id] = MemoryAllocation::Register(location);
            self.registers[location].operand = Some(id);
            return self.registers[location].bitmap;
        }

        fn free(&mut self, id : usize) {
            match self.allocations[id] {
                MemoryAllocation::Register(reg)=> {
                    self.registers[reg].operand = None;
                    self.free_gen |= self.registers[reg].bitmap;
                }
                MemoryAllocation::Memory(index) => {
                    self.memory[index] = false;
                }
                MemoryAllocation::None => {}
            }
            self.allocations[id] = MemoryAllocation::None;
        }

        fn allocation_bitmap(&self, id : usize) -> u64 {
            match self.allocations[id] {
                MemoryAllocation::Register(i) => self.registers[i].bitmap,
                MemoryAllocation::Memory(_) => MEM,
                MemoryAllocation::None => 0
            }
        }

        fn to_string(&self, id : usize) -> String {
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
                }
                MemoryAllocation::None => {
                    "None".to_owned()
                }
            }
        }

        fn register_zero(&mut self, dest : Rc<RefCell<Operand>>, op : BasicOperation) {
            let id = dest.borrow().location().unwrap().location;
            let bitmap = op.dest_bitmap();
            self.allocate(id, bitmap);
            println!("{:?} {}", op, self.to_string(id));

        }

        fn register_one_local(&mut self, first : Rc<RefCell<Operand>>, dest : Option<Rc<RefCell<Operand>>>, op : BasicOperation, reused : bool) {
            let bitmap = op.first_bitmap();
            let first_id = first.borrow().location().unwrap().location;

            if reused && dest.is_some() {
                let dest_id = dest.as_ref().unwrap().borrow().location().unwrap().location;
                self.allocate(dest_id, bitmap);
                println!("Move {}, {}", self.to_string(dest_id), self.to_string(first_id));
                // Add Move
            } else {
                let prev_alloc = self.allocation_bitmap(first_id);
                if bitmap & prev_alloc == 0 {
                    // Add Move
                    let s = self.to_string(first_id);
                    self.free(first_id);
                    self.allocate(first_id, bitmap);
                    println!("Move {}, {}", self.to_string(first_id), s);

                }
                if let Some(dest) = &dest {
                    dest.borrow_mut().location_mut().unwrap().location = first_id;
                }
            }
            if let Some(dest) = &dest {
                println!("{:?}, {}, {}", op, self.to_string(dest.borrow().location().unwrap().location), self.to_string(first_id));
            } else {
                println!("{:?}, {}", op, self.to_string(first_id));
            }

            if dest.is_none() && !reused {
                self.free(first_id);
            }
        }

        fn register_one_imm(&mut self, dest : Option<Rc<RefCell<Operand>>>, op : BasicOperation, imm : u64) {
            if let Some(dest) = dest {
                let bitmap = op.first_bitmap();
                let id = dest.borrow().location().unwrap().location;
                if let MemoryAllocation::None = self.allocations[id] {
                    self.allocate(id, bitmap);
                } else {
                    println!("{:?}, {:?}, {:?}", op, &self.allocations, id);
                    panic!("Dest should not be allocated")
                }
                println!("{:?}, {}, {}", op, self.to_string(id), imm);
            }
        }

        fn register_two_local(&mut self, first : Rc<RefCell<Operand>>, second : Rc<RefCell<Operand>>, dest : Option<Rc<RefCell<Operand>>>, op : BasicOperation, reused_first : bool, reused_second : bool) {
            let first_bitmap_allowed = op.first_bitmap();
            let first_id = first.borrow().location().unwrap().location;
            let second_id = second.borrow().location().unwrap().location;
            let mut first_bitmap;
            if reused_first && dest.is_some() {
                let dest_id = dest.as_ref().unwrap().borrow().location().unwrap().location;
                first_bitmap = self.allocate(dest_id, first_bitmap_allowed);
                println!("Move {}, {}", self.to_string(dest_id), self.to_string(first_id));
                // Add Move
            } else {
                first_bitmap = self.allocation_bitmap(first_id);
                if first_bitmap & first_bitmap_allowed == 0 {
                    // Add Move
                    let s = self.to_string(first_id);
                    self.free(first_id);
                    first_bitmap = self.allocate(first_id, first_bitmap_allowed);
                    println!("Move {}, {}", self.to_string(first_id), s);
                }
                if let Some(dest) = &dest {
                    dest.borrow_mut().location_mut().unwrap().location = first_id;
                }
            }
            let second_bitmap_allowed = op.second_bitmap(first_bitmap);
            if self.allocation_bitmap(second_id) & second_bitmap_allowed == 0 {
                let s = self.to_string(second_id);
                self.free(second_id);
                self.allocate(second_id, second_bitmap_allowed);
                println!("Move {}, {}", self.to_string(second_id), s);
            }
            if let Some(dest) = &dest {
                println!("{:?}, {}, {}, {}", op, self.to_string(dest.borrow().location().unwrap().location), self.to_string(first_id), self.to_string(second_id));
            } else {
                println!("{:?}, {}, {}", op, self.to_string(first_id), self.to_string(second_id));
            }


            if dest.is_none() && !reused_first {
                self.free(first_id);
            }
            if !reused_second {
                self.free(second_id);
            }
        }

        fn register_two_imm(&mut self, first : Rc<RefCell<Operand>>, dest : Option<Rc<RefCell<Operand>>>, op : BasicOperation, reused : bool, imm : u64) {
            let bitmap = op.first_bitmap();
            let first_id = first.borrow().location().unwrap().location;

            if reused && dest.is_some() {
                let dest_id = dest.as_ref().unwrap().borrow().location().unwrap().location;
                self.allocate(dest_id, bitmap);
                println!("Move {}, {}", self.to_string(dest_id), self.to_string(first_id));
                // Add Move
            } else {
                let prev_alloc = self.allocation_bitmap(first_id);
                if bitmap & prev_alloc == 0 {
                    // Add Move
                    let s = self.to_string(first_id);
                    self.free(first_id);
                    self.allocate(first_id, bitmap);
                    println!("Move {}, {}", self.to_string(first_id), s);
                }
                if let Some(dest) = &dest {
                    dest.borrow_mut().location_mut().unwrap().location = first_id;
                }
            }
            if let Some(dest) = &dest {
                println!("{:?}, {}, {}, {}", op, self.to_string(dest.borrow().location().unwrap().location), self.to_string(first_id), imm);
            } else {
                println!("{:?}, {}, {}", op, self.to_string(first_id), imm);
            }

            if dest.is_none() && !reused {
                self.free(first_id);
            }
        }
    }


    #[derive(Copy, Clone, Debug)]
    struct Location {
        location : usize, // Location in register allocation
        size : OperandSize
    }

    impl Location {
        fn new(location : usize, size : OperandSize) -> Location {
            Location { location, size }
        }
    }

    fn type_size(t : Type) -> OperandSize {
        match t {
            Type::U32 | Type::S32 => OperandSize::DWORD,
            Type::Bool => OperandSize::BYTE,
            Type::Plain { .. } => todo!("Not yet implemented"),
            Type::AnyInt | Type::Any => panic!("Not proper type")
        }
    }

    #[derive(Debug, Clone)]
    enum Operands {
        Zero, One(Rc<RefCell<Operand>>), Two(Rc<RefCell<Operand>>, Rc<RefCell<Operand>>),
    }

    #[derive(Debug)]
    enum Operand {
        Local {location : Location, uses : usize, used : usize},
        Immediate {value : u64, size : OperandSize}
    }

    impl Operand {
        fn local(location : Location) -> Operand {
            Operand::Local {
                location,
                uses: 0,
                used : 0
            }
        }

        fn add_use(&mut self) -> bool {
            match self {
                Operand::Local {ref mut used, uses, .. } => {
                    *used += 1;
                    uses > used
                },
                Operand::Immediate { .. } => {
                    false
                }
            }
        }

        fn location(&self) -> Option<&Location> {
            match self {
                Operand::Local { location, .. } => Some(location),
                Operand::Immediate { .. } => None,
            }
        }

        fn location_mut(&mut self) -> Option<&mut Location> {
            match self {
                Operand::Local {location, ..} => Some(location),
                _ => None
            }
        }
    }

    #[derive(Debug, Clone)]
    struct Instruction {
        operator : BasicOperation,
        operands : Operands,
        dest : Option<Rc<RefCell<Operand>>> // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
    }


    impl Instruction {
        fn new(operator : BasicOperation, operands : Operands, dest : Option<Rc<RefCell<Operand>>>) -> Instruction {
            Instruction {
                operator, operands, dest
            }
        }
    }

    struct Compiler<'a> {
        statements : &'a Vec<StatementData<'a>>,
        instr : Vec<Instruction>,
        operands : Vec<Rc<RefCell<Operand>>>,
        memory_locations : usize
    }

    impl <'a> Compiler<'a> {

        fn new<'b> (statements : &'a Vec<StatementData>, vars: &'b HashMap<String, &Variable>) -> Compiler<'a> {
            let mut memory_locations = 0;
            let operands = vars.iter().enumerate().map(|(i, (_, v))|{
                v.id.replace(Some(i));
                memory_locations +=1;
                Rc::new(RefCell::new(Operand::local(Location::new(memory_locations, type_size(v.var_type)))))
            }).collect();
            Compiler {
                statements, instr : Vec::new(),
                operands,
                memory_locations
            }
        }

        fn compile_statements(&mut self) {
            for statement in self.statements {
                match &statement.statement { Statement::Assignment { var, expr } => {
                    self.convert_assigment(var.id.borrow().unwrap(), &expr);
                }}
            }

            self.assign_registers();
        }

        fn new_location(&mut self, size : OperandSize) -> Location {
            self.memory_locations += 1;
            Location::new(self.memory_locations - 1, size)
        }

        fn fix_imm(&mut self, locations : &mut Vec<Rc<RefCell<Operand>>>, first : &mut usize, second : &mut usize, op : BasicOperation) {
            if let Operand::Immediate { size,.. } = locations[*first].clone().borrow().deref() {
                if op.is_symmetric() && locations[*first].borrow().location().is_some() {
                    (*first, *second) = (*second, *first);
                } else {
                    let new = Rc::new(RefCell::new(Operand::local(self.new_location(*size))));
                    self.memory_locations += 1;
                    self.instr.push(Instruction::new(
                        BasicOperation::Move, Operands::One(locations[*first].clone()), Some(new.clone())
                    ));
                    locations[*first] = new;
                }
            }
            if let Operand::Immediate { size, ..} = locations[*second].clone().borrow().deref() {
                if !op.imm_2(*size) {
                    let new = Rc::new(RefCell::new(Operand::local(self.new_location(*size))));
                    self.instr.push(Instruction::new(
                        BasicOperation::Move, Operands::One(locations[*second].clone()), Some(new.clone())
                    ));
                    locations[*second] = new;
                }
            }
        }

        fn convert_dual_operator(&mut self, locations : &mut Vec<Rc<RefCell<Operand>>>, mut first : usize, mut second : usize, size : OperandSize, t : Type, op : DualOperator) {
            if op.is_cmp() {
                self.fix_imm(locations, &mut first, &mut second, BasicOperation::Cmp);
                self.instr.push(Instruction::new(BasicOperation::Cmp,
                                                 Operands::Two(locations[first].clone(),
                                                               locations[second].clone()),
                                                 None));
                let basic_operator = match op {
                    DualOperator::Equal => BasicOperation::SetE,
                    DualOperator::NotEqual => BasicOperation::SetNe,
                    DualOperator::GreaterEqual => if t.is_signed() {BasicOperation::SetGE } else {BasicOperation::SetAE},
                    DualOperator::LesserEqual => if t.is_signed() {BasicOperation::SetLE} else {BasicOperation::SetBE},
                    DualOperator::Greater => if t.is_signed() {BasicOperation::SetG } else {BasicOperation::SetA},
                    DualOperator::Lesser => if t.is_signed() {BasicOperation::SetL } else {BasicOperation::SetB},
                    _ => unreachable!("Not is_cmp")
                };
                let new = Rc::new(RefCell::new(Operand::local(self.new_location(OperandSize::BYTE))));
                locations.push(new.clone());
                self.instr.push(Instruction::new(basic_operator, Operands::Zero, Some(new)));
            } else {
                let (location, op) = match op {
                    DualOperator::Divide =>
                        (self.new_location(size), if t.is_signed() {BasicOperation::IDiv } else {BasicOperation::Div }),
                    DualOperator::Multiply => if t.is_signed() {
                        (self.new_location(size), BasicOperation::IMul)
                    } else {
                        (self.new_location(size), BasicOperation::Mul)
                    },
                    DualOperator::Minus => (self.new_location(size), BasicOperation::Sub),
                    DualOperator::Plus => (self.new_location(size), BasicOperation::Add),
                    DualOperator::BoolAnd => (self.new_location(size), BasicOperation::And),
                    DualOperator::BoolOr => (self.new_location(size), BasicOperation::Or),
                    _ => unreachable!("Covered by is_cmp")
                };
                self.fix_imm(locations, &mut first, &mut second, op);
                let new = Rc::new(RefCell::new(Operand::local(location)));
                locations.push(new.clone());
                self.instr.push(Instruction::new(
                    op, Operands::Two(locations[first].clone(), locations[second].clone()),
                    Some(new)
                ));
            }
        }

        fn convert_single_operator(&mut self, locations : &mut Vec<Rc<RefCell<Operand>>>, mut expr : usize, op : SingleOperator) {
            match op {
                SingleOperator::Not => {
                    if let Operand::Immediate {size, ..} = locations[expr].clone().borrow().deref() {
                        if !BasicOperation::Xor.imm_2(*size) {
                            let new = Rc::new(RefCell::new(Operand::local(self.new_location(*size))));
                            self.instr.push(Instruction::new(
                                BasicOperation::Move, Operands::One(locations[expr].clone()), Some(new.clone())
                            ));
                            locations[expr] = new;
                        }
                    }
                    let new = Rc::new(RefCell::new(Operand::local(self.new_location(OperandSize::BYTE))));
                    let new2 = Rc::new(RefCell::new(Operand::Immediate {value : 1, size : OperandSize::BYTE}));
                    locations.push(new.clone());
                    self.instr.push(Instruction::new(
                        BasicOperation::Xor,
                        Operands::Two(locations[expr].clone(), new2),
                        Some(new)));
                }
                SingleOperator::Pass => unreachable!("Should not exist at this point.")
            }
        }

        fn convert_assigment(&mut self, dest : usize, expr : &Vec<ExpressionData>) {
            let mut locations = Vec::with_capacity(expr.len());
            let prev_len = self.instr.len();
            for (i, e) in expr.iter().enumerate() {
                match e.expression {
                    Expression::Variable(v) => {
                        let id = v.id.borrow().unwrap();
                        locations.push(self.operands[id].clone());
                    },
                    Expression::Operator { mut first, operator, mut second } => {
                        if let Operand::Local {ref mut uses, .. } = *locations[first].borrow_mut() {
                            *uses += 1;
                        }
                        if let Operand::Local {ref mut uses, .. } = *locations[second].borrow_mut() {
                            *uses += 1;
                        }
                        let size = type_size(e.t);
                        self.convert_dual_operator(&mut locations, first, second, size, expr[first].t, operator);
                    }
                    Expression::SingleOperator { operator, expr } => {
                        if let Operand::Local {ref mut uses, .. } = *locations[expr].borrow_mut() {
                            *uses += 1;
                        }
                        self.convert_single_operator(&mut locations, expr, operator);
                    }
                    Expression::IntLiteral(val) => {
                        let new = Rc::new(RefCell::new(Operand::Immediate {value : val, size : type_size(e.t)}));
                        locations.push(new.clone());
                    }
                    Expression::BoolLiteral(b) => {
                        let new = Rc::new(RefCell::new(Operand::Immediate {value : if b {1} else {0}, size : OperandSize::BYTE}));
                        locations.push(new);
                    }
                    Expression::None => unreachable!("Should not exist ever.")
                };
            }
            if prev_len ==  self.instr.len() { // Whole expression was only a variable or immediate value.
                let instruction = Instruction::new(BasicOperation::Move,
                    Operands::One(locations.last().unwrap().clone()), Some(self.operands[dest].clone()));
                self.instr.push(instruction);
            } else if let Some(instruction) =  self.instr.last_mut() {
                //operands[instruction.dest.unwrap()] = Operand::None;
                instruction.dest = Some(self.operands[dest].clone());
            }
        }

        fn assign_registers(&mut self) {
            let mut register_state = RegisterState::new(self.memory_locations);
            for i in &self.instr {
                println!("{:?}, {:?}", i.dest, i.operator);
            }
            for instruction in &mut self.instr {
                match &instruction.operands {
                    Operands::Zero => {
                        register_state.register_zero(instruction.dest.clone().unwrap(), instruction.operator);
                    }
                    Operands::One(first) => {
                        let reused = first.borrow_mut().add_use();
                        match (first.borrow().deref(), &instruction.dest) {
                            (Operand::Local { used, uses, .. }, Some(dest)) => {
                                register_state.register_one_local(first.clone(), Some(dest.clone()), instruction.operator, reused);
                            }
                            (Operand::Immediate { value, .. }, Some(dest)) => {
                                register_state.register_one_imm(Some(dest.clone()), instruction.operator, *value);
                            }
                            (Operand::Local { used, uses, .. }, None) => {
                                register_state.register_one_local(first.clone(), None, instruction.operator, reused);
                            }
                            (Operand::Immediate { value, ..  }, None) => {
                                register_state.register_one_imm(None, instruction.operator, *value);
                            }
                        }

                    }
                    Operands::Two(first, second) => {
                        let reused_first = first.borrow_mut().add_use();
                        let reused_second = second.borrow_mut().add_use();
                        if let Operand::Local {..} = first.borrow().deref() {
                            match (second.borrow().deref(), &instruction.dest) {
                                (Operand::Local {..}, Some(dest)) => {
                                    register_state.register_two_local(first.clone(), second.clone(), Some(dest.clone()), instruction.operator, reused_first, reused_second);
                                }
                                (Operand::Immediate {value, ..}, Some(dest)) => {
                                   register_state.register_two_imm(first.clone(), Some(dest.clone()), instruction.operator, reused_first, *value);
                                }
                                (Operand::Local {..}, None) => {
                                    register_state.register_two_local(first.clone(), second.clone(), None, instruction.operator, reused_first, reused_second);
                                }
                                (Operand::Immediate {value, ..}, None) => {
                                    register_state.register_two_imm(first.clone(), None, instruction.operator, reused_first, *value);
                                }

                            }
                        } else {
                            unreachable!("Should always be local because ims are fixed before this");
                        }

                    }
                }
            }
        }
    }

    pub fn compile_statements(statements : &Vec<StatementData>, vars : &HashMap<String, &parser::Variable>)  {

        let mut compiler = Compiler::new(statements, vars);

        compiler.compile_statements();

        for var in vars {
            println!("{}, {}", var.0, var.1.id.borrow().unwrap());
        }

        for (i, o ) in compiler.instr.iter().enumerate() {
            let out = |index : Rc<RefCell<Operand>>| match index.borrow().deref() {
                Operand::Local {..} => vars.iter()
                    .find(|(s, &v)| Rc::ptr_eq(&compiler.operands[v.id.borrow().unwrap()].clone(), &index.clone()))
                    .map(|(s, _)| s.clone()).unwrap_or("Pos:".to_string() + &(index.as_ptr() as usize).to_string()),
                Operand::Immediate {value, ..} => value.to_string(),
                _ => unreachable!("")
            };
            if let Operands::Two(i1, i2) = &o.operands {
                let out1 = out(i1.clone());
                let out2 = out(i2.clone());
                let dest = o.dest.clone().map(|i| out(i)).unwrap_or("None".to_string());
                println!("{}, {:?}, {}, {}, => {}", i, o.operator, out1, out2, dest);

            } else if let Operands::One(i1) = &o.operands {
                let out1 = out(i1.clone());
                let dest = o.dest.clone().map(|i| out(i)).unwrap_or("None".to_string());
                println!("{}, {:?}, {}, => {}", i, o.operator, out1, dest);
            } else{
                let dest = o.dest.clone().map(|i| out(i)).unwrap_or("None".to_string());
                println!("{}, {:?}, => {:?}", i, o.operator, dest);
            }

        }


        for (i, o ) in compiler.operands.iter().enumerate() {
            println!("{}, {:?}",i, o);
        }

    }
}