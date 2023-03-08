pub mod amd_win64 {
    use std::cell::{Cell};
    use crate::language::parser::{Statement, Expression, ExpressionData, StatementData, Variable};
    use std::collections::HashMap;
    use bumpalo::Bump;
    use crate::language::operator::{DualOperator, SingleOperator};
    use crate::language::types::Type;

    #[derive(Debug, Copy, Clone)]
    enum BasicOperation {
        IMul, Mul, Add, Sub, IDiv, Div, Mov, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
        SetGE, SetLE, And, Or, Xor
    }

    impl BasicOperation {
        // Returns true if swapping first and second operand does not affect the result
        fn is_symmetric(&self) -> bool {
            match self {
                BasicOperation::IMul | BasicOperation::Mul | BasicOperation::Add |
                BasicOperation::And | BasicOperation::Or | BasicOperation::Xor => true,

                BasicOperation::Sub | BasicOperation::IDiv | BasicOperation::Div |
                BasicOperation::Mov | BasicOperation::Push | BasicOperation::Pop |
                BasicOperation::Cmp | BasicOperation::SetE | BasicOperation::SetNe |
                BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
                BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
                BasicOperation::SetGE | BasicOperation::SetLE => false,
            }
        }

        fn bitmap_hint(&self, n : usize) -> u64 {
            match n {
                0 => self.first_bitmap(),
                1 => self.second_bitmap(),
                _ => panic!("To many operands")
            }
        }

        // Returns a bitmap to all operands that are overridden by the operation
        // 00000001 means first, 00000010 means second, 00000101 means first and third etc
        fn destroyed(&self) -> u8 {
            match self {
                BasicOperation::Mov | BasicOperation::Cmp | BasicOperation::Push |
                BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
                BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
                BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
                BasicOperation::SetGE | BasicOperation::SetLE => 0,

                BasicOperation::IMul | BasicOperation::Mul | BasicOperation::Add |
                BasicOperation::Sub | BasicOperation::IDiv | BasicOperation::Div |
                BasicOperation::Or | BasicOperation::And | BasicOperation::Xor => 1
            }
        }

        fn first_bitmap(&self) -> u64 {
            match self {
                BasicOperation::Div | BasicOperation::IDiv | BasicOperation::Mul => RAX,

                BasicOperation::IMul | BasicOperation::Add | BasicOperation::Sub |
                BasicOperation::Mov | BasicOperation::And | BasicOperation::Or |
                BasicOperation::Xor | BasicOperation::Cmp  => MEM_GEN_REG,

                BasicOperation::Push | BasicOperation::Pop | BasicOperation::SetE | BasicOperation::SetNe |
                BasicOperation::SetA | BasicOperation::SetB | BasicOperation::SetAE |
                BasicOperation::SetBE | BasicOperation::SetG | BasicOperation::SetL |
                BasicOperation::SetGE | BasicOperation::SetLE => GEN_REG
            }
        }

        fn second_bitmap(&self) -> u64 {
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
                BasicOperation::SetLE => panic!("No second operand")
            }
        }

        fn next_bitmap(&self, first_map: u64, n : usize) -> u64 {
            debug_assert!(first_map.count_ones() == 1);
            match n {
                0 => self.first_bitmap(),
                1 => ((first_map ^ MEM) | !MEM) & self.second_bitmap(),
                _ => panic!("Too many operands")
            }


        }
    }



    #[derive(Copy, Clone, Debug)]
    enum OperandSize {
        BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
    }

    impl OperandSize {
        fn to_imm(&self) -> u64 {
            match self {
                OperandSize::BYTE => IMM8,
                OperandSize::WORD => IMM16,
                OperandSize::DWORD => IMM32,
                OperandSize::QWORD => IMM64
            }
        }
    }

    struct Register {
       bitmap : u64, index : usize, operand : Option<usize>
    }

    impl Register {
        pub fn new_general(bitmap : u64, index : usize) -> Register {
            Register {bitmap, index, operand : None}
        }
    }

    struct RegisterState {
        registers : Vec<Register>,
        memory : Vec<bool>,
        allocations : Vec<MemoryAllocation>,
        free_gen : u64
    }

    const MEM : u64 = 2_u64.pow(0);
    const R15 : u64 = 2_u64.pow(1);
    const R14 : u64 = 2_u64.pow(2);
    const R13 : u64 = 2_u64.pow(3);
    const R12 : u64 = 2_u64.pow(4);
    const RSP : u64 = 2_u64.pow(5);
    const RSI : u64 = 2_u64.pow(6);
    const RDI : u64 = 2_u64.pow(7);
    const RBP : u64 = 2_u64.pow(8);
    const RBX : u64 = 2_u64.pow(9);
    const R11 : u64 = 2_u64.pow(10);
    const R10 : u64 = 2_u64.pow(11);
    const R9 : u64 = 2_u64.pow(12);
    const R8 : u64 = 2_u64.pow(13);
    const RAX : u64 = 2_u64.pow(14);
    const RDX : u64 = 2_u64.pow(15);
    const RCX : u64 = 2_u64.pow(16);

    const IMM8 : u64  = 2_u64.pow(60);
    const IMM16 : u64 = 2_u64.pow(61);
    const IMM32 : u64 = 2_u64.pow(62);
    const IMM64 : u64 = 2_u64.pow(63);


    const VOL_GEN_REG : u64 = RAX | RCX | RDX | R8 | R9 | R10 | R11;
    const NON_VOL_GEN_REG : u64 = RBX | RBP | RDI | RSI | R12 | R13 | R14 | R15;
    const GEN_REG : u64 = VOL_GEN_REG | NON_VOL_GEN_REG;
    const MEM_GEN_REG : u64 = GEN_REG | MEM;

    #[derive(Debug, Clone, Copy)]
    enum MemoryAllocation{
        Register(usize), Memory(usize), Immediate(u64, u64),
        Hint(u64), None
    }

    impl RegisterState {
        const VOLATILE_REGISTERS : usize = 7;
        const GENERAL_REGISTERS : usize = 15;

        fn new() -> RegisterState {
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

        // Allocates a location for operand to a location contained in bitmap.
        // Potentially inserts a move to free a register, moving it to another
        // register or memory location. The destination of such a move will not be to
        // any location contained in invalidated.
        fn allocate(&mut self, operand : &Operand, bitmap : u64, invalidated : u64) -> u64 {
            if let MemoryAllocation::Hint(map) = self.allocations[operand.id.get()] {
                if let Some(index) = Self::get_register(map & bitmap & self.free_gen) {
                    self.registers[index].operand = Some(self.allocations.len());
                    operand.id.replace(self.allocations.len());
                    self.allocations.push(MemoryAllocation::Register(index));
                    self.free_gen &= !self.registers[index].bitmap;
                    return self.registers[index].bitmap;
                }
            }

            let location = Self::get_register(bitmap & self.free_gen);
            if let Some(index) = location {
                self.registers[index].operand = Some(self.allocations.len());
                operand.id.replace(self.allocations.len());
                self.allocations.push(MemoryAllocation::Register(index));
                self.free_gen &= !self.registers[index].bitmap;
                return self.registers[index].bitmap;
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
            self.registers[location].operand = Some(self.allocations.len());
            operand.id.replace(self.allocations.len());
            self.allocations.push(MemoryAllocation::Register(location));
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
                },
                MemoryAllocation::Immediate(..) => {},
                MemoryAllocation::Hint(_) | MemoryAllocation::None => panic!("Double free")
            }
            self.allocations[id] = MemoryAllocation::None;
        }

        fn invalidate_registers(&mut self, map : u64) {
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

        fn is_free(&self, id : usize) -> bool {
            match self.allocations[id] {
                MemoryAllocation::Register(_) | MemoryAllocation::Memory(_) |
                MemoryAllocation::Immediate(_, _) => false,
                MemoryAllocation::Hint(_) | MemoryAllocation::None => true
            }
        }

        fn allocation_bitmap(&self, id : usize) -> u64 {
            match self.allocations[id] {
                MemoryAllocation::Register(i) => self.registers[i].bitmap,
                MemoryAllocation::Memory(_) => MEM,
                MemoryAllocation::Immediate(_, bitmap) => bitmap,
                MemoryAllocation::Hint(_) |
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

        fn allocate_imm(&mut self, operand : &Operand, imm : u64, size : OperandSize) {
            self.allocations.push(MemoryAllocation::Immediate(imm, size.to_imm()));
            operand.id.replace(self.allocations.len() - 1);
        }

        fn allocate_hint(&mut self, operand : &Operand, hint : u64) -> bool {
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

        fn propagate_hint(&mut self, source : &Operand, dest : &Operand) {
            if let MemoryAllocation::None = self.allocations[dest.id.get()] {
                if let MemoryAllocation::Hint(_) = self.allocations[source.id.get()] {
                    dest.id.replace(source.id.get());
                }
            }
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

    #[derive(Debug)]
    struct Operand {
        id : Cell<usize>, // Index of allocated MemoryLocation
        last_use : Cell<usize>, // Index of the last usage of this operand in instructions vector
        location : Cell<u64>, // Hint where this operand wants to be located
        size : OperandSize
    }

    impl Operand {
        fn new(id : usize, location : u64, size : OperandSize) -> Operand {
            Operand {
                id : Cell::new(id), location : Cell::new(location), last_use : Cell::new(0), size
            }
        }

        fn local(size : OperandSize) -> Operand {
            Self::new(0, MEM_GEN_REG, size)
        }

        fn add_use(&self, index : usize) {
            self.last_use.replace(index);
        }
    }

    #[derive(Debug, Clone)]
    struct Instruction <'a> {
        operator : BasicOperation,
        operands : Vec<&'a Operand>,
        dest : Option<&'a Operand> // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
    }


    impl <'a> Instruction<'a> {
        fn new(operator : BasicOperation, operands : Vec<&'a Operand>, dest : Option<&'a Operand>) -> Instruction<'a> {
            Instruction {
                operator, operands, dest
            }
        }
    }

    struct Compiler<'a> {
        statements : &'a Vec<StatementData<'a>>,
        instr : Vec<Instruction<'a>>,
        invalidations : Vec<(usize, u64)>,
        operands : Vec<&'a Operand>,
        arena : &'a Bump,
        register_state : RegisterState
    }

    impl <'a> Compiler<'a> {
        fn new<'b> (arena : &'a Bump, statements : &'a Vec<StatementData>, vars: &'b HashMap<String, &Variable>) -> Compiler<'a> {
            let register_state = RegisterState::new();
            let operands = vars.iter().enumerate().map(|(i, (_, v))|{
                v.id.replace(Some(i));
                &*arena.alloc(Operand::local(type_size(v.var_type)))
            }).collect();
            Compiler {
                statements, instr : Vec::new(), invalidations : Vec::new(),
                operands, arena, register_state
            }
        }

        fn compile_statements(&mut self) {
            for statement in self.statements {
                match &statement.statement { Statement::Assignment { var, expr } => {
                    self.convert_assigment(*var, &expr);
                }}
            }

            self.assign_registers();
        }

        fn new_operand(&mut self, location : u64, size : OperandSize) -> Operand {
            Operand::new(0, location, size)
        }

        fn convert_dual_operator(&mut self, locations : &mut Vec<&'a Operand>, first : usize, second : usize, size : OperandSize, t : Type, op : DualOperator) {
            if op.is_cmp() {
                self.instr.push(Instruction::new(BasicOperation::Cmp,
                                                 vec![locations[first], locations[second]],
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
                let dest =  self.arena.alloc(Operand::local(size));
                let oper = self.arena.alloc(Operand::local(size));
                locations.push(dest);
                self.instr.push(Instruction::new(basic_operator, vec![oper], Some(dest)));
            } else {
                let operator = match op {
                    DualOperator::Divide => {
                        self.invalidations.push((self.instr.len(), RDX));
                        if t.is_signed() {BasicOperation::IDiv } else {BasicOperation::Div }
                    },
                    DualOperator::Multiply => if t.is_signed() { BasicOperation::IMul } else {
                        self.invalidations.push((self.instr.len(), RDX));
                        BasicOperation::Mul
                    },
                    DualOperator::Minus => BasicOperation::Sub,
                    DualOperator::Plus => BasicOperation::Add,
                    DualOperator::BoolAnd => BasicOperation::And,
                    DualOperator::BoolOr => BasicOperation::Or,
                    _ => unreachable!("Covered by is_cmp")
                };
                let new = self.arena.alloc(Operand::local(size));
                locations.push(new);
                self.instr.push(Instruction::new(
                    operator, vec![locations[first], locations[second]], Some(new)
                ));
            }
        }

        fn convert_single_operator(&mut self, locations : &mut Vec<&'a Operand>, expr : usize, op : SingleOperator) {
            match op {
                SingleOperator::Not => {
                    let new = self.arena.alloc(Operand::local(OperandSize::BYTE));
                    let new2 = self.arena.alloc(Operand::new(0, IMM8, OperandSize::BYTE));
                    self.register_state.allocate_imm(new2, 1, OperandSize::BYTE);
                    locations.push(new);
                    self.instr.push(Instruction::new(
                        BasicOperation::Xor,
                        vec![locations[expr], new2], Some(new))
                    );
                }
                SingleOperator::Pass => unreachable!("Should not exist at this point.")
            }
        }

        fn convert_assigment(&mut self, dest : &Variable, expr : &Vec<ExpressionData>) {
            let mut locations = Vec::with_capacity(expr.len());
            let prev_len = self.instr.len();
            for e in expr.iter() {
                match e.expression {
                    Expression::Variable(v) => {
                        let id = v.id.borrow().unwrap();
                        locations.push(self.operands[id].clone());
                    },
                    Expression::Operator { first, operator, second } => {
                        locations[first].add_use(self.instr.len());
                        locations[second].add_use(self.instr.len());
                        let size = type_size(e.t);
                        self.convert_dual_operator(&mut locations, first, second, size, expr[first].t, operator);
                    }
                    Expression::SingleOperator { operator, expr } => {
                        locations[expr].add_use(self.instr.len());
                        self.convert_single_operator(&mut locations, expr, operator);
                    }
                    Expression::IntLiteral(val) => {
                        let new = self.arena.alloc(Operand::new(0, type_size(e.t).to_imm(), type_size(e.t)));
                        self.register_state.allocate_imm(new, val, type_size(e.t));
                        locations.push(new);
                    }
                    Expression::BoolLiteral(b) => {
                        let new = self.arena.alloc(Operand::new(0, IMM8, OperandSize::BYTE));
                        self.register_state.allocate_imm(new, if b {1} else {0}, OperandSize::BYTE);
                        locations.push(new);
                    }
                    Expression::None => unreachable!("Should not exist ever.")
                };
            }
            if prev_len ==  self.instr.len() { // Whole expression was only a variable or immediate value.
                let operand = *locations.last().unwrap();
                operand.add_use(self.instr.len());
                let first = self.arena.alloc(Operand::local(type_size(dest.var_type)));
                let instruction = Instruction::new(BasicOperation::Mov,
                                                   vec![first, operand], Some(self.operands[dest.id.borrow().unwrap()]));
                self.instr.push(instruction);
            } else if let Some(instruction) =  self.instr.last_mut() {
                instruction.dest = Some(self.operands[dest.id.borrow().unwrap()]);
            }
        }

        fn assign_registers(&mut self) {
            for instruction in self.instr.iter() { // Merge dest and first operand
                for (i, &operand) in instruction.operands.iter().enumerate() {
                    let hint = instruction.operator.bitmap_hint(i);
                    self.register_state.allocate_hint(operand, hint);
                }
                if let (Some(dest), &first) = (instruction.dest, instruction.operands.first().unwrap()) {
                    self.register_state.propagate_hint(first, dest);
                }
            }

            let mut invalidation = 0;
            for (index, instruction) in self.instr.iter_mut().enumerate() {
                let mut invalid = 0;
                if let Some((i, map)) = self.invalidations.get(invalidation) {
                    if *i == index {
                        invalid = *map;
                        invalidation += 1;
                    }
                }
                let destroyed = instruction.operator.destroyed();
                let mut bitmap = 1;
                for (i, operand) in instruction.operands.iter_mut().enumerate() {
                    bitmap = instruction.operator.next_bitmap(
                        bitmap, i
                    );

                    if self.register_state.is_free(operand.id.get()) {
                        self.register_state.allocate(*operand, bitmap, invalid);
                    }
                    let allocation_bitmap = self.register_state.allocation_bitmap(operand.id.get());
                    if operand.last_use.get() > index && destroyed & (1 << i) != 0 || allocation_bitmap & bitmap == 0 {
                        let new = self.arena.alloc(Operand::local(operand.size));
                        let location = self.register_state.allocate(new, bitmap, invalid);
                        if location != allocation_bitmap {
                            println!("Move0 {}, {}", self.register_state.to_string(new.id.get()), self.register_state.to_string(operand.id.get()));
                        }

                        if operand.last_use.get() <= index {
                            self.register_state.free(operand.id.get());
                        }
                        *operand = new;
                        // Add move
                    }
                    bitmap = self.register_state.allocation_bitmap(operand.id.get());
                    if operand.last_use.get() <= index {
                        invalid &= !bitmap;
                    }
                }

                self.register_state.invalidate_registers(invalid);
                let mut free = true;
                if let Some(dest) = instruction.dest {
                    if self.register_state.is_free(dest.id.get()) {
                        dest.id.replace(instruction.operands.first().unwrap().id.get());
                        instruction.operands.first().unwrap().last_use.replace(dest.last_use.get());
                    } else {
                        free = false;
                    }
                }

                let mut s = format!("{:?}", instruction.operator);

                for &o in instruction.operands.iter() {
                    s += " ";
                    s += &self.register_state.to_string(o.id.get());
                }
                let f = format!("{}", self.register_state.to_string(instruction.operands.first().unwrap().id.get()));
                for &operand in &instruction.operands {
                    if operand.last_use.get() <= index {
                        self.register_state.free(operand.id.get());
                    }
                }

                println!("{}", s);
                if let Some(dest) = instruction.dest {
                    if !free {
                        println!("Move1 {}, {}", self.register_state.to_string(dest.id.get()), f);
                    }
                }

            }
        }
    }

    pub fn compile_statements(arena : &Bump, statements : &Vec<StatementData>, vars : &HashMap<String, &Variable>)  {

        let mut compiler = Compiler::new(arena, statements, vars);

        compiler.compile_statements();
    }
}