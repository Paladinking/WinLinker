pub mod amd_win64 {
    use crate::language::parser::{self, Statement, Expression, ExpressionData, StatementData, Variable};
    use std::collections::HashMap;
    use std::ops::{BitXor, Not};
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

        fn second_bitmap(&self, first_map: u64) -> u64 {
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

    #[derive(Debug, Clone)]
    enum Operands {
        One(usize), Two(usize, usize),
    }

    #[derive(Copy, Clone, Debug)]
    enum OperandSize {
        BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8,
    }

    enum Register {
        General {bitmap : u64, index : usize, operand : Option<usize>}
    }

    impl Register {
        pub fn new_general(bitmap : u64, index : usize) -> Register {
            Register::General {bitmap, index, operand : None}
        }

        pub fn bitmap(&self) -> u64 {
            if let Register::General {bitmap, ..} = self {
                return *bitmap;
            }
            panic!("Not general register")
        }
    }

    struct RegisterState {
        registers : Vec<Register>,
        memory : Vec<OperandSize>,
        allocations : Vec<StateLocation>
    }


    const MEM : u64 = 1;
    const RAX : u64 = 2;
    const RBX : u64 = 4;
    const RCX : u64 = 8;
    const RDX : u64 = 16;
    const RSI : u64 = 32;
    const RDI : u64 = 64;
    const RBP : u64 = 128;
    const RSP : u64 = 256;
    const R8 : u64 = 512;
    const R9 : u64 = 1024;
    const R10 : u64 = 2048;
    const R11 : u64 = 4096;
    const R12 : u64 = 8192;
    const R13 : u64 = 16384;
    const R14 : u64 = 32768;
    const R15 : u64 = 65536;


    const VOL_GEN_REG : u64 = RAX | RCX | RDX | R8 | R9 | R10 | R11;
    const NON_VOL_GEN_REG : u64 = RBX | RBP | RDI | RSI | R12 | R13 | R14 | R15;
    const GEN_REG : u64 = VOL_GEN_REG | NON_VOL_GEN_REG;
    const MEM_GEN_REG : u64 = GEN_REG | MEM;

    #[derive(Copy, Clone)]
    enum StateLocation {
        Register(usize, bool), Memory(usize, bool), None
    }

    impl StateLocation {
        fn reserve(&mut self) {
            if let StateLocation::Register(_, ref mut b) = self {
                *b = true;
            } else if let StateLocation::Memory(_, ref mut b) = self {
                *b = true;
            } else {
                panic!("Reserve none location");
            }
        }

        fn bitmap(&self, registers : &Vec<Register>) -> u64 {
            return if let StateLocation::Register(i, _) = self {
                registers[*i].bitmap()
            } else if let StateLocation::Memory(_, _) = self {
                MEM
            } else {
                0
            }
        }
    }

    impl RegisterState {
        const VOLATILE_REGISTERS : usize = 7;
        const GENERAL_REGISTERS : usize = 15;

        fn new(operands: &Vec<Operand>) -> RegisterState {
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
            RegisterState {
                registers, memory : Vec::new(), allocations : vec![StateLocation::None; operands.len()]
            }
        }

        // first: index to deallocated or unallocated operand
        // second: index to allocated or deallocated operand
        fn allocate_two(&mut self, first : usize, second : usize, op : BasicOperation, l1 : Location, l2 : Location) {
            debug_assert!(l1.size == l2.size);
            let first_location = self.allocations[first].bitmap(&self.registers);
            if first_location & l1.location != 0 {
                op.second_bitmap(first_location)
            }
            if let StateLocation::Register(p1, b1) = self.allocations[second] {
                self.allocations[second].reserve();
                if self.allocations[first].bitmap(&self.registers) & l1.location == 0 {
                    // Find reg / mem
                } else {
                    self.allocations[first].reserve();
                }
            } else if let StateLocation::Memory(p1, b1) = self.allocations[second] {
                let location = l1.location & MEM.not();
                if self.allocations[first].bitmap(&self.registers) & location == 0 {

                } else {
                    self.allocations[first].reserve();
                }
            }
        }

        fn deallocate(&mut self, pos : usize) {
            if let StateLocation::Register(_, ref mut b) = self.allocations[pos] {
                debug_assert!(b);
                *b = false;
            } else if let StateLocation::Memory(_, ref mut b) = self.allocations[pos] {
                debug_assert!(b);
                *b = false;
            } else {
                panic!("Deallocate unallocated operand");
            }

        }

        fn resolve(&self, first: usize, second: usize, op: BasicOperation, location: Location) -> Result<(), (usize, usize)> {

            Ok(())
        }

        fn add_operand(&mut self) {
            self.allocations.push(StateLocation::None);
        }
    }


    #[derive(Copy, Clone, Debug)]
    struct Location {
        location : u64, // Bitwise or of all allowed locations
        size : OperandSize
    }

    impl Location {
        fn new(location : u64, size : OperandSize) -> Location {
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

    fn type_location(t : Type) -> Location {
        match t {
            Type::S32 | Type::U32 => Location::new(MEM_GEN_REG, OperandSize::DWORD),
            Type::Bool => Location::new(MEM_GEN_REG, OperandSize::BYTE),
            Type::Plain { .. } => todo!("Not yet implemented"),
            Type::AnyInt | Type::Any => panic!("Not proper type")
        }
    }

    #[derive(Debug)]
    enum Operand {
        Local {location : Location, uses : usize, used : usize},
        NonLocal {location : Location},
        Immediate {value : u64, size : OperandSize},
        Redirect(usize)
    }

    impl Operand {
        fn local(location : Location) -> Operand {
            Operand::Local {
                location,
                uses: 0,
                used : 0
            }
        }

        fn location(&self) -> Option<&Location> {
            match self {
                Operand::Local { location, .. } => Some(location),
                Operand::NonLocal { .. } => todo!("Not yet implemented"),
                Operand::Immediate { .. } => None,
                Operand::Redirect(_) => panic!("Should be redirected")
            }
        }
    }

    #[derive(Debug, Clone)]
    struct Instruction {
        operator : BasicOperation,
        operands : Operands,
        dest : Option<usize> // Many instructions have same dest as first operand, but they need to be kept separate in case the next usage of dest is incompatible.
    }


    impl Instruction {
        fn new(operator : BasicOperation, operands : Operands, dest : Option<usize>) -> Instruction {
            Instruction {
                operator, operands, dest
            }
        }
    }

    struct Compiler<'a> {
        statements : &'a Vec<StatementData<'a>>,
        instr : Vec<Instruction>,
        operands : Vec<Operand>
    }

    impl <'a> Compiler<'a> {

        fn new<'b> (statements : &'a Vec<StatementData>, vars: &'b HashMap<String, &Variable>) -> Compiler<'a> {
            Compiler {
                statements, instr : Vec::new(),
                operands : vars.iter().enumerate().map(|(i, (_, v))|{
                        v.id.replace(Some(i));
                        Operand::local(type_location(v.var_type))
                    }).collect()
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

        fn fix_imm(&mut self, locations : &mut Vec<usize>, first : &mut usize, second : &mut usize, op : BasicOperation, location : u64) {
            if let Operand::Immediate { size,.. } = self.operands[locations[*first]] {
                if op.is_symmetric() && self.operands[locations[*first]].location().is_some() {
                    (*first, *second) = (*second, *first);
                } else {
                    self.instr.push(Instruction::new(
                        BasicOperation::Move, Operands::Two(self.operands.len(), locations[*first]), Some(self.operands.len())
                    ));
                    locations[*first] = self.operands.len();
                    self.operands.push(Operand::local(Location::new(location, size)));
                }
            }
            if let Operand::Immediate { size, ..} = self.operands[locations[*second]] {
                if !op.imm_2(size) {
                    self.instr.push(Instruction::new(
                        BasicOperation::Move, Operands::Two(self.operands.len(), locations[*second]), Some(self.operands.len())
                    ));
                    locations[*second] = self.operands.len();
                    self.operands.push(Operand::local(Location::new(MEM_GEN_REG, size)));
                }
            }
        }

        fn convert_dual_operator(&mut self, locations : &mut Vec<usize>, mut first : usize, mut second : usize, size : OperandSize, t : Type, op : DualOperator) {
            if op.is_cmp() {
                self.fix_imm(locations, &mut first, &mut second, BasicOperation::Cmp, MEM_GEN_REG);
                self.instr.push(Instruction::new(BasicOperation::Cmp, Operands::Two(locations[first], locations[second]), None));
                let basic_operator = match op {
                    DualOperator::Equal => BasicOperation::SetE,
                    DualOperator::NotEqual => BasicOperation::SetNe,
                    DualOperator::GreaterEqual => if t.is_signed() {BasicOperation::SetGE } else {BasicOperation::SetAE},
                    DualOperator::LesserEqual => if t.is_signed() {BasicOperation::SetLE} else {BasicOperation::SetBE},
                    DualOperator::Greater => if t.is_signed() {BasicOperation::SetG } else {BasicOperation::SetA},
                    DualOperator::Lesser => if t.is_signed() {BasicOperation::SetL } else {BasicOperation::SetB},
                    _ => unreachable!("Not is_cmp")
                };
                locations.push(self.operands.len());
                self.instr.push(Instruction::new(basic_operator, Operands::One(self.operands.len()), Some(self.operands.len())));
                self.operands.push(Operand::local(Location::new(MEM_GEN_REG, OperandSize::BYTE)));
            } else {
                let (location, op) = match op {
                    DualOperator::Divide =>
                        (Location::new(RAX, size), if t.is_signed() {BasicOperation::IDiv } else {BasicOperation::Div }),
                    DualOperator::Multiply => if t.is_signed() {
                        (Location::new(GEN_REG, size), BasicOperation::IMul)
                    } else {
                        (Location::new(RAX, size), BasicOperation::Mul)
                    },
                    DualOperator::Minus => (Location::new(MEM_GEN_REG, size), BasicOperation::Sub),
                    DualOperator::Plus => (Location::new(MEM_GEN_REG, size), BasicOperation::Add),
                    DualOperator::BoolAnd => (Location::new(MEM_GEN_REG, size), BasicOperation::And),
                    DualOperator::BoolOr => (Location::new(MEM_GEN_REG, size), BasicOperation::Or),
                    _ => unreachable!("Covered by is_cmp")
                };
                self.fix_imm(locations, &mut first, &mut second, op, location.location);
                locations.push(self.operands.len());
                self.instr.push(Instruction::new(
                    op, Operands::Two(locations[first], locations[second]),
                    Some(self.operands.len()))
                );
                self.operands.push(Operand::local(location));
            }
        }

        fn convert_single_operator(&mut self, locations : &mut Vec<usize>, mut expr : usize, op : SingleOperator) {
            match op {
                SingleOperator::Not => {
                    if let Operand::Immediate {size, ..} = self.operands[locations[expr]] {
                        if !BasicOperation::Xor.imm_2(size) {
                            self.instr.push(Instruction::new(
                                BasicOperation::Move, Operands::Two(self.operands.len(), locations[expr]), Some(self.operands.len())
                            ));
                            locations[expr] = self.operands.len();
                            self.operands.push(Operand::local(Location::new(MEM_GEN_REG, size)));
                        }
                    }
                    locations.push(self.operands.len());
                    self.operands.push(Operand::local(Location::new(MEM_GEN_REG, OperandSize::BYTE)));
                    self.instr.push(Instruction::new(
                        BasicOperation::Xor,
                        Operands::Two(locations[expr], self.operands.len()),
                        Some(self.operands.len() - 1)));
                    self.operands.push(Operand::Immediate {value : 1, size : OperandSize::BYTE});
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
                        locations.push(id);
                    },
                    Expression::Operator { mut first, operator, mut second } => {
                        if let Operand::Local {ref mut uses, .. } = self.operands[locations[first]] {
                            *uses += 1;
                        }
                        if let Operand::Local {ref mut uses, .. } = self.operands[locations[second]] {
                            *uses += 1;
                        }
                        let size = type_size(e.t);
                        self.convert_dual_operator(&mut locations, first, second, size, expr[first].t, operator);
                    }
                    Expression::SingleOperator { operator, expr } => {
                        if let Operand::Local {ref mut uses, .. } = self.operands[locations[expr]] {
                            *uses += 1;
                        }
                        self.convert_single_operator(&mut locations, expr, operator);
                    }
                    Expression::IntLiteral(val) => {
                        locations.push(self.operands.len());
                        self.operands.push(Operand::Immediate {value : val, size : type_size(e.t)});
                    }
                    Expression::BoolLiteral(b) => {
                        locations.push(self.operands.len());
                        self.operands.push(Operand::Immediate {value : if b {1} else {0}, size : OperandSize::BYTE});
                    }
                    Expression::None => unreachable!("Should not exist ever.")
                };
            }
            if prev_len ==  self.instr.len() { // Whole expression was only a variable or immediate value.
                let instruction = Instruction::new(BasicOperation::Move, Operands::Two(dest, *locations.last().unwrap()), Some(dest));
                self.instr.push(instruction);
            } else if let Some(instruction) =  self.instr.last_mut() {
                //operands[instruction.dest.unwrap()] = Operand::None;
                instruction.dest = Some(dest);
            }
        }

        fn assign_registers(&mut self) {
            let mut register_state = RegisterState::new(&self.operands);
            let mut new_instr  = Vec::with_capacity(self.instr.len());

            fn redirect(operands : &mut Vec<Operand>, pos : &mut usize, register_state : &mut RegisterState) {
                while let Operand::Redirect(n) = operands[*pos]  {*pos = n};
                if let Operand::Local { ref mut used, uses, ..} = operands[*pos] {
                    *used += 1;
                    if *used == uses {
                        register_state.deallocate(*pos);
                    }
                }
            }

            for instruction in &mut self.instr {
                if let Operands::One(ref mut pos) = instruction.operands {
                    redirect(&mut self.operands, pos, &mut register_state);

                } else if let Operands::Two(ref mut first, ref mut second) = instruction.operands {
                    redirect(&mut self.operands, second, &mut register_state);
                    while let Operand::Redirect(n) = operands[*first]  {*first = n};
                    if let Operand::Local { location, uses, ref mut used } = self.operands[*first] {
                        *used += 1;
                        if uses > *used { // Previous value needs to survive, add a mov
                            if let Some(dest) = instruction.dest {
                                debug_assert!(dest != *first);
                                new_instr.push(Instruction::new(
                                    BasicOperation::Move, Operands::Two(dest, *first), Some(dest)
                                ));
                                register_state.allocate(dest, Some(*first), BasicOperation::Move, location);
                                *first = dest;
                            } // Else no move is needed since instruction does not destroy the register.
                        } else {
                            register_state.deallocate(*first);
                        }
                        if let Some(dest) = instruction.dest {
                            if *first != dest  {
                                self.operands[dest] = Operand::Redirect(*first);
                                instruction.dest = Some(*first);
                            }
                        }
                        if let Some(_) = self.operands[*second].location() {
                            if let Err((a, b)) = register_state.resolve(*first, *second, instruction.operator, location) {
                                // Need to add move b => a
                                new_instr.push(Instruction::new(
                                    BasicOperation::Move, Operands::Two(a, b), Some(a)
                                ));

                            }
                        }

                        /*if !register_state.resolve(*first, *second, instruction.operator, location) {
                            new_instr.push(Instruction::new(

                            ))
                        }*/
                        new_instr.push(instruction.clone());
                    } else {
                        panic!("Should be local")
                    }
                }
            }
            self.instr = new_instr;
        }
    }

    pub fn compile_statements(statements : &Vec<StatementData>, vars : &HashMap<String, &parser::Variable>)  {

        let mut compiler = Compiler::new(statements, vars);
        compiler.compile_statements();

        for var in vars {
            println!("{}, {}", var.0, var.1.id.borrow().unwrap());
        }
        for (i, o ) in compiler.instr.iter().enumerate() {
            let out = |index| match compiler.operands[index] {
                Operand::Local {..} => vars.iter()
                    .find(|(s, &v)| v.id.borrow().unwrap() == index)
                    .map(|(s, _)| s.clone()).unwrap_or("Pos:".to_string() + &index.to_string()),
                Operand::Immediate {value, ..} => value.to_string(),
                _ => unreachable!("")
            };
            if let Operands::Two(i1, i2) = o.operands {
                let out1 = out(i1);
                let out2 = out(i2);
                let dest = o.dest.map(|i| out(i)).unwrap_or("None".to_string());
                println!("{}, {:?}, {}, {}, => {}", i, o.operator, out1, out2, dest);

            } else if let Operands::One(i1) = o.operands {
                let out1 = out(i1);
                let dest = o.dest.map(|i| out(i)).unwrap_or("None".to_string());
                println!("{}, {:?}, {}, => {}", i, o.operator, out1, dest);
            } else{
                panic!("bad...");
            }

        }
        for (i, o ) in compiler.operands.iter().enumerate() {
            println!("{}, {:?}",i, o);
        }

    }
}