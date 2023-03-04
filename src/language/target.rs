pub mod amd_win64 {
    use crate::language::parser::{self, Statement, Expression, ExpressionData, StatementData};
    use std::collections::HashMap;
    use crate::language::operator::{DualOperator, SingleOperator};
    use crate::language::types::Type;

    #[derive(Debug, Copy, Clone)]
    enum BasicOperation {
        IMul, Mul, Add, Sub, SDiv, UDiv, Move, Push, Pop, Cmp, SetE, SetNe, SetA, SetB, SetAE, SetBE, SetG, SetL,
        SetGE, SetLE, And, Or, Xor
    }

    impl BasicOperation {
        fn is_symmetric(&self) -> bool {
            match self {
                BasicOperation::IMul => true,
                BasicOperation::Mul => true,
                BasicOperation::Add => true,
                BasicOperation::Sub => false,
                BasicOperation::SDiv => false,
                BasicOperation::UDiv => false,
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
                BasicOperation::SDiv => false,
                BasicOperation::UDiv => false,
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
    }

    #[derive(Debug)]
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

    const RAX : u64 = 1;
    const RBX : u64 = 2;
    const RCX : u64 = 4;
    const RDX : u64 = 8;
    const RSI : u64 = 16;
    const RDI : u64 = 32;
    const RBP : u64 = 64;
    const RSP : u64 = 128;
    const R8 : u64 = 256;
    const R9 : u64 = 512;
    const R10 : u64 = 1024;
    const R11 : u64 = 2048;
    const R12 : u64 = 4096;
    const R13 : u64 = 8192;
    const R14 : u64 = 16384;
    const R15 : u64 = 32768;
    const MEM : u64 = 65536;

    const VOL_GEN_REG : u64 = RAX | RCX | RDX | R8 | R9 | R10 | R11;
    const NON_VOL_GEN_REG : u64 = RBX | RBP | RDI | RSI | R12 | R13 | R14 | R15;
    const GEN_REG : u64 = VOL_GEN_REG | NON_VOL_GEN_REG;
    const MEM_GEN_REG : u64 = GEN_REG | MEM;

    #[derive(Copy, Clone)]
    enum StateLocation {
        Register(usize), Memory(usize), None
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

        fn register(&mut self, operands: &Vec<Operand>, index : usize) {
            let mut location = operands[index].location().unwrap().location;
            if location <= GEN_REG {
                let mut free_vol = None;
                for i in 0..RegisterState::GENERAL_REGISTERS {
                    if let Register::General { bitmap, index, ref mut operand } = self.registers[i] {
                        if operand.is_none() {
                            free_vol = Some(i);
                            if bitmap | location != 0 {
                                self.allocations[index] = StateLocation::Register(i);
                                *operand = Some(index);
                            }
                        }
                    }
                }
            }
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

    #[derive(Debug)]
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

    fn fix_imm(locations : &mut Vec<usize>, instr : &mut Vec<Instruction>, operands : &mut Vec<Operand>, first : &mut usize, second : &mut usize, op : BasicOperation, location : u64) {
        if let Operand::Immediate { size,.. } = operands[locations[*first]] {
            if op.is_symmetric() && operands[locations[*first]].location().is_some() {
                (*first, *second) = (*second, *first);
            } else {
                instr.push(Instruction::new(
                    BasicOperation::Move, Operands::Two(operands.len(), locations[*first]), Some(operands.len())
                ));
                locations[*first] = operands.len();
                operands.push(Operand::local(Location::new(location, size)));
            }
        }
        if let Operand::Immediate { size, ..} = operands[locations[*second]] {
            if !op.imm_2(size) {
                instr.push(Instruction::new(
                    BasicOperation::Move, Operands::Two(operands.len(), locations[*second]), Some(operands.len())
                ));
                locations[*second] = operands.len();
                operands.push(Operand::local(Location::new(MEM_GEN_REG, size)));
            }
        }
    }

    fn convert_assigment(dest : usize, expr : &Vec<ExpressionData>, instr : &mut Vec<Instruction>, operands : &mut Vec<Operand>) {
        let mut locations = Vec::with_capacity(expr.len());
        let prev_len = instr.len();
        for (i, e) in expr.iter().enumerate() {
            match e.expression {
                Expression::Variable(v) => {
                    let id = v.id.borrow().unwrap();
                    locations.push(id);
                },
                Expression::Operator { mut first, operator, mut second } => {
                    if let Operand::Local {ref mut uses, .. } = operands[locations[first]] {
                        *uses += 1;
                    }
                    if let Operand::Local {ref mut uses, .. } = operands[locations[second]] {
                        *uses += 1;
                    }
                    let size = type_size(e.t);
                    if operator.is_cmp() {
                        fix_imm(&mut locations, instr, operands, &mut first, &mut second, BasicOperation::Cmp, MEM_GEN_REG);
                        instr.push(Instruction::new(BasicOperation::Cmp, Operands::Two(locations[first], locations[second]), None));
                        let basic_operator = match operator {
                            DualOperator::Equal => BasicOperation::SetE,
                            DualOperator::NotEqual => BasicOperation::SetNe,
                            DualOperator::GreaterEqual => if expr[first].t.is_signed() {BasicOperation::SetGE } else {BasicOperation::SetAE},
                            DualOperator::LesserEqual => if expr[first].t.is_signed() {BasicOperation::SetLE} else {BasicOperation::SetBE},
                            DualOperator::Greater => if expr[first].t.is_signed() {BasicOperation::SetG } else {BasicOperation::SetA},
                            DualOperator::Lesser => if expr[first].t.is_signed() {BasicOperation::SetL } else {BasicOperation::SetB},
                            _ => unreachable!("Not is_cmp")
                        };
                        locations.push(operands.len());
                        operands.push(Operand::local(Location::new(MEM_GEN_REG, OperandSize::BYTE)));
                        instr.push(Instruction::new(basic_operator, Operands::One(locations[i]), Some(locations[i])));
                    } else {
                        let (location, op) = match operator {
                            DualOperator::Divide =>
                                (Location::new(RAX, size), if e.t.is_signed() {BasicOperation::SDiv} else {BasicOperation::UDiv}),
                            DualOperator::Multiply => if e.t.is_signed() {
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
                        fix_imm(&mut locations, instr, operands, &mut first, &mut second, op, location.location);
                        locations.push(operands.len());
                        operands.push(Operand::local(location));
                        instr.push(Instruction::new(
                            op, Operands::Two(locations[first], locations[second]),
                            Some(locations[i]))
                        );
                    }
                }
                Expression::SingleOperator { operator, expr } => {
                    if let Operand::Local {ref mut uses, .. } = operands[locations[expr]] {
                        *uses += 1;
                    }
                    match operator {
                        SingleOperator::Not => {
                            if let Operand::Immediate {size, ..} = operands[locations[expr]] {
                                if !BasicOperation::Xor.imm_2(size) {
                                    instr.push(Instruction::new(
                                        BasicOperation::Move, Operands::Two(operands.len(), locations[expr]), Some(operands.len())
                                    ));
                                    locations[expr] = operands.len();
                                    operands.push(Operand::local(Location::new(MEM_GEN_REG, size)));
                                }
                            }
                            locations.push(operands.len());
                            operands.push(Operand::local(Location::new(MEM_GEN_REG, OperandSize::BYTE)));
                            instr.push(Instruction::new(
                                BasicOperation::Xor,
                                Operands::Two(locations[expr], operands.len()),
                                Some(locations[i])));
                            operands.push(Operand::Immediate {value : 1, size : OperandSize::BYTE});
                        }
                        SingleOperator::Pass => unreachable!("Should not exist at this point.")
                    }
                }
                Expression::IntLiteral(val) => {
                    locations.push(operands.len());
                    operands.push(Operand::Immediate {value : val, size : type_size(e.t)});
                }
                Expression::BoolLiteral(b) => {
                    locations.push(operands.len());
                    operands.push(Operand::Immediate {value : if b {1} else {0}, size : OperandSize::BYTE});
                }
                Expression::None => unreachable!("Should not exist ever.")
            };
        }
        if prev_len == instr.len() { // Whole expression was only a variable or immediate value.
            let instruction = Instruction::new(BasicOperation::Move, Operands::Two(dest, *locations.last().unwrap()), Some(dest));
            instr.push(instruction);
        } else if let Some(instruction) = instr.last_mut() {
            //operands[instruction.dest.unwrap()] = Operand::None;
            instruction.dest = Some(dest);
        }
    }

    fn combine(instr : Vec<Instruction>, operands : &mut Vec<Operand>) -> Vec<Instruction> {
        let mut new_instr = Vec::with_capacity(instr.len() + instr.len() / 5);

        let mut register_state = RegisterState::new(operands);

        for mut instruction in instr {
            match instruction.operands {
                Operands::One(ref mut first) => {
                    if let Operand::Redirect(pos) = operands[*first] {
                        *first = pos;
                    }
                    if let Operand::Local { uses,ref mut used, .. } = operands[*first] {
                        *used += 1;
                        let dest = instruction.dest.unwrap(); // All instructions with one operand (so far) have a dest.
                        if *used < uses && *first != dest { //TODO maybe deal with incompatible destination / first operand
                            new_instr.push(Instruction::new(BasicOperation::Move, Operands::Two(dest, *first), Some(dest)));
                        } else if *first != dest{
                            operands[dest] = Operand::Redirect(*first);
                            instruction.dest = Some(*first);
                        }
                    }
                    new_instr.push(instruction);
                },
                Operands::Two(ref mut first, ref mut second) => {
                    if let Operand::Redirect(pos) = operands[*first] {
                        *first = pos;
                    }
                    if let Operand::Redirect(pos) = operands[*second] {
                        *second = pos;
                    }

                    // Handle first operand being an immediate value
                    if let Operand::Immediate { size, .. } = operands[*first] {
                        if instruction.operator.is_symmetric() && operands[*second].location().is_some() {
                            (*first, *second) = (*second, *first);
                        } else  {
                            new_instr.push(Instruction::new(
                                BasicOperation::Move, Operands::Two(operands.len(), *first), Some(operands.len())
                            ));
                            *first = operands.len();
                            let location = if let Some(dest) = instruction.dest {
                                *operands[dest].location().unwrap()
                            } else {
                                Location::new(MEM_GEN_REG, size)
                            };
                            operands.push(Operand::Local{
                                location, uses: 1, // All imm values are used once
                                used: 0, });
                            register_state.add_operand();
                        }
                    }
                    if let Operand::Local {ref mut used, ..} = operands[*second] {
                        *used += 1;
                    }
                    if let Operand::Local { uses, ref mut used, ..} = operands[*first] {
                        *used += 1;

                        if let Some(dest) = instruction.dest {
                            if *used < uses && *first != dest { // first == dest when a move was inserted due to immediate assignment.
                                new_instr.push(Instruction::new(BasicOperation::Move, Operands::Two(dest, *first), Some(dest)));
                            } else if *first != dest {
                                println!("Redirect: {}, {}", dest, *first);
                                operands[dest] = Operand::Redirect(*first);
                                instruction.dest = Some(*first);
                            }
                        }
                    } else {
                        unreachable!("Should be local");
                    }
                    new_instr.push(instruction);
                }
            }
        }
        return new_instr;
    }

    pub fn compile_statements(statements : &Vec<StatementData>, vars : &HashMap<String, &parser::Variable>)  {
        let mut instr = Vec::new();
        let mut operands = vars.iter().enumerate().map(|(i, (_, v))|{
            v.id.replace(Some(i));
            Operand::local(type_location(v.var_type))
        }).collect::<Vec<_>>();

        for statement in statements {
            match &statement.statement { Statement::Assignment { var, expr } => {
                convert_assigment(var.id.borrow().unwrap(), &expr, &mut instr, &mut operands );
            }}
        }

        for var in vars {
            println!("{}, {}", var.0, var.1.id.borrow().unwrap());
        }
        for (i, o ) in instr.iter().enumerate() {
            let out = |index| match operands[index] {
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
        for (i, o ) in operands.iter().enumerate() {
            println!("{}, {:?}",i, o);
        }
        instr = combine(instr, &mut operands);

    }
}