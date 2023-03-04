mod amd_win64 {
    use crate::language::parser::{self, Statement, Expression, DualOperator, Type, ExpressionData};
    use std::collections::HashMap;
    use std::any::Any;

    enum BasicOperation {
        SMul, UMul, Add, Sub, SDiv, UDiv, Move, Push, Pop
    }

    enum Operands {
        One(usize), Two(usize, usize),
    }

    enum OperandSize {
        BYTE, WORD, DWORD, QWORD,
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
    const IMM : u64 = 131072;

    struct Location {
        location : u64, // Bitwise or of all allowed locations
        size : OperandSize
    }

    enum Variable {
        Local {creation : usize, uses : usize, first_use : usize, last_use : usize, location : Location},
        Global {location : Location, symbol : String}
    }

    struct Instruction {
        operator : BasicOperation,
        operands : Operands,
    }

    impl Instruction {
        fn new(operator : BasicOperation, operands : Operands, creation : usize) -> Instruction {
            Instruction {
                operator, operands
            }
        }
    }

    fn convert_assigment(dest : &Variable, expr : &Vec<ExpressionData>, instr : &mut Vec<Instruction>, variables : &mut Vec<Variable>) {
        for (i, e) in expr.iter().enumerate() {
            match e.expression {
                Expression::Variable(v) => {
                    variables.push(Variable::Global {})
                },

            }
        }
    }

    fn compile_statements(statements : Vec<Statement>, globals : HashMap<String, parser::Variable>)  {
        let mut instr = Vec::new();

        for statement in statements {
            match statement { Statement::Assignment { var, expr } => {
                for (i, e ) in expr.iter().enumerate() {
                    match e.expression {
                        Expression::Variable(v) => {
                            if let Some(pos) = var_pos[v.offset] {
                                instr.push(Instruction::new(
                                    BasicOperation::Move, Operands::One(pos), instr.len()
                                ));
                            } else {
                                panic!("Use of non initialized variable");
                            }
                        },
                        Expression::Operator { first, operator, second } => {
                            let basic_operator = match operator {
                                DualOperator::Divide => if e.t.unwrap().is_signed() {
                                    BasicOperation::SDiv
                                } else {
                                    BasicOperation::UDiv
                                },
                                DualOperator::Multiply => if e.t.unwrap().is_signed() {
                                    BasicOperation::SMul
                                } else {
                                    BasicOperation::UMul
                                },
                                DualOperator::Minus => BasicOperation::Sub,
                                DualOperator::Plus => BasicOperation::Add,
                                _ => panic!("Not yet supported")
                            };
                            instr.push(Instruction::new(
                                basic_operator, Operands::Two(var_pos[first].unwrap(), var_pos[second].unwrap()), instr.len()
                            ));
                        },
                        Expression::SingleOperator { .. } => {

                        },
                        Expression::IntLiteral(_) => {

                        },
                        Expression::BoolLiteral(_) => {

                        },
                        Expression::None => unreachable!()
                    }
                }
                var_pos[var.offset] = Some(instr.len());
                instr.push(Instruction::new(BasicOperation::Move, Operands::One(instr.len() - 1), instr.len()));
            }}
        }
    }
}