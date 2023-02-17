mod amd_win64 {
    use crate::language::parser::{Statement, Expression, Variable, DualOperator, Type};
    use std::collections::HashMap;
    use std::any::Any;

    struct Compiler {

    }

    enum BasicOperation {
        SMul, UMul, Add, Sub, SDiv, UDiv, Move, Push, Pop
    }

    enum Operands {
        Two(usize, usize), One(usize)
    }

    struct Instruction {
        operator : BasicOperation,
        operands : Operands,
        creation : usize,
        uses : usize,
        first_use : usize,
        last_use : usize
    }

    impl Instruction {
        fn new(operator : BasicOperation, operands : Operands, creation : usize) -> Instruction {
            Instruction {
                operator, operands, creation, uses : 0, first_use : 0, last_use : 0
            }
        }
    }


    fn compile_statements(statements : Vec<Statement>, mut variables : HashMap<String, Variable>)  {
        let mut instr = Vec::new();
        let mut var_pos = vec![None; 3];
        variables.iter_mut().enumerate().for_each(|(i, (_, v))| v.offset = i);
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
                                DualOperator::Equal => {


                                },
                                DualOperator::NotEqual => {}
                                DualOperator::GreaterEqual => {}
                                DualOperator::LesserEqual => {}
                                DualOperator::Greater => {}
                                DualOperator::Lesser => {}
                                DualOperator::BoolAnd => {}
                                DualOperator::BoolOr => {}
                            };

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