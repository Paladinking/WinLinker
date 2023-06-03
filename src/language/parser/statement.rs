use std::rc::Rc;
use crate::language::parser::{ExpressionData, Parser, Variable};
use crate::language::parser::parse_error::ParseError;

#[derive(Debug)]
pub struct IfStatement {
    pub condition : Option<Vec<ExpressionData>>, // Final else branch can have no condition
    pub block : Vec<StatementData>,
}

#[derive(Debug)]
pub enum Statement {
    Assignment {var : Rc<Variable>, expr : Vec<ExpressionData>},
    IfBlock(Vec<IfStatement>),
    Block(Vec<StatementData>)
}

#[derive(Debug)]
pub struct StatementData {
    pub statement : Statement,
    pub pos : (usize, usize)
}

impl StatementData {
    pub fn new(statement : Statement, pos : (usize, usize)) -> StatementData {
        StatementData {
            statement,
            pos
        }
    }
}

pub(super) trait BlockStatementParser {
    fn begin_block(parser: &mut Parser, pos: (usize, usize)) -> Result<Box<Self>, ParseError>
        where Self: Sized;

    fn add_statement(&mut self, parser : &mut Parser, statement : StatementData) -> Result<(), ParseError>;

    fn end_block(&mut self, parser: &mut Parser) -> Result<Option<StatementData>, ParseError>;
}

pub struct BlockParser {
    statements : Vec<StatementData>,
    pos : (usize, usize)
}

impl BlockStatementParser for BlockParser {
    fn begin_block(parser: &mut Parser, pos: (usize, usize)) -> Result<Box<Self>, ParseError> {
        parser.assert_char('{')?;
        Ok(Box::new(BlockParser {
            statements : vec![],
            pos,
        }))
    }

    fn add_statement(&mut self, _parser: &mut Parser, statement: StatementData) -> Result<(), ParseError> {
        self.statements.push(statement);
        Ok(())
    }

    fn end_block(&mut self, _parser: &mut Parser) -> Result<Option<StatementData>, ParseError> {
        Ok(Some(StatementData::new(Statement::Block(
            std::mem::take(&mut self.statements),
        ), self.pos)))
    }
}

pub struct IfBlockParser {
    statements : Vec<IfStatement>,
    pos : (usize, usize)
}

impl BlockStatementParser for IfBlockParser {
    fn begin_block(parser: &mut Parser, pos : (usize, usize)) -> Result<Box<Self>, ParseError> {
        let expr = parser.parse_expression()?;
        parser.assert_char('{')?;
        Ok(Box::new(IfBlockParser {
            statements: vec![IfStatement {
                condition: Some(expr),
                block: vec![],
            }],
            pos,
        }))
    }

    fn add_statement(&mut self, parser: &mut Parser, statement: StatementData) -> Result<(), ParseError> {
        self.statements.last_mut().unwrap().block.push(statement);
        Ok(())
    }

    fn end_block(&mut self, parser: &mut Parser) -> Result<Option<StatementData>, ParseError> {
        parser.type_validate(&mut self.statements.last_mut().unwrap().block)?;
        parser.mark();
        let word = parser.read_word();
        println!("word: {:?}", word);
        return if let Ok("else") = word {

            self.statements.push(
                IfStatement {
                    condition: None,
                    block : vec![],
                }
            );
            if parser.peek_char() != Some('{') {
                self.statements.last_mut().unwrap().condition.replace(
                    parser.parse_expression()?);
            }
            parser.assert_char('{')?;
            Ok(None)
        } else {
            parser.reset();
            Ok(Some(StatementData::new(
                Statement::IfBlock(std::mem::take(&mut self.statements)),
                self.pos)))
        }
    }
}