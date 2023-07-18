use std::rc::Rc;
use crate::language::parser::{ExpressionData, Parser, Variable};
use crate::language::parser::parse_error::ParseError;

#[derive(Debug)]
pub struct IfStatement {
    pub condition : Option<Vec<ExpressionData>>, // Final else branch can have no condition
    pub block : Vec<StatementData>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition : Vec<ExpressionData>,
    pub block : Vec<StatementData>
}

#[derive(Debug)]
pub enum Statement {
    Assignment {var : Rc<Variable>, expr : Vec<ExpressionData>},
    IfBlock(Vec<IfStatement>),
    WhileBlock(WhileStatement),
    Return(Vec<ExpressionData>),
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

    fn end_block(&mut self, parser: &mut Parser) -> Result<Option<StatementData>, ParseError> {
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

    fn add_statement(&mut self, _parser: &mut Parser, statement: StatementData) -> Result<(), ParseError> {
        self.statements.last_mut().unwrap().block.push(statement);
        Ok(())
    }

    fn end_block(&mut self, parser: &mut Parser) -> Result<Option<StatementData>, ParseError> {
        parser.type_validate(&mut self.statements.last_mut().unwrap().block)?;
        parser.mark();
        let word = parser.read_word();
        return if let Ok("else") = word {
            let condition = if let Ok("if") = parser.read_word() {
                Some(parser.parse_expression()?)
            } else {
                None
            };
            self.statements.push(
                IfStatement {
                    condition,
                    block : vec![],
                }
            );
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

pub struct WhileBlockParser {
    statement : Option<WhileStatement>,
    pos : (usize, usize)
}

impl BlockStatementParser for WhileBlockParser {
    fn begin_block(parser: &mut Parser, pos: (usize, usize)) -> Result<Box<Self>, ParseError> {
        let expr = parser.parse_expression()?;
        parser.assert_char('{')?;
        return Ok(Box::new(WhileBlockParser {
            statement : Some(WhileStatement {
                condition : expr,
                block : vec![]
            }),
            pos
        }));
    }

    fn add_statement(&mut self, _parser: &mut Parser, statement: StatementData) -> Result<(), ParseError> {
        self.statement.as_mut().unwrap().block.push(statement);
        return Ok(());
    }

    fn end_block(&mut self, parser: &mut Parser) -> Result<Option<StatementData>, ParseError> {
        parser.type_validate(&mut self.statement.as_mut().unwrap().block)?;
        return Ok(Some(
            StatementData::new(
                Statement::WhileBlock(self.statement.take().unwrap()),
                self.pos
            )
        ));
    }
}