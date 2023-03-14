mod expression_builder;
mod parse_error;

use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Debug};
use std::rc::Rc;
use bumpalo::Bump;
use std::str::FromStr;
use crate::language::amd_win64::compiler::InstructionBuilder;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::parser::expression_builder::ExpressionBuilder;
use crate::language::types::Type;
use parse_error::{ParseError, ParseErrorType};


#[derive(Debug)]
pub struct Variable {
    pub var_type : Type,
    pub global : bool,

    pub id : Cell<Option<usize>>
}

impl Variable {
    fn new(var_type : Type) -> Variable {
        Variable {
            var_type, global : false,
            id : Cell::new(None)
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Variable(Rc<Variable>),
    Operator {first : usize, operator : DualOperator, second : usize},
    SingleOperator {operator : SingleOperator, expr :  usize},
    IntLiteral(u64),
    BoolLiteral(bool),
    None
}

#[derive(Debug)]
pub struct ExpressionData {
    pub expression : Expression,
    pub t : Type,
    pub pos : (usize, usize)
}

impl ExpressionData {
    pub fn new(e : Expression, pos : (usize, usize)) -> ExpressionData{
        ExpressionData {
            expression : e,
            t : Type::Any,
            pos
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expression::Variable(v) => f.write_fmt(format_args!("{:?}", v.var_type)),
            Expression::Operator {operator, first, second} =>
                f.write_fmt(format_args!("({} {} {})",first, operator, second)),
            Expression::SingleOperator {operator, expr} =>
                f.write_fmt(format_args!("({}{})", operator, expr)),
            Expression::IntLiteral(i) => f.write_fmt(format_args!("{}", i)),
            Expression::BoolLiteral(b) => f.write_fmt(format_args!("{}", b)) ,
            Expression::None => f.write_str("None")
        }
    }
}

pub enum Statement {
    Assignment {var : Rc<Variable>, expr : Vec<ExpressionData>},
    //IfBlock {statements : Vec<Statement>, condition : Expression}
}

pub struct StatementData {
    pub(crate) statement : Statement,
    pos : (usize, usize)
}

impl StatementData {
    fn new(statement : Statement, pos : (usize, usize)) -> StatementData {
        StatementData {
            statement,
            pos
        }
    }
}

struct Program {
    variables : HashMap<String, Rc<Variable>>,
    statements : Vec<StatementData>
}

struct Parser<'a> {
    chars : std::str::Chars<'a>,
    types : HashMap<String, Type>,
    variables : HashMap<String, Rc<Variable>>,
    row : usize,
    col : usize,
    cr : bool
}



impl <'a>Parser<'a> {
    const SPACES : [char; 4] = ['\t', '\n', '\r', ' '];
    const KEYWORDS : [&'static str; 3] = ["start", "end", "if"];

    fn new(data : &'a str) -> Parser<'a> {
        let types = Type::create_primitives();
        let mut variables : HashMap<String, Rc<Variable>> = HashMap::new();
        variables.insert("exit_code".to_owned(), Rc::new(Variable::new(Type::U32)));
        let chars = data.chars();
        Parser {
            chars,
            types,
            variables,
            row : 0,
            col : 0,
            cr : false
        }
    }

    fn advance(&mut self) -> Option<char>{
        let c = self.chars.next();
        match c {
            Some('\n') => {
                if !self.cr {
                    self.col = 0;
                    self.row += 1;
                }
            },
            Some('\r') => {
                self.col = 0;
                self.row += 1;
                self.cr = true;
            },
            Some(_) => {
                self.cr = false;
                self.col += 1;
            },
            None => {}
        };
        return  c;
    }

    fn skip_while(&mut self, pred : fn(&char) -> bool) {
        for _ in self.chars.clone().take_while(pred) {
            self.advance();
        }
    }

    fn next_matching(&mut self, pred : fn(&char) -> bool) -> Result<char, ParseError> {
        loop {
          match self.advance() {
              Some(c)  if pred(&c) => return Ok(c),
              Some(_) => {},
              None => return Err(ParseError::new(ParseErrorType::EOF, self.get_pos()))
          }
        }
    }

    fn read_until<'b>(&'b mut self, pred : fn(char) -> bool) -> &'a str {
        let slice = self.chars.as_str();
        let end = self.chars.clone().position(pred).unwrap_or(slice.len());
        for _ in 0..end {
            self.advance();
        }
        &slice[..end]
    }

    fn read_char(&mut self) -> Result<char, ParseError> {
        self.next_matching(|c| !Parser::SPACES.contains(c))
    }

    fn peek_char(&mut self) -> Option<char> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        self.chars.clone().next()
    }

    fn assert_char(&mut self, c : char) -> Result<(), ParseError> {
        let next = self.read_char()?;
        if next != c {
            Err(ParseError::new(ParseErrorType::UnexpectedCharacter(next), self.get_pos()))
        } else {
            Ok(())
        }
    }

    fn read_word<'b>(&'b mut self) -> Result<&'a str, ParseError> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let word = self.read_until(|c| !c.is_ascii_alphanumeric() && c != '_');
        match word {
            "" => {
                if let Some(c) = self.chars.clone().next() {
                    Err(ParseError::new(ParseErrorType::UnexpectedCharacter(c), self.get_pos()))
                } else {
                    Err(ParseError::new(ParseErrorType::EOF, self.get_pos()))
                }
            },
            w => Ok(w)
        }
    }

    fn add_variable(&mut self, name : &str, t : Type) -> Result<(), ParseError> {
        if Parser::KEYWORDS.iter().any(|&s| s == name) {
            return Err(ParseError::new(
                ParseErrorType::BadVarName(name.to_owned()), self.get_pos()
            ));
        }
        self.variables.insert(
            name.to_owned(),
            Rc::new(Variable::new(t))
        );
        Ok(())
    }

    fn read_dual_operator(&mut self) -> Option<DualOperator> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut mark = self.chars.clone();
        let(steps, operator) = match mark.next() {
            Some('+') => (1, Some(DualOperator::Plus)),
            Some('-') => (1, Some(DualOperator::Minus)),
            Some('*') => (1, Some(DualOperator::Multiply)),
            Some('/') => (1, Some(DualOperator::Divide)),
            Some(c) => {
                match (c, mark.next()) {
                    ('=', Some('=')) => (2, Some(DualOperator::Equal)),
                    ('!', Some('=')) => (2, Some(DualOperator::NotEqual)),
                    ('>', Some('=')) => (2, Some(DualOperator::GreaterEqual)),
                    ('<', Some('=')) => (2, Some(DualOperator::LesserEqual)),
                    ('&', Some('&')) => (2, Some(DualOperator::BoolAnd)),
                    ('|', Some('|')) => (2, Some(DualOperator::BoolOr)),
                    ('>', _) => (1, Some(DualOperator::Greater)),
                    ('<', _) => (1, Some(DualOperator::Lesser)),
                    _ => (0, None)
                }
            }
            None => (0, None)
        };
        for _ in 0..steps {
            self.advance();
        }
        return operator;
    }

    fn parse_expression(&mut self) -> Result<Vec<ExpressionData>, ParseError> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut builder = ExpressionBuilder::new();
        loop {
            let (mut row, mut col) = self.get_pos();
            let next = self.peek_char().ok_or_else(||ParseError::new(ParseErrorType::EOF, (row, col)))?;
            (row, col) = self.get_pos();
            let expr;
            match next {
                '!' => {
                    self.advance();
                    builder.add_single_operator(SingleOperator::Not, (row, col));
                    continue;
                },
                '(' => {
                    self.advance();
                    builder.open_parentheses();
                    continue;
                },
                '0'..='9' => {
                    let int_str = self.read_until(|c| !('0'..='9').contains(&c));
                    let int = u64::from_str(int_str).map_err(|_|
                        ParseError::new_at(ParseErrorType::InvalidLiteral(int_str.to_owned()),row, col))?;
                    expr = Expression::IntLiteral(int);
                },
                _ => {
                    let word = self.read_word()?;
                    if word == "true" {
                        expr = Expression::BoolLiteral(true);
                    } else if word == "false" {
                        expr =Expression::BoolLiteral(false);
                    } else if let Some(var) = self.variables.get(word) {
                        expr = Expression::Variable(var.clone());
                    } else {
                        return Err(ParseError::new_at(
                            ParseErrorType::UnexpectedLiteral(word.to_owned()), row, col));
                    }
                }
            };
            builder.add_atom(expr, (row, col));
            while let Some(')') = self.peek_char() {
                if let Err(_) = builder.close_parentheses() {
                    break;
                }
                self.advance();
            }
            if let Some(operator) = self.read_dual_operator() {
                builder.add_dual_operator(operator);
            } else {
                break;
            }
        }
        if !builder.is_complete() {
            return Err(ParseError::new(ParseErrorType::UnmatchedParen, self.get_pos()));
        }
        Ok(builder.into_expression())
    }

    fn parse_declarations(&mut self) -> Result<(), ParseError> {
        loop {
            let word = self.read_word()?;
            match word {
                "start" => return Ok(()),
                word => {
                    if let Some(&t) = self.types.get(word) {
                        let name = self.read_word()?;
                        self.add_variable(name, t)?;
                        self.assert_char(';')?;
                    } else {
                        return Err(ParseError::new(
                            ParseErrorType::UnknownType(word.to_owned()),
                            self.get_pos()
                        ));
                    }
                }
            }
        }
    }

    /*fn parse_statement(&mut self) -> Result<StatementData, ParseError> {
        let pos = self.get_pos();
        let word = self.read_word()?;
        match word {
            "if" => {
                let expression = self.parse_expression()?;

            }



        }
        Ok(Statement::IfBlock {statements : Vec::new()})
    }*/

    pub fn parse(mut self) -> Result<Program, ParseError> {
        self.parse_declarations()?;
        let mut statements = Vec::new();
        loop {
            let pos = self.get_pos();
            let word = self.read_word()?;
            match word {
                "end" => break,
                "if" => {

                },
                _ => { // Default to assignment.
                    let var = self.variables.get(word).ok_or_else(||
                        ParseError::new(ParseErrorType::UnexpectedLiteral(word.to_owned()), pos)
                    )?.clone();
                    let (row, col) = self.get_pos();
                    self.assert_char('=')?;
                    let expressions = self.parse_expression()?;
                    self.assert_char(';')?;
                    statements.push(StatementData::new(Statement::Assignment {
                        var,
                        expr : expressions
                    }, (row, col - word.chars().count())));
                }

            }
            if word == "end" {
                break;
            }


        }
        for statement in &mut statements {
            self.type_validate(statement)?;
        }

        Ok(Program {
            statements, variables : self.variables
        })
    }

    pub(crate) fn type_validate(&self, statement: &mut StatementData) -> Result<(), ParseError> {
        match &mut statement.statement {
            Statement::Assignment {var,expr} => {
                let mut target_types = Vec::with_capacity(expr.len());
                for e in expr.iter_mut() {
                    let err_map = |(t1, t2)| ParseError::new(ParseErrorType::TypeError(t1, t2), e.pos);
                    let t = match &e.expression {
                        Expression::Variable(v) => {
                            v.var_type
                        }
                        Expression::Operator { first, second, operator } => {
                            operator.resolve_type(&target_types[*first], &target_types[*second])
                                .map_err(err_map)?
                        }
                        Expression::SingleOperator { operator, expr} => {
                            operator.resolve_type(&target_types[*expr]).map_err(err_map)?
                        }
                        Expression::IntLiteral(val) => {
                            match *val {
                                0..=4294967295 => Type::AnyInt,
                                _ => return Err(ParseError::new(ParseErrorType::InvalidLiteral(val.to_string()), e.pos))
                            }

                        },
                        Expression::BoolLiteral(_) => Type::Bool,
                        Expression::None => unreachable!("None expressions are only created for mem::replace")
                    };
                    target_types.push(t);
                    e.t = t;
                }
                let t = target_types.last().unwrap().resolve(&var.var_type).map_err(
                    |(t1, t2)| ParseError::new(
                            ParseErrorType::TypeError(t1, t2),
                            statement.pos))?;
                *target_types.last_mut().unwrap() = t.finalize();
                for (i, e) in expr.iter_mut().enumerate().rev() {
                    e.t = target_types[i];
                    match e.expression {
                        Expression::Operator { first, operator, second } => {
                            let (t1, t2) = operator.finalize_type(
                                &e.t, &target_types[first], &target_types[second]);
                            target_types[first] = t1;
                            target_types[second] = t2;
                        }
                        Expression::SingleOperator { .. } => {}
                        Expression::None => unreachable!("None expressions are only created for mem::replace"),
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }

    fn get_pos(&self) -> (usize, usize) {
        (self.row, self.col)
    }
}

pub fn read_program(file : String) {
    let arena = Bump::new();
    let data = std::fs::read_to_string(file).unwrap();
    let program = Parser::new(&data).parse();
    let program = match program {
        Ok(program) => program,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    InstructionBuilder::new(&arena, &program.variables)
        .with(&program.statements).compile();

}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::language::operator::DualOperator;
    use crate::language::parser::{Expression, Parser, Statement};
    use crate::language::types::Type;

    #[test]
    fn parse_empty() {
        let data = "start\nend";
        let parser = Parser::new(data);
        let res = parser.parse();
        assert!(res.is_ok(), "Failed parsing empty program");
        let res = res.unwrap();
        assert!(res.statements.is_empty(), "Got statement from empty program");
        assert!(res.variables.get("exit_code").is_some(), "Missing exit code in empty program");
        assert_eq!(res.variables.len(), 1, "Extra variables in empty program");
    }

    #[test]
    fn parse_simple() {
        let data = "u32 x\n;u32 y;\nstart\nx=10    ;y   =\t 20;\r\nexit_code=x+y;end";
        let parser = Parser::new(data);
        let res = parser.parse();
        assert!(res.is_ok(), "Failed parsing simple program");
        let res = res.unwrap();
        assert_eq!(res.statements.len(), 3, "Expected 3 statements, got {}", res.statements.len());
        assert!(res.variables.get("x").is_some(), "Missing x variable");
        assert_eq!(res.variables.get("x").unwrap().var_type, Type::U32, "Wrong type on x, expected U32, got {}",
                   res.variables.get("x").unwrap().var_type);
        assert!(res.variables.get("y").is_some(), "Missing y variable");
        assert_eq!(res.variables.get("y").unwrap().var_type, Type::U32, "Wrong type on y, expected U32, got {}",
                   res.variables.get("y").unwrap().var_type);
        assert_eq!(res.variables.len(), 3, "Expected 3 variables, got {}", res.variables.len());
        let first = &res.statements[0];
        let second = &res.statements[1];
        let third = &res.statements[2];
        assert_eq!(first.pos, (3, 0));
        assert_eq!(second.pos, (3, 9));
        assert_eq!(third.pos, (4, 0));
        match &first.statement {
            Statement::Assignment { var, expr } => {
                assert!(Rc::ptr_eq(var, res.variables.get("x").unwrap()));
                assert_eq!(expr.len(), 1);
                let expr = &expr[0];
                assert_eq!(expr.pos, (3, 2));
                match &expr.expression {
                    Expression::IntLiteral(val) => {
                        assert_eq!(*val, 10);
                    }
                    e => panic!("Expected IntLiteral, got {:?}", e)
                }
            }
        }
        match &second.statement {
            Statement::Assignment { var, expr } => {
                assert!(Rc::ptr_eq(var, res.variables.get("y").unwrap()));
                assert_eq!(expr.len(), 1);
                let expr = &expr[0];
                assert_eq!(expr.pos, (3, 16));
                match &expr.expression {
                    Expression::IntLiteral(val) => {
                        assert_eq!(*val, 20);
                    }
                    e => panic!("Expected IntLiteral, got {:?}", e)
                }
            }
        }
        match &third.statement {
            Statement::Assignment { var, expr } => {
                assert!(Rc::ptr_eq(var, res.variables.get("exit_code").unwrap()));
                assert_eq!(expr.len(), 3);
                let e1 = &expr[0];
                let e2 = &expr[1];
                assert_eq!(e1.pos, (4, 10));
                assert_eq!(e2.pos, (4, 12));
                match (&e1.expression, &e2.expression) {
                    (Expression::Variable(v1), Expression::Variable(v2)) => {
                        assert!(Rc::ptr_eq(v1, res.variables.get("x").unwrap()));
                        assert!(Rc::ptr_eq(v2, res.variables.get("y").unwrap()));
                    },
                    (e1, e2) => panic!("Expected variables, got {:?}, {:?}", e1, e2)
                }
                let e3 = &expr[2];
                assert_eq!(e3.pos, (4, 10));
                match &e3.expression {
                    Expression::Operator { first, second, operator } => {
                        assert_eq!(*operator as usize, DualOperator::Plus as usize);
                        assert_eq!(*first, 0);
                        assert_eq!(*second, 1);
                    }
                    e => panic!("Expected operator, got {:?}", e)
                }
            }
        }
    }
}