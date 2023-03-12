use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Debug};
use bumpalo::Bump;
use std::str::FromStr;
use crate::language::amd_win64::compiler::InstructionBuilder;
use crate::language::expression_builder::ExpressionBuilder;
use crate::language::operator::{DualOperator, SingleOperator};
use crate::language::types::Type;
use crate::language::parse_error::{ParseError, ParseErrorType};


#[derive(Debug)]
pub struct Variable {
    pub var_type : Type,
    pub global : bool,

    pub id : RefCell<Option<usize>>
}

impl Variable {
    fn new(var_type : Type) -> Variable {
        Variable {
            var_type, global : false,
            id : RefCell::new(None)
        }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        return std::ptr::eq(self, other);
    }
}


#[derive(Debug)]
pub enum Expression <'a> {
    Variable(&'a Variable),
    Operator {first : usize, operator : DualOperator, second : usize},
    SingleOperator {operator : SingleOperator, expr :  usize},
    IntLiteral(u64),
    BoolLiteral(bool),
    None
}

#[derive(Debug)]
pub struct ExpressionData<'a> {
    pub expression : Expression<'a>,
    pub t : Type,
    pub pos : (usize, usize)
}

impl <'a> ExpressionData<'a> {
    pub fn new(e : Expression<'a>, pos : (usize, usize)) -> ExpressionData<'a>{
        ExpressionData {
            expression : e,
            t : Type::Any,
            pos
        }
    }
}

impl <'a> Display for Expression<'a> {
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

pub enum Statement <'a>{
    Assignment {var : &'a Variable, expr : Vec<ExpressionData<'a>>}
}

pub struct StatementData<'a> {
    pub(crate) statement : Statement<'a>,
    pos : (usize, usize)
}

impl <'a> StatementData<'a> {
    fn new(statement : Statement<'a>, pos : (usize, usize)) -> StatementData<'a> {
        StatementData {
            statement,
            pos
        }
    }
}

struct Program <'a> {
    statements : Vec<StatementData<'a>>
}

struct Parser<'a> {
    chars : std::str::Chars<'a>,
    arena : &'a Bump,
    types : HashMap<String, Type>,
    variables : HashMap<String, &'a Variable>,
    row : usize,
    col : usize,
    cr : bool
}



impl <'a>Parser<'a> {
    const SPACES : [char; 4] = ['\t', '\n', '\r', ' '];
    const KEYWORDS : [&'static str; 2] = ["start", "end"];

    fn new(data : &'a str, arena : &'a Bump) -> Parser<'a> {
        let types = Type::create_primitives();
        let mut variables : HashMap<String, &Variable> = HashMap::new();
        variables.insert("exit_code".to_owned(), arena.alloc(Variable::new(Type::U32)));
        let chars = data.chars();
        Parser {
            chars,
            arena,
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
            self.arena.alloc(Variable::new(t))
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

    fn parse_expression<'b>(&'b mut self, expressions : &mut Vec<ExpressionData<'a>>) -> Result<(), ParseError> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut builder = ExpressionBuilder::new();
        loop {
            let (row, col) = self.get_pos();
            let next = self.peek_char().ok_or_else(||ParseError::new(ParseErrorType::EOF, (row, col)))?;
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
                    } else if let Some(&var) = self.variables.get(word) {
                        expr = Expression::Variable(var);
                    } else {
                        return Err(ParseError::new_at(
                            ParseErrorType::UnexpectedLiteral(word.to_owned()), row, col));
                    }
                }
            };
            builder.add_atom(expr, self.get_pos());
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
        builder.into_expression(expressions);
        Ok(())
    }

    pub(crate) fn type_validate(&self, statement: &mut StatementData) -> Result<(), ParseError> {
        match &mut statement.statement {
            Statement::Assignment {var,expr} => {
                let mut target_types = Vec::with_capacity(expr.len());
                for e in expr.iter_mut() {
                    let err_map = |(t1, t2)| ParseError::new(ParseErrorType::TypeError(t1, t2), e.pos);
                    let t = match e.expression {
                        Expression::Variable(v) => {
                            v.var_type
                        }
                        Expression::Operator { first, second, operator } => {
                            operator.resolve_type(&target_types[first], &target_types[second])
                                .map_err(err_map)?
                        }
                        Expression::SingleOperator { operator, expr} => {
                            operator.resolve_type(&target_types[expr]).map_err(err_map)?
                        }
                        Expression::IntLiteral(_) => Type::AnyInt,
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



fn parse_declarations<'a> (parser : &mut Parser) -> Result<(), ParseError> {
    loop {
        let word = parser.read_word()?;
        match word {
            "start" => return Ok(()),
            word => {
                if let Some(&t) = parser.types.get(word) {
                    let name = parser.read_word()?;
                    parser.add_variable(name, t)?;
                    parser.assert_char(';')?;
                } else {
                    return Err(ParseError::new(
                        ParseErrorType::UnknownType(word.to_owned()),
                        parser.get_pos()
                    ));
                }
            }
        }
    }
}

fn parse_program(mut parser : Parser) -> Result<Program, ParseError> {
    parse_declarations(&mut parser)?;
    let mut statements = Vec::new();
    loop {
        let pos = parser.get_pos();
        let word = parser.read_word()?;
        if word == "end" {
            break;
        }
        let var = *parser.variables.get(word).ok_or_else(||
            ParseError::new(ParseErrorType::UnexpectedLiteral(word.to_owned()), pos)
        )?;
        let (row, col) = parser.get_pos();
        parser.assert_char('=')?;
        let mut expressions = Vec::new();
        parser.parse_expression(&mut expressions)?;
        parser.assert_char(';')?;
        statements.push(StatementData::new(Statement::Assignment {
            var,
            expr : expressions
        }, (row, col - word.chars().count())));
    }
    for statement in &mut statements {
        parser.type_validate(statement)?;
    }
    InstructionBuilder::new(parser.arena, &parser.variables)
        .with(&statements).compile();
    println!("{:?}", parser.variables);
    Ok(Program {
        statements
    })
}

pub fn read_program(file : String) {
    let arena = Bump::new();
    let data = std::fs::read_to_string(file).unwrap();
    let program = parse_program(
        Parser::new(&data, &arena)
    );
    if let Err(e) = program {
        println!("{}", e);
    }
}