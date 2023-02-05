use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Debug};
use bumpalo::Bump;
use std::str::FromStr;
use crate::language::expression_builder::ExpressionBuilder;

#[derive(Debug)]
pub(super) enum ParseErrorType {
    Unknown,
    UnexpectedCharacter(char),
    UnexpectedLiteral(String),
    UnknownType(String),
    BadVarName(String),
    InvalidLiteral(String),
    UnmatchedParen,
    TypeError(String),
    EOF
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorType::Unknown => f.write_str("Unknown error"),
            ParseErrorType::UnexpectedLiteral(s) =>
                f.write_fmt(format_args!("Syntax Error : Unexpected literal '{}'", s)),
            ParseErrorType::UnexpectedCharacter(c) =>
                f.write_fmt(format_args!("Syntax Error : Unexpected character '{}'", c)),
            ParseErrorType::UnknownType(s) =>
                f.write_fmt(format_args!("Syntax Error : Unknown type {}", s)),
            ParseErrorType::BadVarName(s) =>
                f.write_fmt(format_args!("Invalid Variable Name : {}", s)),
            ParseErrorType::EOF => f.write_str("Unexpected end of file"),
            ParseErrorType::UnmatchedParen =>
                f.write_fmt(format_args!("Syntax Error : Unmatched parentheses")),
            ParseErrorType::TypeError(s) =>
                f.write_fmt(format_args!("Type Error : {}", s)),
            ParseErrorType::InvalidLiteral(s) =>
                f.write_fmt(format_args!("Invalid literal : '{}'", s))
        }
    }
}

struct ParseError {
    error_type : ParseErrorType,
    pos : (usize, usize)
}

impl ParseError {
    fn new(error : ParseErrorType, parser : &Parser) -> ParseError {
        let (row, col) = parser.get_pos();
        match error {
            ParseErrorType::UnexpectedCharacter(_) | ParseErrorType::UnmatchedParen => ParseError {
                error_type : error, pos : (row, col - 1)
            },
            ParseErrorType::UnexpectedLiteral(ref s) |
            ParseErrorType::UnknownType(ref s) |
            ParseErrorType::InvalidLiteral(ref s) |
            ParseErrorType::BadVarName(ref s) => {
                let len = s.chars().count();
                ParseError {
                    error_type : error, pos : (row, col - len)
                }
            },
            ParseErrorType::TypeError(_) |
            ParseErrorType::Unknown | ParseErrorType::EOF => ParseError::new_at(
                error, row, col
            )
        }
    }

    fn new_at(error : ParseErrorType, row : usize, col : usize) -> ParseError{
        ParseError {
            error_type : error,
            pos : (row, col)
        }
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt( format_args!("{} at line {}, col {}", self.error_type, self.pos.0 + 1, self.pos.1 + 1))
    }
}

impl Error for ParseError {}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Type {
    S32,
    U32,
    Bool,
    Plain {id : usize},
    AnyInt,
    Any
}

impl Type {
    fn matches(&self, other : &Type) -> Result<Type, ()> {
        match (self, other) {
            (Type::Any, t) | (t, Type::Any) => Ok(*t),
            (Type::AnyInt, Type::S32) | (Type::S32, Type::AnyInt) => Ok(Type::S32),
            (Type::AnyInt, Type::U32) | (Type::U32, Type::AnyInt) => Ok(Type::U32),
            _ if self == other => Ok(*self),
            _ => Err(())
        }
    }
}


#[derive(Debug)]
pub struct Variable {
    var_type : Type
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        return std::ptr::eq(self, other);
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DualOperator {
    Divide = 0,
    Multiply = 1,
    Minus = 2,
    Plus = 3,
    // BITWISE : 4 - 8
    Equal = 4,
    NotEqual = 5,
    GreaterEqual = 6,
    LesserEqual = 7,
    Greater = 8,
    Lesser = 9,
    BoolAnd = 10,
    BoolOr = 11,
}

impl Display for DualOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", match self {
            DualOperator::Divide => "/",
            DualOperator::Multiply => "*",
            DualOperator::Minus => "-",
            DualOperator::Plus => "+",
            DualOperator::Equal => "==",
            DualOperator::NotEqual => "!=",
            DualOperator::GreaterEqual => ">=",
            DualOperator::LesserEqual => "<=",
            DualOperator::Greater => ">",
            DualOperator::Lesser => "<",
            DualOperator::BoolAnd => "&&",
            DualOperator::BoolOr => "||"
        }))
    }
}


#[derive(Debug, Copy, Clone)]
pub enum SingleOperator {
    Not = -1,
    Pass = isize::MAX,
}

impl Display for SingleOperator{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", match self {
            SingleOperator::Not => "!",
            SingleOperator::Pass => ""
        }))
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

enum Statement <'a>{
    Assignment {var : &'a Variable, expr : Vec<Expression<'a>>}
}

struct Program <'a> {
    statements : Vec<Statement<'a>>
}

struct Parser<'a> {
    data : &'a str,
    chars : std::str::Chars<'a>,
    arena : &'a Bump,
    types : HashMap<String, Type>,
    variables : HashMap<String, &'a Variable>
}

fn create_primitives(arena : &Bump) -> HashMap<String, Type> {
    let mut types : HashMap<String, Type> = HashMap::new();
    types.insert(String::from("bool"), Type::Bool);
    types.insert(String::from("s32"), Type::S32);
    types.insert(String::from("u32"), Type::U32);
    types
}

impl <'a>Parser<'a> {
    const SPACES : [char; 4] = ['\t', '\n', '\r', ' '];
    const KEYWORDS : [&'static str; 2] = ["start", "end"];

    fn new(data : &'a str, arena : &'a Bump) -> Parser<'a> {
        let types = create_primitives(arena);
        let mut variables : HashMap<String, &Variable> = HashMap::new();
        variables.insert("exit_code".to_owned(), arena.alloc(Variable {
            var_type : Type::U32
        }));
        let chars = data.chars();
        Parser {
            data,
            chars,
            arena,
            types,
            variables
        }
    }

    fn skip_while(&mut self, pred : fn(&char) -> bool) {
        for _ in self.chars.clone().take_while(pred) {
            self.chars.next();
        }
    }

    fn next_matching(&mut self, pred : fn(&char) -> bool) -> Result<char, ParseError> {
        (&mut self.chars).filter(pred).next()
            .ok_or(ParseError::new(ParseErrorType::EOF, self))
    }

    fn read_until<'b>(&'b mut self, pred : fn(char) -> bool) -> &'a str {
        let slice = self.chars.as_str();
        let end = self.chars.clone().position(pred).unwrap_or(slice.len());
        (&mut self.chars).take(end).count();
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
            Err(ParseError::new(ParseErrorType::UnexpectedCharacter(next), self))
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
                    Err(ParseError::new(ParseErrorType::UnexpectedCharacter(c), self))
                } else {
                    Err(ParseError::new(ParseErrorType::EOF, self))
                }
            },
            w => Ok(w)
        }
    }

    fn add_variable(&mut self, name : &str, t : Type) -> Result<(), ParseError> {
        if Parser::KEYWORDS.iter().any(|&s| s == name) {
            return Err(ParseError::new(
                ParseErrorType::BadVarName(name.to_owned()), self
            ));
        }
        self.variables.insert(
            name.to_owned(),
            self.arena.alloc(Variable {
                var_type : t
            })
        );
        Ok(())
    }

    fn skip(&mut self) {
        self.chars.next();
    }

    fn read_dual_operator(&mut self) -> Option<DualOperator> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut mark = self.chars.clone();
        match self.chars.next() {
            Some('+') => return Some(DualOperator::Plus),
            Some('-') => return Some(DualOperator::Minus),
            Some('*') => return Some(DualOperator::Multiply),
            Some('/') => return Some(DualOperator::Divide),
            Some(c) => {
                match (c, self.chars.next()) {
                    ('=', Some('=')) => return Some(DualOperator::Equal),
                    ('!', Some('=')) => return Some(DualOperator::NotEqual),
                    ('>', Some('=')) => return Some(DualOperator::GreaterEqual),
                    ('<', Some('=')) => return Some(DualOperator::LesserEqual),
                    ('&', Some('&')) => return Some(DualOperator::BoolAnd),
                    ('|', Some('|')) => return Some(DualOperator::BoolOr),
                    ('>', _) => {
                        self.chars = mark;
                        self.chars.next();
                        return Some(DualOperator::Greater);
                    },
                    ('<', _) => {
                        self.chars = mark;
                        self.chars.next();
                        return Some(DualOperator::Lesser);
                    },
                    _ => ()
                }
            }
            None => ()
        }
        self.chars = mark;
        None
    }

    fn parse_expression<'b>(&'b mut self, expressions : &mut Vec<Expression<'a>>) -> Result<(), ParseError> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut builder = ExpressionBuilder::new();
        loop {
            let next = self.peek_char().ok_or_else(||ParseError::new(ParseErrorType::EOF, self))?;
            let expr;
            match next {
                '!' => {
                    self.skip();
                    builder.add_single_operator(SingleOperator::Not);
                    continue;
                },
                '(' => {
                    self.skip();
                    builder.open_parentheses();
                    continue;
                },
                '0'..='9' => {
                    let int_str = self.read_until(|c| !('0'..='9').contains(&c));
                    let int = u64::from_str(int_str).map_err(|e|
                        ParseError::new(ParseErrorType::InvalidLiteral(int_str.to_owned()), self))?;
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
                        return Err(ParseError::new(ParseErrorType::InvalidLiteral(word.to_owned()), self));
                    }
                }
            };
            builder.add_atom(expr);
            while let Some(')') = self.peek_char() {
                if let Err(_) = builder.close_parentheses() {
                    break;
                }
                self.skip();
            }
            if let Some(operator) = self.read_dual_operator() {
                builder.add_dual_operator(operator);
            } else {
                break;
            }
        }
        if !builder.is_complete() {
            return Err(ParseError::new(ParseErrorType::UnmatchedParen, self));
        }
        builder.into_expression(expressions);
        Ok(())
    }

    pub(crate) fn type_validate(&self, statement: &Statement) -> Result<(), ParseError> {
        let err_map = |_| ParseError::new(ParseErrorType::TypeError("Pass".to_owned()), self);
        match statement {
            Statement::Assignment {var,expr} => {
                let mut target_types = Vec::with_capacity(expr.len());
                for e in expr.iter() {
                    println!("{:?}", e);
                    target_types.push(match e {
                        Expression::Variable(v) => {
                            v.var_type
                        }
                        Expression::Operator { first, second, operator } => {
                            match operator {
                                DualOperator::Divide | DualOperator::Multiply |
                                DualOperator::Minus | DualOperator::Plus => {
                                    println!("{:?}, {:?}", target_types[*first], target_types[*second]);
                                    Type::AnyInt.matches(&target_types[*first])
                                        .map_err(err_map)?
                                        .matches(&target_types[*second]).map_err(err_map)?
                                }
                                DualOperator::Equal | DualOperator::NotEqual => {
                                    target_types[*first].matches(&target_types[*second]).map_err(err_map)?;
                                    Type::Bool
                                },
                                DualOperator::GreaterEqual |  DualOperator::LesserEqual |
                                DualOperator::Greater |DualOperator::Lesser => {
                                    Type::AnyInt.matches(&target_types[*first]).map_err(err_map)?
                                        .matches(&target_types[*second]).map_err(err_map)?;
                                    Type::Bool
                                },
                                DualOperator::BoolAnd | DualOperator::BoolOr => {
                                    Type::Bool.matches(&target_types[*first]).map_err(err_map)?
                                        .matches(&target_types[*second]).map_err(err_map)?
                                }
                            }
                        }
                        Expression::SingleOperator { operator, expr} => {
                            match operator {
                                SingleOperator::Not => Type::Bool.matches(&target_types[*expr])
                                    .map_err(err_map)?,
                                SingleOperator::Pass => Type::Any
                            }
                        }
                        Expression::IntLiteral(_) => Type::AnyInt,
                        Expression::BoolLiteral(_) => Type::Bool,
                        Expression::None => Type::Any
                    })
                }
                println!("last : {:?}, {:?}", target_types.last().unwrap(), var.var_type);
                target_types.last().unwrap().matches(&var.var_type).map_err(err_map)?;
            }
        }
        Ok(())
    }

    fn get_pos(&self) -> (usize, usize) {
        let cur = self.chars.as_str() as *const str;
        let mut iter = self.data.chars();
        let mut row = 0;
        let mut col = 0;
        let mut cr = false;
        while iter.as_str() as *const str != cur && iter.as_str() != "" {
            match iter.next() {
                Some('\n') => {
                    if !cr {
                        col = 0;
                        row += 1;
                        cr = false;
                    }
                },
                Some('\r') => {
                    col = 0;
                    row += 1;
                    cr = true;
                },
                Some(_) => col += 1,
                None => unreachable!("While should exit before")
            }
        }
        (row, col)
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
                        parser
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
        let word = parser.read_word()?;
        if word == "end" {
            break;
        }
        let var = *parser.variables.get(word).ok_or_else(||
            ParseError::new(ParseErrorType::UnexpectedLiteral(word.to_owned()), &parser)
        )?;
        parser.assert_char('=')?;
        let mut expressions = Vec::new();
        parser.parse_expression(&mut expressions)?;
        parser.assert_char(';')?;
        statements.push(Statement::Assignment {
            var,
            expr : expressions
        });
    }
    for statement in &statements {
        parser.type_validate(statement)?;
    }
    println!("{:?}", parser.variables);
    Ok(Program {
        statements
    })
}

pub fn read_program(file : String) {
    let arena = Bump::new();
    let data = std::fs::read_to_string("test_prog.txt").unwrap();
    let program = parse_program(
        Parser::new(&data, &arena)
    );
    if let Err(e) = program {
        println!("{}", e);
    }
}