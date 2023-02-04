use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Debug};
use bumpalo::Bump;
use std::str::FromStr;
use std::cmp::Ordering;

#[derive(Debug)]
enum ParseErrorType {
    Unknown,
    UnexpectedCharacter(char),
    UnexpectedLiteral(String),
    UnknownType(String),
    BadVarName(String),
    InvalidLiteral(String),
    UnmatchedParen,
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

#[derive(Debug)]
enum Type<'a> {
    Plain {size : usize, name : &'a str}
}

impl <'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        return std::ptr::eq(self, other);
    }
}


#[derive(Debug)]
struct Variable <'a> {
    var_type : &'a Type<'a>
}

impl <'a> PartialEq for Variable<'a> {
    fn eq(&self, other: &Self) -> bool {
        return std::ptr::eq(self, other);
    }
}

#[derive(Debug, Copy, Clone)]
enum DualOperator {
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

#[derive(Debug, Copy, Clone)]
enum SingleOperator {
    Not = -1
}

#[derive(Debug)]
enum Expression <'a> {
    Variable(&'a Variable<'a>),
    Operator {first : Box<Expression<'a>>, operator : DualOperator, second : Box<Expression<'a>>},
    SingleOperator {operator : SingleOperator, expr :  Box<Expression<'a>>},
    IntLiteral(u64),
    BoolLiteral(bool),
    None // Used while parsing to indicate a subexpression not yet parsed
}

#[derive(Debug)]
enum ExpressionBuilderType <'a>{
    Atom(Box<Expression<'a>>),
    SingleOperator(SingleOperator),
    DualOperator(DualOperator)
}

struct ExpressionBuilderNode<'a> {
    expression_type: ExpressionBuilderType<'a>,
    par : usize,
    parent : *mut ExpressionBuilder<'a>,
    first_child : *mut ExpressionBuilder<'a>,
    second_child : *mut ExpressionBuilder<'a>
}


#[derive(Debug)]
struct  ExpressionBuilder <'a> {
    expression_type: ExpressionBuilderType<'a>,
    par : usize,
    parent : *mut ExpressionBuilder<'a>,
    first_child : *mut ExpressionBuilder<'a>,
    second_child : *mut ExpressionBuilder<'a>
}

impl<'a> ExpressionBuilder <'a> {
    fn get_expression(ptr : *mut ExpressionBuilder<'a>) -> Box<Expression<'a>> {
        let mut res = Box::new(Expression::None);
        let mut stack = vec![(ptr, &mut res)];
        while let Some((top,  dest)) = stack.pop() {
            let mut top = unsafe {Box::from_raw(top)};
            match &mut top.expression_type {
                ExpressionBuilderType::Atom(e) => {
                    std::mem::swap(dest, e);
                },
                ExpressionBuilderType::SingleOperator(s) => {
                    let mut e = Box::new(Expression::SingleOperator {
                        operator: *s, expr: Box::new(Expression::None)
                    });
                    std::mem::swap(dest, &mut e);
                    if let Expression::SingleOperator {ref mut expr, ..} = dest.as_mut() {
                        stack.push((top.first_child, expr));
                    } else {
                        unreachable!();
                    }
                },
                ExpressionBuilderType::DualOperator(s) => {
                    let mut e = Box::new(Expression::Operator {
                        operator: *s, first: Box::new(Expression::None), second : Box::new(Expression::None)
                    });
                    std::mem::swap(dest, &mut e);
                    if let Expression::Operator {ref mut first, ref mut second, ..} = dest.as_mut() {
                        stack.push((top.first_child, first));
                        stack.push((top.second_child, second));
                    } else {
                        unreachable!();
                    }
                }
            }
            drop(top);
        }
        res
    }

    fn priority(&self) -> isize {
        match self.expression_type {
            ExpressionBuilderType::Atom(_) => isize::MIN,
            ExpressionBuilderType::DualOperator(s) => s as isize,
            ExpressionBuilderType::SingleOperator(s) => s as isize
        }
    }

    fn atom(par : usize, expr : Box<Expression<'a>>, parent : *mut ExpressionBuilder<'a>) -> ExpressionBuilder<'a> {
        ExpressionBuilder {
            expression_type : ExpressionBuilderType::Atom(expr),
            par,
            parent,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut()
        }
    }

    fn single_operator(par : usize, op : SingleOperator, parent : *mut ExpressionBuilder<'a>) -> ExpressionBuilder<'a> {
        ExpressionBuilder {
            expression_type : ExpressionBuilderType::SingleOperator(op),
            par,
            parent,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut()
        }
    }

    fn dual_operator(par : usize, op : DualOperator, parent : *mut ExpressionBuilder<'a>) -> ExpressionBuilder<'a> {
        ExpressionBuilder {
            expression_type : ExpressionBuilderType::DualOperator(op),
            par,
            parent,
            first_child : std::ptr::null_mut(),
            second_child : std::ptr::null_mut()
        }
    }
}

impl <'a> Eq for ExpressionBuilder<'a> {}

impl <'a> PartialEq for ExpressionBuilder<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.priority() == other.priority() && self.par == other.par
    }
}

impl <'a> Ord for ExpressionBuilder<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a> PartialOrd for ExpressionBuilder<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let par_cmp = self.par.cmp(&other.par);
        if par_cmp.is_eq() {
            self.priority().partial_cmp(&other.priority())
        } else {
            Some(par_cmp)
        }
    }
}

enum Statement <'a>{
    Assignment {var : &'a Variable<'a>, expr : Expression<'a>}
}

struct Program <'a> {
    statements : Vec<Statement<'a>>
}

struct Parser<'a> {
    data : &'a str,
    chars : std::str::Chars<'a>,
    arena : &'a Bump,
    types : HashMap<String, &'a Type<'a>>,
    variables : HashMap<String, &'a Variable<'a>>
}

fn create_primitives(arena : &Bump) -> HashMap<String, &Type> {
    let mut types : HashMap<String, &Type> = HashMap::new();
    types.insert(String::from("bool"), arena.alloc(Type::Plain {size : 1, name : "bool"}));
    types.insert(String::from("s32"), arena.alloc(Type::Plain {size : 4, name : "s32"}));
    types.insert(String::from("u32"), arena.alloc(Type::Plain {size : 4, name : "u32"}));
    types
}

impl <'a>Parser<'a> {
    const SPACES : [char; 4] = ['\t', '\n', '\r', ' '];
    const KEYWORDS : [&'static str; 2] = ["start", "end"];

    fn new(data : &'a str, arena : &'a Bump) -> Parser<'a> {
        let types = create_primitives(arena);
        let mut variables : HashMap<String, &Variable> = HashMap::new();
        variables.insert("exit_code".to_owned(), arena.alloc(Variable {
            var_type : *types.get("u32").unwrap()
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

    fn add_variable(&mut self, name : &str, t : &'a Type) -> Result<(), ParseError> {
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


    // TODO, remove loop after, replace remaining with vec containing index to expression
    fn parse_expression<'b>(&'b mut self) -> Result<Expression<'a>, ParseError> {
        self.skip_while(|c| Parser::SPACES.contains(c));
        let mut open_paren = Vec::new();
        let mut prev : *mut ExpressionBuilder = std::ptr::null_mut();
        loop {
            let next = self.peek_char().ok_or_else(||ParseError::new(ParseErrorType::EOF, self))?;
            let expr;
            match next {
                '!' => {
                    self.skip();
                    let new = Box::into_raw(Box::new(
                        ExpressionBuilder::single_operator(open_paren.len(), SingleOperator::Not, prev)
                    ));
                    if !prev.is_null() {
                        unsafe {(*prev).second_child = new;}
                    }
                    prev = new;
                    continue;
                },
                '(' => {
                    self.skip();
                    open_paren.push(0);
                    continue;
                },
                '0'..='9' => {
                    let int_str = self.read_until(|c| !('0'..='9').contains(&c));
                    let int = u64::from_str(int_str).map_err(|e|
                        ParseError::new(ParseErrorType::InvalidLiteral(int_str.to_owned()), self))?;
                    expr = Box::new(Expression::IntLiteral(int));
                },
                _ => {
                    let word = self.read_word()?;
                    if word == "true" {
                        expr = Box::new(Expression::BoolLiteral(true));
                    } else if word == "false" {
                        expr = Box::new(Expression::BoolLiteral(false));
                    } else if let Some(&var) = self.variables.get(word) {
                        expr = Box::new(Expression::Variable(var));
                    } else {
                        return Err(ParseError::new(ParseErrorType::InvalidLiteral(word.to_owned()), self));
                    }
                }
            };
            while let Some(')') = self.peek_char() {
                if let Some(_) = open_paren.pop() {
                    self.skip();
                } else {
                    return Err(ParseError::new(ParseErrorType::UnmatchedParen, self));
                }
            }
            let last = Box::into_raw(Box::new(ExpressionBuilder::atom(open_paren.len(), expr, prev)));
            if !prev.is_null() {
                unsafe {(*prev).second_child = last;}
            }
            prev = last;
            if let Some(operator) = self.read_dual_operator() {
                let mut val = Box::into_raw(
                    Box::new(ExpressionBuilder::dual_operator(open_paren.len(), operator, std::ptr::null_mut()))
                );
                let mut owner = unsafe {(*prev).parent};
                loop {
                    if owner.is_null() {
                        unsafe {
                            (*val).first_child = prev;
                            (*prev).parent = val;
                        }
                        break;
                    }
                    else {
                       unsafe {
                           if *val > *owner {
                               prev = owner;
                               owner = (*owner).parent;
                           } else {
                               (*val).first_child = (*owner).second_child;
                               (*(*owner).second_child).parent = val;
                               (*owner).second_child = val;
                               (*val).parent = owner;
                               break;
                           }
                       }
                    }
                }
                prev = val;
            } else {
                break;
            }
        }
        unsafe {
            while !(*prev).parent.is_null() {
                prev = (*prev).parent;
            }
        }
        let res = ExpressionBuilder::get_expression(prev);
        println!("{:?}", res);
        todo!()
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
                    parser.variables.insert(
                        name.to_owned(),
                        parser.arena.alloc(Variable { var_type : t})
                    );
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


fn parse_program<'a>(mut parser : Parser) -> Result<Program<'a>, ParseError> {
    parse_declarations(&mut parser)?;
    let statements = Vec::new();
    loop {
        let word = parser.read_word()?;
        if word == "end" {
            break;
        }
        let var = *parser.variables.get(word).ok_or_else(||
            ParseError::new(ParseErrorType::UnexpectedLiteral(word.to_owned()), &parser)
        )?;
        parser.assert_char('=')?;
        let expr = parser.parse_expression()?;
        parser.assert_char(';')?;
        println!("{:?}", expr);
        let statement = Statement::Assignment {
            var,
            expr
        };


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