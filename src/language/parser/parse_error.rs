use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::language::types::Type;

#[derive(Debug)]
pub enum ParseErrorType {
    UnexpectedCharacter(char),
    UnexpectedLiteral(String),
    UnknownType(String),
    BadVarName(String),
    InvalidLiteral(String),
    UnmatchedParen,
    TypeError(Type, Type),
    EOF
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
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
            ParseErrorType::TypeError(t1, t2) =>
                f.write_fmt(format_args!("Type mismatch : {} and {}", t1, t2)),
            ParseErrorType::InvalidLiteral(s) =>
                f.write_fmt(format_args!("Invalid literal : '{}'", s))
        }
    }
}

pub struct ParseError {
    error_type : ParseErrorType,
    pos : (usize, usize)
}

impl ParseError {
    pub fn new(error : ParseErrorType, (row, col) : (usize, usize)) -> ParseError {
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
            ParseErrorType::TypeError(_, _) |
            ParseErrorType::EOF => ParseError::new_at(
                error, row, col
            )
        }
    }

    pub fn new_at(error : ParseErrorType, row : usize, col : usize) -> ParseError{
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