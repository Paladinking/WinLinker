use std::fmt::{Display, Formatter};
use crate::language::types::Type;

#[derive(Debug, Copy, Clone)]
pub enum SingleOperator {
    Not = -1,
    Pass = isize::MAX,
}

impl Display for SingleOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", match self {
            SingleOperator::Not => "!",
            SingleOperator::Pass => ""
        }))
    }
}

impl SingleOperator {
    pub fn resolve_type(&self, val : &Type) -> Result<Type, (Type, Type)> {
        match self {
            SingleOperator::Not => Type::Bool.resolve(val),
            SingleOperator::Pass => Ok(Type::Any)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DualOperator {
    Divide = 1,
    Multiply = 0,
    Minus = 2,
    Plus = 3,
    // BITWISE : 4 - 8
    Equal = 9,
    NotEqual = 10,
    GreaterEqual = 11,
    LesserEqual = 12,
    Greater = 13,
    Lesser = 14,
    BoolAnd = 15,
    BoolOr = 16,
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

impl DualOperator {
    pub fn resolve_type(&self, first : &Type, second : &Type) -> Result<Type, (Type, Type)> {
        return match self {
            DualOperator::Divide | DualOperator::Multiply |
            DualOperator::Minus | DualOperator::Plus => {
                Type::AnyInt.resolve(first)?.resolve(second)
            }
            DualOperator::Equal | DualOperator::NotEqual => {
                first.resolve(second)?;
                Ok(Type::Bool)
            }
            DualOperator::GreaterEqual | DualOperator::LesserEqual |
            DualOperator::Greater | DualOperator::Lesser => {
                Type::AnyInt.resolve(first)?.resolve(second)?;
                Ok(Type::Bool)
            }
            DualOperator::BoolAnd | DualOperator::BoolOr => {
                Type::Bool.resolve(first)?.resolve(second)
            }
        };
    }

    pub fn finalize_type(&self, self_type : &Type, first: &Type, second: &Type) -> (Type, Type) {
        return match self {
            DualOperator::Divide | DualOperator::Multiply |
            DualOperator::Minus | DualOperator::Plus => {
                (self_type.finalize(), self_type.finalize())
            },
            _=> {
                (first.finalize(), second.finalize())
            }
        }
    }

    pub fn is_cmp(&self) -> bool {
        match self {
            DualOperator::Equal | DualOperator::NotEqual |
            DualOperator::GreaterEqual | DualOperator::LesserEqual |
            DualOperator::Greater | DualOperator::Lesser => true,
            _ => false
        }
    }
}

