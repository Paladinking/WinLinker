use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    S32,
    U32,
    Bool,
    Plain { id: usize },
    AnyInt,
    Any
}

impl Type {
    pub fn resolve(&self, other : &Type) -> Result<Type, (Type, Type)> {
        match (self, other) {
            (Type::Any, t) | (t, Type::Any) => Ok(*t),
            (Type::AnyInt, Type::S32) | (Type::S32, Type::AnyInt) => Ok(Type::S32),
            (Type::AnyInt, Type::U32) | (Type::U32, Type::AnyInt) => Ok(Type::U32),
            _ if self == other => Ok(*self),
            (t1, t2) => Err((*t1, *t2))
        }
    }

    pub(crate) fn finalize(&self) -> Type {
        match self {
            Type::Any => panic!("Unresolvable type"),
            Type::AnyInt => Type::U32,
            t => *t
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Type::S32 => true,
            Type::U32 => false,
            _ => panic!("Signed check on non integer type")
        }
    }

    pub fn create_primitives() -> HashMap<String, Type> {
        let mut types : HashMap<String, Type> = HashMap::new();
        types.insert(String::from("bool"), Type::Bool);
        types.insert(String::from("s32"), Type::S32);
        types.insert(String::from("u32"), Type::U32);
        types
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

