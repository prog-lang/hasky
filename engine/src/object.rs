use std::fmt;
use std::fmt::Debug;

use enum_as_inner::EnumAsInner;

#[derive(EnumAsInner)]
pub enum Object {
    Unit,
    Int(i32),
    Fn(Box<dyn Callable>),
}

pub trait Callable {
    fn apply(&mut self, o: Object);
    fn call(&mut self) -> Object;
}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Fn(_) => write!(f, "Fn"),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Int(x), Self::Int(y)) => x == y,
            _ => false,
        }
    }
}
