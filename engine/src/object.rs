use std::fmt;
use std::fmt::Debug;

use enum_as_inner::EnumAsInner;

use crate::lambda::Lambda;
use crate::thunk::Thunk;

/// Objects are primary units of intormation upon which we operate. They can lie
/// on the data stack or be part of machine data constants. Functions receive
/// arguments as objects and *always* return an object.
#[derive(EnumAsInner, Clone)]
pub enum Object {
    Unit,
    Int(i32),
    Function(Callable),
}

#[derive(Clone)]
pub enum Callable {
    Lambda(Lambda),
    Thunk(Thunk),
}

pub trait Call {
    fn apply(&mut self, o: Object);
    fn call(&mut self) -> Object;
}

impl Call for Callable {
    fn apply(&mut self, o: Object) {
        match self {
            Self::Lambda(lambda) => lambda.apply(o),
            Self::Thunk(thunk) => thunk.apply(o),
        }
    }

    fn call(&mut self) -> Object {
        match self {
            Self::Lambda(lambda) => lambda.call(),
            Self::Thunk(thunk) => thunk.call(),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Int(i) => write!(f, "{}", i),
            Self::Function(Callable::Lambda(_)) => write!(f, "<Native Function>"),
            Self::Function(Callable::Thunk(_)) => write!(f, "<Function>"),
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
