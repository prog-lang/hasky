use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub export: HashSet<String>,
    pub constants: HashMap<String, i32>, // TODO: constants must become an enum.
    pub code: Vec<Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    pub tag: Option<String>,
    pub opcode: String,
    pub operands: Vec<String>,
}
