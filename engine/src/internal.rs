use crate::thunk::Thunk;

pub type Instruction = fn(i32) -> Operation;
pub type Operation = fn(&mut Thunk);

pub enum Opcode {
    NOP,
    // NOP must remain the last Opcode.
}

/// Instrustion set.
pub const IS: [Instruction; Opcode::NOP as usize + 1] = [/* NOP */ |_| |_| {}];

pub fn op(o: Opcode) -> i32 {
    o as i32
}

pub fn instruction(opcode: i32) -> Instruction {
    IS[opcode as usize]
}
