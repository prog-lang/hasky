use crate::thunk::Thunk;

pub type Instruction = fn(i32) -> Operation;
pub type Operation = fn(&mut Thunk);

pub enum Opcode {
    /// do nothing
    NOP,
    /// return
    RET,
    // RET must remain the last Opcode.
}

/// Instrustion set.
pub const IS: [Instruction; Opcode::RET as usize + 1] = [
    /* NOP */ |_| |_| {},
    /* RET */ |_| |thunk| thunk.ret = true,
];

pub fn op(o: Opcode) -> i32 {
    o as i32
}

pub fn instruction(opcode: i32) -> Instruction {
    IS[opcode as usize]
}
