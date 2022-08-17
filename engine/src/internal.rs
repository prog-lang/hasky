use crate::thunk::Thunk;

pub type Instruction = fn(&mut Thunk);

pub enum Opcode {
    /// Do nothing.
    ///
    /// `opcode: 0B`
    NOP,

    /// Return from function.
    ///
    /// `opcode: 0B`
    RET,
    // RET must remain the last Opcode.
}

pub const OPCODE_BYTE_LENGTH: usize = 1;

/// Instrustion set.
pub const IS: [Instruction; Opcode::RET as usize + 1] = [
    /* NOP */ |_| {},
    /* RET */ |thunk| thunk.ret = true,
];

impl Opcode {
    pub fn bin(self) -> u8 {
        self as u8
    }

    pub fn decode(opcode: u8) -> Instruction {
        IS[opcode as usize]
    }
}
