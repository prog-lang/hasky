use byteorder::{BigEndian, ReadBytesExt};

use crate::{core, lambda::Lambda, object::Object, thunk::Thunk};

pub type Instruction = fn(&mut Thunk);

#[derive(Debug)]
pub enum Opcode {
    /// Do nothing.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    NOP,

    /// Push native function onto the stack.
    ///
    /// ```text
    /// Opcode: 16B
    /// *------------------------------*------------------------------*
    /// | native function address: u32 | function argument count: u32 |
    /// *------------------------------*------------------------------*
    /// ```
    NFN,

    /// Return from function.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    RET,
    // RET must remain the last Opcode.
}

pub const OPCODE_BYTE_LENGTH: usize = 1;

/// Instrustion set.
pub const IS: [Instruction; Opcode::RET as usize + 1] = [
    /* NOP */ |_| {},
    /* NFN */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read native function address");
        let argc = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read function argument count");
        let eval = core::eval_from_addr(addr as usize);
        thunk
            .stack
            .push(Object::Function(Box::new(Lambda::new(eval, argc as usize))))
    },
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
