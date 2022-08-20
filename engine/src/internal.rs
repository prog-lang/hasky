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

    /// Push native function onto the data stack.
    ///
    /// ```text
    /// Opcode: 8B
    /// *------------------------------*
    /// | native function address: u32 |
    /// *------------------------------*
    /// ```
    NFN,

    /// Initialise a thunk and push it onto the data stack.
    ///
    /// ```text
    /// Opcode: 8B
    /// *-------------------------------*
    /// | task instruction address: u32 |
    /// *-------------------------------*
    /// ```
    TASK,

    /// Call function on top of the data stack and replace it with its return
    /// value.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    CALL,

    /// Return from a task.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    RET,
    // RET must remain the last Opcode.
}

/// Instrustion set.
pub const IS: [Instruction; Opcode::RET as usize + 1] = [
    /* NOP */ |_| {},
    /* NFN */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read native function address");
        let eval = core::eval_from_addr(addr as usize);
        thunk
            .stack
            .push(Object::Function(Box::new(Lambda::new(eval))))
    },
    /* TASK */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read task instruction address");
        let task = thunk.task(addr as usize);
        thunk.stack.push(Object::Function(Box::new(task)));
    },
    /* CALL */
    |thunk| {
        let top = thunk
            .stack
            .pop()
            .expect("failed to call task from empty stack");
        let mut func = top
            .into_function()
            .expect("failed to call non-functional object");
        let return_value = func.call();
        thunk.stack.push(return_value);
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
