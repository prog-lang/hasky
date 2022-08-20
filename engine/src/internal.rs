use byteorder::{BigEndian, ReadBytesExt};

use crate::{
    core,
    lambda::Lambda,
    object::{Call, Callable, Object},
    thunk::Thunk,
};

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
    /// Opcode: 4B
    /// *------------------------------*
    /// | native function address: u32 |
    /// *------------------------------*
    /// ```
    NFN,

    /// Initialise a thunk and push it onto the data stack.
    ///
    /// ```text
    /// Opcode: 4B
    /// *-------------------------------*
    /// | task instruction address: u32 |
    /// *-------------------------------*
    /// ```
    TASK,

    /// Use object ontop of the data stack as an argument to the function that
    /// lies beneath it.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    APP,

    /// Call function on top of the data stack and replace it with its return
    /// value.
    ///
    /// ```text
    /// Opcode: 4B
    /// *----------------------------*
    /// | data constant address: u32 |
    /// *----------------------------*
    /// ```
    CALL,

    /// Load machine constant onto the data stack.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    LDC,

    /// Store object as a local persistent value (let).
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    STOL,

    /// Load local persisten value.
    ///
    /// ```text
    /// Opcode: 4B
    /// *--------------------------*
    /// | local value address: u32 |
    /// *--------------------------*
    /// ```
    LDL,

    /// Duplicate value on top of the stack.
    ///
    /// ```text
    /// Opcode: 0B
    /// ```
    DUP,

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
            .push(Object::Function(Callable::Lambda(Lambda::new(eval))))
    },
    /* TASK */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read task instruction address");
        let task = thunk.subtask(addr as usize);
        thunk.stack.push(Object::Function(Callable::Thunk(task)));
    },
    /* APP */
    |thunk| {
        let arg = thunk
            .stack
            .pop()
            .expect("failed to pop object from empty stack");
        let mut func = thunk
            .stack
            .pop()
            .expect("failed to call task from empty stack")
            .into_function()
            .expect("failed to apply argument to a non-functional object");
        func.apply(arg);
        thunk.stack.push(Object::Function(func));
    },
    /* CALL */
    |thunk| {
        let mut func = thunk
            .stack
            .pop()
            .expect("failed to call task from empty stack")
            .into_function()
            .expect("failed to call non-functional object");
        let return_value = func.call();
        thunk.stack.push(return_value);
    },
    /* LDC */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read data constant address");
        let constant = thunk.constant(addr as usize);
        thunk.stack.push(constant);
    },
    /* STOL */
    |thunk| {
        let object = thunk
            .stack
            .pop()
            .expect("failed to call task from empty stack");
        thunk.lets.push(object);
    },
    /* LDL */
    |thunk| {
        let addr = thunk
            .read_u32::<BigEndian>()
            .expect("failed to read local value address");
        let object = thunk.lets[addr as usize].clone();
        thunk.stack.push(object);
    },
    /* DUP */
    |thunk| {
        let object = thunk
            .stack
            .pop()
            .expect("failed to call task from empty stack");
        let clone = object.clone();
        thunk.stack.push(object);
        thunk.stack.push(clone);
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
