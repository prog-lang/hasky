use std::io;
use std::rc::Rc;

use byteorder::ReadBytesExt;

use crate::internal::Opcode;
use crate::object::{Call, Object};

#[derive(Clone)]
pub struct Thunk {
    args: Vec<Object>,
    ip: usize,
    machine: Rc<Machine>,
    pub ret: bool,
    pub stack: Vec<Object>,
}

pub struct Machine {
    code: Vec<u8>,
    constants: Vec<Object>,
}

impl Machine {
    pub fn new(code: Vec<u8>, constants: Vec<Object>) -> Self {
        Self { code, constants }
    }
}

impl Call for Thunk {
    fn apply(&mut self, o: Object) {
        self.args.push(o);
    }

    fn call(&mut self) -> Object {
        while self.cycle() {}
        self.safe_pop()
    }
}

impl io::Read for Thunk {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.machine
            .code
            .get(self.ip..)
            .expect("failed to read ROM")
            .read(buf)
            .and_then(|n| {
                self.ip += n;
                Ok(n)
            })
    }
}

impl Thunk {
    pub fn new(machine: Rc<Machine>, ip: usize) -> Self {
        Self {
            args: Vec::new(),
            ip,
            machine,
            ret: false,
            stack: Vec::new(),
        }
    }

    pub fn main(machine: Machine) -> Self {
        Self::new(Rc::new(machine), 0)
    }

    pub fn subtask(&self, ip: usize) -> Self {
        Thunk::new(self.machine.clone(), ip)
    }

    pub fn constant(&self, index: usize) -> Object {
        self.machine
            .constants
            .get(index)
            .expect(&format!("failed to fetch data constant at index {}", index))
            .clone()
    }

    fn cycle(&mut self) -> bool {
        let opcode = self.fetch();
        let instruction = Opcode::decode(opcode);
        instruction(self);
        !self.ret
    }

    fn fetch(&mut self) -> u8 {
        self.read_u8().expect("failed to fetch opcode")
    }

    fn safe_pop(&mut self) -> Object {
        match self.stack.pop() {
            Some(o) => o,
            _ => Object::Unit,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Machine, Thunk};
    use crate::{
        internal::Opcode::*,
        object::{Call, Object},
    };

    #[test]
    fn it_works() {
        let code = vec![
            NOP.bin(), // start
            TASK.bin(),
            0, // <--*
            0, //    *-- main task address: i32 = 8
            0, //    |
            8, // <--*
            CALL.bin(),
            RET.bin(),
            NFN.bin(), // main
            0,         // <--*
            0,         //    *-- native function address: i32 = 0
            0,         //    |
            0,         // <--*
            LDC.bin(),
            0, // <--*
            0, //    *-- data constant address: i32 = 0
            0, //    |
            0, // <--*
            APP.bin(),
            LDC.bin(),
            0, // <--*
            0, //    *-- data constant address: i32 = 1
            0, //    |
            1, // <--*
            APP.bin(),
            CALL.bin(),
            RET.bin(),
        ];
        let constants = vec![Object::Int(2), Object::Int(40)];
        let machine = Machine::new(code, constants);
        let mut thunk = Thunk::main(machine);
        let result = thunk.call();
        assert_eq!(result.as_int().unwrap().to_owned(), 42);
    }
}
