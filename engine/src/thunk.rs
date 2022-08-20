use std::io;
use std::rc::Rc;

use byteorder::ReadBytesExt;

use crate::internal::Opcode;
use crate::object::{Callable, Object};

pub struct Thunk {
    args: Vec<Object>,
    ip: usize,
    machine: Rc<Machine>,
    pub ret: bool,
    pub stack: Vec<Object>,
}

pub struct Machine {
    code: Vec<u8>,
}

impl Machine {
    pub fn new(code: Vec<u8>) -> Self {
        Self { code }
    }
}

impl Callable for Thunk {
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
    pub fn new(env: Rc<Machine>, ip: usize) -> Self {
        Self {
            args: Vec::new(),
            ip: ip,
            machine: env,
            ret: false,
            stack: Vec::new(),
        }
    }

    pub fn main(env: Machine) -> Self {
        Self::new(Rc::new(env), 0)
    }

    pub fn task(&self, ip: usize) -> Self {
        Thunk::new(self.machine.clone(), ip)
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
        object::{Callable, Object},
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
            RET.bin(),
        ];
        let env = Machine::new(code);
        let mut thunk = Thunk::main(env);
        let result = thunk.call();
        assert!(match result {
            Object::Function(_) => Ok(()),
            _ => Err(()),
        }
        .is_ok())
    }
}
