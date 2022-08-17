use std::io;
use std::rc::Rc;

use byteorder::ReadBytesExt;

use crate::internal::Opcode;
use crate::object::{Callable, Object};

pub struct Thunk {
    argc: usize,
    argi: usize,
    pub args: Vec<Object>,

    pub machine: Rc<Machine>,
    ip: usize,
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
        if self.argi >= self.argc {
            panic!("encountered argument overflow during application")
        }
        self.feed(o);
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
    pub fn new(env: Rc<Machine>, argc: usize, ip: usize) -> Self {
        Self {
            argc: argc,
            argi: 0,
            args: vec![],
            machine: env,
            ip: ip,
            ret: false,
            stack: vec![],
        }
    }

    pub fn main(env: Machine) -> Self {
        Self::new(Rc::new(env), 0, 0)
    }

    fn feed(&mut self, o: Object) {
        self.argi += 1;
        self.args.push(o);
    }

    fn cycle(&mut self) -> bool {
        Opcode::decode(self.fetch())(self);
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
            NOP.bin(),
            NFN.bin(),
            0, // <--*
            0, //    *-- native function address: i32 = 0
            0, //    |
            0, // <--*
            0, // <--*
            0, //    *-- function argument count: i32 = 2
            0, //    |
            2, // <--*
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
