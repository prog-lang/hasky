use std::io;
use std::rc::Rc;

use byteorder::ReadBytesExt;

use crate::internal::Opcode;
use crate::machine::Machine;
use crate::object::{Call, Object};

#[derive(Clone)]
pub struct Thunk {
    args: Vec<Object>,
    ip: usize,
    machine: Rc<Machine>,
    pub ret: bool,
    pub stack: Vec<Object>,
    pub lets: Vec<Object>,
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
            .expect("failed to read code")
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
            lets: Vec::new(),
        }
    }

    pub fn main(machine: Machine) -> Self {
        Self::new(Rc::new(machine), 0)
    }

    pub fn subtask(&self, ip: usize) -> Self {
        Thunk::new(self.machine.clone(), ip)
    }

    pub fn constant(&self, index: usize) -> Object {
        self.machine.constants[index].clone()
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
    fn can_execute_instructions() {
        // main
        //   :: Task Int
        //   := let i := magic
        //   in core.io.print i
        //   >| core.task i
        //
        // magic :: Int := 2 + 40
        let code = vec![
            NOP.bin(),  // main
            TASK.bin(), // stack [@magic]
            0,
            0,
            0,
            18,
            CALL.bin(), // stack [42]
            DUP.bin(),  // stack [42, 42]
            NFN.bin(),  // stack [42, 42, core.io.print]
            0,
            0,
            0,
            0,
            FAP.bin(),  // stack [42, (core.io.print 42)]
            CALL.bin(), // stack [42, ()]
            DROP.bin(), // stack [42]
            RET.bin(),  // main => 42
            NOP.bin(),  // magic
            NFN.bin(),  // stack [+]
            0,
            0,
            0,
            1,
            LDC.bin(), // stack [+, 2]
            0,
            0,
            0,
            0,
            APP.bin(), // stack [(+ 2)]
            LDC.bin(), // stack [(+ 2), 40]
            0,
            0,
            0,
            1,
            APP.bin(),  // stack [(+ 2 40)]
            CALL.bin(), // stack [42]
            RET.bin(),  // magic => 42
        ];
        let constants = vec![Object::Int(2), Object::Int(40)];
        let machine = Machine::new(code, constants);
        let mut thunk = Thunk::main(machine);
        let result = thunk.call();
        assert_eq!(result, Object::Int(42));
    }
}
