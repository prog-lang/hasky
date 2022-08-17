use std::rc::Rc;

use crate::internal::{Opcode, OPCODE_BYTE_LENGTH};
use crate::object::{Callable, Object};

pub struct Thunk {
    argc: usize,
    argi: usize,
    pub args: Vec<Object>,

    pub env: Rc<Env>,
    ip: usize,
    pub ret: bool,

    pub stack: Vec<Object>,
}

pub struct Env {
    code: Vec<u8>,
    // core: Vec<Lambda>,
}

impl Callable for Thunk {
    fn apply(&mut self, o: Object) {
        if self.argi >= self.argc {
            panic!("argument overflow")
        }
        self.feed(o);
    }

    fn call(&mut self) -> Object {
        while self.cycle() {}
        self.safe_pop()
    }
}

impl Thunk {
    pub fn new(env: Rc<Env>, argc: usize, ip: usize) -> Self {
        Self {
            argc: argc,
            argi: 0,
            args: vec![],
            env: env,
            ip: ip,
            ret: false,
            stack: vec![],
        }
    }

    pub fn main(env: Env) -> Self {
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
        self.read_code_bytes(OPCODE_BYTE_LENGTH)[0]
    }

    pub fn read_code_bytes(&mut self, n: usize) -> &[u8] {
        let start = self.ip;
        let stop = start + n;
        self.ip = stop;
        self.env.code.get(start..stop).expect("bad slice length")
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
    use super::{Env, Thunk};
    use crate::{
        internal::Opcode::*,
        object::{Callable, Object},
    };

    #[test]
    fn it_works() {
        let code = vec![NOP.bin(), RET.bin()];
        let env = Env { code };
        let mut thunk = Thunk::main(env);
        let result = thunk.call();
        assert_eq!(result, Object::Unit);
    }
}
