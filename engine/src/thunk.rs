use std::rc::Rc;

use crate::internal::{self, Operation};
use crate::object::{Callable, Object};

pub struct Thunk {
    argc: i32,
    argi: i32,
    pub args: Vec<Object>,

    pub env: Rc<Env>,
    ip: i32,
    pub ret: bool,

    pub stack: Vec<Object>,
}

pub struct Env {
    code: Vec<i32>,
    // core: Vec<Lambda>,
}

impl Callable for Thunk {
    fn apply(&mut self, o: Object) {
        if self.argi >= self.argc {
            panic!("apply: argument overflow error")
        }
        self.feed(o);
    }

    fn call(&mut self) -> Object {
        while self.cycle() {}
        self.safe_pop()
    }
}

impl Thunk {
    pub fn new(env: Rc<Env>, argc: i32, ip: i32) -> Self {
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
        let opcode = self.fetch();
        let execute = self.decode(opcode);
        execute(self);
        !self.ret
    }

    fn fetch(&mut self) -> i32 {
        let opcode = self.env.code[self.ip as usize];
        self.ip += 1;
        opcode
    }

    fn decode(&mut self, opcode: i32) -> Operation {
        let operand = self.env.code[self.ip as usize];
        self.ip += 1;
        internal::instruction(opcode)(operand)
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
        internal::{op, Opcode::*},
        object::{Callable, Object},
    };

    #[test]
    fn it_works() {
        let code = vec![op(NOP), 0, op(RET), 0];
        let env = Env { code };
        let mut thunk = Thunk::main(env);
        let result = thunk.call();
        assert_eq!(result, Object::Unit);
    }
}
