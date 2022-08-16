use crate::object::{Callable, Object};

pub struct Lambda {
    argc: i32,
    argi: i32,
    args: Vec<Object>,
    eval: Eval,
}

type Eval = fn(&Vec<Object>) -> Object;

impl Callable for Lambda {
    fn apply(&mut self, o: Object) {
        if self.argi >= self.argc {
            panic!("apply: argument overflow error")
        }
        self.feed(o);
    }

    fn call(&mut self) -> Object {
        (self.eval)(&self.args)
    }
}

impl Lambda {
    pub fn new(eval: Eval, argc: i32) -> Self {
        Self {
            argc: argc,
            argi: 0,
            args: vec![],
            eval: eval,
        }
    }

    fn feed(&mut self, o: Object) {
        self.argi += 1;
        self.args.push(o);
    }
}
