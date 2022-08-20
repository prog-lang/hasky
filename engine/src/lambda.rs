use crate::object::{Callable, Object};

pub struct Lambda {
    args: Vec<Object>,
    eval: Eval,
}

pub type Eval = fn(&Vec<Object>) -> Object;

impl Callable for Lambda {
    fn apply(&mut self, o: Object) {
        self.args.push(o);
    }

    fn call(&mut self) -> Object {
        (self.eval)(&self.args)
    }
}

impl Lambda {
    pub fn new(eval: Eval) -> Self {
        Self {
            args: Vec::new(),
            eval: eval,
        }
    }
}
