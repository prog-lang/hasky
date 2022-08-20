use std::io::{self, Write};

use crate::{lambda::Eval, object::Object};

const CORE: [Eval; 2] = [
    /* print */
    |args| {
        write!(io::stdout(), "{:?}", args[0]).expect("failed to write to stdout");
        Object::Unit
    },
    /* + */
    |args| {
        let x = args[0]
            .as_int()
            .expect("encountered type mismatch during argument conversion");
        let y = args[1]
            .as_int()
            .expect("encountered type mismatch during argument conversion");
        Object::Int(x + y)
    },
];

pub fn eval_from_addr(addr: usize) -> Eval {
    CORE[addr]
}
