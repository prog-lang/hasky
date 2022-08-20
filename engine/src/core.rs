use crate::{lambda::Eval, object::Object};

pub enum Operation {
    Add,
}

const CORE: [Eval; 1] = [/* + */ |args: &Vec<Object>| {
    let x = args[0]
        .as_int()
        .expect("encountered type mismatch during argument conversion");
    let y = args[1]
        .as_int()
        .expect("encountered type mismatch during argument conversion");
    Object::Int(x + y)
}];

pub fn eval_from_addr(addr: usize) -> Eval {
    CORE[addr]
}
