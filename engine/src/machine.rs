use std::fs::File;

use byteorder::ReadBytesExt;

use crate::object::Object;

pub struct Machine {
    pub code: Vec<u8>,
    pub constants: Vec<Object>,
}

impl Machine {
    pub fn new(code: Vec<u8>, constants: Vec<Object>) -> Self {
        Self { code, constants }
    }

    pub fn empty() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn from_file(name: String) -> Self {
        let mut bc = Self::open_bytecode_file(name).unwrap();
        Self::from_reader(&mut bc)
    }

    pub fn from_reader<R>(bc: &mut R) -> Self
    where
        R: ReadBytesExt,
    {
        if !Self::watermark_ok(bc) {
            panic!("invalid watermark");
        }
        let mut machine = Self::empty();
        machine.read_constants(bc);
        machine.read_code(bc);
        machine
    }

    fn read_code<R>(&mut self, bc: &mut R)
    where
        R: ReadBytesExt,
    {
        bc.read_to_end(&mut self.code).expect("failed to read code");
    }

    fn read_constants<R>(&mut self, bc: &mut R)
    where
        R: ReadBytesExt,
    {
        loop {
            match Object::read_constant(bc) {
                Object::Unit => return,
                constant => self.constants.push(constant),
            }
        }
    }

    fn watermark_ok<R>(bc: &mut R) -> bool
    where
        R: ReadBytesExt,
    {
        match bc.read_u8() {
            Ok(watermark) => watermark == 42,
            Err(_) => false,
        }
    }

    fn open_bytecode_file(name: String) -> Result<File, String> {
        match File::open(name) {
            Err(_) => Err("failed to open bytecode source file".to_string()),
            Ok(file) => Ok(file),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{internal::Opcode::*, object::Object};

    use super::Machine;

    #[test]
    fn can_read_machine() {
        let bc: Vec<u8> = vec![42, 1, 0, 0, 0, 5, 0, RET.bin()];
        let machine = Machine::from_reader(&mut bc.as_slice());
        assert_eq!(machine.constants[0], Object::Int(5));
        assert_eq!(machine.code[0], RET.bin());
    }
}
