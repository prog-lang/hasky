extern crate pest;

use std::collections::{HashMap, HashSet};

use pest::Parser as P;

use crate::ast::Module;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Parser;

impl Parser {
    pub fn parse_module(input: &str) -> Result<Module, String> {
        let mut statements = Parser::parse(Rule::file, input)
            .map_err(|e| format!("{:?}", e))?
            .into_iter()
            .next()
            .unwrap()
            .into_inner();
        let name = statements
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap()
            .as_str()
            .to_string();
        let mut export = HashSet::new();
        let mut constants = HashMap::new();
        let mut code = Vec::new();

        let statements = statements.into_iter();
        for each in statements {
            println!("{}", each);
        }

        Ok(Module {
            name,
            export,
            constants,
            code,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::{Instruction, Module};
    use std::collections::{HashMap, HashSet};

    #[test]
    fn it_works() {
        let input = r"
        module magic;
        export main;

        const MIN: 0;
        const MAX: -42;

        main:
            push MAX;
            ret;
        ";
        let module = Parser::parse_module(input).expect("failed to parse module");
        assert_eq!(
            module,
            Module {
                name: "magic".to_string(),
                export: HashSet::from([("main".to_string())]),
                constants: HashMap::from([("MIN".to_string(), 0), ("MAX".to_string(), -42),]),
                code: vec![
                    Instruction {
                        tag: Some("main".to_string()),
                        opcode: "push".to_string(),
                        operands: vec!["MAX".to_string()]
                    },
                    Instruction {
                        tag: None,
                        opcode: "ret".to_string(),
                        operands: Vec::new(),
                    },
                ],
            }
        );
    }
}
