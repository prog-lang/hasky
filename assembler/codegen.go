package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

type Bytecode struct {
	Data []runtime.Object
	Code []runtime.Instruction
}

func BytecodeFromAST(parsed AST) (bc Bytecode) {
	return
}

func (ast AST) bytes() (code []byte) {
	return
}
