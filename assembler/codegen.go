package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

type Bytecode struct {
	// Labels map[string]int
	Code []runtime.Instruction
}

func generateBytecode(parsed AST) (code []byte) {
	return
}

func process(parsed AST) (bc Bytecode) {
	return
}
