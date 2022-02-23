package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

type Bytecode struct {
	Data []runtime.Object
	Code []Instruction
}

type Instruction struct {
	Opcode  uint8
	Operand int
}
