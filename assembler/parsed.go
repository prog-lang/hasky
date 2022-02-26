package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

const (
	TypeLabel = iota
	TypeInstruction
)

type AST []*ParsedLine

type ParsedLine struct {
	Tag   int
	Value interface{}
}

func NewParsedLine(tag int, value interface{}) *ParsedLine {
	return &ParsedLine{
		Tag:   tag,
		Value: value,
	}
}

func NewLabel(label string) *ParsedLine {
	return NewParsedLine(TypeLabel, label)
}

func NewInstruction(opcode int, operand int) *ParsedLine {
	return NewParsedLine(TypeInstruction, runtime.Instruction{opcode, operand})
}
