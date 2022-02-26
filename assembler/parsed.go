package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

const (
	TypeLabel = iota
	TypeInstruction
)

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

func NewInstruction(i runtime.Instruction) *ParsedLine {
	return NewParsedLine(TypeInstruction, i)
}
