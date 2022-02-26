package assembler

const (
	TypeLabel = iota
	TypeInstruction
)

type AST []*ParsedLine

type Instruction struct {
	Opcode  int
	Operand string
}

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

func NewInstruction(opcode int, operand string) *ParsedLine {
	return NewParsedLine(TypeInstruction, Instruction{opcode, operand})
}
