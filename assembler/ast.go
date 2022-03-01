package assembler

const (
	TypeLabel = iota
	TypeInstruction
)

type AST []*TaggedUnion

type Instruction struct {
	Opcode  int
	Operand string
}

type TaggedUnion struct {
	Tag   int
	Value interface{}
}

func NewTaggedUnion(tag int, value interface{}) *TaggedUnion {
	return &TaggedUnion{
		Tag:   tag,
		Value: value,
	}
}

func NewLabel(label string) *TaggedUnion {
	return NewTaggedUnion(TypeLabel, label)
}

func NewInstruction(opcode int, operand string) *TaggedUnion {
	return NewTaggedUnion(TypeInstruction, Instruction{opcode, operand})
}
