package assembler

const (
	TypeLabel = iota
	TypeInstruction
)

type AST []*TaggedUnion

type Instruction struct {
	Opcode  int32
	Operand string
}

type TaggedUnion struct {
	Tag   int32
	Value interface{}
}

func NewTaggedUnion(tag int32, value interface{}) *TaggedUnion {
	return &TaggedUnion{
		Tag:   tag,
		Value: value,
	}
}

func NewLabel(label string) *TaggedUnion {
	return NewTaggedUnion(TypeLabel, label)
}

func NewInstruction(opcode int32, operand string) *TaggedUnion {
	return NewTaggedUnion(TypeInstruction, Instruction{opcode, operand})
}
