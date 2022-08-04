package assembler

const (
	TypeLabel = iota
	TypeInstruction
)

type SemanticNode interface {
	SemanticNode()
}

type AST []SemanticNode

type Label struct {
	Text string
}

type Instruction struct {
	Opcode  int32
	Operand string
}

func (*Label) SemanticNode()       {}
func (*Instruction) SemanticNode() {}

func NewLabel(label string) *Label {
	return &Label{label}
}

func NewInstruction(opcode int32, operand string) *Instruction {
	return &Instruction{opcode, operand}
}
