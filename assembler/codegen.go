package assembler

import (
	"github.com/sharpvik/hasky/runtime"
)

type Bytecode struct {
	Labels map[string]int
	Data   []runtime.Constant
	Code   []runtime.Instruction
}

func NewBytecode() *Bytecode {
	return &Bytecode{
		Labels: make(map[string]int),
		Data:   make([]runtime.Constant, 0),
		Code:   make([]runtime.Instruction, 0),
	}
}

func (ast AST) Bytecode() (bc *Bytecode, err error) {
	bc = NewBytecode()
	bc.identifyLabels(ast)
	err = bc.generateCodeAndData(ast)
	return
}

func (bc *Bytecode) identifyLabels(ast AST) {
	instructionCount := 0
	for _, line := range ast {
		if line.Tag == TypeInstruction {
			instructionCount++
		} else {
			label := line.Value.(string)
			bc.Labels[label] = instructionCount
		}
	}
}

func (bc *Bytecode) generateCodeAndData(ast AST) (err error) {
	for _, line := range ast {
		if line.Tag == TypeLabel {
			continue
		}
		instruction, err := bc.encode(line.Value.(Instruction))
		if err != nil {
			return err
		}
		bc.Code = append(bc.Code, instruction)
	}
	return
}

func (bc *Bytecode) encode(i Instruction) (runtime.Instruction, error) {
	operand, err := parseOperand(i.Operand)
	if err != nil {
		return runtime.Instruction{}, err
	}
	return runtime.Instruction{
		Opcode:  i.Opcode,
		Operand: bc.operandAddress(operand),
	}, nil
}

func (bc *Bytecode) operandAddress(operand *TaggedUnion) (addr int) {
	if operand == nil {
		return 0
	}
	if operand.Tag == TypeOperandInt {
		addr = len(bc.Data)
		bc.Data = append(bc.Data, runtime.Int(operand.Value.(int)))
		return
	}
	if operand.Tag == TypeOperandName {
		return bc.Labels[operand.Value.(string)]
	}
	return
}

func (bc *Bytecode) Bytes() (code []byte) {
	return
}
