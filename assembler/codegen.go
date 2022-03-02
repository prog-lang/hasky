package assembler

import (
	"bytes"

	"github.com/sharpvik/hasky/runtime"
)

type Bytecode struct {
	Data   runtime.Data
	Code   runtime.Code
	Labels map[string]int32
}

func NewBytecode() *Bytecode {
	return &Bytecode{
		Data:   make(runtime.Data, 0),
		Code:   make([]runtime.Instruction, 0),
		Labels: make(map[string]int32),
	}
}

func (ast AST) Bytecode() (bc *Bytecode, err error) {
	bc = NewBytecode()
	bc.identifyLabels(ast)
	err = bc.generateCodeAndData(ast)
	return
}

func (bc *Bytecode) identifyLabels(ast AST) {
	var instructionCount int32 = 0
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

func (bc *Bytecode) operandAddress(operand *TaggedUnion) (addr int32) {
	if operand == nil {
		return 0
	}
	if operand.Tag == TypeOperandInt {
		addr = int32(len(bc.Data))
		bc.Data = append(bc.Data, runtime.Int(operand.Value.(int)))
		return
	}
	if operand.Tag == TypeOperandName {
		return bc.Labels[operand.Value.(string)]
	}
	return
}

func (bc *Bytecode) Encode() (code []byte, err error) {
	var buf bytes.Buffer
	buf.WriteString("#!/usr/bin/env hasky\n")
	if err = bc.Data.EncodeAndWrite(&buf); err != nil {
		return
	}
	if err = bc.Code.EncodeAndWrite(&buf); err != nil {
		return
	}
	return buf.Bytes(), nil
}