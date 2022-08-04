package assembler

import (
	"bytes"
	"fmt"

	"github.com/prog-lang/hasky/runtime"
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
	for _, node := range ast {
		switch node.(type) {
		case *Instruction:
			instructionCount++
		case *Label:
			bc.Labels[node.(*Label).Text] = instructionCount
		}
	}
}

func (bc *Bytecode) generateCodeAndData(ast AST) (err error) {
	for _, node := range ast {
		switch node.(type) {
		case *Instruction:
			instruction, err := bc.encodeInstruction(node.(*Instruction))
			if err != nil {
				return err
			}
			bc.Code = append(bc.Code, instruction)
		case *Label:
			continue
		}
	}
	return
}

func (bc *Bytecode) encodeInstruction(i *Instruction) (
	instruction runtime.Instruction,
	err error,
) {
	operand, err := parseOperand(i.Operand)
	if err != nil {
		return
	}
	operandAddress, err := bc.operandAddress(operand)
	if err != nil {
		return
	}
	return runtime.Instruction{
		Opcode:  i.Opcode,
		Operand: operandAddress,
	}, nil
}

func (bc *Bytecode) operandAddress(operand Operand) (addr int32, err error) {
	if operand == nil {
		return 0, nil
	}
	switch operand.(type) {
	case *OperandInt:
		addr = int32(len(bc.Data))
		bc.Data = append(bc.Data, runtime.Int(operand.(*OperandInt).Int))
	case *OperandName:
		addr, err = bc.operandNameAddress(operand.(*OperandName).Text)
	}
	return
}

func (bc *Bytecode) operandNameAddress(name string) (addr int32, err error) {
	addr, exists := bc.Labels[name]
	if exists {
		return addr, nil
	}
	addr, exists = runtime.ClosureAddressFromName(name)
	if exists {
		return addr, nil
	}
	return 0, fmt.Errorf("unknown name in operand: %s", name)
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
