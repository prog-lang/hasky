package runtime

import (
	"bytes"
	"io"

	"github.com/sharpvik/hasky/runtime/convert"
)

type Instruction struct {
	Opcode  int32
	Operand int32
}

type Code []Instruction

func (instruction Instruction) EncodeAndWrite(buf *bytes.Buffer) (err error) {
	_, err = buf.Write(convert.Int32AsBytes(instruction.Opcode))
	if err != nil {
		return
	}
	_, err = buf.Write(convert.Int32AsBytes(instruction.Operand))
	if err != nil {
		return
	}
	return
}

func (code Code) EncodeAndWrite(buf *bytes.Buffer) (err error) {
	for _, instruction := range code {
		if err = instruction.EncodeAndWrite(buf); err != nil {
			return
		}
	}
	return
}

func ReadInstruction(r io.Reader) (i Instruction, err error) {
	opcode, err := convert.ReadInt32(r)
	if err == io.EOF {
		return Instruction{}, io.EOF
	}
	if err != nil {
		return Instruction{}, io.ErrUnexpectedEOF
	}
	operand, err := convert.ReadInt32(r)
	if err != nil {
		return Instruction{}, io.ErrUnexpectedEOF
	}
	return Instruction{opcode, operand}, nil
}

func ReadCode(r io.Reader) (code Code, err error) {
	for {
		instruction, err := ReadInstruction(r)
		if err == io.EOF {
			return code, nil
		}
		if err != nil {
			return nil, err
		}
		code = append(code, instruction)
	}
}
