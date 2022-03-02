package runtime

import (
	"bytes"

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
