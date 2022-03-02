package runtime

import (
	"bytes"
	"encoding/binary"

	endian "github.com/yalue/native_endian"
)

type Instruction struct {
	Opcode  int
	Operand int
}

type Code []Instruction

func (instruction Instruction) EncodeAndWrite(buf *bytes.Buffer) (err error) {
	err = binary.Write(buf, endian.NativeEndian(), int32(instruction.Opcode))
	if err != nil {
		return
	}
	err = binary.Write(buf, endian.NativeEndian(), int32(instruction.Operand))
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
