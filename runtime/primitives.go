package runtime

import (
	"github.com/sharpvik/hasky/runtime/convert"
)

const (
	ConstTypeInt byte = iota

	ConstListEnd //! signifies the end of the data section
)

var Nil Unit

type Unit struct{}

func (Unit) Type() Type {
	return TypeUnit
}

/* Constants */

type Int int32

func (Int) Type() Type {
	return TypeInt
}

func (i Int) Encode() (out []byte, err error) {
	encoded := convert.Int32AsBytes(int32(i))
	out = append([]byte{ConstTypeInt}, encoded...)
	return
}
