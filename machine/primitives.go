package machine

import (
	"github.com/prog-lang/hasky/runtime/convert"
)

const (
	ConstTypeInt byte = iota

	ConstListEnd //! signifies the end of the data section
)

var Nil Unit

type Unit struct{}

func (Unit) Object() {}

/* Constants */

type Int int32

func (Int) Object() {}

func (i Int) Encode() (out []byte, err error) {
	encoded := convert.Int32AsBytes(int32(i))
	out = append([]byte{ConstTypeInt}, encoded...)
	return
}
